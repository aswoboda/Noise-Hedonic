

# The goal of this script is to run a basic LWR model using the St. Paul working data...


# What working directory will it be in?
# Try to put it in an "LWR" folder inside analysis.
# Remember to save the data to "Data/R2GIS"
require(foreign)
require(multicore, quietly = TRUE)
mydata = read.dbf("../Data/R2GIS/CleanData/Sales20052007.dbf")
my.observation = 1
DATAFRAME = mydata
MYMODEL = "logSALE_VA~MAX+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT"
KVECTOR = c(50, 100, 200, 500, 1000, 5000)


Data.Frame = DATAFRAME
my.model = MYMODEL
kvector = KVECTOR

#print(summary(DATAFRAME))
# Model in quotes is default model, don't need anything entered for it to run
LWR = function(my.observation, 
               Data.Frame, 
               my.model = "dep.var~indep.var1i+ndep.var2",
               kvector) {
  #my.observation = 2
  #Data.Frame = test.data
  #my.model = "dep.var ~ indep.var1 + indep.var2"
  sample.size = dim(Data.Frame)[1]
  

  numK <- length(kvector)
  
  temp = strsplit(my.model, "~")
  temp2 = strsplit(temp[[1]][2], "\\+")
  numBetas = length(temp2[[1]]) + 1
  myvars = gsub(" ", "", temp2[[1]])
  
  
  # Creates containers for our parameters/metrics. 
  temp.est.betas = matrix(-99, numBetas, numK) # Need a matrix, row for each B, columns for each k
  temp.st.errors = matrix(-99, numBetas, numK) # Same as above
  temp.est.dep.var = matrix(-99, 1, numK ) # Matrix to be consistent with above, but only 1 value per k
  temp.leverage = matrix(-99, 1, numK) # Same as temp.est.dep.var
  temp.est.dep.var.without = matrix(-99, 1, numK)
  # Calculate distance
  # locations are "Long_X" and "Lat_Y" and are decimal degrees
  # first step is to calculate the distance all other observations are from this observation
  require(fields)
  
  Di=t(rdist.earth(cbind(Data.Frame$Long_X[my.observation], Data.Frame$Lat_Y[my.observation]),
                   cbind(Data.Frame$Long_X, Data.Frame$Lat_Y)))
  
  distance.to.k <- sort(Di)[kvector+1] # this populates the row of distances to the kth nearest neighbor
  
  # This loop may be eventually turned into an lapply function. 
  for (j in 1:numK) { # j is the position of our k in the kvector
    
    k <- kvector[j]
    
    # Calculate the appropriate weights for the observations for each k, using
    # previously calculated distance
    threshold = distance.to.k[j] # b is the threshold distance (distance to the k+1 th nearest observation to obs i)
    Weights = (1-(Di/threshold)^2)^2
    Weights[Di>threshold] = 0 
    Data.Frame$Weights = Weights
    
    lmreg = lm(my.model, data = Data.Frame, weights = Weights)
    
    
    temp.est.betas[,j] <- lmreg$coefficients # keep track of the coefficient estimate
    temp.st.errors[,j] <- summary(lmreg)$coefficients[,2]   # keep track the coefficient st error
    temp.est.dep.var[j] <- lmreg$fitted.values[my.observation] # keep track of the predicted value of y
    temp.leverage[j] <- lm.influence(lmreg)$hat[row.names(Data.Frame)[my.observation]] # the leverage value
    
    #Now we are going to exclude the observation itself.
    Data.Frame$Weights[my.observation] = 0
    lmreg = lm(my.model, data = Data.Frame, weights = Weights)
    
    temp.est.dep.var.without[j] = lmreg$fitted.values[my.observation] 
  }
  list(betas = temp.est.betas, st.errors = temp.st.errors, dep.vars = temp.est.dep.var, leverages = temp.leverage,
       dep.vars.without = temp.est.dep.var.without, bandwidths = kvector)
}


Reorganizer = function(lapplyoutput) {
  ### Write a function that takes as input the output from lapply(1:n, LWR, ...) 
  ### and reorganizes from a list of items for each observations
  ### into a list with an item for each type of thing we want to compare ...
  ### for instance, betahats for each variable, se's for each beta, dependent var est, leverage
  
  n = length(lapplyoutput) # essentially the number of observations from our dataset we ran LWR on
  ks = dim(lapplyoutput[[1]][[1]])[2] # should give us the number of ks we used running LWR
  # betas
  temp = sapply(lapplyoutput, "[", 1) # grabbing the estimated beta values
  temp1 = unlist(temp)
  beta0hats = matrix(temp1[seq(1, length(temp1), 3)], n, ks, byrow = T)
  beta1hats = matrix(temp1[seq(2, length(temp1), 3)], n, ks, byrow = T)
  beta2hats = matrix(temp1[seq(3, length(temp1), 3)], n, ks, byrow = T)
  
  # standard errors
  temp = sapply(lapplyoutput, "[", 2) # grabbing the estimated beta values
  temp1 = unlist(temp)
  ses0 = matrix(temp1[seq(1, length(temp1), 3)], n, ks, byrow = T)
  ses1 = matrix(temp1[seq(2, length(temp1), 3)], n, ks, byrow = T)
  ses2 = matrix(temp1[seq(3, length(temp1), 3)], n, ks, byrow = T)
  
  # dependent variable estimates with observation
  temp = sapply(lapplyoutput, "[", 3) # grabbing the estimated dependent variable values
  temp1 = unlist(temp)
  yhats = matrix(temp1, length(temp1)/ks, ks, byrow = T)
  
  # dependent variable estimates without observation
  temp = sapply(lapplyoutput, "[", 5) # grabbing the estimated dependent variable values
  temp1 = unlist(temp)
  yhats.without = matrix(temp1, length(temp1)/ks, ks, byrow = T)
  
  # leverage values
  temp = sapply(lapplyoutput, "[", 4) # grabbing the estimated dependent variable values
  temp1 = unlist(temp)
  leverages = matrix(temp1, length(temp1)/ks, ks, byrow = T)
  
  # put everything together as output for the function
  list(beta0hats = beta0hats, beta1hats = beta1hats, beta2hats = beta2hats,
       ses0 = ses0, ses1 = ses1, ses2 = ses2,
       yhats = yhats, leverages = leverages, yhats.without = yhats.without, bandwidths = lapplyoutput[[1]]$bandwidths)
}

start = Sys.time()
output = mclapply(1:100, # dim(DATAFRAME)[1], 
                  LWR, 
                  Data.Frame = DATAFRAME, 
                  my.model = MYMODEL, 
                  kvector = KVECTOR ,
                  mc.cores = 16
                )
end = Sys.time()
print(end - start)

save.image("LWR20130201.RData")

#OUTPUT = Reorganizer(output)