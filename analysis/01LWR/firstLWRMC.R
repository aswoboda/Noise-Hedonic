

# The goal of this script is to run a basic LWR model using the St. Paul working data...


# What working directory will it be in?
# Try to put it in an "LWR" folder inside analysis.
# Remember to save the data to "Data/R2GIS"
require(foreign)
require(multicore, quietly = TRUE)
require(fields, quietly = TRUE)

LWR = function(my.observation, 
               Data.Frame, 
               my.model,
               kvector) {
  
  # These four lines let you work within the function rather than having to run the function (helpful for debugging)
#     Data.Frame = DATAFRAME
#     my.model = MYMODEL
#     kvector = KVECTOR
#     my.observation = 3
  
  # grab some dimensions for creating our containers
  sample.size = dim(Data.Frame)[1]
  numK <- length(kvector)
  
  # temp = strsplit(my.model, "~")
  # temp2 = strsplit(temp[[1]][2], "\\+")
  # numBetas = length(temp2[[1]]) + 1
  # myvars = gsub(" ", "", temp2[[1]])
  lmreg = lm(my.model, data = Data.Frame)
  numBetas = length(lmreg$coefficients)
  coefNames = names(lmreg$coefficients)
  
  # Creates containers for our parameters/metrics. 
  temp.est.betas = matrix(-99, numBetas, numK) # Need a matrix, row for each B, columns for each k
  rownames(temp.est.betas) = coefNames
  colnames(temp.est.betas) = kvector
  temp.st.errors = matrix(-99, numBetas, numK) # Same as above
  rownames(temp.st.errors) = coefNames
  colnames(temp.st.errors) = kvector  
  temp.est.dep.var = matrix(-99, 1, numK ) # Matrix to be consistent with above, but only 1 value per k
  colnames(temp.est.dep.var) = kvector
  temp.leverage = matrix(-99, 1, numK) # Same as temp.est.dep.var
  colnames(temp.leverage) = kvector
  temp.est.dep.var.without = matrix(-99, 1, numK)
  colnames(temp.est.dep.var.without) = kvector
  
  # Before we run LWR on an observation using different bandwidths we calculate distances
  # locations are expected to be "Long_X" and "Lat_Y" and are decimal degrees
  # require(fields)
  
  Di=t(rdist.earth(cbind(Data.Frame$Long_X[my.observation], Data.Frame$Lat_Y[my.observation]),
                   cbind(Data.Frame$Long_X, Data.Frame$Lat_Y)))
  
  distance.to.k <- sort(Di)[kvector+1] # this populates the row of distances to the kth nearest neighbor
  
  # Could/should we turn this loop into an lapply function?
  # Now march through the different bandwidths running the regressions and collecting important output
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
  outputList = list()
  
  n = length(lapplyoutput) # essentially the number of observations from our dataset we ran LWR on
  kvalues = paste0("k",colnames(lapplyoutput[[1]][[1]]))
  ks = length(kvalues) # should give us the number of ks we used running LWR
  # betas
  temp = sapply(lapplyoutput, "[", 1) # grabbing the estimated beta values
  betaNames = rownames(temp[[1]])
  # temp is now a list of length n, with each element a matrix of coefficients (rows = coefs, cols = ks)
  nCoefs = nrow(temp[[1]])
  temp1 = unlist(temp)
  for (i in 1: nCoefs) {
    nam = paste("beta", betaNames[i], sep = ".")
    temp.Mat = matrix(temp1[seq(i, length(temp1), nCoefs)], n, ks, byrow = T)
    # matrix with rows for each observation and column for each k
    colnames(temp.Mat) = kvalues
    rownames(temp.Mat) = names(lapplyoutput)
    outputList[[nam]] = temp.Mat
  }
  
  # standard errors
  temp = sapply(lapplyoutput, "[", 2) # grabbing the estimated beta values
  temp1 = unlist(temp)
  for (i in 1: nCoefs) {
    nam = paste("ses", betaNames[i], sep = ".")
    temp.Mat = matrix(temp1[seq(i, length(temp1), nCoefs)], n, ks, byrow = T)
    # matrix with rows for each observation and column for each k
    colnames(temp.Mat) = kvalues
    rownames(temp.Mat) = names(lapplyoutput)
    outputList[[nam]] = temp.Mat
  }
  
  # dependent variable estimates with observation
  temp = sapply(lapplyoutput, "[", 3) # grabbing the estimated dependent variable values
  temp1 = unlist(temp)
  temp.Mat = matrix(temp1, length(temp1)/ks, ks, byrow = T)
  # matrix with rows for each observation and column for each k
  colnames(temp.Mat) = kvalues
  rownames(temp.Mat) = names(lapplyoutput)
  outputList[["yhats"]] = temp.Mat
  
  # dependent variable estimates without observation
  temp = sapply(lapplyoutput, "[", 5) # grabbing the estimated dependent variable values
  temp1 = unlist(temp)
  temp.Mat = matrix(temp1, length(temp1)/ks, ks, byrow = T)
  # matrix with rows for each observation and column for each k
  colnames(temp.Mat) = kvalues
  rownames(temp.Mat) = names(lapplyoutput)  
  outputList[["yhats.without"]] = temp.Mat
  
  # leverage values
  temp = sapply(lapplyoutput, "[", 4) # grabbing the estimated dependent variable values
  temp1 = unlist(temp)
  temp.Mat = matrix(temp1, length(temp1)/ks, ks, byrow = T)
  # matrix with rows for each observation and column for each k
  colnames(temp.Mat) = kvalues
  rownames(temp.Mat) = names(lapplyoutput) 
  outputList[["leverages"]] = temp.Mat
  
  outputList
}

MYMODEL = "logSALE_VA~MAX+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT"
KVECTOR = c(100, 200, 500, 1000, 5000)

filePrefix = "../Data/R2GIS/CleanData/"
inputFile = "Sales20082010.dbf"
DATAFRAME = read.dbf(paste0(filePrefix, inputFile))
start = Sys.time()
N = dim(DATAFRAME)[1]
output.raw = mclapply(1:N,
                      LWR,
                      Data.Frame = DATAFRAME,
                      my.model = MYMODEL,
                      kvector = KVECTOR
                      )
end = Sys.time()
print(end - start)
dataSource = strsplit(inputFile, "\\.")[[1]][1]
save(output.raw, inputFile, MYMODEL, file = paste0(filePrefix, dataSource, "LWRoutputRAW", Sys.Date(), ".RData"))

names(output.raw) = DATAFRAME$PIN[1:N]
output = Reorganizer(output.raw)
save(output.raw, output, inputFile, MYMODEL, file = paste0(filePrefix, dataSource, "LWRoutput", Sys.Date(), ".RData"))


# Move on the the 2008 - 10 data
#inputFile = "Sales20082010.dbf"
#DATAFRAME = read.dbf(paste0(filePrefix, inputFile))
