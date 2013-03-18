# Playing with the LWRyear function to have it run with data from the past year and including city dummies

MYMODEL = "logSALE_VA~MAX+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+MED_INCOME+MCA5"
KVECTOR = c(100, 200, 500, 1000, 4000)


LWRyear2 = function(my.observation, 
                   Data.Frame, 
                   my.model, 
                   kvector) {
  
  # These four lines let you work within the function rather than having to run the function (helpful for debugging)
#         Data.Frame = DATAFRAME
#         my.model = MYMODEL#; my.modelSMALL = 
#         kvector = KVECTOR
#         my.observation = 6
  print(my.observation)
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
  
  # Create data.frames for our coefficients and standard errors
  coeffs = data.frame(t(lmreg$coefficients))  
  coeffs = coeffs[-1, ]
        
  ses = data.frame(t(summary(lmreg)$coefficients[, 2]))  
  ses = ses[-1, ]

  temp.est.dep.var = matrix(-99, 1, numK ) # Matrix to be consistent with above, but only 1 value per k
  colnames(temp.est.dep.var) = kvector
        
  temp.leverage = matrix(-99, 1, numK) # Same as temp.est.dep.var
  colnames(temp.leverage) = kvector
        
  temp.est.dep.var.without = matrix(-99, 1, numK)
  colnames(temp.est.dep.var.without) = kvector
  
  # Before we run LWR on an observation using different bandwidths ...
  # we need to restrict the dataset to only those houses sold in the same year as our observation
  # then we calculate distances (locations are expected to be "Long_X" and "Lat_Y" and are decimal degrees)
  # require(fields)
  Data.Frame = subset(Data.Frame, TimePeriod<Data.Frame[as.character(my.observation), "TimePeriod"]+1 & 
    TimePeriod>Data.Frame[as.character(my.observation), "TimePeriod"]-13)
  
  Di=t(rdist.earth(cbind(Data.Frame[as.character(my.observation), "Long_X"], Data.Frame[as.character(my.observation), "Lat_Y"]),
                   cbind(Data.Frame$Long_X, Data.Frame$Lat_Y)))
  
  distance.to.k <- sort(Di)[kvector+1] # this populates the row of distances to the kth nearest neighbor
  distance.to.k[is.na(distance.to.k)] = max(Di)
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
    
    lmreg = try(lm(my.model, data = Data.Frame, weights = Weights))
    #if(class(lmreg) == "try-error") lmreg = lm(my.modelSMALL, data = Data.Frame, weights = Weights)
    # print(summary(lmreg)$r.squared)
    
    temp.betas <- data.frame(t(lmreg$coefficients)) # keep track of the coefficient estimate
    coeffs = merge(coeffs, temp.betas, all = T, sort = F)
    temp.ses <- data.frame(t(summary(lmreg)$coefficients[, 2]))   # keep track the coefficient st errors
    ses = merge(ses, temp.ses, all = T, sort = F)
    temp.est.dep.var[j] <- lmreg$fitted.values[as.character(my.observation)] # keep track of the predicted value of y
    temp.leverage[j] <- lm.influence(lmreg)$hat[as.character(my.observation)] # the leverage value
    
    #Now we are going to exclude the observation itself.
    Data.Frame[as.character(my.observation), "Weights"] = 0
    lmreg = lm(my.model, data = Data.Frame, weights = Weights)
    
    temp.est.dep.var.without[j] = lmreg$fitted.values[as.character(my.observation)] 
  }
        coeffs = coeffs[, coefNames]
        temp.est.betas = matrix(t(coeffs), nrow = length(coefNames), ncol = length(kvector)) # Need a matrix, row for each B, columns for each k
        rownames(temp.est.betas) = coefNames
        colnames(temp.est.betas) = kvector
        ses = ses[, coefNames]
        temp.st.errors = matrix(t(ses), nrow = length(coefNames), ncol = length(kvector)) # Same as above
        rownames(temp.st.errors) = coefNames
        colnames(temp.st.errors) = kvector  
        
  list(betas = temp.est.betas, st.errors = temp.st.errors, dep.vars = temp.est.dep.var, leverages = temp.leverage,
       dep.vars.without = temp.est.dep.var.without, bandwidths = kvector)
}