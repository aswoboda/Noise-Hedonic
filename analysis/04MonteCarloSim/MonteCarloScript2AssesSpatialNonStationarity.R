# Run a Monte Carlo experiment to see whether coefficients really vary over space

require(foreign)
require(multicore)
require(fields, quietly = TRUE)
# the following command loads up some functions we'll use
source("helper/LWRfunctions.R")

myVars = c("Air_Mean", "FIN_SQ_FT", "ACRES_POLY", "YEAR_BUILT", "OWNOCC", "MED_INCOME", "MCA3",
           "LAKE_dist", "PARK_dist", "SHOP_dist", "CBD_dist", "PercWhite", "PercU18", 
           "HOME_STYLE", "factor(TimePeriod)", "CITY")
RHS = paste(myVars, collapse = "+")
MYMODEL = paste("logSALE_VA", RHS, sep = "~")
MYMODELsmall = substr(MYMODEL, 1, nchar(MYMODEL)-5)
KVECTOR = c(1000) # c(200, 400, 650, 1000, 2000, 4000)

# How many times am I going to reshuffle?
iterations = 50 #100
# How many things am I keeping track of each reshuffle? 
# mean and sd of each coefficient i care about + intercept + GCV score + min bandwidth
vars2keep = c("Intercept", myVars[c(1:(length(myVars)-3))])
numMCstats = 2 + 2*length(vars2keep)
MCstats = matrix(NA, 100, numMCstats)
colnames(MCstats) = c("minGCV", "optimalBandwidth", 
                      paste0("meanBeta.", vars2keep),
                      paste0("sterBeta.", vars2keep))
# mcstats = read.csv("analysis/04MonteCarloSim/Revision/Model3/CopyOfLWRMonteCarloStats2014-06-20.csv")
# MCstats = as.matrix(mcstats)

filePrefix = "../Data/R2GIS/CleanData/"
inputFile = "Sales20052010.dbf"
DATAFRAME = read.dbf(paste0(filePrefix, inputFile))
simDATA = DATAFRAME
N = dim(simDATA)[1]
obs2run = which(simDATA$TimePeriod>11)
for (iter in 1:iterations) {
  # Do a reshuffle
  rowShuffle = sample(1:N)
  simDATA[, c("Long_X", "Lat_Y")] = DATAFRAME[rowShuffle, c("Long_X", "Lat_Y")]
  start = Sys.time()
  
  output.raw = mclapply(obs2run,
                        LWRtimelag,
                        Data.Frame = simDATA,
                        my.model = MYMODEL, my.modelSMALL = MYMODELsmall,
                        kvector = KVECTOR,
                        timelag = 12,
                        mc.cores = 16
  )
  names(output.raw) = simDATA$UNIQID[obs2run]
  output = Reorganizer(output.raw)
  
  gcvs = GCV(leverages = output$leverages, 
             yhats = output$yhats, 
             dep.var = simDATA[obs2run, "logSALE_VA"])
  minGCVnumber = which.min(gcvs)
  minGCV = gcvs[minGCVnumber]
  optimalBandwidth = KVECTOR[minGCVnumber]
  
  MCstats[iter, "minGCV"] = minGCV
  MCstats[iter, "optimalBandwidth"] = optimalBandwidth
  for (i in 1:length(vars2keep)) {
    MCstats[iter, i+2] = mean(output[[i]][, minGCVnumber])
    MCstats[iter, i+2+length(vars2keep)] = sd(output[[i]][, minGCVnumber])
  }
  end = print(Sys.time())
  print(paste("iteration ", iter, " took "))
  print(end - start)
  write.csv(MCstats, file = paste0("~/NoiseHedonicProject/Noise-Hedonic/analysis/04MonteCarloSim/Revision/Model3/LWRMonteCarloStats", Sys.Date(), ".csv"), row.names = FALSE)
  rm(output, output.raw)
  gc()
  print(gc())
}
