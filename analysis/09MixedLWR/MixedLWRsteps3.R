
require(foreign)
require(multicore, quietly = TRUE)
require(fields, quietly = TRUE)

# the following command loads up some functions we'll use
source("helper/LWRfunctions.R")


MYMODELS = c("priceHAT ~ noiseHAT + ownoccHAT + percwhiteHAT + perc18HAT")

KVECTOR = c(650)

filePrefix = "../Data/R2GIS/CleanData/"

load("../Data/R2GIS/CleanData/MixedLWR/mixedDATA.RData")
N =  dim(DATAFRAME)[1]
obs2run = which(DATAFRAME$TimePeriod>0)

# Step 3 of the mixed LWR model

print(Sys.time())
for (modelNum in 1:1) { #
  MYMODEL = MYMODELS[modelNum]
  start = print(Sys.time())
  output.raw = mclapply(obs2run,
                        TimelagReg,
                        Data.Frame = DATAFRAME,
                        my.model = MYMODEL, 
                        timelag = 12,
                        mc.cores = 16
  )
  end = print(Sys.time())
  print(end - start)
  names(output.raw) = DATAFRAME$UNIQID[obs2run]
  output = Reorganizer(output.raw)
  save(output, inputFile, MYMODEL, file = paste0(filePrefix, "MixedLWR/step3aoutput", "-", Sys.Date(), ".RData"))
}