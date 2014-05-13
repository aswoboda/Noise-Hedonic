


require(foreign)
require(multicore, quietly = TRUE)
require(fields, quietly = TRUE)

# the following command loads up some functions we'll use
source("helper/LWRfunctions.R")

load("~/NoiseHedonicProject/Data/R2GIS/CleanData/MixedLWR/step3bOutput.RData")

MYMODELS = c("YminusXaahat~FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+MED_INCOME+MCA3+LAKE_dist+PARK_dist+SHOP_dist+CBD_dist+factor(TimePeriod)+CITY")
KVECTOR = c(650)

N =  dim(DATAFRAME)[1]
obs2run = which(DATAFRAME$TimePeriod>11)

print(Sys.time())
for (modelNum in 1:1) { #
  MYMODEL = MYMODELS[modelNum]
  start = print(Sys.time())
  output.raw = mclapply(obs2run,
                        LWRtimelag,
                        Data.Frame = DATAFRAME,
                        my.model = MYMODEL, 
                        kvector = KVECTOR,
                        timelag = 24,
                        mc.cores = 16
  )
  end = print(Sys.time())
  print(end - start)
  names(output.raw) = DATAFRAME$UNIQID[obs2run]
  output = Reorganizer(output.raw)
  save(output, MYMODEL, file = "~/NoiseHedonicProject/Data/R2GIS/CleanData/MixedLWR/mixedStep4.RData")
  rm(output.raw)
  gc()
  print(gc())
}
