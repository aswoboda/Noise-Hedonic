# The goal of this script is to run multiple LWR models using the St. Paul working data to find the best model

require(foreign)
require(multicore, quietly = TRUE)
require(fields, quietly = TRUE)

# the following command loads up some functions we'll use
source("helper/LWRfunctions.R")

MYMODELS = c("logSALE_VA~MAX+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+OWNOCC",
             "logSALE_VA~MAX+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+OWNOCC+MED_INCOME+MCA3+LAKE_dist+PARK_dist+SHOP_dist+CBD_dist",
             "logSALE_VA~MAX+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+OWNOCC+MED_INCOME+MCA3+LAKE_dist+PARK_dist+SHOP_dist+CBD_dist+CITY")
KVECTOR = c(25, 50, 75, 100, 125, 150, 175, 200, 225, 300, 400, 600, 800, 1000, 2000, 4000)

filePrefix = "../Data/R2GIS/CleanData/"
inputFile = "Sales20052010.dbf"
dataSource = strsplit(inputFile, "\\.")[[1]][1]

DATAFRAME = read.dbf(paste0(filePrefix, inputFile))
N =  dim(DATAFRAME)[1]

TIMELAGS = c(6, 12, 24, 9, 4)
inputVARS = expand.grid(models = MYMODELS, lags = TIMELAGS)

for (combo in 13:15) { #
  MYMODEL = as.character(inputVARS$models[combo])
  TIMELAG = inputVARS$lags[combo]
  obs2run = which(DATAFRAME$TimePeriod>(TIMELAG-1))
  start = Sys.time()
  output.raw = mclapply(obs2run,
                        LWRtimelag,
                        Data.Frame = DATAFRAME,
                        my.model = MYMODEL, 
                        my.modelSMALL = "logSALE_VA~MAX+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+OWNOCC+MED_INCOME+MCA3+LAKE_dist+PARK_dist+SHOP_dist+CBD_dist", #substr(MYMODEL, 1, nchar(MYMODEL)-5),
                        kvector = KVECTOR,
                        timelag = TIMELAG,
                        mc.cores = 16
  )
  end = Sys.time()
  print(end - start)
  
  names(output.raw) = DATAFRAME$UNIQID[obs2run]
  output = Reorganizer(output.raw)
  save(output, inputFile, MYMODEL, TIMELAG, file = paste0(filePrefix, dataSource, "LWRmodel", combo, "-", Sys.Date(), ".RData"))
  rm(output, output.raw)
  gc()
  print(gc())
}
