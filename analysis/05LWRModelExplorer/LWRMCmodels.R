# The goal of this script is to run multiple LWR models using the St. Paul working data to find the best model

require(foreign)
require(multicore, quietly = TRUE)
require(fields, quietly = TRUE)

# the following command loads up some functions we'll use
source("helper/LWRfunctions.R")

MYMODELS = c("logSALE_VA~MAX+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+OWNOCC+CITY",
             "logSALE_VA~MAX+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+OWNOCC+MED_INCOME+CITY",
             "logSALE_VA~MAX+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+OWNOCC+MED_INCOME+MCA3+CITY",
             "logSALE_VA~MAX+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+OWNOCC+MED_INCOME+MCA3+PARK_dist+CITY",
             "logSALE_VA~MAX+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+OWNOCC+MED_INCOME+MCA3+LAKE_dist+CITY",
             "logSALE_VA~MAX+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+OWNOCC+MED_INCOME+MCA3+SHOP_dist+CITY",
             "logSALE_VA~MAX+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+OWNOCC+MED_INCOME+MCA3+COLLEGE_di+CITY",
             "logSALE_VA~MAX+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+OWNOCC+MED_INCOME+MCA3+CBD_dist+CITY")
KVECTOR = c(25, 50, 75, 100, 150, 200, 400, 600, 800, 1000)

filePrefix = "../Data/R2GIS/CleanData/"
inputFile = "Sales20052010.dbf"
dataSource = strsplit(inputFile, "\\.")[[1]][1]

DATAFRAME = read.dbf(paste0(filePrefix, inputFile))
N =  dim(DATAFRAME)[1]
obs2run = which(DATAFRAME$TimePeriod>11)

for (modelNum in 1:length(MYMODELS)) { #
  MYMODEL = MYMODELS[modelNum]
  start = Sys.time()
  output.raw = mclapply(obs2run,
                        LWRtimelag,
                        Data.Frame = DATAFRAME,
                        my.model = MYMODEL, my.modelSMALL = substr(MYMODEL, 1, nchar(MYMODEL)-5),
                        kvector = KVECTOR,
                        timelag = 12,
                        mc.cores = 16
                        )
  end = Sys.time()
  print(end - start)

  names(output.raw) = DATAFRAME$UNIQID[obs2run]
  output = Reorganizer(output.raw)
  save(output, inputFile, MYMODEL, file = paste0(filePrefix, dataSource, "LWRmodel", modelNum+8, "-", Sys.Date(), ".RData"))
  rm(output, output.raw)
  gc()
  print(gc())
}
