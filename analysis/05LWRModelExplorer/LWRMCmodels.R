# The goal of this script is to run multiple LWR models using the St. Paul working data to find the best model

# What working directory will it be in?
# Remember to save the data to "Data/R2GIS"
require(foreign)
require(multicore, quietly = TRUE)
require(fields, quietly = TRUE)

# the following command loads up some functions we'll use
source("helper/LWRfunctions.R")

MYMODELS = c("logSALE_VA~MAX+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+OWNOCC",
             "logSALE_VA~MAX+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+OWNOCC+MED_INCOME",
             "logSALE_VA~MAX+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+OWNOCC+MED_INCOME+MCA3",
             "logSALE_VA~MAX+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+OWNOCC+MED_INCOME+MCA3+PARK_dist",
             "logSALE_VA~MAX+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+OWNOCC+MED_INCOME+MCA3+LAKE_dist",
             "logSALE_VA~MAX+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+OWNOCC+MED_INCOME+MCA3+SHOP_dist",
             "logSALE_VA~MAX+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+OWNOCC+MED_INCOME+MCA3+COLLEGE_di",
             "logSALE_VA~MAX+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+OWNOCC+MED_INCOME+MCA3+CBD_dist")
KVECTOR = c(25, 50, 75, 100, 150, 200, 400, 600, 800, 1000)

filePrefix = "../Data/R2GIS/CleanData/"
inputFile = "Sales20052010.dbf"
dataSource = strsplit(inputFile, "\\.")[[1]][1]

DATAFRAME = read.dbf(paste0(filePrefix, inputFile))
N =  dim(DATAFRAME)[1]
obs2run = which(DATAFRAME$TimePeriod>11)

for (modelNum in 1:length(MYMODELS)) {
  MYMODEL = MYMODELS[modelNum]
  start = Sys.time()
  output.raw = mclapply(obs2run,
                        LWRyear2,
                        Data.Frame = DATAFRAME,
                        my.model = MYMODEL, my.modelSMALL = MYMODELsmall,
                        kvector = KVECTOR
  )
  end = Sys.time()
  print(end - start)

  names(output.raw) = DATAFRAME$UNIQID[obs2run]
  output = Reorganizer(output.raw)
  save(output, inputFile, MYMODEL, file = paste0(filePrefix, dataSource, "LWRmodel", modelNum, "-", Sys.Date(), ".RData"))
  # if the save command works here, we can erase the earlier data file we saved as a precaution
}

