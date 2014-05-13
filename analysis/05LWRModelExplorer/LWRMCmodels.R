# The goal of this script is to run multiple LWR models using the St. Paul working data to find the best model

require(foreign)
require(multicore, quietly = TRUE)
require(fields, quietly = TRUE)

# the following command loads up some functions we'll use
source("helper/LWRfunctions.R")

MYMODELS = c("logSALE_VA~Air_Mean+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+OWNOCC+factor(TimePeriod)",
             "logSALE_VA~Air_Mean+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+OWNOCC+PercWhite+PercU18+MED_INCOME+MCA3+LAKE_dist+PARK_dist+SHOP_dist+CBD_dist+factor(TimePeriod)",
             "logSALE_VA~Air_Mean+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+OWNOCC+PercWhite+PercU18+MED_INCOME+MCA3+LAKE_dist+PARK_dist+SHOP_dist+CBD_dist+factor(TimePeriod)+CITY")
KVECTOR = c(100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650, 700, 800, 1000, 1500, 2000, 3000, 4000)

filePrefix = "../Data/R2GIS/CleanData/"
inputFile = "Sales20052010.dbf"
dataSource = strsplit(inputFile, "\\.")[[1]][1]

DATAFRAME = read.dbf(paste0(filePrefix, inputFile))
N =  dim(DATAFRAME)[1]
obs2run = which(DATAFRAME$TimePeriod>11)

print(Sys.time())
for (modelNum in 1:3) { #
  MYMODEL = MYMODELS[modelNum]
  start = print(Sys.time())
  output.raw = mclapply(obs2run,
                        LWRtimelag,
                        Data.Frame = DATAFRAME,
                        my.model = MYMODEL, 
                        my.modelSMALL = "logSALE_VA~Air_Mean+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+OWNOCC+PercWhite+PercU18+MED_INCOME+MCA3+LAKE_dist+PARK_dist+SHOP_dist+CBD_dist+factor(TimePeriod)",
                        kvector = KVECTOR,
                        timelag = 24,
                        mc.cores = 16
                        )
  end = print(Sys.time())
  print(end - start)
  names(output.raw) = DATAFRAME$UNIQID[obs2run]
  #output = output.raw #Reorganizer(output.raw)
  save(output.raw, inputFile, MYMODEL, file = paste0(filePrefix, "TimeLag24months/", "tempLWRmodelAirMean", modelNum, "-", Sys.Date(), ".RData"))
  rm(output.raw)
  gc()
  print(gc())
}


# source("helper/LWRfunctions.R")
# 
# dataPath = "../Data/R2GIS/CleanData/MixedLWR/"
# filelist = print(list.files(path = dataPath))
# 
# i = 5
# myFile = filelist[i]
# load(paste0(dataPath, myFile, sep = ""))
# output = Reorganizer(output.raw)
# save(output, MYMODEL, file = paste0(dataPath, substr(myFile, 5, 100)))
# print("done")
# print("really")