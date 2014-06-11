# The goal of this script is to run a basic LWR model using the St. Paul working data...

# What working directory will it be in?
# Try to put it in an "LWR" folder inside analysis.
# Remember to save the data to "Data/R2GIS"
require(foreign)
require(multicore, quietly = TRUE)
require(fields, quietly = TRUE)

# the following command loads up some functions we'll use
source("helper/LWRfunctions.R")

# BEDS + BATH + GARSQFT + get added or subtracted to compare the LWR model results with and without structural variables

MYMODELS = c("logSALE_VA~Air_Mean+BEDS + BATH + GARSQFT +FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+OWNOCC+factor(TimePeriod)",
             "logSALE_VA~Air_Mean+BEDS + BATH + GARSQFT +FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+OWNOCC+PercWhite+PercU18+MED_INCOME+MCA3+LAKE_dist+PARK_dist+SHOP_dist+CBD_dist+factor(TimePeriod)",
             "logSALE_VA~Air_Mean+BEDS + BATH + GARSQFT +FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+OWNOCC+PercWhite+PercU18+MED_INCOME+MCA3+LAKE_dist+PARK_dist+SHOP_dist+CBD_dist+factor(TimePeriod)+CITY")
MYMODELsmall = "logSALE_VA~Air_Mean+BEDS + BATH + GARSQFT +FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+OWNOCC+factor(TimePeriod)"
KVECTOR = c(25, 50, 75, 100, 150, 200, 400, 600, 800, 1000, 1500, 2000)

filePrefix = "../Data/R2GIS/CleanData/"
#   inputFile = "Sales20052010.dbf"
#   DATAFRAME = read.dbf(paste0(filePrefix, inputFile))
#   dakOBS = which(DATAFRAME$COUNTY_ID == "037" & DATAFRAME$BEDS > 0 &  DATAFRAME$BATH > 0 & DATAFRAME$GARSQFT>0 )
#   dakDATA = DATAFRAME[dakOBS, ]
# outputFile = "dakotaCLEAN.dbf"
# write.dbf(dakDATA, file = paste0(filePrefix, outputFile))
dakDATA = read.dbf(paste0(filePrefix, "dakotaCLEAN.dbf"))
obs2run = rownames(dakDATA)[which(dakDATA$TimePeriod>11)]

for (modelNum in 1:3) { #
  MYMODEL = MYMODELS[modelNum]
  start = Sys.time()

  output.raw = mclapply(obs2run,
                        LWRtimelag,
                        Data.Frame = dakDATA,
                        my.model = MYMODEL, my.modelSMALL = MYMODELsmall,
                        kvector = KVECTOR,
                        timelag = 12,
                        mc.cores = 16
                        )
  end = Sys.time()
  print(end - start)
  # save the output in case something goes wrong later ?
  # save(output.raw, inputFile, MYMODEL, file = paste0(filePrefix, dataSource, "LWRoutputRAW", Sys.Date(), ".RData"))

  names(output.raw) = DATAFRAME$UNIQID[obs2run] #dakDATA
  output = Reorganizer(output.raw)
  save(output, inputFile, MYMODEL, file = paste0(filePrefix, "dakotaLWRoutput", modelNum+3, "-", Sys.Date(), ".RData"))
  rm(output.raw)
  gc()
  print(gc())
}