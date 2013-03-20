# The goal of this script is to run a basic LWR model using the St. Paul working data...

# What working directory will it be in?
# Try to put it in an "LWR" folder inside analysis.
# Remember to save the data to "Data/R2GIS"
require(foreign)
require(multicore, quietly = TRUE)
require(fields, quietly = TRUE)

# the following command loads up some functions we'll use
source("helper/LWRfunctions.R")

MYMODEL = "logSALE_VA~MAX+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+MED_INCOME+HOME_STYLE+ELEM"
MYMODELsmall = "logSALE_VA~MAX+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+MED_INCOME+HOME_STYLE"
KVECTOR = c(25, 50, 75, 100, 150, 200, 400, 600, 800, 1000)

filePrefix = "../Data/R2GIS/CleanData/"
# inputFile = "Sales20052010.dbf"
# DATAFRAME = read.dbf(paste0(filePrefix, inputFile))
# dakOBS = which(DATAFRAME$COUNTY_ID == "037" & DATAFRAME$BEDS > 0 & DATAFRAME$GARSQFT>175)
# dakDATA = DATAFRAME[dakOBS, ]
outputFile = "dakotaCLEAN.dbf"
# write.dbf(dakDATA, file = paste0(filePrefix, outputFile))
dakDATA = read.dbf(paste0(filePrefix, outputFile))

start = Sys.time()
obs2run = rownames(dakDATA)[which(dakDATA$TimePeriod>11)]
output.raw = mclapply(obs2run,
                      LWRyear2,
                      Data.Frame = dakDATA,
                      my.model = MYMODEL, my.modelSMALL = MYMODELsmall,
                      kvector = KVECTOR
                      )
end = Sys.time()
print(end - start)
dataSource = strsplit(inputFile, "\\.")[[1]][1]
# save the output in case something goes wrong later
save(output.raw, inputFile, MYMODEL, file = paste0(filePrefix, dataSource, "LWRoutputRAW", Sys.Date(), ".RData"))

names(output.raw) = DATAFRAME$UNIQID[obs2run] #dakDATA
output = Reorganizer(output.raw)
save(output, inputFile, MYMODEL, file = paste0(filePrefix, dataSource, "LWRoutput", Sys.Date(), ".RData"))
# if the save command works here, we can erase the earlier data file we saved as a precaution
