# The goal of this script is to run a basic LWR model using the St. Paul working data...

# What working directory will it be in?
# Try to put it in an "LWR" folder inside analysis.
# Remember to save the data to "Data/R2GIS"
require(foreign)
require(multicore, quietly = TRUE)
require(fields, quietly = TRUE)

# the following command loads up some functions we'll use
source("helper/LWRfunctions.R")

MYMODEL = "logSALE_VA~log(MAX)+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+MED_INCOME+MCA5+CITY"
MYMODELsmall = "logSALE_VA~log(MAX)+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+MED_INCOME+MCA5"
KVECTOR = c(25, 50, 75, 100, 150, 200, 400, 600, 800, 1000)

filePrefix = "../Data/R2GIS/CleanData/"
inputFile = "Sales20052010.dbf"
DATAFRAME = read.dbf(paste0(filePrefix, inputFile))
start = Sys.time()
N =  dim(DATAFRAME)[1]
obs2run = which(DATAFRAME$TimePeriod>11)
output.raw = mclapply(obs2run,
                      LWRyear2,
                      Data.Frame = DATAFRAME,
                      my.model = MYMODEL, my.modelSMALL = MYMODELsmall,
                      kvector = KVECTOR
                      )
end = Sys.time()
print(end - start)
dataSource = strsplit(inputFile, "\\.")[[1]][1]
# save the output in case something goes wrong later
save(output.raw, inputFile, MYMODEL, file = paste0(filePrefix, dataSource, "LWRoutputRAW", Sys.Date(), ".RData"))

names(output.raw) = DATAFRAME$UNIQID[obs2run]
output = Reorganizer(output.raw)
save(output, inputFile, MYMODEL, file = paste0(filePrefix, dataSource, "LWRoutput", Sys.Date(), ".RData"))
# if the save command works here, we can erase the earlier data file we saved as a precaution
