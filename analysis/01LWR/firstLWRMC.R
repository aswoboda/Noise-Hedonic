# The goal of this script is to run a basic LWR model using the St. Paul working data...

# What working directory will it be in?
# Try to put it in an "LWR" folder inside analysis.
# Remember to save the data to "Data/R2GIS"
require(foreign)
require(multicore, quietly = TRUE)
require(fields, quietly = TRUE)

# the following command loads up some functions we'll use
source("helper/LWRfunctions.R")

MYMODEL = "logSALE_VA~MAX+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT"
KVECTOR = c(100, 200, 500, 1000, 4000)

filePrefix = "../Data/R2GIS/CleanData/"
inputFile = "Sales20052010.dbf"
DATAFRAME = read.dbf(paste0(filePrefix, inputFile))
start = Sys.time()
N =  dim(DATAFRAME)[1]
output.raw = mclapply(1:N,
                      LWRyear,
                      Data.Frame = DATAFRAME,
                      my.model = MYMODEL,
                      kvector = KVECTOR
                      )
end = Sys.time()
print(end - start)
dataSource = strsplit(inputFile, "\\.")[[1]][1]
# save the output in case something goes wrong later
save(output.raw, inputFile, MYMODEL, file = paste0(filePrefix, dataSource, "LWRoutputRAW", Sys.Date(), ".RData"))

names(output.raw) = DATAFRAME$PIN[1:N]
output = Reorganizer(output.raw)
save(output.raw, output, inputFile, MYMODEL, file = paste0(filePrefix, dataSource, "LWRoutput", Sys.Date(), ".RData"))
# if the save command works here, we can erase the earlier data file we saved as a precaution
