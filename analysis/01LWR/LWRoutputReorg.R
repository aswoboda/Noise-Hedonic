

# I've had trouble running the Reorganizer() function to reorganize the output from 
# the LWR function with the elementary school fixed effects. 

# This script aims to run a few commands and save the data via a nohup command via SSH

require(foreign)

source("helper/LWRfunctions.R")

MYMODEL = "logSALE_VA~log(MAX)+FIN_SQ_FT+ACRES_POLY+I(ACRES_POLY^2)+YEAR_BUILT+factor(TimePeriod)+MED_INCOME+ELEM"
MYMODELsmall = "logSALE_VA~log(MAX)+FIN_SQ_FT+ACRES_POLY+I(ACRES_POLY^2)+YEAR_BUILT+factor(TimePeriod)+MED_INCOME"
KVECTOR = c(50, 75, 100, 150, 200, 500, 1000, 2000)

filePrefix = "../Data/R2GIS/CleanData/"
inputFile = "Sales20052010.dbf"
DATAFRAME = read.dbf(paste0(filePrefix, inputFile))
start = Sys.time()
N =  dim(DATAFRAME)[1]
obs2run = which(DATAFRAME$TimePeriod>11)

# here's where we normally ran the LWR code, instead let's load the results
load("~/Noise Hedonic Project/Data/R2GIS/CleanData/Sales20052010LWRoutputRAW2013-03-14.RData")
names(output.raw) = DATAFRAME$UNIQID[obs2run]
output = Reorganizer(output.raw)
dataSource = strsplit(inputFile, "\\.")[[1]][1]
save(output, inputFile, MYMODEL, MYMODELsmall, file = paste0(filePrefix, dataSource, "LWRoutput", Sys.Date(), ".RData"))

