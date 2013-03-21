

# starting to make figures comparing the LWR coefficients over time

require(foreign)
source("helper/LWRfunctions.R")

filePrefix = "../Data/R2GIS/CleanData/"
inputFile = "Sales20052010.dbf"
DATAFRAME = read.dbf(paste0(filePrefix, inputFile))
obs2run = which(DATAFRAME$TimePeriod>11)



