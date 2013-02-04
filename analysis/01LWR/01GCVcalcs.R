

load("~/Noise Hedonic Project/Data/R2GIS/LWRoutput/Sales20052007LWRoutput2013-02-03.RData")
filePrefix = "../Data/R2GIS/CleanData/"
inputFile = "Sales20052007.dbf"
DATAFRAME = read.dbf(paste0(filePrefix, inputFile))

GCV(output$leverages, output$yhats, DATAFRAME$logSALE_VA)

OLS.lm = lm(MYMODEL, data = DATAFRAME)

OLS.leverages = lm.influence(OLS.lm)$hat
GCV(matrix(OLS.leverages, ncol = 1), matrix(OLS.lm$fitted.values, ncol= 1), DATAFRAME$logSALE_VA)

