
# Goal here is to create the GCV plots by model and time lag

# 3 plots (one for each time lag) with 3 lines in the plot (one for each model)
# 3 plots (one for each model) with 3 lines in the plot (one for each time lag)


# need to create a matrix/data.frame/array for the nine different time lag and model combinations

setwd("~/NoiseHedonicProject/Data/R2GIS/CleanData/")
file.list = list.files()

myfiles = file.list[which(substr(file.list, nchar(file.list)-15, nchar(file.list)-6 ) == "2013-06-23")]

source("~/NoiseHedonicProject/Noise-Hedonic/helper/LWRfunctions.R")
require(foreign)
DATAFRAME = read.dbf("~/NoiseHedonicProject/Data/R2GIS/CleanData/Sales20052010.dbf")
load(myfiles[1])

obs2run = which(DATAFRAME$TimePeriod>(TIMELAG-1))

gcvs = GCV(output$leverages, output$yhats, DATAFRAME$logSALE_VA[obs2run])
ks = as.numeric(substr(names(gcvs), 2, 6))

plot(ks, gcvs, type = "o", pch = 16, cex = .5, las = 1)
text(3000, mean(gcvs), paste("optimal bandwidth =", ks[which.min(gcvs)]))
text(3000, min(gcvs), paste("GCV =", round(min(gcvs), 4)), pos = 3)
mtext(MYMODEL, 3, line = 3, cex = .4)
mtext(TIMELAG, 3, line = 1)