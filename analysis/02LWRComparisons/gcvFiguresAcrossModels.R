

getwd() #assume I'm in the project default working directory "Noise Hedonic Project"

require(foreign)
source("helper/LWRfunctions.R")

filePrefix = "../Data/R2GIS/CleanData/"
inputFile = "Sales20052010.dbf"
DATAFRAME = read.dbf(paste0(filePrefix, inputFile))
obs2run = which(DATAFRAME$TimePeriod>11)

dataPath = "../Data/R2GIS/LWRoutput/"
filelist = list.files(path = dataPath)

files2open = filelist[which(substr(filelist, 1, 21) == "Sales20052010LWRmodel")]

pdf("analysis/02LWRComparisons/GCVplot.pdf")
for (myFile in 1:length(files2open)) {
  load(paste0(dataPath, files2open[myFile], sep = ""))
  if (dim(output[[1]])[1] == 42095) gcvs = GCV(output$leverages[obs2run, ], output$yhats[obs2run, ], DATAFRAME$logSALE_VA[obs2run])
  if (dim(output[[1]])[1] == 31737) gcvs = GCV(output$leverages, output$yhats, DATAFRAME$logSALE_VA[obs2run])
#   print(files2open[myFile])
#   print(min(gcvs))
  ks = as.numeric(substr(colnames(output[[1]]), 2, 5))
  plot(ks, gcvs, type = "l", main = "")
  title(MYMODEL, cex.main = .5)
  title(paste0("N = ", dim(output[[1]])[1]), line = .5)
  title(paste("data: ", substr(files2open[myFile], 28, nchar(files2open[myFile]))), line = -1)
  lmreg = lm(MYMODEL, data = DATAFRAME[obs2run, ])
  levs = lm.influence(lmreg, do.coef=FALSE)$hat
  globalGCV = GCV(cbind(levs), cbind(lmreg$fitted.values), DATAFRAME$logSALE_VA[obs2run])
  title(paste("global model gcv:", round(globalGCV, 3)), line = -2)
  title(paste("min gcv:", round(min(gcvs), 4)), line = -3)
  title(paste("at bandwidth =", ks[which.min(gcvs)]), line = -4)
}
dev.off()
