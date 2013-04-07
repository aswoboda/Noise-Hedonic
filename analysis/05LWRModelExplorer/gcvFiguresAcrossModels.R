

getwd() #assume I'm in the project default working directory "Noise Hedonic Project"

require(foreign)
source("helper/LWRfunctions.R")

filePrefix = "../Data/R2GIS/CleanData/"
inputFile = "Sales20052010.dbf"
DATAFRAME = read.dbf(paste0(filePrefix, inputFile))
obs2run = which(DATAFRAME$TimePeriod>11)

dataPath = "../Data/R2GIS/CleanData/"
filelist = list.files(path = dataPath)

files2open = filelist[which(substr(filelist, 1, 21) == "Sales20052010LWRmodel")]

pdf("analysis/05LWRModelExplorer/GCVplotsCITY.pdf")
for (myFile in 9:length(files2open)) {
  load(paste0(dataPath, files2open[myFile], sep = ""))
  if (dim(output[[1]])[1] == 42095) gcvs = GCV(output$leverages[obs2run, ], output$yhats[obs2run, ], DATAFRAME$logSALE_VA[obs2run])
  if (dim(output[[1]])[1] == 31748) gcvs = GCV(output$leverages, output$yhats, DATAFRAME$logSALE_VA[obs2run])
#   print(files2open[myFile])
#   print(min(gcvs))
  ks = as.numeric(substr(colnames(output[[1]]), 2, 5))
  plot(ks, gcvs, type = "l", main = "")
  title(MYMODEL, cex.main = .5)
  title(paste0("N = ", dim(output[[1]])[1]), line = .5)
  title(paste("data: ", substr(files2open[myFile], 22, nchar(files2open[myFile]))), line = -1)
  lmreg = lm(MYMODEL, data = DATAFRAME[obs2run, ])
  levs = lm.influence(lmreg)$hat
  globalGCV = GCV(cbind(levs), cbind(lmreg$fitted.values), DATAFRAME$logSALE_VA[obs2run])
  title(paste("global model gcv:", round(globalGCV, 3)), line = -2)
  title(paste("min gcv:", round(min(gcvs), 5)), line = -3)
}
dev.off()
