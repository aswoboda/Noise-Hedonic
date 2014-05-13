

getwd() #assume I'm in the project default working directory "Noise Hedonic"

require(foreign)
source("helper/LWRfunctions.R")

filePrefix = ""
inputFile = ""
DATAFRAME = read.dbf("../Data/R2GIS/CleanData/Sales20052010.dbf")
obs2run = which(DATAFRAME$TimePeriod>11)

dataPath = "../Data/R2GIS/CleanData/TimeLag24months/"
filelist = list.files(path = dataPath)

files2open = filelist[which(substr(filelist, 1, 8) == "LWRmodel")]

pdf("analysis/05LWRModelExplorer/GCVplotsTimeLag24.pdf")
for (myFile in 1:length(files2open)) {
  load(paste0(dataPath, files2open[myFile], sep = ""))
  if (dim(output[[1]])[1] == 31737) gcvs = GCV(output$leverages, output$yhats, DATAFRAME$logSALE_VA[obs2run])   
  print(files2open[myFile])
  print(round(gcvs, 5))
  print(summary(output[[2]]))
  
  ks = as.numeric(substr(colnames(output[[1]]), 2, 5))
  plot(ks, gcvs, ylim = c(0.02, 0.05), type = "l", main = "")
  title(MYMODEL, cex.main = .5)
  title(paste0("N = ", dim(output[[1]])[1]), line = .5)
  title(paste("data: ", substr(files2open[myFile], 9, nchar(files2open[myFile])-17)), line = -1)
  lmreg = lm(MYMODEL, data = DATAFRAME[obs2run, ])
  levs = lm.influence(lmreg, do.coef = FALSE)$hat
  globalGCV = GCV(cbind(levs), cbind(lmreg$fitted.values), DATAFRAME$logSALE_VA[obs2run])
  title(paste("global model gcv:", round(globalGCV, 3)), line = -2)
  title(paste("min gcv:", round(min(gcvs), 5)), line = -3)
  title(paste("at bandwidth:", names(gcvs)[which.min(gcvs)]), line = -4)
}
dev.off()
