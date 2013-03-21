

getwd() #assume I'm in the project default working directory "Noise Hedonic Project"

require(foreign)
source("helper/LWRfunctions.R")

filePrefix = "../Data/R2GIS/CleanData/"
inputFile = "Sales20052010.dbf"
DATAFRAME = read.dbf(paste0(filePrefix, inputFile))
obs2run = which(DATAFRAME$TimePeriod>11)

dataPath = "../Data/R2GIS/LWRoutput/"
filelist = list.files(path = dataPath)

files2open = filelist[which(substr(filelist, 1, 26) == "Sales20052010LWRoutput2013")]

myFile = 8 # this determines which file we're opening

load(paste0(dataPath, files2open[myFile], sep = ""))
# now we should have "output" loaded in our workspace and "MYMODEL" telling us which regression we ran
gcv.table <- data.frame (subset = DATAFRAME$COUNTY_ID[obs2run], lever = output$leverages, yhat = output$yhats, Sale.Val= DATAFRAME$logSALE_VA[obs2run])
head(gcv.table)

#Wash County
wash = which(gcv.table$subset == 163)
gcv.table.wash = gcv.table[wash, ]
#Ram County
ram = which(gcv.table$subset == 123)
gcv.table.ram = gcv.table[ram, ]
#Dak County
dak = which(gcv.table$subset == "037")
gcv.table.dak = gcv.table[dak, ]

#####Calculate GCV score#####
#Wash
sample.size = dim(gcv.table.wash)[1]
gcv.wash <- data.frame(k25 =1, k50=1, k75=1, k100=1, k150=1, k200=1, k400=1, k600=1, k800=1, k1000=1)
bandwidth = c(1:10)
for (i in bandwidth) {
  band.lever <- i+1
  band.se <- i+11
  band.table <- colnames(gcv.wash)[i]
  v1<- sum(gcv.table.wash[band.lever])
  se <- sum((gcv.table.wash$Sale.Val-gcv.table.wash[band.se])^2)
  gcv.wash[band.table]<- sample.size*se/(sample.size-v1)^2
}
gcv.wash

#Ram
sample.size = dim(gcv.table.ram)[1]
gcv.ram <- data.frame(k25 =1, k50=1, k75=1, k100=1, k150=1, k200=1, k400=1, k600=1, k800=1, k1000=1)
bandwidth = c(1:10)
for (i in bandwidth) {
  band.lever <- i+1
  band.se <- i+11
  band.table <- colnames(gcv.ram)[i]
  v1<- sum(gcv.table.ram[band.lever])
  se <- sum((gcv.table.ram$Sale.Val-gcv.table.ram[band.se])^2)
  gcv.ram[band.table]<- sample.size*se/(sample.size-v1)^2
}
gcv.ram

#Dak
sample.size = dim(gcv.table.dak)[1]
gcv.dak <- data.frame(k25 =1, k50=1, k75=1, k100=1, k150=1, k200=1, k400=1, k600=1, k800=1, k1000=1)
bandwidth = c(1:10)
for (i in bandwidth) {
  band.lever <- i+1
  band.se <- i+11
  band.table <- colnames(gcv.dak)[i]
  v1<- sum(gcv.table.dak[band.lever])
  se <- sum((gcv.table.dak$Sale.Val-gcv.table.dak[band.se])^2)
  gcv.dak[band.table]<- sample.size*se/(sample.size-v1)^2
}
gcv.dak

###Print off .pdf files for visualization
pdf("analysis/02LWRComparisons/WashCountyGCV_3_17b.pdf")
  ks = as.numeric(substr(colnames(output[[1]]), 2, 5))
  plot(ks, gcv.wash, type = "l", main = "")
  title(MYMODEL, cex.main = .5)
  title(paste0("N = ", dim(gcv.table.wash)[1]), line = .5)
  title(paste("data: ", substr(files2open[myFile], 28, nchar(files2open[myFile]))), line = -1)
  lmreg = lm(MYMODEL, data = DATAFRAME[obs2run, ])
  levs = lm.influence(lmreg)$hat
  globalGCV = GCV(cbind(levs), cbind(lmreg$fitted.values), DATAFRAME$logSALE_VA[obs2run])
  title(paste("global model gcv:", round(globalGCV, 3)), line = -2)
  title(paste("min gcv:", round(min(gcv.wash), 4)), line = -3)
dev.off()

pdf("analysis/02LWRComparisons/RamCountyGCV_3_17b.pdf")
ks = as.numeric(substr(colnames(output[[1]]), 2, 5))
plot(ks, gcv.ram, type = "l", main = "")
title(MYMODEL, cex.main = .5)
title(paste0("N = ", dim(gcv.table.ram)[1]), line = .5)
title(paste("data: ", substr(files2open[myFile], 28, nchar(files2open[myFile]))), line = -1)
lmreg = lm(MYMODEL, data = DATAFRAME[obs2run, ])
levs = lm.influence(lmreg)$hat
globalGCV = GCV(cbind(levs), cbind(lmreg$fitted.values), DATAFRAME$logSALE_VA[obs2run])
title(paste("global model gcv:", round(globalGCV, 3)), line = -2)
title(paste("min gcv:", round(min(gcv.ram), 4)), line = -3)
dev.off()

pdf("analysis/02LWRComparisons/DakCountyGCV_3_17b.pdf")
ks = as.numeric(substr(colnames(output[[1]]), 2, 5))
plot(ks, gcv.dak, type = "l", main = "")
title(MYMODEL, cex.main = .5)
title(paste0("N = ", dim(gcv.table.dak)[1]), line = .5)
title(paste("data: ", substr(files2open[myFile], 28, nchar(files2open[myFile]))), line = -1)
lmreg = lm(MYMODEL, data = DATAFRAME[obs2run, ])
levs = lm.influence(lmreg)$hat
globalGCV = GCV(cbind(levs), cbind(lmreg$fitted.values), DATAFRAME$logSALE_VA[obs2run])
title(paste("global model gcv:", round(globalGCV, 3)), line = -2)
title(paste("min gcv:", round(min(gcv.dak), 4)), line = -3)
dev.off()