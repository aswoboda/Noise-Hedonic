# GOAL: compare LWR Noise coefficients across Dakota County under alternative specifications

# Load all the .RData files for Dakota County
# Grab the Noise coefficients for the optimal bandwith in the given model output
# plot the Noise coefficients for the three paired specifications (with beds/baths/garagesqft vs. without)

setwd("~/NoiseHedonicProject/Noise-Hedonic/analysis/06Dakota/") #assume I'm in the project default working directory "Noise Hedonic Project"
require(foreign)
source("../../helper/LWRfunctions.R")

DATAFRAME = read.dbf("~/NoiseHedonicProject/Data/R2GIS/CleanData/Sales20052010.dbf")
obs2run = which(DATAFRAME$COUNTY_ID == "037" & DATAFRAME$BEDS > 0 &  DATAFRAME$BATH > 0 & DATAFRAME$GARSQFT>0 & DATAFRAME$TimePeriod>11)

dataPath = "../../../Data/R2GIS/CleanData/Dakota/"
filelist = list.files(dataPath)
files2open = filelist[which(substr(filelist, 1, 9) == "dakotaLWR")]

numFiles = length(files2open)

Noisecoeffs = matrix(NA, length(obs2run), numFiles)
models = c()
gcvs = matrix(NA, 12, numFiles)

for (myFile in 1:numFiles) {
  load(paste0(dataPath, files2open[myFile]))
  models = c(models, MYMODEL)
  gcvs[, myFile] = GCV(output$leverages, output$yhats, DATAFRAME$logSALE_VA[obs2run])
  print(gcvs)
  ks = colnames(output[[1]])
  Noisecoeffs[, myFile] = output$beta.Air_Mean[, which.min(gcvs[, myFile])] #
  #colnames(Noisecoeffs)[myFile] = paste0("Mod", myFile, ks[which.min(gcvs)])
}

Noisecoeffs.d = as.data.frame(Noisecoeffs)

par(mfrow = c(3, 1))
par(mar = c(4, 5, 4, 2))
lm.structural = lm(V1 ~ V4, data = Noisecoeffs.d)
plot(Noisecoeffs.d$V4, Noisecoeffs.d$V1, cex = .2, 
     xlim = range(Noisecoeffs), ylim = range(Noisecoeffs), las = 1,
     main = models[1], cex.main = .75,
     xlab = "",
     ylab = "")
abline(0, 1, col = "blue")
abline(lm.structural, col = "red")
mtext("without", side = 2, line = 3.5)
mtext("with Beds, Baths, Garage", side = 1, line = 2.5)
summary(lm.structural)

lm.medium = lm(V2 ~ V5, data = Noisecoeffs.d)
plot(Noisecoeffs.d$V5, Noisecoeffs.d$V2, cex = .2, 
     xlim = range(Noisecoeffs), ylim = range(Noisecoeffs), las = 1,
     main = models[2], cex.main = .75,
     xlab = "",
     ylab = "")
abline(0, 1, col = "blue")
abline(lm.medium, col = "red")
mtext("without", side = 2, line = 3.5)
mtext("with Beds, Baths, Garage", side = 1, line = 2.5)
summary(lm.medium)

lm.sink = lm(V3 ~ V6, data = Noisecoeffs.d)
plot(Noisecoeffs.d$V6, Noisecoeffs.d$V3, cex = .2, 
     xlim = range(Noisecoeffs), ylim = range(Noisecoeffs), las = 1,
     main = models[3], cex.main = .75,
     xlab = "",
     ylab = "")
abline(0, 1, col = "blue")
abline(lm.sink, col = "red")
mtext("without", side = 2, line = 3.5)
mtext("with Beds, Baths, Garage", side = 1, line = 2.5)
summary(lm.sink)


means = colMeans(Noisecoeffs)
sds = apply(Noisecoeffs, 2, sd)

comp.tab = matrix(NA, 6, 2)

comp.tab[1, ] = means[c(1, 4)]
comp.tab[3, ] = means[c(2, 5)]
comp.tab[5, ] = means[c(3, 6)]

comp.tab[2, ] = sds[c(1, 4)]
comp.tab[4, ] = sds[c(2, 5)]
comp.tab[6, ] = sds[c(3, 6)]

comp.tab = round(comp.tab, 5)
comp.tab[c(2, 4, 6), ] = paste0("(", comp.tab[c(2, 4, 6), ], ")")

require(xtable)
xtable(comp.tab)

t.test(Noisecoeffs[, c(1, 4)])
t.test(Noisecoeffs[, 1], Noisecoeffs[, 4], paired = T)
t.test(Noisecoeffs[, c(2, 5)])
t.test(Noisecoeffs[, 2], Noisecoeffs[, 5], paired = T)
t.test(Noisecoeffs[, c(3, 6)])
t.test(Noisecoeffs[, 3], Noisecoeffs[, 6], paired = T)