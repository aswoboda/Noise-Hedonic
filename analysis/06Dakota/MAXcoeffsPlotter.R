# GOAL: compare LWR MAX coefficients across Dakota County under alternative specifications

# Load all the .RData files for Dakota County
# Grab the MAX coefficients for the optimal bandwith in the given model output
# plot the Max coefficients for the three paired specifications (with beds/baths/garagesqft vs. without)

setwd("~/Noise Hedonic Project/Noise-Hedonic/analysis/06Dakota/") #assume I'm in the project default working directory "Noise Hedonic Project"
require(foreign)
source("../../helper/LWRfunctions.R")

DATAFRAME = read.dbf("~/Noise Hedonic Project/Data/R2GIS/CleanData/Sales20052010.dbf")
obs2run = which(DATAFRAME$COUNTY_ID == "037" & DATAFRAME$BATH>.8 & DATAFRAME$TimePeriod>11)

#dataPath = "../Data/R2GIS/CleanData/"
filelist = list.files()
files2open = filelist[which(substr(filelist, 1, 6) == "Dakota")]

numFiles = length(files2open)

MAXcoeffs = matrix(NA, length(obs2run), numFiles)
models = c()
gcvs = matrix(NA, 15, numFiles)

for (myFile in 1:numFiles) {
  load(files2open[myFile])
  models = c(models, MYMODEL)
  gcvs[, myFile] = GCV(output$leverages, output$yhats, DATAFRAME$logSALE_VA[obs2run])
  print(gcvs)
  ks = colnames(output[[1]])
  MAXcoeffs[, myFile] = output$beta.MAX[, which.min(gcvs[, myFile])]
  #colnames(MAXcoeffs)[myFile] = paste0("Mod", myFile, ks[which.min(gcvs)])
}

MAXcoeffs.d = as.data.frame(MAXcoeffs)

par(mfrow = c(3, 1))
par(mar = c(4, 5, 4, 2))
lm.structural = lm(V2 ~ V1, data = MAXcoeffs.d)
plot(MAXcoeffs.d$V1, MAXcoeffs.d$V2, cex = .2, 
     xlim = range(MAXcoeffs), ylim = range(MAXcoeffs), las = 1,
     main = models[2], cex.main = .75,
     xlab = "",
     ylab = "")
abline(0, 1, col = "blue")
abline(lm.structural, col = "red")
mtext("without", side = 2, line = 3.5)
mtext("with Beds, Baths, Garage", side = 1, line = 2.5)
summary(lm.structural)

lm.medium = lm(V4 ~ V3, data = MAXcoeffs.d)
plot(MAXcoeffs.d$V3, MAXcoeffs.d$V4, cex = .2, 
     xlim = range(MAXcoeffs), ylim = range(MAXcoeffs), las = 1,
     main = models[4], cex.main = .75,
     xlab = "",
     ylab = "")
abline(0, 1, col = "blue")
abline(lm.medium, col = "red")
mtext("without", side = 2, line = 3.5)
mtext("with Beds, Baths, Garage", side = 1, line = 2.5)
summary(lm.medium)

lm.sink = lm(V5 ~ V6, data = MAXcoeffs.d)
plot(MAXcoeffs.d$V6, MAXcoeffs.d$V5, cex = .2, 
     xlim = range(MAXcoeffs), ylim = range(MAXcoeffs), las = 1,
     main = models[5], cex.main = .75,
     xlab = "",
     ylab = "")
abline(0, 1, col = "blue")
abline(lm.sink, col = "red")
mtext("without", side = 2, line = 3.5)
mtext("with Beds, Baths, Garage", side = 1, line = 2.5)
summary(lm.sink)


means = colMeans(MAXcoeffs)
sds = apply(MAXcoeffs, 2, sd)

comp.tab = matrix(NA, 6, 2)

comp.tab[1, ] = means[1:2]
comp.tab[3, ] = means[3:4]
comp.tab[5, ] = means[5:6]

comp.tab[2, ] = sds[1:2]
comp.tab[4, ] = sds[3:4]
comp.tab[6, ] = sds[5:6]

comp.tab = round(comp.tab, 5)
comp.tab[c(2, 4, 6), ] = paste0("(", comp.tab[c(2, 4, 6), ], ")")

require(xtable)
xtable(comp.tab)

t.test(MAXcoeffs[, 1:2])
t.test(MAXcoeffs[, 1:2], paired = T)
t.test(MAXcoeffs[, 3:4])
t.test(MAXcoeffs[, 3:4], paired = T)
t.test(MAXcoeffs[, 5:6])
t.test(MAXcoeffs[, 5:6], paired = T)