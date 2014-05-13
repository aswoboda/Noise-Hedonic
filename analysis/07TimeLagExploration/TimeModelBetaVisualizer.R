# install.packages("~/lvplot_0.1.tar.gz", repos = NULL, type="source")

require(lvplot)
require(RColorBrewer)
require(foreign)

source("helper/LWRfunctions.R")

# Goal is to compare the dist'n of noise betas across time lags, models, and noise measures

# generate two arrays - one for the minimum GCV at the optimal k across the different dims
myArray1 = array(NA, 
                 dim = c(2, 3, 4, 3),
                 dimnames = list(c("k", "GCV"),
                                 model = 1:3,
                                 lag = sprintf("%02d", c(6, 12, 18, 24)),
                                 noise = c("min", "mean", "max")))
# and another to capture the betahats at these optimal bandwidths
N = 31737 # number of betahats for each model
myArray2 = array(NA,
                 dim = c(N, 3, 4, 3),
                 dimnames = list(betahat = 1:N,
                                 model = 1:3,
                                 lag = sprintf("%02d", c(6, 12, 18, 24)),
                                 noise = c("min", "mean", "max")))

DATAFRAME = read.dbf("../Data/R2GIS/CleanData/Sales20052010.dbf")
obs2run = which(DATAFRAME$TimePeriod>11)

# establish a loop throughout the 36 lag-model-noise combinations...
models = rep(1:3, 3)
noises = rep(c("max", "mean", "min"), rep(3, 3))

for (timelag in c("06", "12", "18", "24")) {
  dataPath = paste0("../Data/R2GIS/CleanData/TimeLag", timelag, "months/")
  filelist = list.files(path = dataPath) # first 9 files max1, max2, max3, mean1, mean2... min3
  print(timelag)
  for(myFile in 1:9) {
    load(paste0(dataPath, filelist[myFile], sep = ""))
    gcvs = GCV(output$leverages, output$yhats, DATAFRAME$logSALE_VA[obs2run])
    myArray1["GCV", models[myFile], timelag, noises[myFile]] = round(min(gcvs), 5)
    myArray1["k", models[myFile], timelag, noises[myFile]] = names(gcvs)[which.min(gcvs)]
    
    myArray2[, models[myFile], timelag, noises[myFile]] = output[[2]][,myArray1["k", models[myFile], timelag, noises[myFile]]]
    print(myFile)
  }
}

pdf("analysis/05LWRModelExplorer/BetasByModelTimeNoise.pdf", height = 9, width = 12)
par(mfrow = c(3, 4))
for (i in 1:3) {
  for (j in 1:4) {
    temp = data.frame(betahat = c(myArray2[, i, j, 1],
                                  myArray2[, i, j, 2],
                                  myArray2[, i, j, 3]),
                      noise = rep(c("max", "mean", "min"), rep(31737, 3)))
    LVboxplot.formula(temp$betahat~temp$noise, xlim = c(-.03, .01))
    title(paste("Time Lag:", dimnames(myArray2)$lag[j], " Model:", i))
  }
}
dev.off()

pdf("analysis/05LWRModelExplorer/BetasByModelTimeNoise2.pdf", height = 12, width = 9)
par(mfrow = c(4, 3))
 for (j in 1:4) {
  for (i in 1:3) {
    temp = data.frame(betahat = c(myArray2[, i, j, 1],
                                  myArray2[, i, j, 2],
                                  myArray2[, i, j, 3]),
                      noise = rep(c("max", "mean", "min"), rep(31737, 3)))
    LVboxplot.formula(temp$betahat~temp$noise, xlim = c(-.03, .01))
    title(paste("Time Lag:", dimnames(myArray2)$lag[j], " Model:", i))
  }
}
dev.off()