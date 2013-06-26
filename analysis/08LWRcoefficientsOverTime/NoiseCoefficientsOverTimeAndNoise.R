

# Does the impact of traffic noise vary with the level of traffic noise?

# plot the LWR noise coefficients against the MAX levels and see if there are any differences

# Do the LWR coefficients seem to vary over time?
# the results of these regressions get plopped into the appropriate sections and tables in the article
# ultimately might want to create a bigger table

load("~/Noise Hedonic Project/Noise-Hedonic/analysis/04MonteCarloSim/ModelBigCity/CopyOfSales20052010LWRmodel18-2013-04-13.RData")
source("~/Noise Hedonic Project/Noise-Hedonic/helper/LWRfunctions.R")
require(foreign)
DATAFRAME = read.dbf("~/Noise Hedonic Project/Data/R2GIS/CleanData/Sales20052010.dbf")

optimalK = "k200"
TIMELAG = 12
obs2run = which(DATAFRAME$TimePeriod > (TIMELAG -1))
breakMonth = 45 #september 2008

tempData = data.frame(betaMAX = output$beta.MAX[, optimalK],
                      MAX = DATAFRAME$MAX[obs2run],
                      TimePeriod = DATAFRAME$TimePeriod[obs2run])
tempData$Post = ifelse(tempData$TimePeriod > breakMonth, 1, 0)
tempData$month = tempData$TimePeriod - breakMonth
tempData$MAXcat = cut(tempData$MAX, breaks = seq(25, 100, 5))

lm.mod = lm(betaMAX ~ MAX*Post, data = tempData)
predict(object = lm.mod, newdata = data.frame(MAX = c(50, 50, 75, 75), Post = c(1, 0, 1, 0)), se.fit= T)

MAXcat.model = summary(lm(betaMAX ~ MAXcat-1, data = tempData)) #, subset = tempData$MAX > 50
MAXmeans = MAXcat.model$coefficients[, 1]
MAXlow = MAXcat.model$coefficients[, 1] - abs(MAXcat.model$coefficients[, 2])
MAXhigh = MAXcat.model$coefficients[, 1] + abs(MAXcat.model$coefficients[, 2])
plot(MAXmeans, ylim = c(-.005, 0), las = 1)
for (i in 1:length(MAXlow)) lines(rep(i, 2), c(MAXlow[i], MAXhigh[i]))


MAXcat.post = summary(lm(betaMAX ~ MAXcat*Post -1, data = tempData))
MAXcat.post
dim(MAXcat.post$coefficients)

tempData$NewCat = as.numeric(tempData$MAXcat) - .1
tempData$NewCat = tempData$NewCat + .2*tempData$Post
tempData$NewCat = factor(tempData$NewCat)

MAXcat.time = summary(lm(betaMAX ~ NewCat - 1, data = tempData))
MAXcat.time.data = data.frame(Noise = as.numeric(substr(names(MAXcat.time$coefficients[, 1]), 7, 12)),
                              Mean = MAXcat.time$coefficients[, 1])
MAXcat.time.data$Low = MAXcat.time$coefficients[, 1] - abs(MAXcat.time$coefficients[, 2])
MAXcat.time.data$High = MAXcat.time$coefficients[, 1] + abs(MAXcat.time$coefficients[, 2])
MAXcat.time.data$Pre = "post"
MAXcat.time.data$Pre[which(as.character(MAXcat.time.data$Noise%%1) == "0.9")] = "pre"
MAXcat.time.data$Color = ifelse(MAXcat.time.data$Pre == "pre", "red", "blue")

pdf("~/Noise Hedonic Project/Noise-Hedonic/graphs/LWRbetaMAXbyCatTime.pdf", width = 6, height = 4,
    family = "Palatino")
par(mar = c(4, 5, 4, 2))
with(MAXcat.time.data,
     plot(round(Noise, 0), Mean, ylim = c(-.006, 0.004), xlim = c(0.4, 13.9), axes = F,
          las = 1, col = Color,  pch = 16, cex = .75,
          xlab = "Noise (dB)", ylab = ""))
axis(2, las = 1)
mtext("Mean LWR Traffic Noise Coefficient", side = 2, line = 4)
MYAT = seq(.5, 13.5, 1)
MYLAB = seq(25, 90, 5)
axis(1, at = MYAT, labels = MYLAB)
for (i in 1:dim(MAXcat.time.data)[1]) {
  lines(rep(round(MAXcat.time.data$Noise[i], 0), 2), c(MAXcat.time.data$Low[i], MAXcat.time.data$High[i]), col = MAXcat.time.data$Color[i])
}
title("Mean LWR Noise Coefficient vs. Noise Level")
text(4.5, 0, "pre-September 2008", col = "red")
text(4.5, -.005, "post-September 2008", col = "blue")
dev.off()

with(tempData,
     smoothScatter(MAX, betaMAX, nrpoints = 1000))

#loess.smooth(tempData$MAX, tempData$betaMAX, span = .01, degree = 2)
points(loess.smooth(tempData$MAX, tempData$betaMAX, span = .01, degree = 2), pch = 16, col = "red", cex = .5)

