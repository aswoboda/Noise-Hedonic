# Assess the Monte Carlo simulation results
# goal is to assess whether/which variables exhibit spatial non-stationarity

# load the results of the true data analysis
# calculate some statistics
# load the monte carlo data
# create distributions of the MC stats
# add the true statistic to the distribution
setwd("~/NoiseHedonicProject/Noise-Hedonic/analysis/04MonteCarloSim/ModelBigCity/")
load("CopyOfSales20052010LWRmodel18-2013-04-13.RData") # LWR output from model run with real data
MCMaster = read.csv("bandwidthsAll/LWRMonteCarloStats2013-05-06.csv") # Monte Carlo stats from all bandwidths
MCMaster200 = read.csv("bandwidth200/LWRMonteCarloStats2013-05-10.csv") # Monte Carlo stats from nearest 200 obs

summary(MCMaster)

MCplotter = function(outputcoefficient, statcoefficient, meanORsd = "mean") {
  if (meanORsd == "mean") actual = mean(output[[outputcoefficient]][, "k200"], na.rm =T)
  if (meanORsd == "sd")   actual = sd(output[[outputcoefficient]][, "k200"], na.rm =T)
  
  temp = density(MCMaster200[, statcoefficient])
  plot(temp, 
       xlim = c(min(c(range(temp$x), actual), na.rm = T), max(c(range(temp$x), actual), na.rm = T)),
       main = paste(meanORsd, sub("beta.", "", outputcoefficient)),
       axes = F, ylab = "", xlab = "")
  axis(1)
  mtext("relative frequency", 2, 1, cex = .7)
  abline(v = actual, col = "red")
}

outputCOEFS = rep(names(output)[c(1:5, 20:26)], 2)
statCOEFS = names(MCMaster200)[3:26]
meanORsds = rep(c("mean", "sd"), c(12, 12))

pdf("MCsimResults.pdf", height = 6, width = 24)
par(mfrow = c(2, 12))
for (i in 1:length(outputCOEFS)) {
  MCplotter(outputCOEFS[i], statCOEFS[i], meanORsds[i])
  title(MYMODEL, line = 1, outer = T)
}
dev.off()

pdf("MCsimResultsSDs.pdf", height = 6, width = 8)
par(mfrow = c(3, 4))
for (i in 13:24) {
  MCplotter(outputCOEFS[i], statCOEFS[i], meanORsds[i])
  title(MYMODEL, line = 1, outer = T)
}
dev.off()