# Assess the Monte Carlo simulation results
# goal is to assess whether/which variables exhibit spatial non-stationarity

# load the results of the true data analysis
# calculate some statistics
# load the monte carlo data
# create distributions of the MC stats
# add the true statistic to the distribution
setwd("~/NoiseHedonicProject/Noise-Hedonic/analysis/04MonteCarloSim/ModelStructural/")
MCMaster = read.csv("bandwidthsAll/LWRMonteCarloStatsMaster.csv")
MCMaster100 = read.csv("bandwidthK100/LWRMonteCarloStatsMasterNearest100obs.csv")
load("Sales20052010LWRoutput2013-03-21.RData")

summary(MCMaster)

MCplotter = function(outputcoefficient, statcoefficient, meanORsd = "mean") {
  if (meanORsd == "mean") actual = mean(output[[outputcoefficient]][, "k100"], na.rm = T)
  if (meanORsd == "sd")   actual = sd(output[[outputcoefficient]][, "k100"], na.rm = T)
  
  temp = density(MCMaster100[, statcoefficient])
  plot(temp, 
       xlim = c(min(c(range(temp$x), actual), na.rm = T), max(c(range(temp$x), actual), na.rm = T)),
       main = paste(meanORsd, outputcoefficient),
       axes = F, ylab = "relative frequency", xlab = "")
  axis(1)
  abline(v = actual, col = "red")
}

outputCOEFS = rep(names(output)[1:5], 2)
statCOEFS = names(MCMaster100)[3:12]
meanORsds = rep(c("mean", "sd"), c(5, 5))

pdf("MCsimResults.pdf", height = 6, width = 12)
par(mfrow = c(2, 5))
par(oma = c(1, 1, 3, 1))
for (i in 1:length(outputCOEFS)) {
  MCplotter(outputCOEFS[i], statCOEFS[i], meanORsds[i])
}
title(MYMODEL, line = 1.5, outer = T)
dev.off()