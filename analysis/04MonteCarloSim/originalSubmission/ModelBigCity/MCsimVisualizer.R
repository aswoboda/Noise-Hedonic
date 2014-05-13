# Assess the Monte Carlo simulation results
# goal is to assess whether/which variables exhibit spatial non-stationarity

# load the results of the true data analysis
# calculate some statistics
# load the monte carlo data
# create distributions of the MC stats
# add the true statistic to the distribution

load("../Data/R2GIS/CleanData/TimeLag12months/Sales20052010LWRmodelAirMean3-2014-03-19.RData") # LWR output from model run with real data

LWRMonteCarloStats2014.03.29 <- read.csv("~/NoiseHedonicProject/Noise-Hedonic/analysis/04MonteCarloSim/Revision/Model3/LWRMonteCarloStats2014-03-29.csv")
LWRMonteCarloStats2014.04.18 <- read.csv("~/NoiseHedonicProject/Noise-Hedonic/analysis/04MonteCarloSim/Revision/Model3/LWRMonteCarloStats2014-04-18.csv")
MCMaster650 <- rbind(LWRMonteCarloStats2014.03.29, LWRMonteCarloStats2014.04.18)


MCplotter = function(outputcoefficient, statcoefficient, meanORsd = "mean") {
  if (meanORsd == "mean") actual = mean(output[[outputcoefficient]][, "k650"], na.rm =T)
  if (meanORsd == "sd")   actual = sd(output[[outputcoefficient]][, "k650"], na.rm =T)
  
  temp = density(MCMaster650[, statcoefficient])
  plot(temp, 
       xlim = c(min(c(range(temp$x), actual), na.rm = T), max(c(range(temp$x), actual), na.rm = T)),
       main = paste(meanORsd, sub("beta.", "", outputcoefficient)),
       axes = F, ylab = "", xlab = "")
  axis(1)
  mtext("relative frequency", 2, 1, cex = .7)
  abline(v = actual, col = "red")
}

outputCOEFS = rep(names(output)[c(1:5, 16, 19, 20, 21, 22, 23, 24, 17, 18)], 2)
statCOEFS = names(MCMaster650)[3:30]
meanORsds = rep(c("mean", "sd"), c(14, 14))

pdf("analysis/04MonteCarloSim/Revision/MCsimResults.pdf", height = 6, width = 24)
par(mfrow = c(2, 14))
for (i in 1:length(outputCOEFS)) {
  MCplotter(outputCOEFS[i], statCOEFS[i], meanORsds[i])
  title(MYMODEL, line = 1, outer = T)
}
dev.off()

pdf("analysis/04MonteCarloSim/Revision/MCsimResultsSDs.pdf", height = 6, width = 8)
par(mfrow = c(3, 5))
for (i in 15:28) {
  MCplotter(outputCOEFS[i], statCOEFS[i], meanORsds[i])
  title(MYMODEL, line = 1, outer = T)
}
dev.off()