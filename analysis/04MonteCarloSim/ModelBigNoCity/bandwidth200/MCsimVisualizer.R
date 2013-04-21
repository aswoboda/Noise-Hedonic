# Assess the Monte Carlo simulation results
# goal is to assess whether/which variables exhibit spatial non-stationarity

# load the results of the true data analysis
# calculate some statistics
# load the monte carlo data
# create distributions of the MC stats
# add the true statistic to the distribution
setwd("~/NoiseHedonicProject/Noise-Hedonic/analysis/04MonteCarloSim/ModelBigNoCity/bandwidth200/")

MCMaster = read.csv("LWRMonteCarloStatsMasterBigModelK200.csv")

load("CopyOfSales20052010LWRmodel17-2013-04-13.RData")

summary(MCMaster)

MCplotter = function(outputcoefficient, statcoefficient, meanORsd = "mean") {
  if (meanORsd == "mean") actual = mean(output[[outputcoefficient]][, "k200"], na.rm = T)
  if (meanORsd == "sd")   actual = sd(output[[outputcoefficient]][, "k200"], na.rm = T)
  
  temp = density(MCMaster[, statcoefficient])
  plot(temp, 
       xlim = c(min(c(range(temp$x), actual)), max(c(range(temp$x), actual))),
       main = paste(meanORsd, outputcoefficient),
       axes = F, ylab = "relative frequency", xlab = "")
  axis(1)
  abline(v = actual, col = "red")
}

outputCOEFS = rep(names(output)[c(1:5, 20:26)], 2)
statCOEFS = names(MCMaster)[c(3:26)] # which columns in MCMaster have the mean and sd of the coefficients we're interested in
meanORsds = rep(c("mean", "sd"), c(12, 12))

pdf("MCsimResults.pdf", height = 6, width = 24)
par(mfrow = c(2, 12))
for (i in 1:length(outputCOEFS)) {
  MCplotter(outputCOEFS[i], statCOEFS[i], meanORsds[i])
}
dev.off()