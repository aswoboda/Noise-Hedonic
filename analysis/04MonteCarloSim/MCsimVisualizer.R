# Assess the Monte Carlo simulation results
# goal is to assess whether/which variables exhibit spatial non-stationarity

# load the results of the true data analysis
# calculate some statistics
# load the monte carlo data
# create distributions of the MC stats
# add the true statistic to the distribution

MCMaster = read.csv("../Data/R2GIS/MonteCarlo/LWRMonteCarloStatsMaster.csv")
MCMaster100 = read.csv("../Data/R2GIS/MonteCarlo/LWRMonteCarloStatsMasterNearest100obs.csv")

load("../Data/R2GIS/MonteCarlo/Sales20052010LWRoutput2013-03-21.RData")

summary(MCMaster100)

MCplotter = function(outputcoefficient, statcoefficient, meanORsd = "mean") {
  if (meanORsd == "mean") actual = mean(output[[outputcoefficient]][, "k100"])
  if (meanORsd == "sd")   actual = sd(output[[outputcoefficient]][, "k100"])
  
  temp = density(MCMaster100[, statcoefficient])
  plot(temp, 
       xlim = c(min(c(range(temp$x), actual)), max(c(range(temp$x), actual))),
       main = paste(meanORsd, outputcoefficient),
       axes = F, ylab = "relative frequency", xlab = "")
  axis(1)
  abline(v = actual, col = "red")
}

outputCOEFS = rep(names(output)[1:5], 2)
statCOEFS = names(MCMaster100)[3:12]
meanORsds = rep(c("mean", "sd"), c(5, 5))

pdf("analysis/04MonteCarloSim/MCsimResults.pdf", height = 6, width = 12)
par(mfrow = c(2, 5))
for (i in 1:length(outputCOEFS)) {
  MCplotter(outputCOEFS[i], statCOEFS[i], meanORsds[i])
}
dev.off()