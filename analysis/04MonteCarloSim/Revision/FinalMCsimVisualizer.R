# Assess the Monte Carlo simulation results for both k = 500 and k = 2,000 all in one graph
# goal is to assess whether/which variables exhibit spatial non-stationarity

# load the results of the true data analysis
# calculate some statistics
# load the monte carlo data
# create distributions of the MC stats
# add the true statistic to the distribution

load("../Data/R2GIS/CleanData/TimeLag12months/Sales20052010LWRmodelAirMean3-2014-03-19.RData") # LWR output from model run with real data

# LWRMonteCarloStats2014.03.29 <- read.csv("~/NoiseHedonicProject/Noise-Hedonic/analysis/04MonteCarloSim/Revision/Model3/LWRMonteCarloStats2014-03-29.csv")
# LWRMonteCarloStats2014.04.18 <- read.csv("~/NoiseHedonicProject/Noise-Hedonic/analysis/04MonteCarloSim/Revision/Model3/LWRMonteCarloStats2014-04-18.csv")
# MCMaster <- rbind(LWRMonteCarloStats2014.03.29, LWRMonteCarloStats2014.04.18)
MCMaster2000 = read.csv("~/NoiseHedonicProject/Noise-Hedonic/analysis/04MonteCarloSim/Revision/Model3/k2000/LWRMonteCarloStats2014-07-03.csv")
MCMaster2000 = MCMaster2000[1:100, ]

MCMaster500 = read.csv("~/NoiseHedonicProject/Noise-Hedonic/analysis/04MonteCarloSim/Revision/Model3/LWRMonteCarloStats2014-07-05.csv")
MCMaster500 = MCMaster500[1:80, ]

MCplotter = function(outputcoefficient, statcoefficient) {
  actual = sd(output[[outputcoefficient]][, c("k500", "k2000")], na.rm =T)
  
  temp500 = density(MCMaster500[, statcoefficient])
  temp2000 = density(MCMaster2000[, statcoefficient])
  
  plot(temp500, type = "n",
       xlim = c(0, max(c(temp500$x, temp2000$x, actual), na.rm = T)),
       ylim = 1.2*c(0, max(c(temp500$y, temp2000$y), na.rm = T)),
       main = "", axes = F, 
       ylab = "", xlab = "")
  axis(1, at = c(0, max(c(temp500$x, temp2000$x, actual))), 
       labels = c(0, signif(max(c(temp500$x, temp2000$x, actual)), 2)))
  lines(temp500, col = "blue", lwd = 2)
  lines(temp2000, col = "red", lwd = 2)
  abline(v = actual, col = c("blue", "red"), lty = 3, lwd = 2)
}

outputCOEFS = rep(names(output)[c(2:5, 16, 19, 20, 21, 22, 23, 24, 17, 18)], 2)
statCOEFS = names(MCMaster)[c(-(1:3),-17)]
titles = c("Noise", "House Size", "Lot Size", "Year Built", "Owner Occupancy",
           "Median Income", "School Test Scores", "Distance to Lake", "Distance to Park", "Distance to Shop",
           "Distance to CBD", "Percent White", "Percent Under Age 18")

pdf("analysis/04MonteCarloSim/Revision/MCsimResultsSDsk500and2000.pdf", height = 8, width = 6, family = "Palatino")
#par(mfrow = c(3, 5))
par(oma = c(.5, .5, 3, .5))
layout(matrix(c(1, 1:14), 5, 3, byrow = TRUE))
par(mar = c(0, 0, 0, 0))
# legend to describe the colors and line types
plot(0, 0, type = "n", ylab = "", xlab = "", axes = F)
legend("center", c("Simulated (2000)", "Actual (2000)", "Simulated (500)", "Actual (500)"), 
       col = c("red", "red", "blue", "blue"), 
       lty = c(1, 3, 1, 3), lwd = 2,
       box.col = "white", cex = 1.7)
par(mar = c(2.5, 1, 2, 1.5))
for (i in 14:26) {
  MCplotter(outputCOEFS[i], statCOEFS[i])
  title(titles[i-13], main.cex = 1.5, line = 0)
}
title("Distribution of Simulated and Actual\n LWR Regression Coefficient Standard Deviations", line = -0.5, outer = T, cex.main = 2)
dev.off()
