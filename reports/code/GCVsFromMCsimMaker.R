# make a figure showing the distribution of GCV scores for MC simulation 1

# idea is to show the distribution of GCV scores (as density plots) by bandwidth from reshuffled data
# then show our GCV score on the same plot

# conclusions: 
# - our GCV score is different from the simulated values
# - GCV scores tend to DECREASE as we INCREASE bandwidths for simulated data


# load up the simulation results

# 500
mcstats500 = read.csv("analysis/04MonteCarloSim/Revision/Model3/k500/LWRMonteCarloStats2014-07-06.csv")
mcstats500Density = density(mcstats500$minGCV)

# 1000
mcstats1000 = read.csv("analysis/04MonteCarloSim/Revision/Model3/k1000/LWRMonteCarloStats2014-06-25.csv")
mcstats1000Density = density(mcstats1000$minGCV[1:50]) # need to run the other 50 just to be sure..

# 2000
mcstats2000 = read.csv("analysis/04MonteCarloSim/Revision/Model3/k2000/LWRMonteCarloStats2014-07-03.csv")
mcstats2000Density = density(mcstats2000$minGCV)

# 4000
mcstats4000 = read.csv("analysis/04MonteCarloSim/Revision/Model3/k4000/LWRMonteCarloStats2014-07-11.csv")
mcstats4000Density = density(mcstats4000$minGCV) 


maxX = max(mcstats500Density$x, mcstats1000Density$x, mcstats2000Density$x, mcstats4000Density$x)
# > maxX
# [1] 0.04649656
maxY = max(mcstats500Density$y, mcstats1000Density$y, mcstats2000Density$y, mcstats4000Density$y)
# > maxY
# [1] 7755.237

pdf("graphs/GCVsFromMCsim.pdf", width = 6, height = 4, family = "Palatino")
par(mar = c(4, 1, 4, 1))
par(oma = c(.5, .5, 3, .5))
layout(matrix(c(1, 1, 2), 1, 3))
plot(0, 0, type = "n",
     xlim = c(0.02, 0.05), 
     ylim = c(0, maxY),
     xlab = "",
     ylab = "",
     main = "",
     axes = F)
axis(1)
mtext("relative frequency", side = 2)
mtext("GCV score", side = 1, line = 3)
mtext("GCV Scores for Simulated and Actual Data", side = 3, outer = TRUE, cex = 1.8)
lines(mcstats500Density, col = "blue", lwd = 3)
lines(mcstats1000Density, col = "purple", lwd = 3)
lines(mcstats2000Density, col = "red", lwd = 3)
lines(mcstats4000Density, col = "black", lwd = 3)
abline(v = 0.0271, lty = 2)
plot(0, 0, type = "n", xlab ="", ylab ="", main = "", axes = FALSE)
legend("center", title = "simulated data",
       legend = c("Nearest 4,000", "Nearest 2,000", "Nearest 1,000", "Nearest 500"),
       lty = 1, lwd = 3,
       col = c("black", "red", "purple", "blue"),
       box.col = "white", cex = 1.7)
legend(-1.1, -0.6, title = "actual data",
       legend = c("Nearest 500"),
       lty = 2, lwd = 1,
       col = c("black"),
       box.col = "white", cex = 1.7)
# text(0.026, 0, "actual data (nearest 500)", pos = 4, srt = 90)
dev.off()
