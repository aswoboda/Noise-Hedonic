
# get the previous examples of the k = 4,000 MC simulation results together...

MCstats0109 = read.csv("analysis/04MonteCarloSim/Revision/Model3/k4000/LWRMonteCarloStats2014-04-09.csv")
MCstats1034 = read.csv("analysis/04MonteCarloSim/Revision/Model3/k4000/LWRMonteCarloStats2014-04-13.csv")
MCstats = rbind(MCstats0109, MCstats1034)
temp = as.matrix(MCstats)
temp2 = matrix(NA, 100, 30)
temp2[1:34, ] = temp

colnames(temp2) = names(MCstats0109)
write.csv(temp2, "analysis/04MonteCarloSim/Revision/Model3/k4000/LWRMonteCarloStatsTemp.csv", row.names = FALSE)

temp3 = read.csv("analysis/04MonteCarloSim/Revision/Model3/k4000/LWRMonteCarloStatsTemp.csv")
summary(temp3)