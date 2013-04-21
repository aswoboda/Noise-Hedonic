
dataPath = "~/NoiseHedonicProject/Noise-Hedonic/analysis/04MonteCarloSim/ModelBigNoCity/bandwidth200/"
setwd(dataPath)
allFiles = list.files()
MCstatFiles = allFiles[substr(allFiles, 1, 25) == "LWRMonteCarloStats2013-04"]

MCstat = read.csv(MCstatFiles[1])
MCstat = MCstat[!is.na(MCstat[, 1]), ]
MCstat = unique(MCstat)
print(dim(MCstat)[1])
for (i in 2:length(MCstatFiles)) {
  MCstat = rbind(MCstat, read.csv(MCstatFiles[i]))
  MCstat = MCstat[!is.na(MCstat[, 1]), ]
  MCstat = unique(MCstat)
  print(dim(MCstat)[1])
  print(MCstatFiles[i])
}

MCstat = MCstat[order(MCstat$minGCV), ]

write.csv(MCstat, "LWRMonteCarloStatsMasterBigModelK200.csv", row.names = F)

for (i in 1:length(MCstatFiles)) {
  MCstat = read.csv(MCstatFiles[i])
  MCstat = MCstat[!is.na(MCstat[, 1]), ]
  print(dim(MCstat)[1])
  print(MCstatFiles[i])
}
