
dataPath = "~/NoiseHedonicProject/Data/R2GIS/CleanData/"
setwd(dataPath)
allFiles = list.files()
MCstatFiles = allFiles[substr(allFiles, 1, 4) == "LWRM"]

MCstat = read.csv(MCstatFiles[1])
MCstat = MCstat[!is.na(MCstat[, 1]), ]
MCstat = unique(MCstat)
print(dim(MCstat)[1])
for (i in 2:length(MCstatFiles)) {
  MCstat = rbind(MCstat, read.csv(MCstatFiles[i]))
  MCstat = MCstat[!is.na(MCstat[, 1]), ]
  MCstat = unique(MCstat)
  print(dim(MCstat)[1])
}

MCstat = MCstat[order(MCstat$minGCV), ]

write.csv(MCstat, "LWRMonteCarloStatsMaster.csv", row.names = F)

