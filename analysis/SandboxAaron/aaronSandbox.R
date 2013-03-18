

# starting to make figures comparing the LWR coefficients over time

require(foreign)
source("helper/LWRfunctions.R")

filePrefix = "../Data/R2GIS/CleanData/"
inputFile = "Sales20052010.dbf"
DATAFRAME = read.dbf(paste0(filePrefix, inputFile))
obs2run = which(DATAFRAME$TimePeriod>11)

dataPath = "../Data/R2GIS/LWRoutput/"
filelist = list.files(path = dataPath)

files2open = filelist[which(substr(filelist, 1, 26) == "Sales20052010LWRoutput2013")]
myFile = 12 # this determines which file we're opening

load(paste0(dataPath, files2open[myFile], sep = ""))
# now we should have "output" loaded in our workspace and "MYMODEL" telling us which regression we ran

# the basic idea is to 
# 1) choose a coefficient and bandwidth
# 2) plot the coefficient value vs. the month of sale
# 3) add a linear best fit line to look for a trend
# 4) repeat above with different combinations of coefficients, bandwidths, and models

names(output)
myCoeff = names(output)[2] # choose a coefficient to work with
colnames(output[[myCoeff]])
bandwidths2plot = c(2, 3, 4, 6, 8, 10)

pdf("analysis/02LWRComparisons/LWRcoefPlotOverTime.pdf", height = 8, width = 12)
par(mfrow = c(2, 3))
for (i in bandwidths2plot) {
  myBandwidth = colnames(output[[myCoeff]])[i] # choose a bandwidth
  myX = DATAFRAME$TimePeriod[obs2run]
  myY = output[[myCoeff]][, myBandwidth]
  plot(myX, myY, cex = .2,
       xlab = "Time",
       ylab = myCoeff,
       main = myBandwidth)
  abline(lm(myY ~ myX), col = "red")
}
dev.off()



