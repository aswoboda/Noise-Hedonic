

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
myFile = 8 # this determines which file we're opening

load(paste0(dataPath, files2open[myFile], sep = ""))
# now we should have "output" loaded in our workspace and "MYMODEL" telling us which regression we ran

# the basic idea is to 
# 1) choose a coefficient and bandwidth
# 2) plot the coefficient value vs. the month of sale
# 3) add a linear best fit line to look for a trend
# 4) repeat above with different combinations of coefficients, bandwidths, and models

####'Linear' noise variable
names(output)
beta.noise = which(names(output)=="beta.MAX") #find which column beta noise is in
ses.noise = which(names(output)=="ses.MAX") #find which column standard error noise is in
myCoeff.beta = names(output)[beta.noise]# choose a coefficient to work with
myCoeff.ses = names(output)[ses.noise]
test.table <- data.frame (coef = output[myCoeff.beta], se = output[myCoeff.ses], noise = DATAFRAME$MAX[obs2run], Sale.Val= DATAFRAME$SALE_VALUE[obs2run])
head(test.table)
#Calculate marginal effects for log-transformed noise variable.
test.table$mfx.MAX.k25 <- (test.table$coef.beta.MAX..k25) * test.table$Sale.Val
test.table$mfx.MAX.k50 <- (test.table$coef.beta.MAX..k50) * test.table$Sale.Val
test.table$mfx.MAX.k75 <- (test.table$coef.beta.MAX..k75) * test.table$Sale.Val
test.table$mfx.MAX.k100 <- (test.table$coef.beta.MAX..k100) * test.table$Sale.Val
test.table$mfx.MAX.k150 <- (test.table$coef.beta.MAX..k150) * test.table$Sale.Val
test.table$mfx.MAX.k200 <- (test.table$coef.beta.MAX..k200) * test.table$Sale.Val
test.table$mfx.MAX.k400 <- (test.table$coef.beta.MAX..k400) * test.table$Sale.Val
test.table$mfx.MAX.k600 <- (test.table$coef.beta.MAX..k600) * test.table$Sale.Val
test.table$mfx.MAX.k1000 <- (test.table$coef.beta.MAX..k1000) * test.table$Sale.Val
test.table$mfx.MAX.k800 <- (test.table$coef.beta.MAX..k800) * test.table$Sale.Val
colnames(test.table)
bandwidths2plot = c(24, 25, 26, 27, 30, 31)

pdf("analysis/02LWRComparisons/Noise/LWRcoefPlotOverTime3_17b_Noise.pdf", height = 8, width = 12)
par(mfrow = c(2, 3))
for (i in bandwidths2plot) {
  myBandwidth = colnames(test.table)[i] # choose a bandwidth
  myX = DATAFRAME$TimePeriod[obs2run]
  myY = test.table[, myBandwidth]
  plot(myX, myY, cex = .2,
       xlab = "Time",
       ylab = myCoeff.beta,
       main = myBandwidth)
  abline(lm(myY ~ myX), col = "red")
}
dev.off()

###Linear home size
names(output)
beta.sqft = which(names(output)=="beta.FIN_SQ_FT") #find which column beta home size is in
ses.sqft = which(names(output)=="ses.FIN_SQ_FT") #find which column standard error home size is in
myCoeff.beta = names(output)[beta.sqft]# choose a coefficient to work with
myCoeff.ses = names(output)[ses.sqft]
test.table <- data.frame (coef = output[myCoeff.beta], se = output[myCoeff.ses], Sale.Val= DATAFRAME$SALE_VALUE[obs2run])
head(test.table)
#Calculate marginal effects for log-transformed noise variable.
test.table$mfx.sqft.k25 <- test.table$coef.beta.FIN_SQ_FT.k25 * test.table$Sale.Val
test.table$mfx.sqft.k50 <- test.table$coef.beta.FIN_SQ_FT.k50 * test.table$Sale.Val
test.table$mfx.sqft.k75 <- test.table$coef.beta.FIN_SQ_FT.k75 * test.table$Sale.Val
test.table$mfx.sqft.k100 <- test.table$coef.beta.FIN_SQ_FT.k100 * test.table$Sale.Val
test.table$mfx.sqft.k150 <- test.table$coef.beta.FIN_SQ_FT.k150 * test.table$Sale.Val
test.table$mfx.sqft.k200 <- test.table$coef.beta.FIN_SQ_FT.k200 * test.table$Sale.Val
test.table$mfx.sqft.k400 <- test.table$coef.beta.FIN_SQ_FT.k400 *test.table$Sale.Val
test.table$mfx.sqft.k600 <- test.table$coef.beta.FIN_SQ_FT.k600 * test.table$Sale.Val
test.table$mfx.sqft.k800 <- test.table$coef.beta.FIN_SQ_FT.k800 * test.table$Sale.Val
test.table$mfx.sqft.k1000 <- test.table$coef.beta.FIN_SQ_FT.k1000 * test.table$Sale.Val
colnames(test.table)
bandwidths2plot = c(12, 13, 14, 16, 18, 20)

pdf("analysis/02LWRComparisons/Home Size/LWRcoefPlotOverTime3_17b_sqft.pdf", height = 8, width = 12)
par(mfrow = c(2, 3))
for (i in bandwidths2plot) {
  myBandwidth = colnames(test.table)[i] # choose a bandwidth
  myX = DATAFRAME$TimePeriod[obs2run]
  myY = test.table[, myBandwidth]
  plot(myX, myY, cex = .2,
       xlab = "Time",
       ylab = myCoeff.beta,
       main = myBandwidth)
  abline(lm(myY ~ myX), col = "red")
}
dev.off()

###Quadratic land size
names(output)
beta.land= which(names(output)=="beta.ACRES_POLY") #find which column beta land size is in
beta.land1 = which(names(output)=="beta.I(ACRES_POLY^2)")
ses.land = which(names(output)=="ses.ACRES_POLY") #find which column standard error land size is in
ses.land1 = which(names(output)=="ses.I(ACRES_POLY^2)")
myCoeff.beta = names(output)[beta.land]# choose a coefficient to work with
myCoeff.beta1 = names(output)[beta.land1]
myCoeff.ses = names(output)[ses.land]
myCoeff.ses1 = names(output)[ses.land1]
test.table <- data.frame (coef = output[myCoeff.beta], coef1 = output[myCoeff.beta1], se = output[myCoeff.ses], se1 = output[myCoeff.ses1], land= DATAFRAME$ACRES_POLY[obs2run], Sale.Val = DATAFRAME$SALE_VALUE[obs2run])
head(test.table)
#Calculate marginal effects for log-transformed noise variable.
test.table$mfx.land.k25 <- (test.table$coef.beta.ACRES_POLY.k25 + (2*test.table$land * test.table$coef1.beta.I.ACRES_POLY.2..k25))*test.table$Sale.Val
test.table$mfx.land.k50 <- (test.table$coef.beta.ACRES_POLY.k25 + (2*test.table$land * test.table$coef1.beta.I.ACRES_POLY.2..k25))*test.table$Sale.Val
test.table$mfx.land.k75 <- (test.table$coef.beta.ACRES_POLY.k25 + (2*test.table$land * test.table$coef1.beta.I.ACRES_POLY.2..k25))*test.table$Sale.Val
test.table$mfx.land.k100 <- (test.table$coef.beta.ACRES_POLY.k25 + (2*test.table$land * test.table$coef1.beta.I.ACRES_POLY.2..k25))*test.table$Sale.Val
test.table$mfx.land.k150 <- (test.table$coef.beta.ACRES_POLY.k25 + (2*test.table$land * test.table$coef1.beta.I.ACRES_POLY.2..k25))*test.table$Sale.Val
test.table$mfx.land.k200 <- (test.table$coef.beta.ACRES_POLY.k25 + (2*test.table$land * test.table$coef1.beta.I.ACRES_POLY.2..k25))*test.table$Sale.Val
test.table$mfx.land.k400 <- (test.table$coef.beta.ACRES_POLY.k25 + (2*test.table$land * test.table$coef1.beta.I.ACRES_POLY.2..k25))*test.table$Sale.Val
test.table$mfx.land.k600 <- (test.table$coef.beta.ACRES_POLY.k25 + (2*test.table$land * test.table$coef1.beta.I.ACRES_POLY.2..k25))*test.table$Sale.Val
test.table$mfx.land.k800 <- (test.table$coef.beta.ACRES_POLY.k25 + (2*test.table$land * test.table$coef1.beta.I.ACRES_POLY.2..k25))*test.table$Sale.Val
test.table$mfx.land.k1000 <- (test.table$coef.beta.ACRES_POLY.k25 + (2*test.table$land * test.table$coef1.beta.I.ACRES_POLY.2..k25))*test.table$Sale.Val
colnames(test.table)
bandwidths2plot = c(44, 45, 46, 48, 50, 52)

pdf("analysis/02LWRComparisons/Land Size/LWRcoefPlotOverTime3_17b_land.pdf", height = 8, width = 12)
par(mfrow = c(2, 3))
for (i in bandwidths2plot) {
  myBandwidth = colnames(test.table)[i] # choose a bandwidth
  myX = DATAFRAME$TimePeriod[obs2run]
  myY = test.table[, myBandwidth]
  plot(myX, myY, cex = .2,
       xlab = "Time",
       ylab = myCoeff.beta,
       main = myBandwidth)
  abline(lm(myY ~ myX), col = "red")
}
dev.off()