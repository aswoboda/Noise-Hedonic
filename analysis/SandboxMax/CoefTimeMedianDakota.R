

# starting to make figures comparing the LWR coefficients over time

require(foreign)
source("helper/LWRfunctions.R")

filePrefix = "../Data/R2GIS/CleanData/"
inputFile = "Sales20052010_Dakota.dbf"
DATAFRAME = read.dbf(paste0(filePrefix, inputFile))
obs2run = which(DATAFRAME$TimePeriod>11)

dataPath = "../Data/R2GIS/LWRoutput/Dakota/TimeLag06Months/"
filelist = list.files(path = dataPath)

files2open = filelist[which(substr(filelist, 1, 26) == "Sales20052010LWRoutput2013")]
myFile = 2 # this determines which file we're opening

load(paste0(dataPath, files2open[myFile], sep = ""))
# now we should have "output" loaded in our workspace and "MYMODEL" telling us which regression we ran

# the basic idea is to 
# 1) choose a coefficient and bandwidth
# 2) plot the coefficient value vs. the month of sale
# 3) add a linear best fit line to look for a trend
# 4) repeat above with different combinations of coefficients, bandwidths, and models

###########################'Linear' noise variable
names(output)
beta.noise = which(names(output)=="beta.MAX") #find which column beta noise is in
ses.noise = which(names(output)=="ses.MAX") #find which column standard error noise is in
myCoeff.beta = names(output)[beta.noise]# choose a coefficient to work with
myCoeff.ses = names(output)[ses.noise]
#Problem with test.table -- <NA> produced for row names in output file
test.table <- data.frame (time = DATAFRAME$TimePeriod[obs2run], coef = output[myCoeff.beta], se = output[myCoeff.ses], noise = DATAFRAME$MAX[obs2run], Sale.Val= DATAFRAME$SALE_VALUE[obs2run])
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
test.table$mfx.MAX.k800 <- (test.table$coef.beta.MAX..k800) * test.table$Sale.Val
test.table$mfx.MAX.k1000 <- (test.table$coef.beta.MAX..k1000) * test.table$Sale.Val
colnames(test.table)

#Time period in question will always be from 12 to 72
ti = c(12:72)
#Median table variable names may change as we fine-tune which bandwidths we are investigating.
med.table <- data.frame(med.mfx.MAX.k25=c(1:61),med.mfx.MAX.k50=c(1:61),med.mfx.MAX.k75=c(1:61),med.mfx.MAX.k100=c(1:61),med.mfx.MAX.k150=c(1:61),med.mfx.MAX.k200=c(1:61),med.mfx.MAX.k400=c(1:61),med.mfx.MAX.k600=c(1:61),med.mfx.MAX.k800=c(1:61),med.mfx.MAX.k1000=c(1:61))

#This for loop creates a table with 60 observations that contain the median marginal effect at each time period.
for (i in ti) {
  myTime = which(test.table$time == i) #chooses observations from given time period
  myPlace = i-11 #Chooses row we are going to place the median value of the marginal effect for the given time period.
  table.time = test.table [myTime,] #Creates table with only observations from given period
  #Perform median score
  table.time$med.mfx.MAX.k25 <- median(table.time$mfx.MAX.k25)
  table.time$med.mfx.MAX.k50 <- median(table.time$mfx.MAX.k50)
  table.time$med.mfx.MAX.k75 <- median(table.time$mfx.MAX.k75)
  table.time$med.mfx.MAX.k100 <- median(table.time$mfx.MAX.k100)
  table.time$med.mfx.MAX.k150 <- median(table.time$mfx.MAX.k150)
  table.time$med.mfx.MAX.k200 <- median(table.time$mfx.MAX.k200)
  table.time$med.mfx.MAX.k400 <- median(table.time$mfx.MAX.k400)
  table.time$med.mfx.MAX.k600 <- median(table.time$mfx.MAX.k600)
  table.time$med.mfx.MAX.k800 <- median(table.time$mfx.MAX.k800)
  table.time$med.mfx.MAX.k1000 <- median(table.time$mfx.MAX.k1000)
  #Place it in median table
  med.table$med.mfx.MAX.k25[myPlace] <- table.time$med.mfx.MAX.k25[1]
  med.table$med.mfx.MAX.k50[myPlace] <- table.time$med.mfx.MAX.k50[1]
  med.table$med.mfx.MAX.k75[myPlace] <- table.time$med.mfx.MAX.k75[1]
  med.table$med.mfx.MAX.k100[myPlace] <- table.time$med.mfx.MAX.k100[1]
  med.table$med.mfx.MAX.k150[myPlace] <- table.time$med.mfx.MAX.k150[1]
  med.table$med.mfx.MAX.k200[myPlace] <- table.time$med.mfx.MAX.k200[1]
  med.table$med.mfx.MAX.k400[myPlace] <- table.time$med.mfx.MAX.k400[1]
  med.table$med.mfx.MAX.k600[myPlace] <- table.time$med.mfx.MAX.k600[1]
  med.table$med.mfx.MAX.k800[myPlace] <- table.time$med.mfx.MAX.k800[1]
  med.table$med.mfx.MAX.k1000[myPlace] <- table.time$med.mfx.MAX.k1000[1]
  #Calculate which time period
  med.table$time.period[myPlace] <- table.time$time[1]
}

bandwidths2plot = c(2,3,4,6,8,10)

pdf("analysis/02LWRComparisons/Noise/LWRcoefPlotOverTime3_17b_MedianNoise.pdf", height = 8, width = 12)
par(mfrow = c(2, 3))
for (i in bandwidths2plot) {
  myBandwidth = colnames(med.table)[i] # choose a bandwidth
  myX = med.table$time.period
  myY = med.table[, myBandwidth]
  plot(myX, myY, cex = .2,
       xlab = "Time",
       ylab = myCoeff.beta,
       main = myBandwidth)
  abline(lm(myY ~ myX), col = "red")
}
dev.off()



######################Linear home size
names(output)
beta.sqft = which(names(output)=="beta.FIN_SQ_FT") #find which column beta home size is in
ses.sqft = which(names(output)=="ses.FIN_SQ_FT") #find which column standard error home size is in
myCoeff.beta = names(output)[beta.sqft]# choose a coefficient to work with
myCoeff.ses = names(output)[ses.sqft]
test.table <- data.frame (time = DATAFRAME$TimePeriod[obs2run], coef = output[myCoeff.beta], se = output[myCoeff.ses], Sale.Val= DATAFRAME$SALE_VALUE[obs2run])
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

#Time period in question will always be from 12 to 72
ti = c(12:72)
#Median table variable names may change as we fine-tune which bandwidths we are investigating.
med.table <- data.frame(med.mfx.sqft.k25=c(1:61),med.mfx.sqft.k50=c(1:61),med.mfx.sqft.k75=c(1:61),med.mfx.sqft.k100=c(1:61),med.mfx.sqft.k150=c(1:61),med.mfx.sqft.k200=c(1:61),med.mfx.sqft.k400=c(1:61),med.mfx.sqft.k600=c(1:61),med.mfx.sqft.k800=c(1:61),med.mfx.sqft.k1000=c(1:61))

#This for loop creates a table with 60 observations that contain the median marginal effect at each time period.
for (i in ti) {
  myTime = which(test.table$time == i) #chooses observations from given time period
  myPlace = i-11 #Chooses row we are going to place the median value of the marginal effect for the given time period.
  table.time = test.table [myTime,] #Creates table with only observations from given period
  #Perform median score
  table.time$med.mfx.sqft.k25 <- median(table.time$mfx.sqft.k25)
  table.time$med.mfx.sqft.k50 <- median(table.time$mfx.sqft.k50)
  table.time$med.mfx.sqft.k75 <- median(table.time$mfx.sqft.k75)
  table.time$med.mfx.sqft.k100 <- median(table.time$mfx.sqft.k100)
  table.time$med.mfx.sqft.k150 <- median(table.time$mfx.sqft.k150)
  table.time$med.mfx.sqft.k200 <- median(table.time$mfx.sqft.k200)
  table.time$med.mfx.sqft.k400 <- median(table.time$mfx.sqft.k400)
  table.time$med.mfx.sqft.k600 <- median(table.time$mfx.sqft.k600)
  table.time$med.mfx.sqft.k800 <- median(table.time$mfx.sqft.k800)
  table.time$med.mfx.sqft.k1000 <- median(table.time$mfx.sqft.k1000)
  #Place it in median table
  med.table$med.mfx.sqft.k25[myPlace] <- table.time$med.mfx.sqft.k25[1]
  med.table$med.mfx.sqft.k50[myPlace] <- table.time$med.mfx.sqft.k50[1]
  med.table$med.mfx.sqft.k75[myPlace] <- table.time$med.mfx.sqft.k75[1]
  med.table$med.mfx.sqft.k100[myPlace] <- table.time$med.mfx.sqft.k100[1]
  med.table$med.mfx.sqft.k150[myPlace] <- table.time$med.mfx.sqft.k150[1]
  med.table$med.mfx.sqft.k200[myPlace] <- table.time$med.mfx.sqft.k200[1]
  med.table$med.mfx.sqft.k400[myPlace] <- table.time$med.mfx.sqft.k400[1]
  med.table$med.mfx.sqft.k600[myPlace] <- table.time$med.mfx.sqft.k600[1]
  med.table$med.mfx.sqft.k800[myPlace] <- table.time$med.mfx.sqft.k800[1]
  med.table$med.mfx.sqft.k1000[myPlace] <- table.time$med.mfx.sqft.k1000[1]
  #Calculate which time period
  med.table$time.period[myPlace] <- table.time$time[1]
}

bandwidths2plot = c(2,3,4,6,8,10) 

pdf("analysis/02LWRComparisons/Home Size/LWRcoefPlotOverTime3_17b_Mediansqft.pdf", height = 8, width = 12)
par(mfrow = c(2, 3))
for (i in bandwidths2plot) {
  myBandwidth = colnames(med.table)[i] # choose a bandwidth
  myX = med.table$time.period
  myY = med.table[, myBandwidth]
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
test.table <- data.frame (time = DATAFRAME$TimePeriod[obs2run], coef = output[myCoeff.beta], coef1 = output[myCoeff.beta1], se = output[myCoeff.ses], se1 = output[myCoeff.ses1], land= DATAFRAME$ACRES_POLY[obs2run], Sale.Val = DATAFRAME$SALE_VALUE[obs2run])
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

#Time period in question will always be from 12 to 72
ti = c(12:72)
#Median table variable names may change as we fine-tune which bandwidths we are investigating.
med.table <- data.frame(med.mfx.land.k25=c(1:61),med.mfx.land.k50=c(1:61),med.mfx.land.k75=c(1:61),med.mfx.land.k100=c(1:61),med.mfx.land.k150=c(1:61),med.mfx.land.k200=c(1:61),med.mfx.land.k400=c(1:61),med.mfx.land.k600=c(1:61),med.mfx.land.k800=c(1:61),med.mfx.land.k1000=c(1:61))

#This for loop creates a table with 60 observations that contain the median marginal effect at each time period.
for (i in ti) {
  myTime = which(test.table$time == i) #chooses observations from given time period
  myPlace = i-11 #Chooses row we are going to place the median value of the marginal effect for the given time period.
  table.time = test.table [myTime,] #Creates table with only observations from given period
  #Perform median score
  table.time$med.mfx.land.k25 <- median(table.time$mfx.land.k25)
  table.time$med.mfx.land.k50 <- median(table.time$mfx.land.k50)
  table.time$med.mfx.land.k75 <- median(table.time$mfx.land.k75)
  table.time$med.mfx.land.k100 <- median(table.time$mfx.land.k100)
  table.time$med.mfx.land.k150 <- median(table.time$mfx.land.k150)
  table.time$med.mfx.land.k200 <- median(table.time$mfx.land.k200)
  table.time$med.mfx.land.k400 <- median(table.time$mfx.land.k400)
  table.time$med.mfx.land.k600 <- median(table.time$mfx.land.k600)
  table.time$med.mfx.land.k800 <- median(table.time$mfx.land.k800)
  table.time$med.mfx.land.k1000 <- median(table.time$mfx.land.k1000)
  #Place it in median table
  med.table$med.mfx.land.k25[myPlace] <- table.time$med.mfx.land.k25[1]
  med.table$med.mfx.land.k50[myPlace] <- table.time$med.mfx.land.k50[1]
  med.table$med.mfx.land.k75[myPlace] <- table.time$med.mfx.land.k75[1]
  med.table$med.mfx.land.k100[myPlace] <- table.time$med.mfx.land.k100[1]
  med.table$med.mfx.land.k150[myPlace] <- table.time$med.mfx.land.k150[1]
  med.table$med.mfx.land.k200[myPlace] <- table.time$med.mfx.land.k200[1]
  med.table$med.mfx.land.k400[myPlace] <- table.time$med.mfx.land.k400[1]
  med.table$med.mfx.land.k600[myPlace] <- table.time$med.mfx.land.k600[1]
  med.table$med.mfx.land.k800[myPlace] <- table.time$med.mfx.land.k800[1]
  med.table$med.mfx.land.k1000[myPlace] <- table.time$med.mfx.land.k1000[1]
  #Calculate which time period
  med.table$time.period[myPlace] <- table.time$time[1]
}

bandwidths2plot = c(2,3,4,6,8,10) 

pdf("analysis/02LWRComparisons/Land Size/LWRcoefPlotOverTime3_17b_MedianLand.pdf", height = 8, width = 12)
par(mfrow = c(2, 3))
for (i in bandwidths2plot) {
  myBandwidth = colnames(med.table)[i] # choose a bandwidth
  myX = med.table$time.period
  myY = med.table[, myBandwidth]
  plot(myX, myY, cex = .2,
       xlab = "Time",
       ylab = myCoeff.beta,
       main = myBandwidth)
  abline(lm(myY ~ myX), col = "red")
}
dev.off()