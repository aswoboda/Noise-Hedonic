

# starting to make figures comparing the LWR coefficients over time

require(foreign)
source("helper/LWRfunctions.R")

filePrefix = "../Data/R2GIS/CleanData/"
inputFile = "Sales20052010.dbf"
DATAFRAME = read.dbf(paste0(filePrefix, inputFile))
obs2run = which(DATAFRAME$TimePeriod>11)

dataPath = "../Noise-Hedonic/analysis/04MonteCarloSim/ModelBigCity/"
filelist = list.files(path = dataPath)

myFile = 3 # this determines which file we're opening

load(paste0(dataPath, filelist[myFile], sep = ""))
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
test.table <- data.frame (time = DATAFRAME$TimePeriod[obs2run], coef = output[myCoeff.beta], se = output[myCoeff.ses], noise = DATAFRAME$MAX[obs2run], Sale.Val= DATAFRAME$SALE_VALUE[obs2run])
head(test.table)
#Calculate marginal effects for log-transformed noise variable.
test.table$mfx.MAX.k200 <- (test.table$coef.beta.MAX.k200) * test.table$Sale.Val
colnames(test.table)

#Time period in question will always be from 12 to 72
ti = c(12:72)
#Median table variable names may change as we fine-tune which bandwidths we are investigating.
med.table <- data.frame(med.mfx.MAX.k200=c(1:61))

#This for loop creates a table with 60 observations that contain the median marginal effect at each time period.
for (i in ti) {
  myTime = which(test.table$time == i) #chooses observations from given time period
  myPlace = i-11 #Chooses row we are going to place the median value of the marginal effect for the given time period.
  table.time = test.table [myTime,] #Creates table with only observations from given period
  #Perform median score
  table.time$med.mfx.MAX.k200 <- median(table.time$mfx.MAX.k200)
  #Place it in median table
  med.table$med.mfx.MAX.k200[myPlace] <- table.time$med.mfx.MAX.k200[1]
  #Calculate which time period
  med.table$time.period[myPlace] <- table.time$time[1]
}


pdf("analysis/02LWRComparisons/Noise/LWRcoefPlotOverTime_BaseLWR_06_12_MedianNoise.pdf", height = 8, width = 12)
par(mfrow = c(2, 3))
myBandwidth = colnames(med.table)[1] # choose a bandwidth
myX = med.table$time.period
myY = med.table[, myBandwidth]
plot(myX, myY, cex = .2,
    xlab = "Time",
    ylab = myCoeff.beta,
    main = myBandwidth)
abline(lm(myY ~ myX), col = "red")
  #points(x,y)
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
test.table$mfx.sqft.k200 <- test.table$coef.beta.FIN_SQ_FT.k200 * test.table$Sale.Val

#Time period in question will always be from 12 to 72
ti = c(12:72)
#Median table variable names may change as we fine-tune which bandwidths we are investigating.
med.table <- data.frame(med.mfx.sqft.k200=c(1:61))

#This for loop creates a table with 60 observations that contain the median marginal effect at each time period.
for (i in ti) {
  myTime = which(test.table$time == i) #chooses observations from given time period
  myPlace = i-11 #Chooses row we are going to place the median value of the marginal effect for the given time period.
  table.time = test.table [myTime,] #Creates table with only observations from given period
  #Perform median score
  table.time$med.mfx.sqft.k200 <- median(table.time$mfx.sqft.k200)
  #Place it in median table
  med.table$med.mfx.sqft.k200[myPlace] <- table.time$med.mfx.sqft.k200[1]
  #Calculate which time period
  med.table$time.period[myPlace] <- table.time$time[1]
}


pdf("analysis/02LWRComparisons/Home Size/LWRcoefPlotOverTime_BaseLWR_06_12_MedianHome.pdf", height = 8, width = 12)
par(mfrow = c(2, 3))
  myBandwidth = colnames(med.table)[1] # choose a bandwidth
  myX = med.table$time.period
  myY = med.table[, myBandwidth]
  plot(myX, myY, cex = .2,
       xlab = "Time",
       ylab = myCoeff.beta,
       main = myBandwidth)
  abline(lm(myY ~ myX), col = "red")
dev.off()

###Linear land size
names(output)
beta.land= which(names(output)=="beta.ACRES_POLY") #find which column beta land size is in
ses.land = which(names(output)=="ses.ACRES_POLY") #find which column standard error land size is in
myCoeff.beta = names(output)[beta.land]# choose a coefficient to work with
myCoeff.ses = names(output)[ses.land]
test.table <- data.frame (time = DATAFRAME$TimePeriod[obs2run], coef = output[myCoeff.beta], se = output[myCoeff.ses], land= DATAFRAME$ACRES_POLY[obs2run], Sale.Val = DATAFRAME$SALE_VALUE[obs2run])
head(test.table)
#Calculate marginal effects for log-transformed noise variable.
test.table$mfx.land.k200 <- (test.table$coef.beta.ACRES_POLY.k200 *test.table$Sale.Val)
colnames(test.table)

#Time period in question will always be from 12 to 72
ti = c(12:72)
#Median table variable names may change as we fine-tune which bandwidths we are investigating.
med.table <- data.frame(med.mfx.land.k200=c(1:61))

#This for loop creates a table with 60 observations that contain the median marginal effect at each time period.
for (i in ti) {
  myTime = which(test.table$time == i) #chooses observations from given time period
  myPlace = i-11 #Chooses row we are going to place the median value of the marginal effect for the given time period.
  table.time = test.table [myTime,] #Creates table with only observations from given period
  #Perform median score
  table.time$med.mfx.land.k200 <- median(table.time$mfx.land.k200)
  #Place it in median table
  med.table$med.mfx.land.k200[myPlace] <- table.time$med.mfx.land.k200[1]
  #Calculate which time period
  med.table$time.period[myPlace] <- table.time$time[1]
}


pdf("analysis/02LWRComparisons/Land Size/LWRcoefPlotOverTime_BaseLWR_06_12_MedianLand.pdf", height = 8, width = 12)
par(mfrow = c(2, 3))
  myBandwidth = colnames(med.table)[1] # choose a bandwidth
  myX = med.table$time.period
  myY = med.table[, myBandwidth]
  plot(myX, myY, cex = .2,
       xlab = "Time",
       ylab = myCoeff.beta,
       main = myBandwidth)
  abline(lm(myY ~ myX), col = "red")
dev.off()