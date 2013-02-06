## 2005 to 2007 Locally Weighted Regression
load("~/Noise Hedonic Project/Data/R2GIS/LWRoutput/Sales20052007LWRoutput2013-02-03.RData") #Loads LWR output
workingdata2005 <- read.dbf("../Data/R2GIS/CleanData/Sales20052007.dbf") #Loads input data file
summary(names(output.raw))
names(output)
head(output$beta.MAX)
test.table = data.frame (beta.MAX = output$beta.MAX, beta.ACRES = output$beta.ACRES_POLY, PIN = names(output.raw))
head(test.table)

test.data <- workingdata2005 [ , ]

temp = merge(test.table, test.data, all = TRUE)
dataNames <- names(temp)
Output <- which(dataNames %in% c("beta.ACRES.k100", "beta.ACRES.k200", "beta.ACRES.k500", "beta.ACRES.k1000","beta.ACRES.k5000", "beta.MAX.k100", "beta.MAX.k200", "beta.MAX.k500", "beta.MAX.k1000","beta.MAX.k5000","X_Meter", "Y_Meter","SALE_VALUE", "UNIQID"))
temp = temp[, Output]

#Calculating marginal effects
#Traffic
temp$mfx.MAX.k100 <- temp$beta.MAX.k100 * temp$SALE_VALUE
temp$mfx.MAX.k200 <- temp$beta.MAX.k200 * temp$SALE_VALUE
temp$mfx.MAX.k500 <- temp$beta.MAX.k500 * temp$SALE_VALUE
temp$mfx.MAX.k1000 <- temp$beta.MAX.k1000 * temp$SALE_VALUE
temp$mfx.MAX.k5000 <- temp$beta.MAX.k5000 * temp$SALE_VALUE
#Land
temp$mfx.ACRES.k100 <- temp$beta.ACRES.k100 * temp$SALE_VALUE
temp$mfx.ACRES.k200 <- temp$beta.ACRES.k200 * temp$SALE_VALUE
temp$mfx.ACRES.k500 <- temp$beta.ACRES.k500 * temp$SALE_VALUE
temp$mfx.ACRES.k1000 <- temp$beta.ACRES.k1000 * temp$SALE_VALUE
temp$mfx.ACRES.k5000 <- temp$beta.ACRES.k5000 * temp$SALE_VALUE

dataNames <- names(temp)
output2 <- which(dataNames %in% c("mfx.ACRES.k100", "mfx.ACRES.k200", "mfx.ACRES.k500", "mfx.ACRES.k1000","mfx.ACRES.k5000", "mfx.MAX.k100", "mfx.MAX.k200", "mfx.MAX.k500", "mfx.MAX.k1000","mfx.MAX.k5000","X_Meter", "Y_Meter","SALE_VALUE", "UNIQID"))
temp = temp[, output2]

write.dbf(temp, "../Data/R2GIS/LWRoutput/Sales20052007_LWR.dbf")


## 2008 to 2010 Locally Weighted Regression
load("~/Noise Hedonic Project/Data/R2GIS/LWRoutput/Sales20082010LWRoutput2013-02-03.RData")
workingdata2008 <- read.dbf("../Data/R2GIS/CleanData/Sales20082010.dbf")
summary(names(output.raw))
names(output)
head(output$beta.MAX)
test.table = data.frame (beta.MAX = output$beta.MAX, beta.ACRES = output$beta.ACRES_POLY, PIN = names(output.raw))
head(test.table)

test.data <- workingdata2008 [ , ]

temp = merge(test.table, test.data, all = TRUE)
dataNames <- names(temp)
Output <- which(dataNames %in% c("beta.ACRES.k100", "beta.ACRES.k200", "beta.ACRES.k500", "beta.ACRES.k1000","beta.ACRES.k5000", "beta.MAX.k100", "beta.MAX.k200", "beta.MAX.k500", "beta.MAX.k1000","beta.MAX.k5000","X_Meter", "Y_Meter","SALE_VALUE", "UNIQID"))
temp = temp[, Output]

#Calculating marginal effects
#Traffic
temp$mfx.MAX.k100 <- temp$beta.MAX.k100 * temp$SALE_VALUE
temp$mfx.MAX.k200 <- temp$beta.MAX.k200 * temp$SALE_VALUE
temp$mfx.MAX.k500 <- temp$beta.MAX.k500 * temp$SALE_VALUE
temp$mfx.MAX.k1000 <- temp$beta.MAX.k1000 * temp$SALE_VALUE
temp$mfx.MAX.k5000 <- temp$beta.MAX.k5000 * temp$SALE_VALUE
#Land
temp$mfx.ACRES.k100 <- temp$beta.ACRES.k100 * temp$SALE_VALUE
temp$mfx.ACRES.k200 <- temp$beta.ACRES.k200 * temp$SALE_VALUE
temp$mfx.ACRES.k500 <- temp$beta.ACRES.k500 * temp$SALE_VALUE
temp$mfx.ACRES.k1000 <- temp$beta.ACRES.k1000 * temp$SALE_VALUE
temp$mfx.ACRES.k5000 <- temp$beta.ACRES.k5000 * temp$SALE_VALUE

dataNames <- names(temp)
output2 <- which(dataNames %in% c("mfx.ACRES.k100", "mfx.ACRES.k200", "mfx.ACRES.k500", "mfx.ACRES.k1000","mfx.ACRES.k5000", "mfx.MAX.k100", "mfx.MAX.k200", "mfx.MAX.k500", "mfx.MAX.k1000","mfx.MAX.k5000","X_Meter", "Y_Meter","SALE_VALUE", "UNIQID"))
temp = temp[, output2]

write.dbf(temp, "../Data/R2GIS/LWRoutput/Sales20082010_LWR.dbf")
