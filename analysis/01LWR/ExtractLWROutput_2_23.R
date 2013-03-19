## 2005 to 2010 Sales Locally Weighted Regression
load("~/Noise Hedonic Project/Data/R2GIS/LWRoutput/Sales20052010LWRoutput2013-03-17e.RData") #Loads LWR output
workingdata <- read.dbf("../Data/R2GIS/CleanData/Sales20052010.dbf") #Loads input data file
#summary(names(output.raw))
#names(output)
#Extract yhats, and betas for traffic noise and land size
obs2run = which(workingdata$TimePeriod>11)
test.table = data.frame (hat = output$yhats, UNIQID = workingdata$UNIQID[obs2run])
head(test.table)

test.data <- workingdata [ , ]

temp = merge(test.table, test.data, all = FALSE)
dataNames <- names(temp)
Output <- which(dataNames %in% c("hat.k25","hat.k50","hat.k75", "hat.k100","hat.k150", "hat.k200", "hat.k400", "hat.k600", "hat.k800", "hat.k1000","X_Meter", "Y_Meter","logSALE_VA", "UNIQID"))
temp = temp[, Output]


#Calculating residuals
temp$res.k25 <- temp$logSALE_VA - temp$hat.k25
temp$res.k50 <- temp$logSALE_VA - temp$hat.k50
temp$res.k75 <- temp$logSALE_VA - temp$hat.k75
temp$res.k100 <- temp$logSALE_VA - temp$hat.k100
temp$res.k150 <- temp$logSALE_VA - temp$hat.k150
temp$res.k200 <- temp$logSALE_VA - temp$hat.k200
temp$res.k400 <- temp$logSALE_VA - temp$hat.k400
temp$res.k600 <- temp$logSALE_VA - temp$hat.k600
temp$res.k800 <- temp$logSALE_VA - temp$hat.k800
temp$res.k1000 <- temp$logSALE_VA - temp$hat.k1000

dataNames <- names(temp)
output2 <- which(dataNames %in% c("res.k25","res.k50","res.k75", "res.k100","res.k150", "res.k200", "res.k400", "res.k600", "res.k800", "res.k1000","X_Meter", "Y_Meter","SALE_VALUE", "UNIQID"))
temp = temp[, output2]


write.dbf(temp, "../Data/R2GIS/LWRoutput/Sales20052010_LWR_03_17e.dbf")

