## 2005 to 2010 Sales Locally Weighted Regression
load("~/Noise Hedonic Project/Data/R2GIS/LWRoutput/Sales20052010LWRoutput2013-02-23.RData") #Loads LWR output
workingdata <- read.dbf("../Data/R2GIS/CleanData/Sales20052010.dbf") #Loads input data file
summary(names(output.raw))
names(output)
head(output$beta.MAX)
#Extract yhats, and betas for traffic noise and land size
test.table = data.frame (hat = output$yhats, beta.MAX = output$beta.MAX, beta.ACRES = output$beta.ACRES_POLY, UNIQID = names(output.raw))
head(test.table)

test.data <- workingdata [ , ]

temp = merge(test.table, test.data, all = FALSE)
dataNames <- names(temp)
Output <- which(dataNames %in% c("hat.k100","hat.k200", "hat.k500", "hat.k1000", "hat.k2000",  "beta.ACRES.k100", "beta.ACRES.k200", "beta.ACRES.k500", "beta.ACRES.k1000","beta.ACRES.k2000", "beta.MAX.k100", "beta.MAX.k200", "beta.MAX.k500", "beta.MAX.k1000","beta.MAX.k2000","X_Meter", "Y_Meter","SALE_VALUE","logSALE_VA", "UNIQID", "SALE_YR"))
temp = temp[, Output]

#Calculating marginal effects
#Traffic
temp$mfx.MAX.k100 <- temp$beta.MAX.k100 * temp$SALE_VALUE
temp$mfx.MAX.k200 <- temp$beta.MAX.k200 * temp$SALE_VALUE
temp$mfx.MAX.k500 <- temp$beta.MAX.k500 * temp$SALE_VALUE
temp$mfx.MAX.k1000 <- temp$beta.MAX.k1000 * temp$SALE_VALUE
temp$mfx.MAX.k2000 <- temp$beta.MAX.k2000 * temp$SALE_VALUE
#Land
temp$mfx.ACRES.k100 <- temp$beta.ACRES.k100 * temp$SALE_VALUE
temp$mfx.ACRES.k200 <- temp$beta.ACRES.k200 * temp$SALE_VALUE
temp$mfx.ACRES.k500 <- temp$beta.ACRES.k500 * temp$SALE_VALUE
temp$mfx.ACRES.k1000 <- temp$beta.ACRES.k1000 * temp$SALE_VALUE
temp$mfx.ACRES.k2000 <- temp$beta.ACRES.k2000 * temp$SALE_VALUE

#Calculating residuals
temp$res.k100 <- temp$logSALE_VA - temp$hat.k100
temp$res.k200 <- temp$logSALE_VA - temp$hat.k200
temp$res.k500 <- temp$logSALE_VA - temp$hat.k500
temp$res.k1000 <- temp$logSALE_VA - temp$hat.k1000
temp$res.k2000 <- temp$logSALE_VA - temp$hat.k2000

dataNames <- names(temp)
output2 <- which(dataNames %in% c("res.k100","res.k200", "res.k500", "res.k1000", "res.k2000","mfx.ACRES.k100", "mfx.ACRES.k200", "mfx.ACRES.k500", "mfx.ACRES.k1000","mfx.ACRES.k2000", "mfx.MAX.k100", "mfx.MAX.k200", "mfx.MAX.k500", "mfx.MAX.k1000","mfx.MAX.k2000","X_Meter", "Y_Meter","SALE_VALUE", "UNIQID", "SALE_YR"))
temp = temp[, output2]

#Extracting marginal effects by year
output2005 <- which(temp$SALE_YR == 2005)
output2006 <- which(temp$SALE_YR == 2006)
output2007 <- which(temp$SALE_YR == 2007)
output2008 <- which(temp$SALE_YR == 2008)
output2009 <- which(temp$SALE_YR == 2009)
output2010 <- which(temp$SALE_YR == 2010)

fx2005 <- temp[output2005, ]
fx2006 <- temp[output2006, ]
fx2007 <- temp[output2007, ]
fx2008 <- temp[output2008, ]
fx2009 <- temp[output2009, ]
fx2010 <- temp[output2010, ]


write.dbf(fx2005, "../Data/R2GIS/LWRoutput/Sales2005_LWR_02_23.dbf")
write.dbf(fx2006, "../Data/R2GIS/LWRoutput/Sales2006_LWR_02_23.dbf")
write.dbf(fx2007, "../Data/R2GIS/LWRoutput/Sales2007_LWR_02_23.dbf")
write.dbf(fx2008, "../Data/R2GIS/LWRoutput/Sales2008_LWR_02_23.dbf")
write.dbf(fx2009, "../Data/R2GIS/LWRoutput/Sales2009_LWR_02_23.dbf")
write.dbf(fx2010, "../Data/R2GIS/LWRoutput/Sales2010_LWR_02_23.dbf")

