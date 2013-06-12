## 2005 to 2010 Sales Locally Weighted Regression
load("~/Noise Hedonic Project/Noise-Hedonic/analysis/04MonteCarloSim/ModelBigCity/CopyOfSales20052010LWRmodel18-2013-04-13.RData") #Loads LWR output
workingdata <- read.dbf("../Data/R2GIS/CleanData/Sales20052010.dbf") #Loads input data file
names(output)
head(output$beta.MAX)
#Extract yhats, and betas for traffic noise and land size
test.table = data.frame (hat = output$yhats, beta.MAX = output$beta.MAX, beta.ACRES = output$beta.ACRES_POLY)
test.table$UNIQID <- rownames(test.table)
head(test.table)

test.data <- workingdata [ , ]

temp = merge(test.table, test.data, all = FALSE)
head(temp)
dataNames <- names(temp)
Output <- which(dataNames %in% c("hat.k200", "beta.ACRES.k200", "beta.MAX.k200", "X_Meter", "Y_Meter","SALE_VALUE","logSALE_VA", "UNIQID", "SALE_YR"))
temp = temp[, Output]

#Calculating marginal effects
#Traffic
temp$mfx.MAX.k200 <- temp$beta.MAX.k200 * temp$SALE_VALUE
#Land
temp$mfx.ACRES.k200 <- temp$beta.ACRES.k200 * temp$SALE_VALUE

#Calculating residuals
temp$res.k200 <- temp$logSALE_VA - temp$hat.k200

dataNames <- names(temp)
output2 <- which(dataNames %in% c("res.k200", "mfx.ACRES.k200","mfx.MAX.k200", "X_Meter", "Y_Meter","SALE_VALUE", "UNIQID", "SALE_YR"))
temp = temp[, output2]

write.dbf(temp,"../Data/R2GIS/LWRoutput/Sales2010_LWR_06_12.dbf")

