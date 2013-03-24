#This script runs the most up to date global regression models that include interaction effects and non linear relationships.
#After running the model, the script calculates the marginal effects for land and traffic noise.
#Lastly, the script writes a .dbf table that includes marginal effects for land and traffic noise, residuals, xy coordinates (in meters) and a Unique ID that is ready for analysis in ArcGIS

##Preparation for regression
require(zoo)
require(lmtest)
require(car)

####### 2005 to 2010 Sales Model ########
workingdata20052010 <- read.dbf("../Data/R2GIS/CleanData/Sales20052010.dbf")
workingdata20052010$PostCrash = 0
crashobs <- which(workingdata20052010$TimePeriod>46)
workingdata20052010$PostCrash[crashobs] = 1

##Run model (All)
model.SaleValue20052010 <- lm (logSALE_VA ~ COUNTY_ID + CITY + factor(SALE_YR)+ ACRES_POLY+ ACRES_POLY * CBD_dist + I(ACRES_POLY^2)* I(CBD_dist ^2) +ACRES_POLY*I(CBD_dist^2) +I(ACRES_POLY^2)*CBD_dist + MAX + HOMESTEAD + log(FIN_SQ_FT) + YEAR_BUILT + LAKE_dist + I(LAKE_dist^2) + PARK_dist + I(PARK_dist^2)  + MCA3 + SHOP_dist + I(SHOP_dist^2) + MED_INCOME + COLLEGE_di + SALE_MO, data= workingdata20052010)
summary(model.SaleValue20052010)
##Calculating marginal effects for land and traffic noise (All)
mfx.TRAFFIC.20052010 = (model.SaleValue20052010$coefficients["MAX"]) * (workingdata20052010$SALE_VALUE)
mfx.LAND.20052010 = (model.SaleValue20052010$coefficients["ACRES_POLY"]+ (2*workingdata20052010$ACRES_POLY * model.SaleValue20052010$coefficients["I(ACRES_POLY^2)"])+
  (model.SaleValue20052010$coefficients["ACRES_POLY:CBD_dist"]*workingdata20052010$CBD_dist) + (model.SaleValue20052010$coefficients["ACRES_POLY:I(CBD_dist^2)"]*(workingdata20052010$CBD_dist ^2)) + 
  (model.SaleValue20052010$coefficients["CBD_dist:I(ACRES_POLY^2)"]*2*workingdata20052010$CBD_dist*workingdata20052010$ACRES_POLY)+
  (model.SaleValue20052010$coefficients["I(ACRES_POLY^2):I(CBD_dist^2)"]*2*workingdata20052010$ACRES_POLY*(workingdata20052010$CBD_dist ^2))) * (workingdata20052010$SALE_VALUE)


##Run model (Post Crash)
model.SaleValue20052010.post <- lm (logSALE_VA ~ COUNTY_ID + CITY + factor(SALE_YR)+ ACRES_POLY+ ACRES_POLY * CBD_dist +ACRES_POLY*I(CBD_dist^2) + MAX + HOMESTEAD + log(FIN_SQ_FT) + YEAR_BUILT + LAKE_dist + I(LAKE_dist^2) + PARK_dist + I(PARK_dist^2)  + MCA3 + SHOP_dist + I(SHOP_dist^2) + MED_INCOME + COLLEGE_di + SALE_MO, data= workingdata20052010, subset=(PostCrash == 1))
summary(model.SaleValue20052010.post)
##Run model (Pre Crash)
model.SaleValue20052010.pre <- lm (logSALE_VA ~ COUNTY_ID + CITY + factor(SALE_YR)+ ACRES_POLY+ ACRES_POLY * CBD_dist + I(ACRES_POLY^2)* I(CBD_dist ^2) +ACRES_POLY*I(CBD_dist^2) +I(ACRES_POLY^2)*CBD_dist + MAX + HOMESTEAD + log(FIN_SQ_FT) + YEAR_BUILT + LAKE_dist + I(LAKE_dist^2) + PARK_dist + I(PARK_dist^2)  + MCA3 + SHOP_dist + I(SHOP_dist^2) + MED_INCOME + COLLEGE_di + SALE_MO, data= workingdata20052010, subset=(PostCrash == 0))
summary(model.SaleValue20052010.pre)
##Creating/writing .dbf file that is ready for ArcGIS analysis
workingdata20052010$RowName = row.names(workingdata20052010)
table.data = data.frame(Res = model.SaleValue20052010$residuals, mfxTRAFFIC = mfx.TRAFFIC.20052010, mfxLAND = mfx.LAND.20052010, RowName = names (model.SaleValue20052010$residuals))
temp = merge(table.data, workingdata20052010, all = TRUE)
dataNames <- names(temp)
Output <- which(dataNames %in% c("Res", "mfxTRAFFIC", "mfxLAND", "PIN", "X_Meter", "Y_Meter", "UNIQID"))
temp = temp[, Output]
write.dbf(temp, "../Data/R2GIS/GlobalRegressionOutput/Sales20052010.dbf")


####### 2005 to 2010 Sales Model for Dakota County########
workingdata20052010.Dakota <- read.dbf("../Data/R2GIS/CleanData/Sales20052010_Dakota.dbf")
##Run model
model.SaleValue20052010.Dakota <- lm (logSALE_VA ~  CITY + factor(SALE_YR)  + (ACRES_POLY*CBD_dist) + I(ACRES_POLY ^2)*I(CBD_dist ^2) + ACRES_POLY*I(CBD_dist ^2)+ I(ACRES_POLY ^2)*CBD_dist +HOMESTEAD + log(FIN_SQ_FT) + YEAR_BUILT + MAX * I(CBD_dist ^2)+ MAX*CBD_dist + PARK_dist  + LAKE_dist+ I(LAKE_dist ^2) + MCA3 + MED_INCOME + COLLEGE_di+ I(COLLEGE_di ^2) + SHOP_dist + SALE_MO+BEDS + BATH + I(BATH^2) + BLDG_QUAL, data=workingdata20052010.Dakota)
summary(model.SaleValue20052010.Dakota)
##Calculating marginal effects for land and traffic noise
mfx.TRAFFIC.20052010.Dakota = ((model.SaleValue20052010.Dakota$coefficients["MAX"])+((workingdata20052010.Dakota$CBD_dist * model.SaleValue20052010.Dakota$coefficients["CBD_dist:MAX"]))+
  ((workingdata20052010.Dakota$CBD_dist^2 * model.SaleValue20052010.Dakota$coefficients["I(CBD_dist^2):MAX"]))) * (workingdata20052010.Dakota$SALE_VALUE)
mfx.LAND.20052010.Dakota = (model.SaleValue20052010.Dakota$coefficients["ACRES_POLY"]+ (2*workingdata20052010.Dakota$ACRES_POLY * model.SaleValue20052010.Dakota$coefficients["I(ACRES_POLY^2)"])+
  (model.SaleValue20052010.Dakota$coefficients["ACRES_POLY:CBD_dist"]*workingdata20052010.Dakota$CBD_dist) + (model.SaleValue20052010.Dakota$coefficients["ACRES_POLY:I(CBD_dist^2)"]*(workingdata20052010.Dakota$CBD_dist ^2)) + 
  (model.SaleValue20052010.Dakota$coefficients["CBD_dist:I(ACRES_POLY^2)"]*2*workingdata20052010.Dakota$CBD_dist*workingdata20052010.Dakota$ACRES_POLY)+
  (model.SaleValue20052010.Dakota$coefficients["I(ACRES_POLY^2):I(CBD_dist^2)"]*2*workingdata20052010.Dakota$ACRES_POLY*(workingdata20052010.Dakota$CBD_dist ^2))) * (workingdata20052010.Dakota$SALE_VALUE)

##Creating/writing .dbf file that is ready for ArcGIS analysis
workingdata20052010.Dakota$RowName = row.names(workingdata20052010.Dakota)
table.data = data.frame(Res = model.SaleValue20052010.Dakota$residuals, mfxTRAFFIC = mfx.TRAFFIC.20052010.Dakota, mfxLAND = mfx.LAND.20052010.Dakota, RowName = names (model.SaleValue20052010.Dakota$residuals))
temp = merge(table.data, workingdata20052010.Dakota, all = TRUE)
dataNames <- names(temp)
Output <- which(dataNames %in% c("Res", "mfxTRAFFIC", "mfxLAND", "PIN", "X_Meter", "Y_Meter", "UNIQID"))
temp = temp[, Output]
write.dbf(temp, "../Data/R2GIS/GlobalRegressionOutput/Sales20052010_Dakota.dbf")

####### 2005 to 2010 Sales Model for Dakota County w/o Structural Variables########
##Run model
model.SaleValue20052010.Dakota.NoBeds <- lm (logSALE_VA ~  CITY + factor(SALE_YR)  + (ACRES_POLY*CBD_dist) + I(ACRES_POLY ^2)*I(CBD_dist ^2) + ACRES_POLY*I(CBD_dist ^2)+ I(ACRES_POLY ^2)*CBD_dist +HOMESTEAD + log(FIN_SQ_FT) + YEAR_BUILT + MAX * I(CBD_dist ^2)+ MAX*CBD_dist + PARK_dist  + LAKE_dist+ I(LAKE_dist ^2) + MCA3 + MED_INCOME + COLLEGE_di+ I(COLLEGE_di ^2) + SHOP_dist + SALE_MO, data=workingdata20052010.Dakota)
summary(model.SaleValue20052010.Dakota.NoBeds)
##Calculating marginal effects for land and traffic noise
mfx.TRAFFIC.20052010.Dakota.NoBeds = ((model.SaleValue20052010.Dakota.NoBeds$coefficients["MAX"])+((workingdata20052010.Dakota$CBD_dist * model.SaleValue20052010.Dakota.NoBeds$coefficients["CBD_dist:MAX"])+
  ((workingdata20052010.Dakota$CBD_dist^2 * model.SaleValue20052010.Dakota.NoBeds$coefficients["I(CBD_dist^2):MAX"])))) * (workingdata20052010.Dakota$SALE_VALUE)

##Creating/writing .dbf file that is ready for ArcGIS analysis
workingdata20052010.Dakota$RowName = row.names(workingdata20052010.Dakota)
table.data = data.frame(Res = model.SaleValue20052010.Dakota$residuals, mfxTRAFFIC = mfx.TRAFFIC.20052010.Dakota, mfxLAND = mfx.LAND.20052010.Dakota, RowName = names (model.SaleValue20052010.Dakota$residuals))
temp = merge(table.data, workingdata20052010.Dakota, all = TRUE)
dataNames <- names(temp)
Output <- which(dataNames %in% c("Res", "mfxTRAFFIC", "mfxLAND", "PIN", "X_Meter", "Y_Meter", "UNIQID"))
temp = temp[, Output]
write.dbf(temp, "../Data/R2GIS/GlobalRegressionOutput/Sales20052010_Dakota.dbf")


#Plot marginal effects to compare between DAK county datasets
pdf("analysis/03DakotaLWR/GlobalReg.Traffic.pdf", height = 8, width = 12)
myX = mfx.TRAFFIC.20052010.Dakota
myY = mfx.TRAFFIC.20052010.Dakota.NoBeds
plot(myX, myY, cex = .2,
xlab = "w/ structural vars",
ylab = "NO structural vars",
main = "mfx.Traffic.Dakota")
reg <- lm(myY ~ myX)
abline(reg, col = "red")
abline(0,1)
title(summary(reg)$r.squared, line=-1)
dev.off()
