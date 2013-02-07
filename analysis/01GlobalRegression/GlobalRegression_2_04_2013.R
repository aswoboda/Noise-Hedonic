#This script runs the most up to date global regression models that include interaction effects and non linear relationships.
#After running the model, the script calculates the marginal effects for land and traffic noise.
#Lastly, the script writes a .dbf table that includes marginal effects for land and traffic noise, residuals, xy coordinates (in meters) and a Unique ID that is ready for analysis in ArcGIS

##Preparation for regression
require(zoo)
require(lmtest)
require(car)

####### 2005 to 2007 Sales Model ########
workingdata2005 <- read.dbf("../Data/R2GIS/CleanData/Sales20052007.dbf")
##Run model
model.SaleValue2005 <- lm (logSALE_VA ~ COUNTY_ID + CITY + factor(SALE_YR)  + ACRES_POLY * CBD_dist + I(ACRES_POLY^2) * CBD_dist + I(ACRES_POLY^2) * I(CBD_dist^2)+ ACRES_POLY * I(CBD_dist^2)+log(MAX) + HOMESTEAD + log(FIN_SQ_FT) + YEAR_BUILT + LAKE_dist + I(LAKE_dist^2) + PARK_dist + I(PARK_dist^2)  + MCA3 + SHOP_dist + I(SHOP_dist^2) + MED_INCOME + COLLEGE_di + SALE_MO, data=workingdata2005)
summary(model.SaleValue2005)
##Calculating marginal effects for land and traffic noise
mfx.TRAFFIC.2005 = (model.SaleValue2005$coefficients["log(MAX)"]/workingdata2005$MAX) * (workingdata2005$SALE_VALUE)
mfx.LAND.2005 = (model.SaleValue2005$coefficients["ACRES_POLY"]+ (2*workingdata2005$ACRES_POLY * model.SaleValue2005$coefficients["I(ACRES_POLY^2)"])+
  (model.SaleValue2005$coefficients["ACRES_POLY:CBD_dist"]*workingdata2005$CBD_dist) + (model.SaleValue2005$coefficients["ACRES_POLY:I(CBD_dist^2)"]*(workingdata2005$CBD_dist ^2)) + 
  (model.SaleValue2005$coefficients["CBD_dist:I(ACRES_POLY^2)"]*2*workingdata2005$CBD_dist*workingdata2005$ACRES_POLY)+
  (model.SaleValue2005$coefficients["I(ACRES_POLY^2):I(CBD_dist^2)"]*2*workingdata2005$ACRES_POLY*(workingdata2005$CBD_dist ^2))) * (workingdata2005$SALE_VALUE)
##Creating/writing .dbf file that is ready for ArcGIS analysis
workingdata2005$RowName = row.names(workingdata2005)
table.data = data.frame(Res = model.SaleValue2005$residuals, mfxTRAFFIC = mfx.TRAFFIC.2005, mfxLAND = mfx.LAND.2005, RowName = names (model.SaleValue2005$residuals))
temp = merge(table.data, workingdata2005, all = TRUE)
dataNames <- names(temp)
Output <- which(dataNames %in% c("Res", "mfxTRAFFIC", "mfxLAND", "PIN", "X_Meter", "Y_Meter", "UNIQID"))
temp = temp[, Output]
write.dbf(temp, "../Data/R2GIS/GlobalRegressionOutput/Sales20052007.dbf")


####### 2008 to 2010 Sales Model ########
workingdata2008 <- read.dbf("../Data/R2GIS/CleanData/Sales20082010.dbf")
##Run model
model.SaleValue2008 <- lm (logSALE_VA ~ COUNTY_ID + CITY + factor(SALE_YR)  + ACRES_POLY * CBD_dist + I(ACRES_POLY^2)+ACRES_POLY*I(CBD_dist^2)+ log(MAX) * CBD_dist +  HOMESTEAD + log(FIN_SQ_FT) + YEAR_BUILT + LAKE_dist + I(LAKE_dist^2) + PARK_dist + I(PARK_dist^2)  + MCA3 + SHOP_dist + I(SHOP_dist^2) + MED_INCOME + COLLEGE_di + SALE_MO, data=workingdata2008)
summary(model.SaleValue2008)
##Calculating marginal effects for land and traffic noise
mfx.TRAFFIC.2008 = ((model.SaleValue2008$coefficients["log(MAX)"]*(1/(workingdata2008$MAX))) + (model.SaleValue2008$coefficients["CBD_dist:log(MAX)"]*(workingdata2008$CBD_dist/workingdata2008$MAX))) * (workingdata2008$SALE_VALUE)
mfx.LAND.2008 = (model.SaleValue2008$coefficients["ACRES_POLY"]+ (2*workingdata2008$ACRES_POLY * model.SaleValue2008$coefficients["I(ACRES_POLY^2)"])+
  (model.SaleValue2008$coefficients["ACRES_POLY:CBD_dist"]*workingdata2008$CBD_dist) + (model.SaleValue2008$coefficients["ACRES_POLY:I(CBD_dist^2)"]*(workingdata2008$CBD_dist ^2))) * (workingdata2008$SALE_VALUE)
##Creating/writing .dbf file that is ready for ArcGIS analysis
workingdata2008$RowName = row.names(workingdata2008)
table.data = data.frame(Res = model.SaleValue2008$residuals, mfxTRAFFIC = mfx.TRAFFIC.2008, mfxLAND = mfx.LAND.2008, RowName = names (model.SaleValue2008$residuals))
temp = merge(table.data, workingdata2008, all = TRUE)
dataNames <- names(temp)
Output <- which(dataNames %in% c("Res", "mfxTRAFFIC", "mfxLAND", "PIN", "X_Meter", "Y_Meter", "UNIQID"))
temp = temp[, Output]
write.dbf(temp, "../Data/R2GIS/GlobalRegressionOutput/Sales20082010.dbf")


####### 2005 to 2010 Sales Model ########
workingdata20052010 <- read.dbf("../Data/R2GIS/CleanData/Sales20052010.dbf")
##Run model
model.SaleValue20052010 <- lm (logSALE_VA ~ COUNTY_ID + CITY + factor(SALE_YR)  + ACRES_POLY * CBD_dist + I(ACRES_POLY^2)* I(CBD_dist ^2) +ACRES_POLY*I(CBD_dist^2) +I(ACRES_POLY^2)*CBD_dist + log(MAX) + HOMESTEAD + log(FIN_SQ_FT) + YEAR_BUILT + LAKE_dist + I(LAKE_dist^2) + PARK_dist + I(PARK_dist^2)  + MCA3 + SHOP_dist + I(SHOP_dist^2) + MED_INCOME + COLLEGE_di + SALE_MO, data= workingdata20052010)
summary(model.SaleValue20052010)
##Calculating marginal effects for land and traffic noise
mfx.TRAFFIC.20052010 = (model.SaleValue20052010$coefficients["log(MAX)"]/workingdata20052010$MAX) * (workingdata20052010$SALE_VALUE)
mfx.LAND.20052010 = (model.SaleValue20052010$coefficients["ACRES_POLY"]+ (2*workingdata20052010$ACRES_POLY * model.SaleValue20052010$coefficients["I(ACRES_POLY^2)"])+
  (model.SaleValue20052010$coefficients["ACRES_POLY:CBD_dist"]*workingdata20052010$CBD_dist) + (model.SaleValue20052010$coefficients["ACRES_POLY:I(CBD_dist^2)"]*(workingdata20052010$CBD_dist ^2)) + 
  (model.SaleValue20052010$coefficients["CBD_dist:I(ACRES_POLY^2)"]*2*workingdata20052010$CBD_dist*workingdata20052010$ACRES_POLY)+
  (model.SaleValue20052010$coefficients["I(ACRES_POLY^2):I(CBD_dist^2)"]*2*workingdata20052010$ACRES_POLY*(workingdata20052010$CBD_dist ^2))) * (workingdata20052010$SALE_VALUE)

##Creating/writing .dbf file that is ready for ArcGIS analysis
workingdata20052010$RowName = row.names(workingdata20052010)
table.data = data.frame(Res = model.SaleValue20052010$residuals, mfxTRAFFIC = mfx.TRAFFIC.20052010, mfxLAND = mfx.LAND.20052010, RowName = names (model.SaleValue20052010$residuals))
temp = merge(table.data, workingdata20052010, all = TRUE)
dataNames <- names(temp)
Output <- which(dataNames %in% c("Res", "mfxTRAFFIC", "mfxLAND", "PIN", "X_Meter", "Y_Meter", "UNIQID"))
temp = temp[, Output]
write.dbf(temp, "../Data/R2GIS/GlobalRegressionOutput/Sales20052010.dbf")
