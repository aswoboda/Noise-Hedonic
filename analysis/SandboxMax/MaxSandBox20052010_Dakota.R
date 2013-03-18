library (foreign)
workingdata <- read.dbf("../Data/R2GIS/CleanData/Sales20052010_Dakota.dbf")
summary(workingdata)

###Preparation for regression analysis, load packages for tests
require(zoo)
require(lmtest)
require(car)



######Normality Issues arise so we investigate with the log transformation on the dependent variable
##Dependent variable: Sales Value (log transformed) w/o MCA5, SDNUM
model.logSaleValue1 <- lm (logSALE_VA ~  CITY + factor(SALE_YR)  + ACRES_POLY+ HOMESTEAD + log(FIN_SQ_FT) + YEAR_BUILT + log(MAX) + PARK_dist + LAKE_dist + MCA3 + MED_INCOME+COLLEGE_di+ SHOP_dist + CBD_dist + SALE_MO+BEDS+BATH+BLDG_QUAL, data=workingdata)
summary(model.logSaleValue1)
anova(model.logSaleValue1)
#Diagnostics
outlierTest(model.logSaleValue1)
ncvTest(model.logSaleValue1) #
dwtest (model.logSaleValue1) #autocorrelation
bptest(model.logSaleValue1)
plot(model.logSaleValue1) 

###Dependent variable: Sales Value (log transformed) w/o MCA5, SDNUM, SALE_MO
model.logSaleValue2 <- lm (logSALE_VA ~ CITY + factor(SALE_YR)  + ACRES_POLY+ I(ACRES_POLY ^2) + HOMESTEAD + log(FIN_SQ_FT) + YEAR_BUILT + log(MAX) + PARK_dist + I(PARK_dist ^2) + LAKE_dist+ I(LAKE_dist ^2) + MCA3 + MED_INCOME + COLLEGE_di+ I(COLLEGE_di ^2) + SHOP_dist+ I(SHOP_dist^2) + CBD_dist + I(CBD_dist ^2)+ SALE_MO + BEDS + I(BEDS^2) + BATH + I(BATH^2) + BLDG_QUAL, data=workingdata)
summary(model.logSaleValue2)
anova( model.logSaleValue2)
plot(model.logSaleValue2)


###Dependent variable: Sales Value (log transformed) w/ GARAGE
model.logSaleValue3 <- lm (logSALE_VA ~  CITY + factor(SALE_YR)  + (ACRES_POLY*CBD_dist) + I(ACRES_POLY ^2) + HOMESTEAD + log(FIN_SQ_FT) + YEAR_BUILT + (log(MAX)*CBD_dist) + PARK_dist + I(PARK_dist ^2) + LAKE_dist+ I(LAKE_dist ^2) + MCA3 + MED_INCOME + COLLEGE_di+ I(COLLEGE_di ^2) + SHOP_dist+ I(SHOP_dist^2) + I(CBD_dist ^2)+ SALE_MO+BEDS + BATH + I(BATH^2) + BLDG_QUAL, data=workingdata)
summary(model.logSaleValue3)
plot(model.logSaleValue3)

###Dependent variable: Sales Value (log transformed) w/ GARAGE
model.logSaleValue4 <- lm (logSALE_VA ~  CITY + factor(SALE_YR)  + (ACRES_POLY*CBD_dist) + I(ACRES_POLY ^2)*I(CBD_dist ^2) + ACRES_POLY*I(CBD_dist ^2)+ I(ACRES_POLY ^2)*CBD_dist +HOMESTEAD + log(FIN_SQ_FT) + YEAR_BUILT + log(MAX) * I(CBD_dist ^2)+ log(MAX)*CBD_dist + PARK_dist  + LAKE_dist+ I(LAKE_dist ^2) + MCA3 + MED_INCOME + COLLEGE_di+ I(COLLEGE_di ^2) + SHOP_dist + SALE_MO+BEDS + BATH + I(BATH^2) + BLDG_QUAL, data=workingdata)
summary(model.logSaleValue4)


#########From 20082010SalesData_Interaction.Rmd, takes into account interaction terms
model.SaleValue5 <- lm (logSALE_VA ~ COUNTY_ID + CITY + factor(SALE_YR)* log(MAX)  + ACRES_POLY * CBD_dist + I(ACRES_POLY^2)* I(CBD_dist ^2) +ACRES_POLY*I(CBD_dist^2) +I(ACRES_POLY^2)*CBD_dist + HOMESTEAD + log(FIN_SQ_FT) + YEAR_BUILT + LAKE_dist + I(LAKE_dist^2) + PARK_dist + I(PARK_dist^2)  + MCA3 + SHOP_dist + I(SHOP_dist^2) + MED_INCOME + COLLEGE_di + SALE_MO, data=workingdata)
summary(model.SaleValue5)

#Acquire marginal effect for Acres and Traffic Noise and place it into a table
mfx.TRAFFIC = ((model.SaleValue5$coefficients["log(MAX)"]*(1/(workingdata$MAX))) + (model.SaleValue5$coefficients["CBD_dist:log(MAX)"]*(workingdata$CBD_dist/workingdata$MAX))) * (workingdata$SALE_VALUE)

mfx.LAND = (model.SaleValue5$coefficients["ACRES_POLY"]+ (2*workingdata$ACRES_POLY * model.SaleValue5$coefficients["I(ACRES_POLY^2)"])+
(model.SaleValue5$coefficients["ACRES_POLY:CBD_dist"]*workingdata$CBD_dist) + (model.SaleValue5$coefficients["ACRES_POLY:I(CBD_dist^2)"]*(workingdata$CBD_dist ^2))) * (workingdata$SALE_VALUE)
             

#Add column of row names to the workingdata
workingdata$RowName = row.names(workingdata)
mfx.data = data.frame(mfxTRAFFIC = mfx.TRAFFIC, mfxLAND = mfx.LAND, RowName = names (model.SaleValue5$residuals))
#Merge the resData table from model with workingdata table
temp2 = merge(mfx.data, workingdata, all = TRUE)
#From temporary table that merged resData table and workingdata table, extract residuals and PIN
names(temp2)
temp2 = temp2[, c(2,3, 5)]
write.dbf(temp2, "../Data/R2GIS/20082010mfx_Land.dbf")

##Write .dbf file for residual table to plug into Spatial Autocorrelatoin (Moran's I) in model builder
##ResidualTable
#Grab residual from model and row name from the model
resData3 = data.frame(Res = model.SaleValue5$residuals, RowName = names(model.SaleValue5$residuals))
#Add column of row names to the workingdata
workingdata$RowName = row.names(workingdata)
#Merge the resData table from model with workingdata table
temp = merge(resData3, workingdata, all = TRUE)
#From temporary table that merged resData table and workingdata table, extract residuals and PIN
names(temp)
temp = temp[, c(2, 4, 25:27)]
write.dbf(temp, "../Data/R2GIS/20082010logSales_Modelbuilder.dbf")
