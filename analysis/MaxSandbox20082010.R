library (foreign)
workingdata <- read.dbf("../Data/R2GIS/CleanData/Sales20082010.dbf")
summary(workingdata)

###Preparation for regression analysis, load packages for tests
require(zoo)
require(lmtest)
require(car)



####Linear model investigation
##Dependent variable: Sales Value (untransformed)
model.SaleValue1 <- lm (SALE_VALUE ~ COUNTY_ID + CITY + SALE_YR + SDNUM + ACRES_POLY + HOMESTEAD + FIN_SQ_FT + YEAR_BUILT + MAX + PARK_dist + LAKE_dist + MCA3 + MCA5 + SHOP_dist + CBD_dist +SALE_SEASO, data=workingdata)
summary(model.SaleValue1)
anova(model.SaleValue1) #MCA5 and SALE_MO 
#Diagnostics
plot(model.SaleValue1) #Normality issues once again #5222 large leverage
vif(model.SaleValue1) #multicollinearity #SDNUM VIF above 10 #aliased coefficients means that there is perfect multicollinearity with one of the variables. In the summary table the variable will be NA
dwtest (model.SaleValue1) #autocorrelation # Signs of autocorrelation since DW value is less than 1.5 (1.4538)
bptest(model.SaleValue1)
outlierTest(model.SaleValue1)

workingdata = workingdata [-5222, ]

##Dependent variable: Sales Value (untransformed) w/o MCA5, SDNUM
model.SaleValue2 <- lm (SALE_VALUE ~ COUNTY_ID + CITY + SALE_YR  + ACRES_POLY + HOMESTEAD + FIN_SQ_FT + YEAR_BUILT + MAX + PARK_dist + LAKE_dist + MCA3 + SHOP_dist + CBD_dist  +SALE_SEASO, data=workingdata)
summary(model.SaleValue2)
anova(model.SaleValue1, model.SaleValue2)
#Diagnostics
plot(model.SaleValue2) #Normality issues once again 
vif(model.SaleValue2) #multicollinearity #No real signs of severe multicollinearity
dwtest (model.SaleValue2) #autocorrelation 
bptest(model.SaleValue2)
outlierTest(model.SaleValue2)




######Normality Issues arise so we investigate with the log transformation on the dependent variable
##Dependent variable: Sales Value (log transformed) w/o MCA5, SDNUM | inclusion of Acres2, logMax, logFIN_SQ_FT, logPARK, logCBD, logSHOP, logLAKE
model.logSaleValue1 <- lm (logSALE_VA ~ COUNTY_ID + CITY + SALE_YR  + ACRES_POLY + ACRES2 + HOMESTEAD + logFIN_SQ_ + YEAR_BUILT + logMAX + logPARK + logLAKE + MCA3 + logSHOP + logCBD + SALE_SEASO, data=workingdata)
summary(model.logSaleValue1)
anova(model.logSaleValue1)
#Diagnostics
outlierTest(model.logSaleValue1)
ncvTest(model.logSaleValue1) #
dwtest (model.logSaleValue1) #autocorrelation
bptest(model.logSaleValue1)
plot(model.logSaleValue1) 

###Dependent variable: Sales Value (log transformed) w/o MCA5, SDNUM, SALE_MO
model.logSaleValue2 <- lm (logSALE_VA ~ COUNTY_ID + CITY + SALE_YR  + ACRES_POLY + ACRES2 + HOMESTEAD + logFIN_SQ_ + YEAR_BUILT + logMAX + logPARK + logLAKE+ MCA3 + logSHOP + logCBD + SALE_SEASO, data=workingdata)
summary(model.logSaleValue2)
anova( model.logSaleValue2)
plot(model.logSaleValue2)

##ResidualTable
#Grab residual from model and row name from the model
resData1 = data.frame(Res = model.logSaleValue2$residuals, RowName = names(model.logSaleValue2$residuals))
#Add column of row names to the workingdata
workingdata$RowName = row.names(workingdata)
#Merge the resData table from model with workingdata table
temp = merge(resData1, workingdata, all = TRUE)
#From temporary table that merged resData table and workingdata table, extract residuals and PIN
names(temp)
temp = temp[, c(2, 4)]
write.dbf(temp, "../Data/R2GIS/20082010logSales.dbf")


###Dependent variable: Sales Value (log transformed) w/ GARAGE
model.logSaleValue3 <- lm (logSALE_VA ~ COUNTY_ID + CITY + SALE_YR  + GARAGE + ACRES_POLY + ACRES2 + HOMESTEAD + logFIN_SQ_ + YEAR_BUILT + logMAX + logPARK  + MCA3 + logSHOP + logCBD + SALE_SEASO, data=workingdata)
summary(model.logSaleValue3)
plot(model.logSaleValue3)
##ResidualTable
#Grab residual from model and row name from the model
resData2 = data.frame(Res = model.logSaleValue3$residuals, RowName = names(model.logSaleValue3$residuals))
#Merge the resData table from model with workingdata table
temp1 = merge(resData2, workingdata, all = TRUE)
#From temporary table that merged resData table and workingdata table, extract residuals and PIN
names(temp1)
temp1 = temp1[, c(2, 4)]
write.dbf(temp1, "../Data/R2GIS/20082010logSales_GARAGE.dbf")
