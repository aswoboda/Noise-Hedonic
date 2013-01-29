library (foreign)
workingdata <- read.dbf("../Data/CleanData/Sales20082010.dbf")
summary(workingdata)

###Preparation for regression analysis, load packages for tests
install.packages ("zoo")
install.packages ("lmtest")
install.packages ("car")
require(zoo)
require(lmtest)
require(car)



####Linear model investigation
##Dependent variable: Sales Value (untransformed)
model.SaleValue1 <- lm (SALE_VALUE ~ CITY + SALE_YR + SDNUM + ACRES_POLY+HOMESTEAD+FIN_SQ_FT+YEAR_BUILT+MAX+PARK_dist+LAKE_dist+MCA3+MCA5+SHOP_dist+CBD_dist, data=workingdata)
summary(model.SaleValue1)
anova(model.SaleValue1)
#Diagnostics
plot(model.SaleValue1) #Normality issues once again #Observation 4067 high leverage
vif(model.SaleValue1) #multicollinearity #SDNUM shows multicollinearity (probably with MCA scores)
dwtest (model.SaleValue1) #autocorrelation # Signs of autocorrelation since DW value is less than 1.5 (1.4266)
bptest(model.SaleValue1) 
outlierTest(model.SaleValue1) #15579 13619 11911 5390 11994 5604 13873 13870 14681  14680 
ncvTest(model.SaleValue1) #nonconstant variance

workingdata = workingdata [-c(4067,15579, 13619, 11911, 5390, 11994, 5604, 13873, 13870, 14681,14680), ] #Removed leveraged observation


##Dependent variable: Sales Value (untransformed) w/o MCA5, PARK_dist, SHOP_dist, SDnum
model.SaleValue2 <- lm (SALE_VALUE ~ CITY + SALE_YR  + ACRES_POLY+HOMESTEAD+FIN_SQ_FT+YEAR_BUILT+MAX+LAKE_dist+MCA3+CBD_dist, data=workingdata)
summary(model.SaleValue2)
anova(model.SaleValue2)
#Diagnostics
plot(model.SaleValue2) #Normality issues once again
vif(model.SaleValue2) #multicollinearity #No real signs of severe multicollinearity
dwtest (model.SaleValue2) #autocorrelation # Signs of autocorrelation since DW value is less than 1.5 (1.4266)
bptest(model.SaleValue2) #shows some signs of heteroscedasticity
outlierTest(model.SaleValue2) #14679  16705 10530  14678  15316  8497  13872 13472 10932  15283 


#Dependent variable: Sales Value (untransformed) w/o MCA5, PARK_dist, SHOP_dist, SDNUM | inclusion of Acres2 
model.SaleValue3 <- lm (SALE_VALUE ~ COUNTY_ID + CITY +SALE_YR  + ACRES_POLY+ ACRES2 +HOMESTEAD+FIN_SQ_FT+YEAR_BUILT+MAX+LAKE_dist+MCA3+CBD_dist, data=workingdata)
summary(model.SaleValue3)
#Diagnostics
plot(model.SaleValue3) #Normality issues once again #Non constant variance
outlierTest(model.SaleValue3)
ncvTest(model.SaleValue3) #nonconstant variance
dwtest (model.SaleValue3) #autocorrelation # Signs of autocorrelation since DW value is less than 1.5 (1.4266)
bptest(model.SaleValue3)
crPlots(model.SaleValue3)


######Normality and Heteroskedasticity arise issues so we investigate with the log transformation on the dependent variable
##Dependent variable: Sales Value (log transformed) w/o MCA5, PARK_dist, SHOP_dist SDNUM | inclusion of Acres2, logMax, logFIN_SQ_FT
model.logSaleValue1 <- lm (logSALE_VA ~ COUNTY_ID + CITY + SALE_YR + ACRES_POLY+ ACRES2 +HOMESTEAD+ logFIN_SQ_ + YEAR_BUILT+logMAX+LAKE_dist+MCA3+CBD_dist, data=workingdata)
summary(model.logSaleValue1) #Explains significantly less variation than untransformed model
anova(model.logSaleValue1)
#Diagnostics
outlierTest(model.logSaleValue1)
ncvTest(model.logSaleValue1) #nonconstant variance does not improve
dwtest (model.logSaleValue1) #autocorrelation # Signs of autocorrelation since DW value is less than 1.5 (1.4871)
bptest(model.logSaleValue1)
plot(model.logSaleValue1) #QQ plot improves #Residual plot improves #ScaleLocation worsens
