library (foreign)
workingdata <- read.dbf("../Data/CleanData/Sales20052007.dbf")
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
plot(model.SaleValue1) #Normality issues once again #16300 17135 large leverage
anova(model.SaleValue1)

workingdata = workingdata[-c(16300, 17135), ] #Removed influential observations

##Dependent variable: Sales Value (untransformed) w/o MCA5
model.SaleValue2 <- lm (SALE_VALUE ~ CITY + SALE_YR + SDNUM + ACRES_POLY+HOMESTEAD+FIN_SQ_FT+YEAR_BUILT+MAX+PARK_dist+LAKE_dist+MCA3+SHOP_dist+CBD_dist, data=workingdata)
summary(model.SaleValue2)
plot(model.SaleValue2) #Normality issues once again 
anova(model.SaleValue2, model.SaleValue1)

##Dependent variable: Sales Value (untransformed) w/o MCA5, PARK_dist, SHOP_dist, SDnum
model.SaleValue3 <- lm (SALE_VALUE ~ CITY + SALE_YR  + ACRES_POLY+HOMESTEAD+FIN_SQ_FT+YEAR_BUILT+MAX+LAKE_dist+MCA3+CBD_dist, data=workingdata)
summary(model.SaleValue3)
plot(model.SaleValue3) #Normality issues once again
anova(model.SaleValue2, model.SaleValue3) #Comparing Anova values suggests to keep the smaller model.
vif(model.SaleValue3) #multicollinearity #No real signs of severe multicollinearity
dwtest (model.SaleValue3) #autocorrelation # Signs of autocorrelation since DW value is less than 1.5 (1.4266)
bptest(model.SaleValue3) #shows some signs of heteroscedasticity

#Dependent variable: Sales Value (untransformed) w/o MCA5, PARK_dist, SHOP_dist, SDNUM | inclusion of Acres2 
model.SaleValue4 <- lm (SALE_VALUE ~ CITY + SALE_YR  + ACRES_POLY+ ACRES2 +HOMESTEAD+FIN_SQ_FT+YEAR_BUILT+MAX+LAKE_dist+MCA3+CBD_dist, data=workingdata)
summary(model.SaleValue4)
plot(model.SaleValue4) #Normality issues once again
#Diagnostics
outlierTest(model.SaleValue4)
ncvTest(model.SaleValue4) #nonconstant variance
dwtest (model.SaleValue4) #autocorrelation # Signs of autocorrelation since DW value is less than 1.5 (1.4266)
bptest(model.SaleValue4)




######Normality Issues arise so we investigate with the log transformation on the dependent variable
##Dependent variable: Sales Value (log transformed) w/o MCA5, PARK_dist, SHOP_dist SDNUM | inclusion of Acres2, logMax, logFIN_SQ_FT
model.logSaleValue1 <- lm (logSALE_VALUE ~ CITY + SALE_YR + ACRES_POLY+ACRES2+HOMESTEAD+ logFIN_SQ_FT + YEAR_BUILT+logMAX+LAKE_dist+MCA3+SHOP_dist+CBD_dist, data=workingdata)
summary(model.logSaleValue1) #Explains significantly less variation than untransformed model
plot(model.logSaleValue1)
anova(model.logSaleValue1)
#Diagnostics
outlierTest(model.logSaleValue1)
ncvTest(model.logSaleValue1) #nonconstant variance does not improve
dwtest (model.logSaleValue1) #autocorrelation
bptest(model.logSaleValue1)
