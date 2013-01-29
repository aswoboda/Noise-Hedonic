library (foreign)
workingdata <- read.dbf("../Data/CleanData/Sales20052007.dbf")
names(workingdata)
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
model.SaleValue1 <- lm (SALE_VALUE ~ COUNTY_ID + CITY + SALE_YR + SDNUM + GARAGE + ACRES_POLY+HOMESTEAD+FIN_SQ_FT+YEAR_BUILT+MAX+PARK_dist+LAKE_dist+MCA3+MCA5+SHOP_dist+CBD_dist, data=workingdata)
summary(model.SaleValue1)
anova(model.SaleValue1)
#Diagnostics
plot(model.SaleValue1) #Normality issues once again #926, 19852 large leverage
vif(model.SaleValue1) #multicollinearity #No real signs of severe multicollinearity
dwtest (model.SaleValue1) #autocorrelation # Signs of autocorrelation since DW value is less than 1.5 (1.2974)
bptest(model.SaleValue1)
outlierTest(model.SaleValue1)


workingdata = workingdata[-c(926, 19852), ] #Removed influential observations

##Dependent variable: Sales Value (untransformed) w/o MCA5, SDNUM, PARK_dist
model.SaleValue2 <- lm (SALE_VALUE ~ COUNTY_ID + CITY + SALE_YR + GARAGE + ACRES_POLY + HOMESTEAD+FIN_SQ_FT+YEAR_BUILT+MAX+LAKE_dist+MCA3+SHOP_dist+CBD_dist, data=workingdata)
summary(model.SaleValue2)
#Diagnostics
plot(model.SaleValue2) #Normality issues once again 
vif(model.SaleValue2) #multicollinearity #No real signs of severe multicollinearity
dwtest (model.SaleValue2) #autocorrelation # Signs of autocorrelation since DW value is less than 1.5 (1.2941)
bptest(model.SaleValue2)
outlierTest(model.SaleValue2)


#Dependent variable: Sales Value (untransformed) w/o MCA5, PARK_dist, SHOP_dist, SDNUM | inclusion of Acres2 
model.SaleValue3 <- lm (SALE_VALUE ~ COUNTY_ID + CITY + SALE_YR + GARAGE + GARSQFT + ACRES_POLY + ACRES2 + HOMESTEAD+FIN_SQ_FT+YEAR_BUILT+MAX+LAKE_dist+MCA3+SHOP_dist+CBD_dist, data=workingdata)
summary(model.SaleValue3)
anova(model.SaleValue3)
#Diagnostics
plot(model.SaleValue3) #Normality issues once again 
vif(model.SaleValue3) #multicollinearity #No real signs of severe multicollinearity
dwtest (model.SaleValue3) #autocorrelation # Signs of autocorrelation since DW value is less than 1.5 (1.2874)
bptest(model.SaleValue3)
outlierTest(model.SaleValue3)



######Normality Issues arise so we investigate with the log transformation on the dependent variable
##Dependent variable: Sales Value (log transformed) w/o MCA5, PARK_dist, SHOP_dist SDNUM | inclusion of Acres2, logMax, logFIN_SQ_FT
model.logSaleValue1 <- lm (logSALE_VA ~ COUNTY_ID + CITY + SALE_YR + GARAGE + GARSQFT + ACRES_POLY + ACRES2 + HOMESTEAD+ logFIN_SQ_+YEAR_BUILT+logMAX+LAKE_dist+MCA3+SHOP_dist+CBD_dist, data=workingdata)
summary(model.logSaleValue1)
anova(model.logSaleValue1)
#Diagnostics
outlierTest(model.logSaleValue1)
ncvTest(model.logSaleValue1) #nonconstant variance does not improve
dwtest (model.logSaleValue1) #autocorrelation
bptest(model.logSaleValue1)
plot(model.logSaleValue1)

workingdata [-c(16294,16293, 24494), ]

###Dependent variable: Sales Value (log transformed) w/o MCA5, PARK_dist, SHOP_dist SDNUM | inclusion of Acres2, logMax, logFIN_SQ_FT
model.logSaleValue2 <- lm (logSALE_VA ~ COUNTY_ID + CITY + SALE_YR + GARAGE + GARSQFT + ACRES_POLY + ACRES2 + HOMESTEAD+ logFIN_SQ_+YEAR_BUILT+logMAX+LAKE_dist+MCA3+SHOP_dist+CBD_dist, data=workingdata)
summary(model.logSaleValue2)
plot(model.logSaleValue2)
