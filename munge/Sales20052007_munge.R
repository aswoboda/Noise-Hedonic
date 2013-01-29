library (foreign)
sales0507 <- read.dbf("../Data/GIS2R/Sales20052007.dbf")
summary(sales0507)


#Pulling the variables we WANT into a new, working dataframe.
dataNames <- names (sales0507)
VarsIWant <- which(dataNames %in% c("COUNTY_ID", "CITY", "ZIP", "ACRES_POLY", "HOMESTEAD", "TOTAL_TAX", "HOME_STYLE", "FIN_SQ_FT", "GARAGE", "YEAR_BUILT", "SALE_VALUE", "SALE_YR", "BEDS", "BATH", "MAX", "TRACTCE10", "BLDG_QUAL", "PARK_dist", "LAKE_dist", "GARAGESQFT", "SALE_QRT", "SDNUM", "MCA3", "MCA5", "UNIQID", "Long_X", "Lat_Y", "SHOP_dist", "CBD_dist", "PIN"))
workingdata = sales0507[ , VarsIWant]
summary (workingdata)
workingdata$logSALE_VALUE = log(workingdata$SALE_VALUE) #Transforming sales values into logs

##Outlier Investigation -- Looking to not violate normality assumption by looking at histograms
hist(workingdata$ACRES_POLY) #Large outliers -- very right skewed
hist(workingdata$FIN_SQ_FT) #Also right skewed
hist(workingdata$MAX) #Left skewed
hist(workingdata$SALE_VALUE) #Right skewed
LivingAreaOutliers = which (workingdata$FIN_SQ_FT > 5000) #Limit dataset to houses with less than 5000 sq ft.
AcreageOutliers = which (workingdata$ACRES_POLY > 1) #Limit dataset to parcels less than one acre
TrafficNoiseOutliers = which(workingdata$MAX <25) #Limit dataset to properties exposed to more than 25 dBA
SalesValueOutliers = which (workingdata$SALE_VALUE > 675000)#Limit dataset to properties that sold for less than $675,000
svoutlier = which (workingdata$logSALE_VALUE < 2)
OutlierParcels = c(LivingAreaOutliers,AcreageOutliers,TrafficNoiseOutliers,SalesValueOutliers, svoutlier)
workingdata <- workingdata [-OutlierParcels, ] #Excludes observations that are outliers (n=27552)

##Plot Investigation -- Looking for relationships with the dependent variable that may violate the linearity assumption
#Cannot use pairs() function because of memory overload
#First batch uses sales value field untransformed
plot (workingdata$ACRES_POLY, workingdata$SALE_VALUE) #Possibility for x^2 relationship or log transformation
plot (workingdata$FIN_SQ_FT, workingdata$SALE_VALUE) #Linear relationship
plot (workingdata$MAX, workingdata$SALE_VALUE) #Not a clear linear relationship, possiblity for x^2 relationship
pairs (~SALE_VALUE + CBD_dist + SHOP_dist + LAKE_dist + PARK_dist, data = workingdata) #Unclear linear relationships
#Second batch uses log transformation of sales value
plot (workingdata$ACRES_POLY, workingdata$logSALE_VALUE) #Possibility of x^2 relationship
plot (workingdata$FIN_SQ_FT, workingdata$logSALE_VALUE) #Possibility of log transformation
plot (workingdata$MAX, workingdata$logSALE_VALUE) #Not a clear linear relationship, possiblity for x^2 relationship

#After the plot investigations, make some transformations to explanatory variables to see if it improves upon the linearity assumption
workingdata$logFIN_SQ_FT = log(workingdata$FIN_SQ_FT) #Transforming living area into logs
workingdata$logMAX = log(workingdata$MAX) #Transforming traffic noise into logs
workingdata$ACRES2 = (workingdata$ACRES_POLY*workingdata$ACRES_POLY)
#Plot investigation with log transformations
plot (workingdata$logFIN_SQ_FT, workingdata$logSALE_VALUE) #Didn't improve
plot (workingdata$logMAX, workingdata$logSALE_VALUE)  #Didn't improve

##Multicollinearity investigation through correlation matrix #Trouble with making collinearity table but diagnostics do not show any multicollinearity
dataNames <- names (workingdata)
testcor <- which(dataNames %in% c("ACRES_POLY", "FIN_SQ_FT", "YEAR_BUILT", "SALE_VALUE", "BEDS", "BATH", "MAX", "BLDG_QUAL", "PARK_dist", "LAKE_dist", "GARAGESQFT", "MCA3", "MCA5",  "SHOP_dist", "CBD_dist"))
testcor1 = workingdata[ , testcor]
cor(testcor1) 

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
