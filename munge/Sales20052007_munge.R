library (foreign)
sales0507 <- read.dbf("../Data/GIS2R/Sales20052007.dbf")
summary(sales0507)


#Pulling the variables we WANT into a new, working dataframe.
sales0507$GARSQFT <- as.numeric(sales0507$GARAGESQFT) #Converting factor to numeric
dataNames <- names (sales0507)
VarsIWant <- which(dataNames %in% c("COUNTY_ID", "CITY", "ZIP", "ACRES_POLY", "HOMESTEAD", "TOTAL_TAX", "HOME_STYLE", "FIN_SQ_FT", "GARAGE", "YEAR_BUILT", "SALE_VALUE", "SALE_YR", "BEDS", "BATH", "MAX", "TRACTCE10", "BLDG_QUAL", "PARK_dist", "LAKE_dist", "GARSQFT", "SALE_QRT", "SDNUM", "MCA3", "MCA5", "UNIQID", "Long_X", "Lat_Y", "SHOP_dist", "CBD_dist", "PIN"))
workingdata = sales0507[ , VarsIWant]
summary (workingdata)
workingdata$logSALE_VALUE = log(workingdata$SALE_VALUE) #Transforming sales values into logs

##Outlier Investigation -- Looking to not violate normality assumption by looking at histograms
hist(workingdata$ACRES_POLY) #Large outliers -- very right skewed
hist(workingdata$FIN_SQ_FT) #Also right skewed
hist(workingdata$MAX) #Left skewed
hist(workingdata$SALE_VALUE) #Right skewed
#Limit Dataset
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

##Write new .dbf table and store it in 'CleanData' folder
Sales20052007Clean = data.frame(workingdata)
write.dbf(Sales20052007Clean, "../Data/CleanData/Sales20052007.dbf" )