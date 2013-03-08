library (foreign)
salesdak <- read.dbf("../Data/GIS2R/Sales20052010.dbf")
summary(salesdak) #found that some observations have a 0 or negative sales value
t <- which(salesdak$SALE_VALUE < 1)
salesdak = salesdak [-t, ] #Excludes observations without sales value information
beds <- which(salesdak$BEDS > 0)
salesdak = salesdak [beds, ]

#Pulling the variables we WANT into a new, working dataframe.
salesdak$GARSQFT <- as.numeric(as.character(salesdak$GARAGESQFT)) #Converting factor to numeric
salesdak$SALE_SEASON <- factor(salesdak$SALE_QRT)
salesdak$SALE_MO <- factor(salesdak$SALE_MONTH)
#Creating a time period variable so we can pull data in LWR from a time lag of a 12 month window for each parcel
salesdak$YEARtemp <- salesdak$SALE_YR - 2005
salesdak$TimePeriod <- salesdak$YEARtemp*12+salesdak$SALE_MONTH
dataNames <- names (salesdak)
VarsIWant <- which(dataNames %in% c("ELEM","HIGH", "GARSQFT","TimePeriod", "SALE_SEASON", "SALE_MO","COUNTY_ID", "CITY", "ZIP", "ACRES_POLY", "HOMESTEAD", "TOTAL_TAX", "HOME_STYLE", "FIN_SQ_FT", "GARAGE", "YEAR_BUILT", "SALE_VALUE", "SALE_YR", "BEDS", "BATH", "MAX", "TRACTCE10", "BLDG_QUAL", "PARK_dist", "LAKE_dist", "SDNUM", "MCA3", "MCA5", "UNIQID", "Long_X", "Lat_Y", "SHOP_dist", "CBD_dist", "PIN", "SP_dist", "MPS_dist", "COLLEGE_di", "MED_INCOME","X_Meter", "Y_Meter"))
workingdata = salesdak[ , VarsIWant]
summary (workingdata)
workingdata$logSALE_VALUE = log(workingdata$SALE_VALUE) #Transforming sales values into logs

##Outlier Investigation -- Looking to not violate normality assumption by looking at histograms
#hist(workingdata$ACRES_POLY) #Large outliers -- very right skewed
#hist(workingdata$FIN_SQ_FT) #Also right skewed
#hist(workingdata$MAX) #Left skewed
#hist(workingdata$SALE_VALUE) #Right skewed
LivingAreaOutliers = which (workingdata$FIN_SQ_FT > 4000) #Limit dataset to houses with less than 5000 sq ft.
AcreageOutliers = which (workingdata$ACRES_POLY > .6) #Limit dataset to parcels less than one acre
TrafficNoiseOutliers = which(workingdata$MAX <25) #Limit dataset to properties exposed to more than 25 dBA
SalesValueOutliers = which (workingdata$SALE_VALUE > 675000)#Limit dataset to properties that sold for less than $675,000
svoutlier = which (workingdata$logSALE_VALUE < 11.5)
OutlierParcels = c(LivingAreaOutliers,AcreageOutliers,TrafficNoiseOutliers,SalesValueOutliers, svoutlier)
workingdata <- workingdata [-OutlierParcels, ] #Excludes observations that are outliers (n=15670)

#Recheck histograms with outliers removed
# hist(workingdata$ACRES_POLY) #Normal
# hist(workingdata$FIN_SQ_FT) #Slightly right-skewed
# hist(workingdata$MAX) #Normal
# hist(workingdata$logSALE_VALUE) #Normal

##Plot Investigation -- Looking for relationships with the dependent variable that may violate the linearity assumption
#Cannot use pairs() function because of memory overload
#First batch uses sales value field untransformed
# plot (workingdata$ACRES_POLY, workingdata$SALE_VALUE) #Possibility for x^2 relationship or log transformation
# plot (workingdata$FIN_SQ_FT, workingdata$SALE_VALUE) #Linear relationship
# plot (workingdata$MAX, workingdata$SALE_VALUE) #Not a clear linear relationship, possiblity for x^2 relationship
# 
# #Second batch uses log transformation of sales value
# plot (workingdata$ACRES_POLY, workingdata$logSALE_VALUE) #Possibility of x^2 transformation (values too small for log transformation)
# plot (workingdata$FIN_SQ_FT, workingdata$logSALE_VALUE) #Possibility of log transformation
# plot (workingdata$MAX, workingdata$logSALE_VALUE) #Not a clear linear relationship, possiblity for log transformation
# 
# png(filename="graphs/PairsPlot.png", width= 12, height=12, units="in", res = 72)
# pairs (~logSALE_VALUE + CBD_dist + SHOP_dist + LAKE_dist + PARK_dist, data = workingdata, cex = .25) #Possibility for log or x^2 relationships for CBD, SHOP, PARK
# dev.off()
# 
# #After the plot investigations, make some transformations to explanatory variables to see if it improves upon the linearity assumption
# workingdata$logFIN_SQ_FT = log(workingdata$FIN_SQ_FT) #Transforming living area into logs
# workingdata$logMAX = log(workingdata$MAX) #Transforming traffic noise into logs
# workingdata$ACRES2 = (workingdata$ACRES_POLY*workingdata$ACRES_POLY) #Transforming acres to a quadratic relationship
# workingdata$logCBD = log(workingdata$CBD_dist) #Transforming CBD distance into logs
# workingdata$logSHOP = log(workingdata$SHOP_dist) #Transforming shopping distance into logs
# workingdata$logPARK = log(workingdata$PARK_dist) #Transforming park distance into logs
# workingdata$logLAKE = log(workingdata$LAKE_dist) #Transforming lake distance into logs
# #Plot investigation with log transformations
# plot (workingdata$logFIN_SQ_FT, workingdata$logSALE_VALUE) #Improved
# plot (workingdata$logMAX, workingdata$logSALE_VALUE)  #Didn't improve
# plot (workingdata$logCBD, workingdata$logSALE_VALUE) #improved
# plot (workingdata$logSHOP, workingdata$logSALE_VALUE) #improved
# plot (workingdata$logPARK, workingdata$logSALE_VALUE)
# plot (workingdata$logLAKE, workingdata$logSALE_VALUE)
# 
# 
# ##Multicollinearity investigation through correlation matrix 
# cor(workingdata[, c("logSALE_VALUE", "logFIN_SQ_FT", "logMAX", "ACRES_POLY", "logCBD", "logSHOP", "logPARK", "logLAKE", "MCA3", "MCA5", "GARSQFT", "SALE_YR", "SDNUM")])

##Write new .dbf table and store it in 'CleanData' folder
SalesDakotaClean = data.frame(workingdata)
write.dbf(SalesDakotaClean, "../Data/R2GIS/CleanData/Sales20052010_Dakota.dbf" )