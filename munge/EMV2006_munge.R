library (foreign)
emv2006 <- read.dbf("../Data/GIS2R/EMV2006.dbf")
names(emv2006) #found that some observations have a 0 or negative sales value


#Pulling the variables we WANT into a new, working dataframe.
emv2006$GARSQFT <- as.numeric(as.character(emv2006$GARAGESQFT)) #Converting factor to numeric
dataNames <- names (emv2006)
VarsIWant <- which(dataNames %in% c("ELEM", "MED_INCOME", "GARSQFT","COUNTY_ID", "CITY", "ZIP", "ACRES_POLY","SDNUM","HOMESTEAD", "FIN_SQ_FT", "GARAGE", "YEAR_BUILT", "EMV_TOTAL", "BEDS", "BATH", "MAX", "BLDG_QUAL", "PARK_dist", "LAKE_dist", "MCA3", "UNIQID", "Long_X", "Lat_Y", "SHOP_dist", "PIN", "SP_dist", "MPS_dist","X_Meter", "Y_Meter"))
workingdata = emv2006[ , VarsIWant]
summary (workingdata)
workingdata$logEMV = log(workingdata$EMV_TOTAL) #Transforming sales values into logs

##Outlier Investigation -- Looking to not violate normality assumption by looking at histograms
#hist(workingdata$ACRES_POLY) #Large outliers -- very right skewed
#hist(workingdata$FIN_SQ_FT) #Also right skewed
#hist(workingdata$MAX) #Left skewed
#hist(workingdata$logEMV) #Right skewed
LivingAreaOutliers = which (workingdata$FIN_SQ_FT > 4000) #Limit dataset to houses with less than 5000 sq ft.
AcreageOutliers = which (workingdata$ACRES_POLY > .6) #Limit dataset to parcels less than one acre
TrafficNoiseOutliers = which(workingdata$MAX <25) #Limit dataset to properties exposed to more than 25 dBA
OutlierParcels = c(LivingAreaOutliers,AcreageOutliers,TrafficNoiseOutliers)
workingdata <- workingdata [-OutlierParcels, ] #Excludes observations that are outliers (n=15670)

#Recheck histograms with outliers removed
#hist(workingdata$ACRES_POLY) #Normal
#hist(workingdata$FIN_SQ_FT) #Slightly right-skewed
#hist(workingdata$MAX) #Normal
#hist(workingdata$logEMV) #Normal

##Plot Investigation -- Looking for relationships with the dependent variable that may violate the linearity assumption
#Cannot use pairs() function because of memory overload
#First batch uses sales value field untransformed
#plot (workingdata$ACRES_POLY, workingdata$SALE_VALUE) #Possibility for x^2 relationship or log transformation
#plot (workingdata$FIN_SQ_FT, workingdata$SALE_VALUE) #Linear relationship
#plot (workingdata$MAX, workingdata$SALE_VALUE) #Not a clear linear relationship, possiblity for x^2 relationship

#Second batch uses log transformation of sales value
#plot (workingdata$ACRES_POLY, workingdata$logSALE_VALUE) #Possibility of x^2 transformation (values too small for log transformation)
#plot (workingdata$FIN_SQ_FT, workingdata$logSALE_VALUE) #Possibility of log transformation
#plot (workingdata$MAX, workingdata$logSALE_VALUE) #Not a clear linear relationship, possiblity for log transformation

#png(filename="graphs/PairsPlot.png", width= 12, height=12, units="in", res = 72)
#pairs (~logSALE_VALUE + CBD_dist + SHOP_dist + LAKE_dist + PARK_dist, data = workingdata, cex = .25) #Possibility for log or x^2 relationships for CBD, SHOP, PARK
#dev.off()

#After the plot investigations, make some transformations to explanatory variables to see if it improves upon the linearity assumption
#workingdata$logFIN_SQ_FT = log(workingdata$FIN_SQ_FT) #Transforming living area into logs
#workingdata$logMAX = log(workingdata$MAX) #Transforming traffic noise into logs
#workingdata$ACRES2 = (workingdata$ACRES_POLY*workingdata$ACRES_POLY) #Transforming acres to a quadratic relationship
#workingdata$logCBD = log(workingdata$CBD_dist) #Transforming CBD distance into logs
#workingdata$logSHOP = log(workingdata$SHOP_dist) #Transforming shopping distance into logs
#workingdata$logPARK = log(workingdata$PARK_dist) #Transforming park distance into logs
#workingdata$logLAKE = log(workingdata$LAKE_dist) #Transforming lake distance into logs
#Plot investigation with log transformations
#plot (workingdata$logFIN_SQ_FT, workingdata$logSALE_VALUE) #Improved
#plot (workingdata$logMAX, workingdata$logSALE_VALUE)  #Didn't improve
#plot (workingdata$logCBD, workingdata$logSALE_VALUE) #improved
#plot (workingdata$logSHOP, workingdata$logSALE_VALUE) #improved
#plot (workingdata$logPARK, workingdata$logSALE_VALUE)
#plot (workingdata$logLAKE, workingdata$logSALE_VALUE)

##Multicollinearity investigation through correlation matrix 
#cor(workingdata[, c("logSALE_VALUE", "logFIN_SQ_FT", "logMAX", "ACRES_POLY", "logCBD", "logSHOP", "logPARK", "logLAKE", "MCA3", "MCA5", "GARSQFT", "SALE_YR", "SDNUM")])

##Write new .dbf table and store it in 'CleanData' folder
EMV2006Clean = data.frame(workingdata)
write.dbf(EMV2006Clean, "../Data/R2GIS/CleanData/EMV2006.dbf" )