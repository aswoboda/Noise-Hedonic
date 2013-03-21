library (foreign)
sales0510 <- read.dbf("../Data/GIS2R/Sales20052010.dbf")
summary(sales0510) #found that some observations have a 0 or negative sales value
t <- which(sales0510$SALE_VALUE < 1)
sales0510 = sales0510 [-t, ] #Excludes observations without sales value information

#Pulling the variables we WANT into a new, working dataframe.
sales0510$GARSQFT <- as.numeric(as.character(sales0510$GARAGESQFT)) #Converting factor to numeric
sales0510$SALE_SEASON <- factor(sales0510$SALE_QRT)
sales0510$SALE_MO <- factor(sales0510$SALE_MONTH)
sales0510$OWNOCC <- as.numeric(sales0510$HOMESTEAD)-1
#Creating a time period variable so we can pull data in LWR from a time lag of a 12 month window for each parcel
sales0510$YEARtemp <- sales0510$SALE_YR - 2005
sales0510$TimePeriod <- sales0510$YEARtemp*12+sales0510$SALE_MONTH

#Cleaning HOME_STYLE independent variable
table(sales0510$HOME_STYLE)
#Split
x <- which (sales0510$HOME_STYLE == "Split/entry")
x1 <- which (sales0510$HOME_STYLE == "SPLIT/ENTRY")
x2 <- which (sales0510$HOME_STYLE == "Split Foyer Frame")
x3 <- which (sales0510$HOME_STYLE == "Split/level")
x4 <- which (sales0510$HOME_STYLE == "SPLIT LEVEL")
x5 <- which (sales0510$HOME_STYLE == "Split Level Brick")
x6 <- which (sales0510$HOME_STYLE == "Split Level Frame")
x7 <- which (sales0510$HOME_STYLE == "SPLIT LEVL")
split.home <- sales0510[x1, ]
split <- c(x,x1,x2,x3,x4,x5,x6,x7)
sales0510$HOME_STYLE[split] <- split.home$HOME_STYLE[1]
#One Story
x <- which (sales0510$HOME_STYLE == "1 Story Townhouse")
x1 <- which (sales0510$HOME_STYLE == "ONE STORY")
x2 <- which (sales0510$HOME_STYLE == "1 Story Frame")
x3 <- which (sales0510$HOME_STYLE == "One Story")
onestory.home <- sales0510[x1, ]
onestory <- c(x,x1,x2,x3)
sales0510$HOME_STYLE[onestory] <- onestory.home$HOME_STYLE[1]
#One 1/2 Story
x <- which (sales0510$HOME_STYLE == "1 1/2 Story Fram")
x1 <- which (sales0510$HOME_STYLE == "1-1/2 STRY")
x2 <- which (sales0510$HOME_STYLE == "1 1/2 Story Frame")
onehalfstory.home <- sales0510[x1, ]
onehalfstory <- c(x,x1,x2)
sales0510$HOME_STYLE[onehalfstory] <- onehalfstory.home$HOME_STYLE[1]
#One 3/4 Story
x <- which (sales0510$HOME_STYLE == "One And 3/4 Story")
x1 <- which (sales0510$HOME_STYLE == "1-3/4 STRY")
one3quartstory.home <- sales0510[x1, ]
one3quartstory <- c(x,x1)
sales0510$HOME_STYLE[one3quartstory] <- one3quartstory.home$HOME_STYLE[1]
#Two Story
x <- which (sales0510$HOME_STYLE == "2 Story Brick")
x1 <- which (sales0510$HOME_STYLE == "TWO STORY")
x2 <- which (sales0510$HOME_STYLE == "2 Story Townhouse")
x3 <- which (sales0510$HOME_STYLE == "Old 2 Story")
x4 <- which (sales0510$HOME_STYLE == "2 Story Frame")
x5 <- which (sales0510$HOME_STYLE == "Two Story")
twostory.home <- sales0510[x1, ]
twostory <- c(x,x1,x2,x3,x4,x5)
sales0510$HOME_STYLE[twostory] <- twostory.home$HOME_STYLE[1]
#Bungalow
x <- which (sales0510$HOME_STYLE == "Bungalow")
x1 <- which (sales0510$HOME_STYLE == "BUNGALOW")
bung.home <- sales0510[x1, ]
bung <- c(x,x1)
sales0510$HOME_STYLE[bung] <- bung.home$HOME_STYLE[1]

dataNames <- names (sales0510)
VarsIWant <- which(dataNames %in% c("ELEM", "HIGH", "GARSQFT","TimePeriod","HOMESTEAD", "SALE_SEASON", "SALE_MO","COUNTY_ID", "CITY", "ZIP", "ACRES_POLY", "OWNOCC", "TOTAL_TAX", "HOME_STYLE", "FIN_SQ_FT", "GARAGE", "YEAR_BUILT", "SALE_VALUE", "SALE_YR", "BEDS", "BATH", "MAX", "TRACTCE10", "BLDG_QUAL", "PARK_dist", "LAKE_dist", "SDNUM", "MCA3", "MCA5", "UNIQID", "Long_X", "Lat_Y", "SHOP_dist", "CBD_dist", "PIN", "SP_dist", "MPS_dist", "COLLEGE_di", "MED_INCOME","X_Meter", "Y_Meter"))
workingdata = sales0510[ , VarsIWant]
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
Sales20052010Clean = data.frame(workingdata)
write.dbf(Sales20052010Clean, "../Data/R2GIS/CleanData/Sales20052010.dbf" )