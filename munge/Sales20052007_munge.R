library (foreign)
sales0507 <- read.dbf("../Data/GIS2R/Sales20052007.dbf")
str(sales0507)
summary(sales0507)

#Pulling the variables we WANT into a new, working dataframe.
dataNames <- names (sales0507)
VarsIWant <- which(dataNames %in% c("COUNTY_ID", "CITY", "ZIP", "ACRES_POLY", "HOMESTEAD", "TOTAL_TAX", "HOME_STYLE", "FIN_SQ_FT", "GARAGE", "YEAR_BUILT", "SALE_VALUE", "SALE_YR", "BEDS", "BATH", "MAX", "TRACTCE10", "BLDG_QUAL", "PARK_dist", "LAKE_dist", "GARAGESQFT", "SALE_QRT", "SDNUM", "MCA3", "MCA5", "UNIQID", "Long_X", "Lat_Y", "SHOP_dist", "CBD_dist", "PIN"))
