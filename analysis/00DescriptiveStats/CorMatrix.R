
library (foreign)
workingdata <- read.dbf("../Data/R2GIS/CleanData/Sales20052010.dbf")


quantVars = c("SALE_VALUE", "FIN_SQ_FT", "ACRES_POLY", "YEAR_BUILT", 
              "Air_Mean", "CBD_dist", "PARK_dist", "LAKE_dist",  "SHOP_dist",
              "PercWhite", "PercU18",
              "MED_INCOME", "MCA3")

corMat = cor(workingdata[, quantVars])

corMat = round(corMat, 2)

corMat[upper.tri(corMat)] <- ""
corMat

rownames(corMat) = c("Sale Price", "House Size", "Lot Size", "Year Built", "Noise", 
                     "Distance to CBD", "Distance to Park", "Distance to Lake", "Distance to Shop",
                     "Percent Pop White", "Percent Under 18",   "Income", "Elementary Test Scores",)
colnames(corMat) = c("Price", "House", "Lot", "Built", "Noise", "CBD", "Park", "Lake", "Shop", "White", "U18", "Income", "Test")

require(xtable)
print(xtable(corMat, align = "lrrrrrrrrrrrrr"))