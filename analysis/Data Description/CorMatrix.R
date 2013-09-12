
library (foreign)
workingdata <- read.dbf("../Data/R2GIS/CleanData/Sales20052010.dbf")


quantVars = c("logSALE_VA", "FIN_SQ_FT", "ACRES_POLY", "YEAR_BUILT", 
              "MAX", "MED_INCOME", "MCA3", "CBD_dist", "PARK_dist", "LAKE_dist",  "SHOP_dist")

corMat = cor(workingdata[, quantVars])

corMat = round(corMat, 2)

corMat[upper.tri(corMat)] <- ""
corMat

rownames(corMat) = c("ln Sale Price", "House Size", "Lot Size", "Year Built", "Noise", "Income", "Elementary Test Scores",
                     "Distance to CBD", "Distance to Park", "Distance to Lake", "Distance to Shop")
colnames(corMat) = c("Price", "House", "Lot", "Built", "Noise", "Income", "Test", "CBD", "Park", "Lake", "Shop")

require(xtable)
print(xtable(corMat, align = "lrrrrrrrrrrr"))

cor(workingdata$ACRES_POLY, workingdata$MAX)