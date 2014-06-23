

# goal of this file is to create some descriptive stats for the paper

require(foreign)
DATAFRAME = read.dbf("~/NoiseHedonicProject/Data/R2GIS/CleanData/Sales20052010.dbf")

myvars = c("SALE_VALUE", "YEAR_BUILT", "FIN_SQ_FT", "ACRES_POLY", "OWNOCC", "Air_Mean", 
           "CBD_dist", "PARK_dist", "LAKE_dist", "SHOP_dist", 
           "PercWhite", "PercU18", "MED_INCOME", "MCA3")

mynames = c("Sale Price (thousands)", "Year House was Built", "House Size (square feet)", 
            "Lot Size (acres)", "Owner Occupancy (Yes = 1, No = 0)", "Traffic Noise (dB)", 
            "Distance to Central Business District (km)",
            "Distance to nearest Park (km)", "Distance to nearest Lake (km)", "Distance to nearest Shopping Center (km)",
            "Percent of Census Block Population Race = White", "Percent of Census Block Population Under Age 18",
            "Median Income in Census Tract (thousands)", "Elementary Test Scores")

sumStats = matrix(NA, length(myvars), 7)
for (i in 1:length(myvars)) {
  sumStats[i, 1:6] = summary(DATAFRAME[ , myvars[i]])  
  sumStats[i, 7] = sd(DATAFRAME[ , myvars[i]])
}
sumStats
sumStats = sumStats/c(1000, 1, 1, 1, 1, 1, 
                      1000, 1000, 1000, 1000, 1, 1, 1000, 1)

rownames(sumStats) = mynames

mydig = matrix(rep(c(0, 0, 2, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0), 8), length(myvars), 8)

require(xtable)
xtable(sumStats, digits = mydig)
