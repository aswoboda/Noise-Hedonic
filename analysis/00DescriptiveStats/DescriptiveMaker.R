

# goal of this file is to create some descriptive stats for the paper

require(foreign)
DATAFRAME = read.dbf("~/NoiseHedonicProject/Data/R2GIS/CleanData/Sales20052010.dbf")

myvars = c("SALE_VALUE", "FIN_SQ_FT", "ACRES_POLY", "OWNOCC", "YEAR_BUILT", "MAX", 
           "MED_INCOME", "MCA3", "CBD_dist", "PARK_dist", "LAKE_dist", "SHOP_dist")

mynames = c("Sale Price (thousands)", "House Size (thousands square feet)", "Lot Size (acres)", "Owner Occupancy Dummy",
            "Year House was Built", "Traffic Noise (dB)", "Median Income in Census Tract (thousands)", 
            "Elementary Test Scores", "Distance to Central Business District (km)",
            "Distance to nearest Park (km)", "Distance to nearest Lake (km)", "Distance to nearest Shopping Center (km)")

sumStats = matrix(NA, length(myvars), 7)

for (i in 1:length(myvars)) {
  sumStats[i, 1:6] = summary(DATAFRAME[ , myvars[i]])  
  sumStats[i, 7] = sd(DATAFRAME[ , myvars[i]])
}

sumStats

sumStats = sumStats/c(1000, 1, 1, 1, 1, 1, 
                      1000, 1, 1000, 1000, 1000, 1000)

rownames(sumStats) = mynames


mydig = matrix(rep(c(1, 0, 2, 1, 0, 1, 1, 1, 1, 1, 1, 1), 8), length(myvars), 8)

require(xtable)
xtable(sumStats, digits = mydig)
















