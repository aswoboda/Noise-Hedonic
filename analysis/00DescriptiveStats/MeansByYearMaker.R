
myvars = c("SALE_VALUE", "YEAR_BUILT", "FIN_SQ_FT", "ACRES_POLY", "OWNOCC", "Air_Mean", 
           "CBD_dist", "PARK_dist", "LAKE_dist", "SHOP_dist", 
           "PercWhite", "PercU18", "MED_INCOME", "MCA3")
divisor = c(1000, 1, 1, 1, 1, 1, 1000, 1000, 1000, 1000, 1, 1, 1000, 1)


yearstats = matrix(NA, length(myvars)+1, 7)

for (i in 1:length(myvars)) {
  yearstats[i, 1:6] = tapply(DATAFRAME[, myvars[i]], DATAFRAME$SALE_YR, mean)/divisor[i]
  yearstats[i, 7] = mean(DATAFRAME[, myvars[i]]/divisor[i])
}

i = i + 1
yearstats[i, 1:6] = tapply(DATAFRAME[, myvars[i-1]], DATAFRAME$SALE_YR, length)
yearstats[i, 7] = length(DATAFRAME[, myvars[i-1]])

yearstats
mynames = c("Sale Price (thousands)", "Year House was Built", "House Size (square feet)", 
            "Lot Size (acres)", "Owner Occupancy (Yes = 1, No = 0)", "Traffic Noise (dB)", 
            "Distance to Central Business District (km)",
            "Distance to nearest Park (km)", "Distance to nearest Lake (km)", "Distance to nearest Shopping Center (km)",
            "Percent of Census Block Population Race = White", "Percent of Census Block Population Under Age 18",
            "Median Income in Census Tract (thousands)", "Elementary Test Scores",
            "Number of Sales")
rownames(yearstats) = mynames

mydig = matrix(rep(c(0, 0, 0, 2, 2, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0), 8), length(myvars)+1, 8)

require(xtable)
xtable(yearstats, digits = mydig)
