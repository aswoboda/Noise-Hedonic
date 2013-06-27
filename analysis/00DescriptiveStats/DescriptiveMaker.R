

# goal of this file is to create some descriptive stats for the paper

require(foreign)
DATAFRAME = read.dbf("~/NoiseHedonicProject/Data/R2GIS/CleanData/Sales20052010.dbf")

myvars = c("SALE_VALUE", "FIN_SQ_FT", "ACRES_POLY", "OWNOCC", "YEAR_BUILT", "MAX", "MED_INCOME", "MCA3",
           "CBD_dist", "PARK_dist", "LAKE_dist", "SHOP_dist")

sumStats = matrix(NA, length(myvars), 7)

for (i in 1:length(myvars)) {
  sumStats[i, 1:6] = summary(DATAFRAME[ , myvars[i]])  
  sumStats[i, 7] = sd(DATAFRAME[ , myvars[i]])
}


                
















