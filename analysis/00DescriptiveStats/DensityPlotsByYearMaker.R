

# goal of this file is to create a figure showing the density plot of each variable by year
# to chekc for substantial changes over time

require(foreign)
DATAFRAME = read.dbf("~/NoiseHedonicProject/Data/R2GIS/CleanData/Sales20052010.dbf")

myvars = c("SALE_VALUE", "FIN_SQ_FT", "ACRES_POLY", "OWNOCC", "YEAR_BUILT", "MAX", 
           "MED_INCOME", "MCA3", "CBD_dist", "PARK_dist", "LAKE_dist", "SHOP_dist")

mynames = c("Sale Price (thousands)", "House Size (thousands square feet)", "Lot Size (acres)", "Owner Occupancy Dummy",
            "Year House was Built", "Traffic Noise (dB)", "Median Income in Census Tract (thousands)", 
            "Elementary Test Scores", "Distance to Central Business District (km)",
            "Distance to nearest Park (km)", "Distance to nearest Lake (km)", "Distance to nearest Shopping Center (km)")


require("sm")
require("RColorBrewer")

my.col = brewer.pal(6, "Set2")
year.f = factor(DATAFRAME$SALE_YR)
table(year.f)

pdf("graphs/DensityPlotsByYear.pdf", width = 8, height = 8)
for (i in myvars) {
  sm.density.compare(DATAFRAME[, as.character(i)], year.f, 
                     col = my.col, lwd = 3, lty = rep(1, 6),
                     xlab = as.character(i))
  legend("topright", levels(year.f), fill=my.col)
}
dev.off()
