

# goal of this file is to create a figure showing the density plot of each variable by year
# to chekc for substantial changes over time

require(foreign)
DATAFRAME = read.dbf("~/NoiseHedonicProject/Data/R2GIS/CleanData/Sales20052010.dbf")
names(DATAFRAME)
myvars = c("SALE_VALUE", "Air_Mean", "FIN_SQ_FT", "ACRES_POLY", "YEAR_BUILT",  
           "MED_INCOME", "MCA3", "PercWhite", "PercU18", 
           "CBD_dist", "PARK_dist", "LAKE_dist", "SHOP_dist")

mynames = c("Sale Price", "Noise", "House Size", "Lot Size", "Year Built",
            "Median Income in Census Tract", "Elementary Test Scores", 
            "Percent of Census Block Race = White", "Percent of Census Block Under Age 18",
            "Distance to Central Business District", "Distance to nearest Park", 
            "Distance to nearest Lake", "Distance to nearest Shopping Center")


require("sm")
require("RColorBrewer")

my.col = brewer.pal(6, "Set2")
year.f = factor(DATAFRAME$SALE_YR)
table(year.f)

pdf("graphs/DensityPlotsByYear.pdf", width = 9, height = 15)
par(mfrow = c(5, 3))
par(oma = c(1, 1, 3, 1))
par(mar = c(3, 4, 3, 1))
plot(0, 0, type = "n", ylab = "", xlab = "", axes = F)
legend("center", levels(year.f), fill=my.col, cex = 2, title = "Year", box.col = "white")
for (i in 1:length(myvars)) {
  sm.density.compare(DATAFRAME[, as.character(myvars[i])], year.f, 
                     col = my.col, lwd = 3, lty = rep(1, 6),
                     xlab = "",
                     ylab = "relative frequency",
                     main = "",
                     axes = FALSE)
  title(mynames[i], line = 1)
}
mtext("Univariate Density Plots by Year of Sale", outer = TRUE)
dev.off()
