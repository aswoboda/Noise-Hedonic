# Goal is to make a table of LWR results for the paper...

# mega columns (for each bandwidth: 500, 650, 1000, 2000)
# for each bandwidth 
# mean LWR Beta hats
# 10th and 90th percentile of LWR beta hats


# load the dataframe and helper functions
require(foreign)
DATAFRAME = read.dbf("~/NoiseHedonicProject/Data/R2GIS/CleanData/Sales20052010.dbf")
obs2run = which(DATAFRAME$TimePeriod>11)
source("~/NoiseHedonicProject/Noise-Hedonic/helper/LWRfunctions.R")

####################
# Model 3
####################
load("~/NoiseHedonicProject/Data/R2GIS/CleanData/TimeLag12months/Sales20052010LWRmodelAirMean3-2014-03-19.RData")

# right now I make the table have three columns for each bandwidth - mean, 10th, 90th percentile
BWs = c("k500", "k650", "k1000")
variableOrder = c(2:5, 16:24)
names(output)[variableOrder]
# [1] "beta.Air_Mean"   "beta.FIN_SQ_FT"  "beta.ACRES_POLY" "beta.YEAR_BUILT" "beta.OWNOCC"     "beta.PercWhite" 
# [7] "beta.PercU18"    "beta.MED_INCOME" "beta.MCA3"       "beta.LAKE_dist"  "beta.PARK_dist"  "beta.SHOP_dist" 
# [13] "beta.CBD_dist"
unitsAdjuster = c(1, 1000, 1, 1, 1, 1, 1, 1000, 1, 1000, 1000, 1000, 1000)
numvars = length(variableOrder)
LWRtable = matrix(NA, numvars, 9)
for (BW in 1:length(BWs)) {
  for (i in 1:numvars) {
    LWRtable[i, (BW-1)*3+1] = signif(mean(output[[variableOrder[i]]][,BWs[BW]]), 2)
    LWRtable[i, (BW-1)*3+2:3] = signif(quantile(output[[variableOrder[i]]][,BWs[BW]], c(.1, .9)), 2)
  }
}

niceTable = LWRtable*unitsAdjuster
require(stargazer)
varlabels = c("Noise (dB)", "House Size (1,000s ft2)", "Lot Size (acres)", "Year House Built",
                 "Owner Occupancy Dummy", "Percent White", "Percent Under 18",
                  "Median Income (1,000s)", "Elementary Test Scores",
                  "Dist to Lake (km)", "Dist to Park (km)", "Dist to Shop (km)", "Dist to CBD (km)")
rownames(niceTable) = varlabels
stargazer(niceTable,
          align = TRUE,
          digits = 4, column.sep.width = "-1pt"
          )

###################
# another attempt at the table
# now I want to have two columns for each bandwidth and two rows for each variable 
# (top row has mean, second row has 10th and 90th percentiles)
###################
BWs = c("k500", "k650", "k1000", "k2000")
variableOrder = c(2:5, 16:24)
names(output)[variableOrder]
# [1] "beta.Air_Mean"   "beta.FIN_SQ_FT"  "beta.ACRES_POLY" "beta.YEAR_BUILT" "beta.OWNOCC"     "beta.PercWhite" 
# [7] "beta.PercU18"    "beta.MED_INCOME" "beta.MCA3"       "beta.LAKE_dist"  "beta.PARK_dist"  "beta.SHOP_dist" 
# [13] "beta.CBD_dist"
unitsAdjuster = c(1, 1000, 1, 1, 1, 1, 1, 1000, 1, 1000, 1000, 1000, 1000)
numvars = length(variableOrder)
LWRtable = matrix(NA, numvars*3, 4)
for (BW in 1:length(BWs)) {
  for (i in 1:numvars) {
    LWRtable[3*i-2, BW] = signif(unitsAdjuster[i]*mean(output[[variableOrder[i]]][,BWs[BW]]), 2)
    
    temp = signif(unitsAdjuster[i]*quantile(output[[variableOrder[i]]][,BWs[BW]], c(.1, .9)), 2)
    LWRtable[3*i-1, BW] = paste0("(", paste(temp, sep = "", collapse =" to "), ")")
  }
}

require(stargazer)
varlabels = c("Noise (dB)", "","", "House Size (1,000s ft2)", "","", "Lot Size (acres)", "","", 
              "Year House Built","", "",
              "Owner Occupancy Dummy", "", "", "Percent White", "","", "Percent Under 18", "","",
              "Median Income (1,000s)", "","", "Elementary Test Scores", "","",
              "Dist to Lake (km)", "","", "Dist to Park (km)", "","", "Dist to Shop (km)", "","", 
              "Dist to CBD (km)","", "")
rownames(LWRtable) = varlabels
stargazer(LWRtable,
          align = TRUE,
          digits = 4, column.sep.width = "-1pt"
)