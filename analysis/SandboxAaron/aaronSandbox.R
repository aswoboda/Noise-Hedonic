

# goal is to export the results of our LWR model to eventually create some maps in ArcMAP...

# 1 - open the appropriate R data file
load("~/NoiseHedonicProject/Data/R2GIS/CleanData/TimeLag12months/Sales20052010LWRmodelAirMean3-2014-03-19.RData")

# 2 - select the appropriate variables to export 
# remember the structure of "output" is a list of the 279 things we calculated...
# > names(output)
# [1] "beta.(Intercept)"             "beta.Air_Mean"                "beta.FIN_SQ_FT"              
# [4] "beta.ACRES_POLY"              "beta.YEAR_BUILT"              "beta.HOME_STYLE1-1/4 STRY"      
# [16] "beta.OWNOCC"                  "beta.PercWhite"               "beta.PercU18"                
# [19] "beta.MED_INCOME"              "beta.MCA3"                    "beta.LAKE_dist"              
# [22] "beta.PARK_dist"               "beta.SHOP_dist"               "beta.CBD_dist"  
# [139] "ses.(Intercept)"              "ses.Air_Mean"                 "ses.FIN_SQ_FT"               
# [142] "ses.ACRES_POLY"               "ses.YEAR_BUILT"               "ses.HOME_STYLE1-1/4 STRY"    
# [154] "ses.OWNOCC"                   "ses.PercWhite"                "ses.PercU18"                 
# [157] "ses.MED_INCOME"               "ses.MCA3"                     "ses.LAKE_dist"               
# [160] "ses.PARK_dist"                "ses.SHOP_dist"                "ses.CBD_dist" 

data2xport = data.frame(output$beta.Air_Mean[,"k650"])
names(data2xport) = "bnoise65"
data2xport$bfinft65 = output$beta.FIN_SQ_FT[,"k650"]
data2xport$bacres65 = output$beta.ACRES_POLY[,"k650"]
data2xport$senoise65 = output$ses.Air_Mean[, "k650"]
data2xport$sefinft65 = output$ses.FIN_SQ_FT[,"k650"]
data2xport$seacres65 = output$ses.ACRES_POLY[,"k650"]
data2xport$yhats65 = output$yhats[, "k650"]
data2xport$UNIQID = as.numeric(rownames(data2xport))

data2xport$bnoise50 = output$beta.Air_Mean[,"k500"]
data2xport$bfinft50 = output$beta.FIN_SQ_FT[,"k500"]
data2xport$bacres50 = output$beta.ACRES_POLY[,"k500"]
data2xport$senoise50 = output$ses.Air_Mean[, "k500"]
data2xport$sefinft50 = output$ses.FIN_SQ_FT[,"k500"]
data2xport$seacres50 = output$ses.ACRES_POLY[,"k500"]
data2xport$yhats50 = output$yhats[, "k500"]

data2xport$bnoise100 = output$beta.Air_Mean[,"k1000"]
data2xport$bfinft100 = output$beta.FIN_SQ_FT[,"k1000"]
data2xport$bacres100 = output$beta.ACRES_POLY[,"k1000"]
data2xport$yhats100 = output$yhats[, "k1000"]

data2xport$bnoise200 = output$beta.Air_Mean[,"k2000"]
data2xport$bfinft200 = output$beta.FIN_SQ_FT[,"k2000"]
data2xport$bacres200 = output$beta.ACRES_POLY[,"k2000"]
data2xport$yhats200 = output$yhats[, "k2000"]

require(foreign)
DATAFRAME = read.dbf("../Data/R2GIS/CleanData/Sales20052010.dbf")
obs2run = which(DATAFRAME$TimePeriod>11)
lm.global = lm(MYMODEL, data = DATAFRAME[obs2run, ])
head(DATAFRAME$UNIQID[obs2run])
head(rownames(data2xport))
data2xport$yhatsglob = lm.global$fitted.values

# 3 - create a flat table of the variables (make sure they have names <10 characters long)

# 4 - export as .dbf file
require(foreign)
write.dbf(dataframe=data2xport, "../Data/R2GIS/CleanData/TimeLag12months/AirMean3LWRresults.dbf")


# MIXED GWR attempt...



load("~/NoiseHedonicProject/Data/R2GIS/CleanData/MixedLWR/mixedStep4.RData")
# output and MYMODEL loaded

# names(output) # 271 different betas, ses, etc.
dim(output[["yhats"]]) # only for obs with Timeperiod >11

# what is the GCV score for this model?

require(foreign)
require(multicore, quietly = TRUE)
require(fields, quietly = TRUE)

# the following command loads up some functions we'll use
source("helper/LWRfunctions.R")

load("~/NoiseHedonicProject/Data/R2GIS/CleanData/MixedLWR/step3bOutput.RData")
# DATAFRAME
names(DATAFRAME)

myobs = which(DATAFRAME$TimePeriod > 11)

# our estimated logged sales price is equal to the yhats in "output" + DATAFRAME$Xaahat

mixedLWRyhat = output$yhats + DATAFRAME$Xaahat[myobs]
gcvs = GCV(output$leverages, mixedLWRyhat, DATAFRAME$logSALE_VA[myobs]) # GCV of 2.19 doesn't make sense
print(gcvs) # 0.296 is about 10% higher than we had with the full LWR

# what does a time series plot of noise coefficients and se's look like?

plot(DATAFRAME$TimePeriod, DATAFRAME$Noisebeta)

DATAtimecollapsed = data.frame(TimePeriod = 1:72,
                               Noisebeta = tapply(DATAFRAME$Noisebeta, DATAFRAME$TimePeriod, mean),
                               Noisese = tapply(DATAFRAME$Noisese, DATAFRAME$TimePeriod, mean))

plot(DATAtimecollapsed$TimePeriod, DATAtimecollapsed$Noisebeta, type = "l", lwd = "3")
lines(DATAtimecollapsed$TimePeriod, DATAtimecollapsed$Noisebeta + 2*DATAtimecollapsed$Noisese)
lines(DATAtimecollapsed$TimePeriod, DATAtimecollapsed$Noisebeta - 2*DATAtimecollapsed$Noisese)
abline(v = 12)

names(output)
require(graphics) 
smoothScatter(DATAFRAME$TimePeriod[myobs], output$beta.FIN_SQ_FT)

smoothScatter(DATAFRAME$TimePeriod, DATAFRAME$Air_Mean)

