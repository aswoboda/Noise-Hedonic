
require(foreign)
DATAFRAME = read.dbf("~/NoiseHedonicProject/Data/R2GIS/CleanData/Sales20052010.dbf")
DATAFRAME$OrigOrder = 1:42083
# I have five files with predicted values of the dependent variable and 
# the four stationary variables
# for observations with timperiod > 1

# MYMODELS = c("logSALE_VA", "Air_Mean", "OWNOCC", "PercWhite", "PercU18")

load("~/NoiseHedonicProject/Data/R2GIS/CleanData/MixedLWR/tempmodel1-2014-04-27.RData")
temp = output$yhats
tempData = data.frame(priceHAT = temp,
                      UNIQID = as.numeric(rownames(temp)),
                      tempORDER = 1:41535)
names(tempData)[1] = "priceHAT"
load("~/NoiseHedonicProject/Data/R2GIS/CleanData/MixedLWR/tempmodel2-2014-04-27.RData")
temp = output$yhats
tempData$noiseHAT = temp

load("~/NoiseHedonicProject/Data/R2GIS/CleanData/MixedLWR/tempmodel3-2014-04-27.RData")
temp = output$yhats
tempData$ownoccHAT = temp

load("~/NoiseHedonicProject/Data/R2GIS/CleanData/MixedLWR/tempmodel4-2014-04-27.RData")
temp = output$yhats
tempData$percwhiteHAT = temp

load("~/NoiseHedonicProject/Data/R2GIS/CleanData/MixedLWR/tempmodel5-2014-04-27.RData")
temp = output$yhats
tempData$perc18HAT = temp

TEMPDATA = as.matrix(tempData)

MYMODELS = c("logSALE_VA ~ FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+MED_INCOME+MCA3+LAKE_dist+PARK_dist+SHOP_dist+CBD_dist+CITY",
             "Air_Mean ~ FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+MED_INCOME+MCA3+LAKE_dist+PARK_dist+SHOP_dist+CBD_dist+CITY",
             "OWNOCC ~ FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+MED_INCOME+MCA3+LAKE_dist+PARK_dist+SHOP_dist+CBD_dist+CITY",
             "PercWhite ~ FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+MED_INCOME+MCA3+LAKE_dist+PARK_dist+SHOP_dist+CBD_dist+CITY",
             "PercU18 ~ FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+MED_INCOME+MCA3+LAKE_dist+PARK_dist+SHOP_dist+CBD_dist+CITY")

myobs = which(DATAFRAME$TimePeriod < 2)

lm1 = lm(MYMODELS[1], data = DATAFRAME, subset = myobs)
temptemp = lm1$fitted.values
temptemp = data.frame(priceHAT = lm1$fitted.values,
                      UNIQID = DATAFRAME$UNIQID[myobs],
                      tempORDER = 41536:42083)
                      
lm2 = lm(MYMODELS[2], data = DATAFRAME, subset = myobs)
temptemp$noiseHAT = lm2$fitted.values                     

lm3 = lm(MYMODELS[3], data = DATAFRAME, subset = myobs)
temptemp$ownoccHAT = lm3$fitted.values   

lm4 = lm(MYMODELS[4], data = DATAFRAME, subset = myobs)
temptemp$percwhiteHAT = lm4$fitted.values 

lm5 = lm(MYMODELS[5], data = DATAFRAME, subset = myobs)
temptemp$perc18HAT = lm4$fitted.values 

TEMPTEMP = as.matrix(temptemp)

colnames(TEMPDATA)
colnames(TEMPTEMP)

masterTEMP = as.data.frame(rbind(TEMPDATA, TEMPTEMP))
names(masterTEMP)

newDATA = merge(DATAFRAME, masterTEMP, sort = FALSE)

summary(DATAFRAME$UNIQID - newDATA$UNIQID) # should be 42083 zeros

DATAFRAME = newDATA
save(DATAFRAME, file = "~/NoiseHedonicProject/Data/R2GIS/CleanData/MixedLWR/mixedDATA.RData")
