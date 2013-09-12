#This script runs the most up to date global regression models that include interaction effects and non linear relationships.
require(foreign)
####### 2005 to 2010 Sales Model ########
workingdata20052010 <- read.dbf("../Data/R2GIS/CleanData/Sales20052010.dbf")
workingdata20052010$PostCrash = 0
crashobs <- which(workingdata20052010$TimePeriod>46)
workingdata20052010$PostCrash[crashobs] = 1

myModel = "logSALE_VA ~ FIN_SQ_FT + ACRES_POLY +  HOMESTEAD +  YEAR_BUILT + MAX +
 MED_INCOME + MCA3 + CBD_dist + LAKE_dist + PARK_dist + SHOP_dist  + COUNTY_ID +  factor(SALE_YR) + SALE_MO"

#ACRES_POLY * CBD_dist I(ACRES_POLY^2)* I(CBD_dist ^2)
##Run model (All)
lm.mini = lm(myModel, data = workingdata20052010)
summary(lm.mini, digits = 2)

ModelBig = paste0(myModel, " + ACRES_POLY * CBD_dist")
lm.big = lm(ModelBig, data = workingdata20052010)
summary(lm.big)

## Run model (Pre- and Post- Crash)

lm.bigPre = lm(ModelBig, data = workingdata20052010, subset = (PostCrash == 0))
summary(lm.bigPre)

lm.bigPost = lm(ModelBig, data = workingdata20052010, subset = (PostCrash == 1))
summary(lm.bigPost)