#This script runs the most up to date global regression models that include interaction effects and non linear relationships.
require(foreign)
####### 2005 to 2010 Sales Model ########
workingdata20052010 <- read.dbf("../Data/R2GIS/CleanData/Sales20052010.dbf")
workingdata20052010$PostCrash = 0
crashobs <- which(workingdata20052010$TimePeriod>46)
workingdata20052010$PostCrash[crashobs] = 1

myModel = "logSALE_VA ~ Air_Mean + I(FIN_SQ_FT/1000) + ACRES_POLY +  OWNOCC +  YEAR_BUILT + PercU18 + PercWhite + 
I(MED_INCOME/1000) + MCA3 + I(CBD_dist/1000) + I(LAKE_dist/1000) + I(PARK_dist/1000) + I(SHOP_dist/1000) + factor(TimePeriod) + HOME_STYLE"

#ACRES_POLY * CBD_dist I(ACRES_POLY^2)* I(CBD_dist ^2)
##Run model (All)
lm.mini = lm(myModel, data = workingdata20052010)
summary(lm.mini, digits = 2)

ModelBig = paste0(myModel, "+ CITY")
lm.big = lm(ModelBig, data = workingdata20052010)#, subset = (SALE_YR == 2010))
#summary(lm.big)

ModelMonsterLot = paste0(myModel, "+ CITY*ACRES_POLY")
lm.monsterLot = lm(ModelMonsterLot, data = workingdata20052010)#, subset = (SALE_YR == 2010))
#summary(lm.monsterLot)
# F test of ModelMonster vs. ModelBig
anova(lm.monsterLot, lm.big)

ModelMonsterSqft = paste0(myModel, "+ CITY*FIN_SQ_FT")
lm.monsterSqft = lm(ModelMonsterSqft, data = workingdata20052010)#, subset = (SALE_YR == 2010))
#summary(lm.monsterSqft)
anova(lm.monsterSqft, lm.big)

ModelMonsterNoise = paste0(myModel, "+ CITY*Air_Mean")
lm.monsterNoise = lm(ModelMonsterNoise, data = workingdata20052010)#, subset = (SALE_YR == 2010))
#summary(lm.monsterNoise)
anova(lm.monsterNoise, lm.big)

ModelMonster = paste0(myModel, "+ CITY*ACRES_POLY + CITY*FIN_SQ_FT + CITY*Air_Mean")
lm.monster = lm(ModelMonster, data = workingdata20052010)#, subset = (SALE_YR == 2010))
#summary(lm.monster)
anova(lm.big, lm.monster)

# run model for a couple years 
lm.big06 = lm(ModelBig, data = workingdata20052010, subset = (SALE_YR == 2006))
summary(lm.big06)

lm.big07 = lm(ModelBig, data = workingdata20052010, subset = (SALE_YR == 2007))
summary(lm.big07)

lm.big08 = lm(ModelBig, data = workingdata20052010, subset = (SALE_YR == 2008))
summary(lm.big08)

lm.big10 = lm(ModelBig, data = workingdata20052010, subset = (SALE_YR == 2010))
summary(lm.big10)

# MegaMonster - Interaction Terms Across Time Periods and ... Acres, Sqft, Noise
ModelMegaMonster = paste0(myModel, "+ CITY*ACRES_POLY + CITY*FIN_SQ_FT + CITY*Air_Mean +
                          factor(TimePeriod)*ACRES_POLY + 
                          factor(TimePeriod)*FIN_SQ_FT + 
                          factor(TimePeriod)*Air_Mean")
lm.Megamonster = lm(ModelMegaMonster, data = workingdata20052010)#, subset = (SALE_YR == 2010))
#summary(lm.monster)
anova(lm.monster, lm.Megamonster)


indvarlabels = c("Noise (dB)", "House Size (1,000s ft$^2$)", "Lot Size (acres)",
                 "Owner Occupancy Dummy", "Year House Built", "Percent Under 18",
                 "Percent White", "Median Income (\\$1,000s)", "Elementary Test Scores",
                 "Dist to CBD (km)", "Dist to Lake (km)", "Dist to Park (km)", "Dist to Shop (km)",
                 "Constant")
require(stargazer)
stargazer(lm.big, lm.big06, lm.big08, lm.big10,
          digits.extra = 4, digits = 4,
          align = TRUE,
          omit.stat=c("adj.rsq","ser","f"),
          omit = c("HOME", "TimePeriod", "CITY"),
          omit.labels = c("Home Style Fixed Effects", "Month*Year Fixed Effects", "City Fixed Effects"),
          font.size = "scriptsize",
          column.sep.width = "-1pt",
          dep.var.labels = "ln Sale Price",
          covariate.labels = indvarlabels)