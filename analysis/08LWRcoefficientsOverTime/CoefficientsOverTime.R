# Do the LWR coefficients seem to vary over time?
# the results of these regressions get plopped into the appropriate sections and tables in the article
# ultimately might want to create a bigger table

load("~/Noise Hedonic Project/Noise-Hedonic/analysis/04MonteCarloSim/ModelBigCity/CopyOfSales20052010LWRmodel18-2013-04-13.RData")
source("~/Noise Hedonic Project/Noise-Hedonic/helper/LWRfunctions.R")
require(foreign)
DATAFRAME = read.dbf("~/Noise Hedonic Project/Data/R2GIS/CleanData/Sales20052010.dbf")

optimalK = "k200"
TIMELAG = 12
obs2run = which(DATAFRAME$TimePeriod > (TIMELAG -1))
breakMonth = 45 #september 2008

tempData = data.frame(betaMAX = output$beta.MAX[, optimalK],
                      betaSQFT = output$beta.FIN_SQ_FT[, optimalK],
                      betaACRES = output$beta.ACRES_POLY[, optimalK],
                      betaINT = output[["beta.(Intercept)"]][, optimalK],
                      TimePeriod = DATAFRAME$TimePeriod[obs2run])
tempData$Post = ifelse(tempData$TimePeriod > breakMonth, 1, 0)
tempData$month = tempData$TimePeriod - breakMonth


lm.MAX = lm(betaMAX ~ month*Post, data = tempData)
summary(lm.MAX)

lm.INT = lm(betaINT ~ month*Post, data = tempData)
summary(lm.INT)

# what about some crazy outliers? doesn't matter
# INTz = (tempData$betaINT - mean(tempData$betaINT))/sd(tempData$betaINT)
# lm.INT = lm(betaINT ~ month*Post, data = tempData, subset = abs(INTz)<10)
# summary(lm.INT)


lm.SQFT = lm(betaSQFT ~ month*Post, data = tempData)
summary(lm.SQFT)

lm.ACRES = lm(betaACRES ~ month*Post, data = tempData)
summary(lm.ACRES)


# dataByMonth = data.frame(TimePeriod = TIMELAG:72,
#                          betaMAX = tapply(tempData$betaMAX, tempData$TimePeriod, median),
#                          betaSQFT = tapply(tempData$betaSQFT, tempData$TimePeriod, median),
#                          betaACRES = tapply(tempData$betaACRES, tempData$TimePeriod, median),
#                          betaINT = tapply(tempData$betaINT, tempData$TimePeriod, median),
#                          Post = tapply(tempData$Post, tempData$TimePeriod, median),
#                          month = tapply(tempData$month, tempData$TimePeriod, median))
# 
# require(graphics)
# with(tempData,
#      smoothScatter(period, betaINT))
# with(dataByMonth,
#      points(period, betaINT, col = "red"))
# 
# ###
# with(dataByMonth,
#      lines(period, lm.mod$fitted.values))

