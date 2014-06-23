
getwd()
LWRresults = read.dbf("../Data/R2GIS/CleanData/TimeLag12months/AirMean3LWRresults.dbf")
DATAFRAME <- read.dbf("../Data/R2GIS/CleanData/Sales20052010.dbf")

bigDATA = merge(LWRresults, DATAFRAME, sort= FALSE)

bigDATA$CITY = relevel(bigDATA$CITY, "ST. PAUL")
lm0 = lm(bnoise ~ TimePeriod, data = bigDATA)
summary(lm0)
lm02 = lm(bnoise ~ TimePeriod + I(TimePeriod^2), data = bigDATA)
summary(lm02)
lm1 = lm(bnoise ~ TimePeriod + CITY, data = bigDATA)
summary(lm1)
lm12 = lm(bnoise ~ TimePeriod + I(TimePeriod^2) + CITY, data = bigDATA)
summary(lm12)

lm0st = lm(bnoise ~ TimePeriod, data = bigDATA, subset = (CITY == "ST. PAUL"))
summary(lm0st)
lm02st = lm(bnoise ~ TimePeriod + I(TimePeriod^2), data = bigDATA, subset = (CITY == "ST. PAUL"))
summary(lm02st)
require(stargazer)

indvarlabels = c("Months since Jan 2005", "(Months since Jan 2005)$^2$",
                 "Constant")

stargazer(lm0, lm02, lm1, lm12, lm0st, lm02st,
          digits.extra = 4, digits = 4,
          align = TRUE,
          omit.stat=c("adj.rsq","ser","f"),
          omit = c("CITY"),
          omit.labels = c("City Fixed Effects"),
          font.size = "scriptsize",
          column.sep.width = "-1pt",
          dep.var.labels = "Noise Beta",
          covariate.labels = indvarlabels)


require(graphics)
smoothScatter(bigDATA$TimePeriod, bigDATA$bnoise)
newdata = data.frame(TimePeriod = 1:72, CITY = "ST. PAUL")
lines(1:72, predict(lm0, newdata), col = "red")
lines(1:72, predict(lm02, newdata), col = "red")
lines(1:72, predict(lm1, newdata), col = "red")
lines(1:72, predict(lm12, newdata), col = "red")

smoothScatter(bigDATA$TimePeriod[bigDATA$CITY == "ST. PAUL"], bigDATA$bnoise[bigDATA$CITY == "ST. PAUL"])
lines(1:72, predict(lm1, newdata), col = "red")
lines(1:72, predict(lm12, newdata), col = "red")