
getwd()
LWRresults = read.dbf("../Data/R2GIS/CleanData/TimeLag12months/AirMean3LWRresults.dbf")
DATAFRAME <- read.dbf("../Data/R2GIS/CleanData/Sales20052010.dbf")

bigDATA = merge(LWRresults, DATAFRAME, sort= FALSE)

bigDATA$CITY = relevel(bigDATA$CITY, "ST. PAUL")

# 500 bandwidth
lm0.50 = lm(bnoise50 ~ TimePeriod, data = bigDATA)
#summary(lm0.50)
lm02.50 = lm(bnoise50 ~ TimePeriod + I(TimePeriod^2), data = bigDATA)
#summary(lm02.50)
lm1.50 = lm(bnoise50 ~ TimePeriod + CITY, data = bigDATA)
#summary(lm1.50)
lm12.50 = lm(bnoise50 ~ TimePeriod + I(TimePeriod^2) + CITY, data = bigDATA)
#summary(lm12.50)

lm0st.50 = lm(bnoise50 ~ TimePeriod, data = bigDATA, subset = (CITY == "ST. PAUL"))
#summary(lm0st.50)
lm02st.50 = lm(bnoise50 ~ TimePeriod + I(TimePeriod^2), data = bigDATA, subset = (CITY == "ST. PAUL"))
#summary(lm02st.50)

# 2000 bandwidth
lm0.200 = lm(bnoise200 ~ TimePeriod, data = bigDATA)
summary(lm0.200)
lm02.200 = lm(bnoise200 ~ TimePeriod + I(TimePeriod^2), data = bigDATA)
summary(lm02.200)
lm1.200 = lm(bnoise200 ~ TimePeriod + CITY, data = bigDATA)
summary(lm1.200)
lm12.200 = lm(bnoise200 ~ TimePeriod + I(TimePeriod^2) + CITY, data = bigDATA)
summary(lm12.200)

lm0st.200 = lm(bnoise200 ~ TimePeriod, data = bigDATA, subset = (CITY == "ST. PAUL"))
summary(lm0st.200)
lm02st.200 = lm(bnoise200 ~ TimePeriod + I(TimePeriod^2), data = bigDATA, subset = (CITY == "ST. PAUL"))
summary(lm02st.200)

require(stargazer)

indvarlabels = c("Months since Jan 2005", "(Months since Jan 2005)$^2$",
                 "Constant")

stargazer(lm0.50, lm02.50, lm1.50, lm12.50, lm0st.50, lm02st.50,
          digits.extra = 4, digits = 4,
          align = TRUE,
          omit.stat=c("adj.rsq","ser","f"),
          omit = c("CITY"),
          omit.labels = c("City Fixed Effects"),
          font.size = "scriptsize",
          column.sep.width = "-1pt",
          dep.var.labels = "Bandwidth 500",
          covariate.labels = indvarlabels)

stargazer(lm0.200, lm02.200, lm1.200, lm12.200, lm0st.200, lm02st.200,
          digits.extra = 4, digits = 4,
          align = TRUE,
          omit.stat=c("adj.rsq","ser","f"),
          omit = c("CITY"),
          omit.labels = c("City Fixed Effects"),
          font.size = "scriptsize",
          column.sep.width = "-1pt",
          dep.var.labels = "Bandwidth 2000",
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