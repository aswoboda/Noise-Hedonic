library("foreign")
sales0810 <- read.dbf("../Data/GIS2R/Sales20082010.dbf")
summary(sales0810)
str(sales0810)

# here's some code about how to eliminate variables we don't want...
dataNames = names(sales0810)
varsIdontWant = which(dataNames %in% c("PIN", "BLDG_NUM", "X7_MCA"))

sales0810[1:5, -varsIdontWant]


names(sales0810)

model1 = lm(EMV_TOTAL ~ HOME_STYLE + SALE_YR, data = sales0810)

names(model1)

ResidualsDF = data.frame(PIN = sales0810$PIN, model1$residuals)

write.dbf(ResidualsDF, "../Data/FromRservertoGISVol/example.dbf" )