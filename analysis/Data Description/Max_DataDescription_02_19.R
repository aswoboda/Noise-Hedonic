library (foreign)
workingdata <- read.dbf("../Data/R2GIS/CleanData/Sales20052010.dbf")

x2005 <- which (workingdata$SALE_YR == 2005)
x2006 <- which (workingdata$SALE_YR == 2006)
x2007 <- which (workingdata$SALE_YR == 2007)
x2008 <- which (workingdata$SALE_YR == 2008)
x2009 <- which (workingdata$SALE_YR == 2009)
x2010 <- which (workingdata$SALE_YR == 2010)

data2005 <- workingdata [x2005, ]
data2006 <- workingdata [x2006, ]
data2007 <- workingdata [x2007, ]
data2008 <- workingdata [x2008, ]
data2009 <- workingdata [x2009, ]
data2010 <- workingdata [x2010, ]

summary(data2005)
summary(data2006)
summary(data2007)
summary(data2008)
summary(data2009)
summary(data2010)
