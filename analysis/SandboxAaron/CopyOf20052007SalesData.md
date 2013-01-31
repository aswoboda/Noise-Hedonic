Max's Analysis -- 2005-2007 SalesData 1/30/13
========================================================

This is a test run of analysis through RMarkdown using 2005-2007 Sales dataset. And Aaron is making a few small changes.

Preparation for regression analysis...

```r
getwd()
```

```
## [1] "/home/aswoboda/Noise Hedonic Project/Noise-Hedonic/analysis/SandboxAaron"
```

```r

library(foreign)
workingdata <- read.dbf("../../../Data/R2GIS/CleanData/Sales20052007.dbf")
```

```
## Error: unable to open DBF file
```

```r
require(zoo)
```

```
## Loading required package: zoo
```

```
## Attaching package: 'zoo'
```

```
## The following object(s) are masked from 'package:base':
## 
## as.Date, as.Date.numeric
```

```r
require(lmtest)
```

```
## Loading required package: lmtest
```

```r
require(car)
```

```
## Loading required package: car
```

```
## Loading required package: MASS
```

```
## Loading required package: nnet
```


In order to improve the model, I did three things:

(1) After normality issues arose in preliminary regressions, used more restrictive constraints on outliers to make distributions to key variables more normal.
Limited parcels to under 4000 square feet in living area, less than .6 acres, experiencing more than 25 dBA in traffic noise and sales values between $100k and $675k
Sample size went from ~28k to 25,994

(2) Along with restricting dataset to exclude outliers, we also transformed structural and distance variables to logarithms when appropriate in order to better fit the nonlinear relationship they had with the log transformed dependent variables (sales value).

(3) Included more fixed effect variables related to the timing of the sale (either by month or season). In the regression we only included fixed effect for season because there is perfect multicollinearity between sales month and sales season.


```r
hist(workingdata$logSALE_VA)
```

```
## Error: object 'workingdata' not found
```

```r
hist(workingdata$logFIN_SQ_)
```

```
## Error: object 'workingdata' not found
```

```r
hist(workingdata$logMAX)
```

```
## Error: object 'workingdata' not found
```


The first regression will be a baseline without any log transformations. Notice that this regression does not explain as much of the variation seen in sales value as the 2005-2007 based regression.



```r
model.SaleValue1 <- lm(SALE_VALUE ~ COUNTY_ID + CITY + SALE_YR + SDNUM + ACRES_POLY + 
    HOMESTEAD + FIN_SQ_FT + YEAR_BUILT + MAX + PARK_dist + LAKE_dist + MCA3 + 
    MCA5 + SHOP_dist + CBD_dist + SALE_SEASO, data = workingdata)
```

```
## Error: object 'workingdata' not found
```

```r
summary(model.SaleValue1)
```

```
## Error: object 'model.SaleValue1' not found
```


Looking at the residual and qqplot, notice that there may be some pattern in the variance as well as a violation of normality.


```r
unlogged = data.frame(yhat = model.SaleValue1$fitted.values, rr = model.SaleValue1$residuals)
```

```
## Error: object 'model.SaleValue1' not found
```

```r
plot(unlogged$yhat, unlogged$rr)
```

```
## Error: object 'unlogged' not found
```

```r
qqnorm(unlogged$rr)
```

```
## Error: object 'unlogged' not found
```

```r
qqline(unlogged$rr)
```

```
## Error: object 'unlogged' not found
```


The second regression implements the log transformations to sales value and the explanatory variables that have a nonlinear relationship with logged sales value.


```r
model.logSaleValue2 <- lm(logSALE_VA ~ COUNTY_ID + CITY + SALE_YR + ACRES_POLY + 
    ACRES2 + HOMESTEAD + logFIN_SQ_ + YEAR_BUILT + logMAX + logPARK + MCA3 + 
    logSHOP + logCBD + SALE_SEASO, data = workingdata)
```

```
## Error: object 'workingdata' not found
```

```r
summary(model.logSaleValue2)
```

```
## Error: object 'model.logSaleValue2' not found
```


Looking at the residual and qqplot, note that the residuals seem to follow a more pronounced pattern that is in violation with homoscedasticity. However, normality conditions improve, yet there are still some issues. I am joining the residuals with the shapefile in order to see if these observations with either large over- or under-estimates are clustered.


```r
logged = data.frame(yhat = model.logSaleValue2$fitted.values, rr = model.logSaleValue2$residuals)
```

```
## Error: object 'model.logSaleValue2' not found
```

```r
plot(logged$yhat, unlogged$rr)
```

```
## Error: object 'logged' not found
```

```r
qqnorm(logged$rr)
```

```
## Error: object 'logged' not found
```

```r
qqline(logged$rr)
```

```
## Error: object 'logged' not found
```


The last regression inputs the dummy variable for GARAGE. The inclusion of this variable excludes ~ 4k parcels mostly in Dakota County which brings the sample size to ~21k. This regression has similar looking residual and qqplots.


```r
model.logSaleValue3 <- lm(logSALE_VA ~ COUNTY_ID + CITY + SALE_YR + GARAGE + 
    ACRES_POLY + ACRES2 + HOMESTEAD + logFIN_SQ_ + YEAR_BUILT + logMAX + logPARK + 
    MCA3 + logSHOP + logCBD + SALE_SEASO, data = workingdata)
```

```
## Error: object 'workingdata' not found
```

```r
summary(model.logSaleValue3)
```

```
## Error: object 'model.logSaleValue3' not found
```


