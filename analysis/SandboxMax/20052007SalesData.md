Max's Analysis -- 2005-2007 SalesData 1/30/13
========================================================

This is a test run of analysis through RMarkdown using 2005-2007 Sales dataset. And Aaron is making a few small changes.

Preparation for regression analysis...

```r
getwd()
```

```
## [1] "/home/timmm/Noise Hedonic Project/Noise-Hedonic/analysis/SandboxMax"
```

```r

library(foreign)
workingdata <- read.dbf("../../../Data/R2GIS/CleanData/Sales20052007.dbf")
require(zoo)
require(lmtest)
require(car)
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

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-21.png) 

```r
hist(workingdata$logFIN_SQ_)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-22.png) 

```r
hist(workingdata$logMAX)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-23.png) 


The first regression will be a baseline without any log transformations. Notice that this regression does not explain as much of the variation seen in sales value as the 2005-2007 based regression.



```r
model.SaleValue1 <- lm(SALE_VALUE ~ COUNTY_ID + CITY + SALE_YR + SDNUM + ACRES_POLY + 
    HOMESTEAD + FIN_SQ_FT + YEAR_BUILT + MAX + PARK_dist + LAKE_dist + MCA3 + 
    MCA5 + SHOP_dist + CBD_dist + SALE_SEASO, data = workingdata)
summary(model.SaleValue1)
```

```
## 
## Call:
## lm(formula = SALE_VALUE ~ COUNTY_ID + CITY + SALE_YR + SDNUM + 
##     ACRES_POLY + HOMESTEAD + FIN_SQ_FT + YEAR_BUILT + MAX + PARK_dist + 
##     LAKE_dist + MCA3 + MCA5 + SHOP_dist + CBD_dist + SALE_SEASO, 
##     data = workingdata)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -374678  -31662   -5399   22334  372792 
## 
## Coefficients: (2 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   1.40e+06   8.52e+05    1.64  0.10027    
## COUNTY_ID123                  5.35e+04   9.39e+03    5.70  1.2e-08 ***
## COUNTY_ID163                  4.81e+04   4.04e+04    1.19  0.23361    
## CITYARDEN HILLS               3.75e+04   5.89e+03    6.35  2.1e-10 ***
## CITYBURNSVILLE               -7.70e+03   2.36e+03   -3.26  0.00110 ** 
## CITYCITY OF BAYPORT          -2.20e+04   3.91e+04   -0.56  0.57267    
## CITYCITY OF BIRCHWOOD         1.06e+05   4.05e+04    2.62  0.00871 ** 
## CITYCITY OF COTTAGE GROVE     1.73e+04   3.84e+04    0.45  0.65256    
## CITYCITY OF HUGO             -1.88e+04   3.89e+04   -0.48  0.62799    
## CITYCITY OF LAKE ELMO         3.64e+04   3.94e+04    0.92  0.35591    
## CITYCITY OF MAHTOMEDI         8.95e+04   3.87e+04    2.31  0.02077 *  
## CITYCITY OF NEWPORT           2.43e+04   3.89e+04    0.63  0.53167    
## CITYCITY OF OAKDALE           7.78e+04   3.87e+04    2.01  0.04465 *  
## CITYCITY OF OAK PARK HEIGHTS  6.01e+03   3.88e+04    0.15  0.87701    
## CITYCITY OF STILLWATER        2.50e+04   3.85e+04    0.65  0.51627    
## CITYCITY OF ST PAUL PARK      2.08e+04   3.86e+04    0.54  0.59112    
## CITYCITY OF WHITE BEAR LAKE   3.92e+04   4.25e+04    0.92  0.35719    
## CITYCITY OF WILLERNIE         1.74e+04   3.99e+04    0.44  0.66304    
## CITYCITY OF WOODBURY          6.53e+04   3.85e+04    1.70  0.08989 .  
## CITYEAGAN                     5.12e+04   2.68e+03   19.12  < 2e-16 ***
## CITYEMPIRE TOWNSHIP          -2.94e+04   8.00e+03   -3.67  0.00024 ***
## CITYFALCON HEIGHTS            9.24e+04   6.93e+03   13.35  < 2e-16 ***
## CITYFARMINGTON               -2.15e+04   3.08e+03   -7.00  2.6e-12 ***
## CITYGEM LAKE                  3.53e+04   3.85e+04    0.92  0.35940    
## CITYINVER GROVE HEIGHTS       7.90e+04   3.89e+03   20.31  < 2e-16 ***
## CITYLAKEVILLE                -1.85e+04   2.60e+03   -7.11  1.2e-12 ***
## CITYLAUDERDALE                7.35e+04   8.49e+03    8.66  < 2e-16 ***
## CITYLITTLE CANADA             5.07e+04   6.44e+03    7.87  3.7e-15 ***
## CITYMAPLEWOOD                 4.51e+04   4.33e+03   10.40  < 2e-16 ***
## CITYMENDOTA                   2.78e+04   5.44e+04    0.51  0.60857    
## CITYMENDOTA HEIGHTS           1.12e+05   5.60e+03   19.94  < 2e-16 ***
## CITYMOUNDS VIEW               9.12e+03   5.04e+03    1.81  0.07042 .  
## CITYNEW BRIGHTON              2.50e+04   4.49e+03    5.57  2.5e-08 ***
## CITYNORTH ST. PAUL            4.27e+04   4.85e+03    8.80  < 2e-16 ***
## CITYROSEMOUNT                 2.21e+04   2.55e+03    8.66  < 2e-16 ***
## CITYROSEVILLE                 5.69e+04   4.42e+03   12.88  < 2e-16 ***
## CITYSHOREVIEW                 3.10e+04   4.26e+03    7.28  3.3e-13 ***
## CITYSOUTH ST PAUL             1.01e+05   5.40e+03   18.66  < 2e-16 ***
## CITYSPRING LAKE PARK         -7.78e+02   5.43e+04   -0.01  0.98858    
## CITYST. ANTHONY               3.39e+04   1.64e+04    2.07  0.03863 *  
## CITYST. PAUL                  9.57e+04   4.28e+03   22.35  < 2e-16 ***
## CITYTOWN OF BAYTOWN                 NA         NA      NA       NA    
## CITYVADNAIS HEIGHTS           3.05e+04   5.15e+03    5.93  3.1e-09 ***
## CITYWEST ST PAUL              1.09e+05   4.94e+03   22.06  < 2e-16 ***
## CITYWHITE BEAR LAKE           2.92e+04   4.28e+03    6.81  9.7e-12 ***
## CITYWHITE BEAR TOWNSHIP             NA         NA      NA       NA    
## SALE_YR                      -1.32e+03   4.25e+02   -3.12  0.00182 ** 
## SDNUM                         2.87e+01   1.95e+01    1.47  0.14050    
## ACRES_POLY                    7.81e+04   4.45e+03   17.54  < 2e-16 ***
## HOMESTEADY                    5.45e+03   1.06e+03    5.16  2.4e-07 ***
## FIN_SQ_FT                     1.10e+02   6.67e-01  165.01  < 2e-16 ***
## YEAR_BUILT                    3.42e+02   1.75e+01   19.54  < 2e-16 ***
## MAX                          -5.79e+02   4.50e+01  -12.86  < 2e-16 ***
## PARK_dist                    -1.03e+00   3.39e-01   -3.04  0.00233 ** 
## LAKE_dist                     5.50e+00   6.44e-01    8.54  < 2e-16 ***
## MCA3                          1.35e+03   6.29e+01   21.47  < 2e-16 ***
## MCA5                          4.48e+01   1.15e+01    3.91  9.1e-05 ***
## SHOP_dist                     2.82e+00   3.68e-01    7.67  1.8e-14 ***
## CBD_dist                      4.24e+00   1.89e-01   22.49  < 2e-16 ***
## SALE_SEASO2                   5.10e+03   9.77e+02    5.22  1.8e-07 ***
## SALE_SEASO3                   6.05e+03   9.88e+02    6.13  9.2e-10 ***
## SALE_SEASO4                   2.61e+03   1.09e+03    2.40  0.01623 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 54200 on 25934 degrees of freedom
## Multiple R-squared: 0.715,	Adjusted R-squared: 0.714 
## F-statistic: 1.1e+03 on 59 and 25934 DF,  p-value: <2e-16
```


Looking at the residual and qqplot, notice that there may be some pattern in the variance as well as a violation of normality.


```r
unlogged = data.frame(yhat = model.SaleValue1$fitted.values, rr = model.SaleValue1$residuals)
plot(unlogged$yhat, unlogged$rr)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-41.png) 

```r
qqnorm(unlogged$rr)
qqline(unlogged$rr)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-42.png) 


The second regression implements the log transformations to sales value and the explanatory variables that have a nonlinear relationship with logged sales value.


```r
model.logSaleValue2 <- lm(logSALE_VA ~ COUNTY_ID + CITY + SALE_YR + ACRES_POLY + 
    ACRES2 + HOMESTEAD + logFIN_SQ_ + YEAR_BUILT + logMAX + logPARK + MCA3 + 
    logSHOP + logCBD + SALE_SEASO, data = workingdata)
summary(model.logSaleValue2)
```

```
## 
## Call:
## lm(formula = logSALE_VA ~ COUNTY_ID + CITY + SALE_YR + ACRES_POLY + 
##     ACRES2 + HOMESTEAD + logFIN_SQ_ + YEAR_BUILT + logMAX + logPARK + 
##     MCA3 + logSHOP + logCBD + SALE_SEASO, data = workingdata)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.2630 -0.1072 -0.0106  0.0964  1.0352 
## 
## Coefficients: (2 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   1.71e+01   2.68e+00    6.39  1.7e-10 ***
## COUNTY_ID123                  1.70e-01   1.25e-02   13.63  < 2e-16 ***
## COUNTY_ID163                  1.92e-01   1.21e-01    1.59   0.1119    
## CITYARDEN HILLS               1.47e-01   1.84e-02    8.03  1.0e-15 ***
## CITYBURNSVILLE               -1.93e-02   7.43e-03   -2.60   0.0094 ** 
## CITYCITY OF BAYPORT          -9.74e-02   1.23e-01   -0.79   0.4277    
## CITYCITY OF BIRCHWOOD         2.83e-01   1.26e-01    2.24   0.0253 *  
## CITYCITY OF COTTAGE GROVE     5.40e-02   1.21e-01    0.45   0.6553    
## CITYCITY OF HUGO             -5.43e-03   1.21e-01   -0.04   0.9643    
## CITYCITY OF LAKE ELMO         6.97e-02   1.24e-01    0.56   0.5742    
## CITYCITY OF MAHTOMEDI         2.38e-01   1.22e-01    1.96   0.0504 .  
## CITYCITY OF NEWPORT           2.26e-02   1.22e-01    0.18   0.8534    
## CITYCITY OF OAKDALE           2.44e-01   1.21e-01    2.02   0.0438 *  
## CITYCITY OF OAK PARK HEIGHTS  2.50e-02   1.22e-01    0.20   0.8379    
## CITYCITY OF STILLWATER        6.36e-02   1.21e-01    0.53   0.5992    
## CITYCITY OF ST PAUL PARK      2.99e-02   1.21e-01    0.25   0.8054    
## CITYCITY OF WHITE BEAR LAKE   1.45e-01   1.34e-01    1.09   0.2778    
## CITYCITY OF WILLERNIE        -1.97e-02   1.25e-01   -0.16   0.8747    
## CITYCITY OF WOODBURY          1.77e-01   1.21e-01    1.46   0.1437    
## CITYEAGAN                     1.34e-01   7.33e-03   18.25  < 2e-16 ***
## CITYEMPIRE TOWNSHIP          -6.79e-02   2.47e-02   -2.75   0.0060 ** 
## CITYFALCON HEIGHTS            3.94e-01   2.15e-02   18.37  < 2e-16 ***
## CITYFARMINGTON               -4.04e-02   8.29e-03   -4.88  1.1e-06 ***
## CITYGEM LAKE                  8.91e-02   1.21e-01    0.74   0.4621    
## CITYINVER GROVE HEIGHTS       2.11e-01   1.05e-02   20.09  < 2e-16 ***
## CITYLAKEVILLE                -1.66e-02   7.01e-03   -2.36   0.0181 *  
## CITYLAUDERDALE                3.16e-01   2.63e-02   12.02  < 2e-16 ***
## CITYLITTLE CANADA             1.94e-01   1.99e-02    9.73  < 2e-16 ***
## CITYMAPLEWOOD                 1.90e-01   1.31e-02   14.46  < 2e-16 ***
## CITYMENDOTA                   2.92e-02   1.71e-01    0.17   0.8640    
## CITYMENDOTA HEIGHTS           4.26e-01   1.51e-02   28.20  < 2e-16 ***
## CITYMOUNDS VIEW               2.63e-02   1.58e-02    1.67   0.0948 .  
## CITYNEW BRIGHTON              1.03e-01   1.38e-02    7.42  1.2e-13 ***
## CITYNORTH ST. PAUL            1.51e-01   1.47e-02   10.21  < 2e-16 ***
## CITYROSEMOUNT                 6.29e-02   7.97e-03    7.89  3.1e-15 ***
## CITYROSEVILLE                 2.45e-01   1.34e-02   18.20  < 2e-16 ***
## CITYSHOREVIEW                 1.04e-01   1.34e-02    7.75  9.8e-15 ***
## CITYSOUTH ST PAUL             2.82e-01   1.04e-02   27.11  < 2e-16 ***
## CITYSPRING LAKE PARK         -7.24e-02   1.71e-01   -0.42   0.6721    
## CITYST. ANTHONY               1.14e-01   4.71e-02    2.41   0.0159 *  
## CITYST. PAUL                  4.05e-01   1.30e-02   31.09  < 2e-16 ***
## CITYTOWN OF BAYTOWN                 NA         NA      NA       NA    
## CITYVADNAIS HEIGHTS           8.80e-02   1.61e-02    5.48  4.3e-08 ***
## CITYWEST ST PAUL              4.52e-01   1.29e-02   34.96  < 2e-16 ***
## CITYWHITE BEAR LAKE           9.26e-02   1.33e-02    6.97  3.2e-12 ***
## CITYWHITE BEAR TOWNSHIP             NA         NA      NA       NA    
## SALE_YR                      -7.71e-03   1.34e-03   -5.77  7.8e-09 ***
## ACRES_POLY                    4.36e-01   5.42e-02    8.04  9.7e-16 ***
## ACRES2                       -2.67e-01   8.25e-02   -3.23   0.0012 ** 
## HOMESTEADY                    2.46e-02   3.32e-03    7.40  1.4e-13 ***
## logFIN_SQ_                    6.04e-01   3.70e-03  163.03  < 2e-16 ***
## YEAR_BUILT                    1.27e-03   5.57e-05   22.85  < 2e-16 ***
## logMAX                       -1.20e-01   8.47e-03  -14.15  < 2e-16 ***
## logPARK                      -4.25e-03   1.48e-03   -2.88   0.0040 ** 
## MCA3                          4.81e-03   2.00e-04   24.06  < 2e-16 ***
## logSHOP                       2.42e-02   1.84e-03   13.19  < 2e-16 ***
## logCBD                        2.14e-01   5.14e-03   41.72  < 2e-16 ***
## SALE_SEASO2                   1.91e-02   3.07e-03    6.21  5.5e-10 ***
## SALE_SEASO3                   2.00e-02   3.11e-03    6.45  1.2e-10 ***
## SALE_SEASO4                   4.27e-03   3.42e-03    1.25   0.2116    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 0.171 on 25936 degrees of freedom
## Multiple R-squared: 0.744,	Adjusted R-squared: 0.743 
## F-statistic: 1.32e+03 on 57 and 25936 DF,  p-value: <2e-16
```


Looking at the residual and qqplot, note that the residuals seem to follow a more pronounced pattern that is in violation with homoscedasticity. However, normality conditions improve, yet there are still some issues. I am joining the residuals with the shapefile in order to see if these observations with either large over- or under-estimates are clustered.


```r
logged = data.frame(yhat = model.logSaleValue2$fitted.values, rr = model.logSaleValue2$residuals)
plot(logged$yhat, unlogged$rr)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-61.png) 

```r
qqnorm(logged$rr)
qqline(logged$rr)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-62.png) 


The last regression inputs the dummy variable for GARAGE. The inclusion of this variable excludes ~ 4k parcels mostly in Dakota County which brings the sample size to ~21k. This regression has similar looking residual and qqplots.


```r
model.logSaleValue3 <- lm(logSALE_VA ~ COUNTY_ID + CITY + SALE_YR + GARAGE + 
    ACRES_POLY + ACRES2 + HOMESTEAD + logFIN_SQ_ + YEAR_BUILT + logMAX + logPARK + 
    MCA3 + logSHOP + logCBD + SALE_SEASO, data = workingdata)
summary(model.logSaleValue3)
```

```
## 
## Call:
## lm(formula = logSALE_VA ~ COUNTY_ID + CITY + SALE_YR + GARAGE + 
##     ACRES_POLY + ACRES2 + HOMESTEAD + logFIN_SQ_ + YEAR_BUILT + 
##     logMAX + logPARK + MCA3 + logSHOP + logCBD + SALE_SEASO, 
##     data = workingdata)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.2440 -0.1068 -0.0121  0.0952  1.0046 
## 
## Coefficients: (2 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   2.16e+01   2.89e+00    7.45  9.7e-14 ***
## COUNTY_ID123                  1.74e-01   1.36e-02   12.79  < 2e-16 ***
## COUNTY_ID163                  1.88e-01   1.19e-01    1.58  0.11487    
## CITYARDEN HILLS               1.25e-01   1.98e-02    6.29  3.1e-10 ***
## CITYBURNSVILLE               -2.02e-02   7.54e-03   -2.67  0.00751 ** 
## CITYCITY OF BAYPORT          -8.87e-02   1.22e-01   -0.73  0.46704    
## CITYCITY OF BIRCHWOOD         2.58e-01   1.26e-01    2.04  0.04133 *  
## CITYCITY OF COTTAGE GROVE     4.68e-02   1.19e-01    0.39  0.69470    
## CITYCITY OF HUGO             -1.24e-02   1.20e-01   -0.10  0.91766    
## CITYCITY OF LAKE ELMO         9.47e-02   1.24e-01    0.76  0.44455    
## CITYCITY OF MAHTOMEDI         2.58e-01   1.20e-01    2.14  0.03208 *  
## CITYCITY OF NEWPORT           2.04e-02   1.21e-01    0.17  0.86633    
## CITYCITY OF OAKDALE           2.39e-01   1.19e-01    2.00  0.04529 *  
## CITYCITY OF OAK PARK HEIGHTS  3.48e-02   1.21e-01    0.29  0.77364    
## CITYCITY OF STILLWATER        8.12e-02   1.19e-01    0.68  0.49661    
## CITYCITY OF ST PAUL PARK      2.38e-02   1.20e-01    0.20  0.84284    
## CITYCITY OF WHITE BEAR LAKE   1.44e-01   1.32e-01    1.09  0.27442    
## CITYCITY OF WILLERNIE        -1.75e-02   1.30e-01   -0.14  0.89235    
## CITYCITY OF WOODBURY          1.67e-01   1.19e-01    1.40  0.16269    
## CITYEAGAN                     1.29e-01   7.41e-03   17.42  < 2e-16 ***
## CITYEMPIRE TOWNSHIP          -6.86e-02   2.44e-02   -2.80  0.00505 ** 
## CITYFALCON HEIGHTS            3.78e-01   2.35e-02   16.09  < 2e-16 ***
## CITYFARMINGTON               -3.84e-02   8.45e-03   -4.54  5.6e-06 ***
## CITYGEM LAKE                  2.48e-02   1.69e-01    0.15  0.88309    
## CITYINVER GROVE HEIGHTS       2.03e-01   1.08e-02   18.78  < 2e-16 ***
## CITYLAKEVILLE                -1.35e-02   7.09e-03   -1.91  0.05663 .  
## CITYLAUDERDALE                3.04e-01   2.97e-02   10.26  < 2e-16 ***
## CITYLITTLE CANADA             1.99e-01   2.27e-02    8.76  < 2e-16 ***
## CITYMAPLEWOOD                 1.70e-01   1.49e-02   11.43  < 2e-16 ***
## CITYMENDOTA                   1.58e-02   1.68e-01    0.09  0.92512    
## CITYMENDOTA HEIGHTS           3.99e-01   1.59e-02   25.08  < 2e-16 ***
## CITYMOUNDS VIEW               2.45e-02   1.73e-02    1.41  0.15747    
## CITYNEW BRIGHTON              8.38e-02   1.54e-02    5.46  4.9e-08 ***
## CITYNORTH ST. PAUL            1.33e-01   1.67e-02    7.98  1.6e-15 ***
## CITYROSEMOUNT                 6.01e-02   7.97e-03    7.54  4.9e-14 ***
## CITYROSEVILLE                 2.32e-01   1.50e-02   15.47  < 2e-16 ***
## CITYSHOREVIEW                 9.57e-02   1.49e-02    6.44  1.2e-10 ***
## CITYSOUTH ST PAUL             2.76e-01   1.13e-02   24.40  < 2e-16 ***
## CITYSPRING LAKE PARK         -7.24e-02   1.69e-01   -0.43  0.66805    
## CITYST. ANTHONY               8.66e-02   5.04e-02    1.72  0.08571 .  
## CITYST. PAUL                  3.89e-01   1.47e-02   26.52  < 2e-16 ***
## CITYTOWN OF BAYTOWN                 NA         NA      NA       NA    
## CITYVADNAIS HEIGHTS           6.89e-02   1.82e-02    3.79  0.00015 ***
## CITYWEST ST PAUL              4.33e-01   1.43e-02   30.27  < 2e-16 ***
## CITYWHITE BEAR LAKE           8.35e-02   1.48e-02    5.63  1.8e-08 ***
## CITYWHITE BEAR TOWNSHIP             NA         NA      NA       NA    
## SALE_YR                      -9.86e-03   1.44e-03   -6.85  7.7e-12 ***
## GARAGEY                       7.94e-02   8.74e-03    9.08  < 2e-16 ***
## ACRES_POLY                    4.18e-01   5.98e-02    6.99  2.9e-12 ***
## ACRES2                       -2.29e-01   8.99e-02   -2.55  0.01072 *  
## HOMESTEADY                    2.60e-02   3.92e-03    6.64  3.2e-11 ***
## logFIN_SQ_                    6.12e-01   4.10e-03  149.45  < 2e-16 ***
## YEAR_BUILT                    1.35e-03   6.32e-05   21.39  < 2e-16 ***
## logMAX                       -1.37e-01   9.21e-03  -14.85  < 2e-16 ***
## logPARK                      -5.22e-03   1.62e-03   -3.23  0.00125 ** 
## MCA3                          4.28e-03   2.24e-04   19.12  < 2e-16 ***
## logSHOP                       2.39e-02   1.99e-03   12.02  < 2e-16 ***
## logCBD                        2.02e-01   6.00e-03   33.77  < 2e-16 ***
## SALE_SEASO2                   1.76e-02   3.35e-03    5.24  1.6e-07 ***
## SALE_SEASO3                   1.97e-02   3.38e-03    5.84  5.2e-09 ***
## SALE_SEASO4                   2.54e-03   3.72e-03    0.68  0.49503    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 0.168 on 21397 degrees of freedom
##   (4538 observations deleted due to missingness)
## Multiple R-squared: 0.746,	Adjusted R-squared: 0.745 
## F-statistic: 1.08e+03 on 58 and 21397 DF,  p-value: <2e-16
```


Allowing the marginal effect of lot size to vary
-------
We have seen that including a quadratic term in lot size is useful. However, the value of additional lot size is likely to vary over space. This is one of the reasons we'll want to run LWR. In the meantime, we can interact lot size with our location variables, like distance to the St. Paul CBD and Minneapolis CBD. And, given that we have seen a quadratic relationship between distance to CBD and house prices (signifying and 'optimal' distance to live at), perhaps we can even interact lot size with distance to CBD and distance to CBD squared.

Let's first rerun a simple semi-log regression

```r
model.logSaleValue3 <- lm(logSALE_VA ~ COUNTY_ID + CITY + factor(SALE_YR) + 
    ACRES_POLY + CBD_dist + HOMESTEAD + FIN_SQ_FT + YEAR_BUILT + logMAX + logPARK + 
    MCA3 + logSHOP + SALE_SEASO, data = workingdata)
summary(model.logSaleValue3)
```

```
## 
## Call:
## lm(formula = logSALE_VA ~ COUNTY_ID + CITY + factor(SALE_YR) + 
##     ACRES_POLY + CBD_dist + HOMESTEAD + FIN_SQ_FT + YEAR_BUILT + 
##     logMAX + logPARK + MCA3 + logSHOP + SALE_SEASO, data = workingdata)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.3532 -0.1052 -0.0109  0.0879  0.9941 
## 
## Coefficients: (2 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   6.23e+00   1.35e-01   46.11  < 2e-16 ***
## COUNTY_ID123                  2.19e-01   1.31e-02   16.65  < 2e-16 ***
## COUNTY_ID163                  2.09e-01   1.22e-01    1.71  0.08691 .  
## CITYARDEN HILLS               1.39e-01   1.87e-02    7.40  1.5e-13 ***
## CITYBURNSVILLE               -9.39e-03   7.51e-03   -1.25  0.21100    
## CITYCITY OF BAYPORT          -9.57e-02   1.24e-01   -0.77  0.44142    
## CITYCITY OF BIRCHWOOD         3.37e-01   1.28e-01    2.64  0.00841 ** 
## CITYCITY OF COTTAGE GROVE     6.49e-02   1.22e-01    0.53  0.59549    
## CITYCITY OF HUGO             -5.00e-04   1.22e-01    0.00  0.99674    
## CITYCITY OF LAKE ELMO         1.01e-01   1.25e-01    0.80  0.42089    
## CITYCITY OF MAHTOMEDI         2.59e-01   1.23e-01    2.10  0.03553 *  
## CITYCITY OF NEWPORT           3.47e-02   1.24e-01    0.28  0.77897    
## CITYCITY OF OAKDALE           2.54e-01   1.23e-01    2.07  0.03825 *  
## CITYCITY OF OAK PARK HEIGHTS  2.25e-02   1.24e-01    0.18  0.85564    
## CITYCITY OF STILLWATER        6.67e-02   1.22e-01    0.55  0.58551    
## CITYCITY OF ST PAUL PARK      4.94e-02   1.23e-01    0.40  0.68810    
## CITYCITY OF WHITE BEAR LAKE   1.32e-01   1.35e-01    0.97  0.33052    
## CITYCITY OF WILLERNIE        -4.62e-02   1.27e-01   -0.36  0.71516    
## CITYCITY OF WOODBURY          1.96e-01   1.22e-01    1.60  0.10877    
## CITYEAGAN                     1.64e-01   8.41e-03   19.48  < 2e-16 ***
## CITYEMPIRE TOWNSHIP          -8.54e-02   2.54e-02   -3.37  0.00076 ***
## CITYFALCON HEIGHTS            3.55e-01   2.20e-02   16.11  < 2e-16 ***
## CITYFARMINGTON               -7.17e-02   9.31e-03   -7.70  1.4e-14 ***
## CITYGEM LAKE                  6.59e-02   1.23e-01    0.54  0.59055    
## CITYINVER GROVE HEIGHTS       2.57e-01   1.21e-02   21.16  < 2e-16 ***
## CITYLAKEVILLE                -7.47e-02   7.94e-03   -9.41  < 2e-16 ***
## CITYLAUDERDALE                2.06e-01   2.69e-02    7.65  2.1e-14 ***
## CITYLITTLE CANADA             1.80e-01   2.05e-02    8.78  < 2e-16 ***
## CITYMAPLEWOOD                 1.48e-01   1.37e-02   10.74  < 2e-16 ***
## CITYMENDOTA                   1.00e-01   1.73e-01    0.58  0.56144    
## CITYMENDOTA HEIGHTS           3.88e-01   1.70e-02   22.80  < 2e-16 ***
## CITYMOUNDS VIEW               1.94e-02   1.60e-02    1.21  0.22523    
## CITYNEW BRIGHTON              8.85e-02   1.43e-02    6.20  5.6e-10 ***
## CITYNORTH ST. PAUL            1.28e-01   1.52e-02    8.38  < 2e-16 ***
## CITYROSEMOUNT                 5.85e-02   8.10e-03    7.22  5.4e-13 ***
## CITYROSEVILLE                 2.10e-01   1.40e-02   14.92  < 2e-16 ***
## CITYSHOREVIEW                 9.70e-02   1.35e-02    7.16  8.2e-13 ***
## CITYSOUTH ST PAUL             2.94e-01   1.30e-02   22.55  < 2e-16 ***
## CITYSPRING LAKE PARK         -1.25e-01   1.73e-01   -0.72  0.46934    
## CITYST. ANTHONY               9.20e-02   4.78e-02    1.93  0.05419 .  
## CITYST. PAUL                  3.06e-01   1.35e-02   22.61  < 2e-16 ***
## CITYTOWN OF BAYTOWN                 NA         NA      NA       NA    
## CITYVADNAIS HEIGHTS           9.27e-02   1.64e-02    5.67  1.5e-08 ***
## CITYWEST ST PAUL              3.87e-01   1.51e-02   25.63  < 2e-16 ***
## CITYWHITE BEAR LAKE           8.53e-02   1.35e-02    6.31  2.8e-10 ***
## CITYWHITE BEAR TOWNSHIP             NA         NA      NA       NA    
## factor(SALE_YR)2006           1.15e-02   2.48e-03    4.64  3.4e-06 ***
## factor(SALE_YR)2007          -1.49e-02   2.73e-03   -5.46  4.7e-08 ***
## ACRES_POLY                    2.36e-01   1.41e-02   16.73  < 2e-16 ***
## CBD_dist                      1.48e-05   5.80e-07   25.60  < 2e-16 ***
## HOMESTEADY                    3.22e-02   3.36e-03    9.60  < 2e-16 ***
## FIN_SQ_FT                     3.44e-04   2.12e-06  162.21  < 2e-16 ***
## YEAR_BUILT                    1.62e-03   5.53e-05   29.32  < 2e-16 ***
## logMAX                       -1.04e-01   8.58e-03  -12.16  < 2e-16 ***
## logPARK                      -3.57e-03   1.49e-03   -2.39  0.01681 *  
## MCA3                          5.99e-03   1.98e-04   30.18  < 2e-16 ***
## logSHOP                       1.54e-02   1.85e-03    8.33  < 2e-16 ***
## SALE_SEASO2                   2.23e-02   3.11e-03    7.19  6.7e-13 ***
## SALE_SEASO3                   2.40e-02   3.14e-03    7.64  2.3e-14 ***
## SALE_SEASO4                   7.32e-03   3.46e-03    2.12  0.03430 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 0.172 on 25936 degrees of freedom
## Multiple R-squared: 0.738,	Adjusted R-squared: 0.737 
## F-statistic: 1.28e+03 on 57 and 25936 DF,  p-value: <2e-16
```


Now let's interact lot size with CBD to see how the price of land might change over space

```r
model.logSaleValue4 <- lm(logSALE_VA ~ COUNTY_ID + CITY + factor(SALE_YR) + 
    ACRES_POLY * CBD_dist + HOMESTEAD + FIN_SQ_FT + YEAR_BUILT + logMAX + logPARK + 
    MCA3 + logSHOP + +SALE_SEASO, data = workingdata)
summary(model.logSaleValue4)
```

```
## 
## Call:
## lm(formula = logSALE_VA ~ COUNTY_ID + CITY + factor(SALE_YR) + 
##     ACRES_POLY * CBD_dist + HOMESTEAD + FIN_SQ_FT + YEAR_BUILT + 
##     logMAX + logPARK + MCA3 + logSHOP + +SALE_SEASO, data = workingdata)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.3520 -0.1051 -0.0111  0.0881  0.9801 
## 
## Coefficients: (2 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   6.26e+00   1.35e-01   46.37  < 2e-16 ***
## COUNTY_ID123                  2.13e-01   1.31e-02   16.23  < 2e-16 ***
## COUNTY_ID163                  2.19e-01   1.22e-01    1.79   0.0730 .  
## CITYARDEN HILLS               1.31e-01   1.87e-02    7.00  2.6e-12 ***
## CITYBURNSVILLE               -7.78e-03   7.50e-03   -1.04   0.2996    
## CITYCITY OF BAYPORT          -1.13e-01   1.24e-01   -0.91   0.3645    
## CITYCITY OF BIRCHWOOD         3.20e-01   1.28e-01    2.50   0.0124 *  
## CITYCITY OF COTTAGE GROVE     4.94e-02   1.22e-01    0.40   0.6862    
## CITYCITY OF HUGO             -1.39e-02   1.22e-01   -0.11   0.9098    
## CITYCITY OF LAKE ELMO         8.66e-02   1.25e-01    0.69   0.4893    
## CITYCITY OF MAHTOMEDI         2.41e-01   1.23e-01    1.96   0.0494 *  
## CITYCITY OF NEWPORT           1.53e-02   1.24e-01    0.12   0.9016    
## CITYCITY OF OAKDALE           2.36e-01   1.22e-01    1.92   0.0543 .  
## CITYCITY OF OAK PARK HEIGHTS  9.70e-03   1.23e-01    0.08   0.9374    
## CITYCITY OF STILLWATER        5.14e-02   1.22e-01    0.42   0.6743    
## CITYCITY OF ST PAUL PARK      3.24e-02   1.23e-01    0.26   0.7918    
## CITYCITY OF WHITE BEAR LAKE   1.16e-01   1.35e-01    0.86   0.3902    
## CITYCITY OF WILLERNIE        -6.23e-02   1.27e-01   -0.49   0.6223    
## CITYCITY OF WOODBURY          1.79e-01   1.22e-01    1.47   0.1424    
## CITYEAGAN                     1.58e-01   8.44e-03   18.69  < 2e-16 ***
## CITYEMPIRE TOWNSHIP          -6.57e-02   2.55e-02   -2.58   0.0099 ** 
## CITYFALCON HEIGHTS            3.55e-01   2.20e-02   16.16  < 2e-16 ***
## CITYFARMINGTON               -7.69e-02   9.33e-03   -8.25  < 2e-16 ***
## CITYGEM LAKE                  6.15e-02   1.22e-01    0.50   0.6152    
## CITYINVER GROVE HEIGHTS       2.48e-01   1.22e-02   20.32  < 2e-16 ***
## CITYLAKEVILLE                -6.57e-02   8.03e-03   -8.18  2.9e-16 ***
## CITYLAUDERDALE                2.13e-01   2.69e-02    7.94  2.1e-15 ***
## CITYLITTLE CANADA             1.72e-01   2.05e-02    8.39  < 2e-16 ***
## CITYMAPLEWOOD                 1.40e-01   1.38e-02   10.19  < 2e-16 ***
## CITYMENDOTA                   9.31e-02   1.73e-01    0.54   0.5896    
## CITYMENDOTA HEIGHTS           3.67e-01   1.73e-02   21.28  < 2e-16 ***
## CITYMOUNDS VIEW               1.70e-02   1.60e-02    1.07   0.2859    
## CITYNEW BRIGHTON              8.50e-02   1.43e-02    5.96  2.5e-09 ***
## CITYNORTH ST. PAUL            1.28e-01   1.52e-02    8.38  < 2e-16 ***
## CITYROSEMOUNT                 5.76e-02   8.09e-03    7.12  1.1e-12 ***
## CITYROSEVILLE                 2.04e-01   1.41e-02   14.48  < 2e-16 ***
## CITYSHOREVIEW                 9.49e-02   1.35e-02    7.01  2.4e-12 ***
## CITYSOUTH ST PAUL             2.93e-01   1.30e-02   22.53  < 2e-16 ***
## CITYSPRING LAKE PARK         -1.29e-01   1.73e-01   -0.75   0.4549    
## CITYST. ANTHONY               8.88e-02   4.78e-02    1.86   0.0630 .  
## CITYST. PAUL                  3.15e-01   1.36e-02   23.19  < 2e-16 ***
## CITYTOWN OF BAYTOWN                 NA         NA      NA       NA    
## CITYVADNAIS HEIGHTS           8.88e-02   1.63e-02    5.43  5.6e-08 ***
## CITYWEST ST PAUL              3.86e-01   1.51e-02   25.54  < 2e-16 ***
## CITYWHITE BEAR LAKE           8.32e-02   1.35e-02    6.17  6.9e-10 ***
## CITYWHITE BEAR TOWNSHIP             NA         NA      NA       NA    
## factor(SALE_YR)2006           1.17e-02   2.48e-03    4.73  2.2e-06 ***
## factor(SALE_YR)2007          -1.53e-02   2.73e-03   -5.59  2.3e-08 ***
## ACRES_POLY                    4.16e-01   2.87e-02   14.50  < 2e-16 ***
## CBD_dist                      1.74e-05   6.78e-07   25.62  < 2e-16 ***
## HOMESTEADY                    3.21e-02   3.36e-03    9.57  < 2e-16 ***
## FIN_SQ_FT                     3.44e-04   2.12e-06  162.50  < 2e-16 ***
## YEAR_BUILT                    1.59e-03   5.55e-05   28.70  < 2e-16 ***
## logMAX                       -1.04e-01   8.57e-03  -12.14  < 2e-16 ***
## logPARK                      -3.53e-03   1.49e-03   -2.37   0.0179 *  
## MCA3                          5.96e-03   1.98e-04   30.06  < 2e-16 ***
## logSHOP                       1.58e-02   1.85e-03    8.54  < 2e-16 ***
## SALE_SEASO2                   2.24e-02   3.10e-03    7.21  5.6e-13 ***
## SALE_SEASO3                   2.40e-02   3.14e-03    7.66  1.9e-14 ***
## SALE_SEASO4                   7.32e-03   3.46e-03    2.12   0.0341 *  
## ACRES_POLY:CBD_dist          -1.09e-05   1.51e-06   -7.19  6.6e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 0.172 on 25935 degrees of freedom
## Multiple R-squared: 0.739,	Adjusted R-squared: 0.738 
## F-statistic: 1.26e+03 on 58 and 25935 DF,  p-value: <2e-16
```


And now let's include acres squared

```r
model.logSaleValue5 <- lm(logSALE_VA ~ COUNTY_ID + CITY + factor(SALE_YR) + 
    ACRES_POLY * CBD_dist + I(ACRES_POLY^2) * CBD_dist + HOMESTEAD + FIN_SQ_FT + 
    YEAR_BUILT + logMAX + logPARK + MCA3 + logSHOP + SALE_SEASO, data = workingdata)
summary(model.logSaleValue5)
```

```
## 
## Call:
## lm(formula = logSALE_VA ~ COUNTY_ID + CITY + factor(SALE_YR) + 
##     ACRES_POLY * CBD_dist + I(ACRES_POLY^2) * CBD_dist + HOMESTEAD + 
##     FIN_SQ_FT + YEAR_BUILT + logMAX + logPARK + MCA3 + logSHOP + 
##     SALE_SEASO, data = workingdata)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.3512 -0.1056 -0.0112  0.0884  0.9724 
## 
## Coefficients: (2 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   6.31e+00   1.35e-01   46.81  < 2e-16 ***
## COUNTY_ID123                  2.10e-01   1.31e-02   15.98  < 2e-16 ***
## COUNTY_ID163                  2.18e-01   1.22e-01    1.79    0.073 .  
## CITYARDEN HILLS               1.32e-01   1.87e-02    7.05  1.9e-12 ***
## CITYBURNSVILLE               -9.36e-03   7.49e-03   -1.25    0.211    
## CITYCITY OF BAYPORT          -1.14e-01   1.24e-01   -0.92    0.359    
## CITYCITY OF BIRCHWOOD         3.13e-01   1.28e-01    2.45    0.014 *  
## CITYCITY OF COTTAGE GROVE     4.58e-02   1.22e-01    0.38    0.707    
## CITYCITY OF HUGO             -1.16e-02   1.22e-01   -0.10    0.924    
## CITYCITY OF LAKE ELMO         8.90e-02   1.25e-01    0.71    0.477    
## CITYCITY OF MAHTOMEDI         2.39e-01   1.23e-01    1.95    0.052 .  
## CITYCITY OF NEWPORT           8.67e-03   1.23e-01    0.07    0.944    
## CITYCITY OF OAKDALE           2.30e-01   1.22e-01    1.88    0.060 .  
## CITYCITY OF OAK PARK HEIGHTS  1.01e-02   1.23e-01    0.08    0.935    
## CITYCITY OF STILLWATER        4.93e-02   1.22e-01    0.40    0.686    
## CITYCITY OF ST PAUL PARK      2.41e-02   1.23e-01    0.20    0.844    
## CITYCITY OF WHITE BEAR LAKE   1.07e-01   1.35e-01    0.80    0.426    
## CITYCITY OF WILLERNIE        -5.89e-02   1.26e-01   -0.47    0.641    
## CITYCITY OF WOODBURY          1.77e-01   1.22e-01    1.45    0.148    
## CITYEAGAN                     1.55e-01   8.45e-03   18.31  < 2e-16 ***
## CITYEMPIRE TOWNSHIP          -6.87e-02   2.54e-02   -2.70    0.007 ** 
## CITYFALCON HEIGHTS            3.54e-01   2.20e-02   16.13  < 2e-16 ***
## CITYFARMINGTON               -7.34e-02   9.33e-03   -7.86  3.8e-15 ***
## CITYGEM LAKE                  5.81e-02   1.22e-01    0.48    0.634    
## CITYINVER GROVE HEIGHTS       2.42e-01   1.22e-02   19.76  < 2e-16 ***
## CITYLAKEVILLE                -6.46e-02   8.04e-03   -8.04  9.7e-16 ***
## CITYLAUDERDALE                2.21e-01   2.68e-02    8.22  < 2e-16 ***
## CITYLITTLE CANADA             1.69e-01   2.04e-02    8.26  < 2e-16 ***
## CITYMAPLEWOOD                 1.36e-01   1.38e-02    9.89  < 2e-16 ***
## CITYMENDOTA                   7.92e-02   1.72e-01    0.46    0.646    
## CITYMENDOTA HEIGHTS           3.70e-01   1.73e-02   21.46  < 2e-16 ***
## CITYMOUNDS VIEW               1.51e-02   1.59e-02    0.95    0.344    
## CITYNEW BRIGHTON              7.95e-02   1.42e-02    5.58  2.4e-08 ***
## CITYNORTH ST. PAUL            1.24e-01   1.52e-02    8.12  5.0e-16 ***
## CITYROSEMOUNT                 5.71e-02   8.08e-03    7.07  1.5e-12 ***
## CITYROSEVILLE                 1.95e-01   1.41e-02   13.84  < 2e-16 ***
## CITYSHOREVIEW                 9.56e-02   1.35e-02    7.08  1.5e-12 ***
## CITYSOUTH ST PAUL             2.96e-01   1.31e-02   22.56  < 2e-16 ***
## CITYSPRING LAKE PARK         -1.28e-01   1.72e-01   -0.74    0.457    
## CITYST. ANTHONY               8.20e-02   4.77e-02    1.72    0.085 .  
## CITYST. PAUL                  3.26e-01   1.36e-02   23.92  < 2e-16 ***
## CITYTOWN OF BAYTOWN                 NA         NA      NA       NA    
## CITYVADNAIS HEIGHTS           8.63e-02   1.63e-02    5.29  1.2e-07 ***
## CITYWEST ST PAUL              3.81e-01   1.52e-02   25.15  < 2e-16 ***
## CITYWHITE BEAR LAKE           7.84e-02   1.35e-02    5.82  6.1e-09 ***
## CITYWHITE BEAR TOWNSHIP             NA         NA      NA       NA    
## factor(SALE_YR)2006           1.15e-02   2.48e-03    4.63  3.7e-06 ***
## factor(SALE_YR)2007          -1.59e-02   2.73e-03   -5.82  6.1e-09 ***
## ACRES_POLY                    1.37e+00   9.79e-02   13.97  < 2e-16 ***
## CBD_dist                      2.15e-05   9.68e-07   22.18  < 2e-16 ***
## I(ACRES_POLY^2)              -1.68e+00   1.64e-01  -10.21  < 2e-16 ***
## HOMESTEADY                    3.08e-02   3.35e-03    9.19  < 2e-16 ***
## FIN_SQ_FT                     3.44e-04   2.12e-06  162.56  < 2e-16 ***
## YEAR_BUILT                    1.52e-03   5.58e-05   27.15  < 2e-16 ***
## logMAX                       -1.03e-01   8.55e-03  -12.10  < 2e-16 ***
## logPARK                      -2.45e-03   1.49e-03   -1.64    0.101    
## MCA3                          5.91e-03   1.98e-04   29.85  < 2e-16 ***
## logSHOP                       1.60e-02   1.85e-03    8.63  < 2e-16 ***
## SALE_SEASO2                   2.19e-02   3.10e-03    7.08  1.5e-12 ***
## SALE_SEASO3                   2.36e-02   3.13e-03    7.54  4.9e-14 ***
## SALE_SEASO4                   7.26e-03   3.45e-03    2.10    0.035 *  
## ACRES_POLY:CBD_dist          -4.75e-05   5.82e-06   -8.16  3.4e-16 ***
## CBD_dist:I(ACRES_POLY^2)      6.62e-05   9.29e-06    7.12  1.1e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 0.172 on 25933 degrees of freedom
## Multiple R-squared: 0.74,	Adjusted R-squared: 0.739 
## F-statistic: 1.23e+03 on 60 and 25933 DF,  p-value: <2e-16
```


AND CBD distance squared

```r
model.logSaleValue6 <- lm(logSALE_VA ~ COUNTY_ID + CITY + factor(SALE_YR) + 
    ACRES_POLY * CBD_dist + I(ACRES_POLY^2) * CBD_dist + ACRES_POLY * I(CBD_dist^2) + 
    I(ACRES_POLY^2) * I(CBD_dist^2) + HOMESTEAD + FIN_SQ_FT + YEAR_BUILT + logMAX + 
    logPARK + MCA3 + logSHOP + SALE_SEASO, data = workingdata)
summary(model.logSaleValue6)
```

```
## 
## Call:
## lm(formula = logSALE_VA ~ COUNTY_ID + CITY + factor(SALE_YR) + 
##     ACRES_POLY * CBD_dist + I(ACRES_POLY^2) * CBD_dist + ACRES_POLY * 
##     I(CBD_dist^2) + I(ACRES_POLY^2) * I(CBD_dist^2) + HOMESTEAD + 
##     FIN_SQ_FT + YEAR_BUILT + logMAX + logPARK + MCA3 + logSHOP + 
##     SALE_SEASO, data = workingdata)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.3697 -0.1027 -0.0100  0.0882  1.0188 
## 
## Coefficients: (2 not defined because of singularities)
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    6.73e+00   1.33e-01   50.53  < 2e-16 ***
## COUNTY_ID123                   1.67e-01   1.30e-02   12.86  < 2e-16 ***
## COUNTY_ID163                   1.99e-01   1.20e-01    1.66  0.09680 .  
## CITYARDEN HILLS                1.49e-01   1.85e-02    8.05  8.8e-16 ***
## CITYBURNSVILLE                -1.22e-02   7.35e-03   -1.67  0.09588 .  
## CITYCITY OF BAYPORT           -1.03e-01   1.22e-01   -0.84  0.39840    
## CITYCITY OF BIRCHWOOD          2.75e-01   1.25e-01    2.20  0.02816 *  
## CITYCITY OF COTTAGE GROVE      1.94e-02   1.20e-01    0.16  0.87114    
## CITYCITY OF HUGO               8.86e-03   1.20e-01    0.07  0.94107    
## CITYCITY OF LAKE ELMO          7.16e-02   1.23e-01    0.58  0.55967    
## CITYCITY OF MAHTOMEDI          2.04e-01   1.20e-01    1.70  0.08954 .  
## CITYCITY OF NEWPORT           -1.88e-02   1.21e-01   -0.15  0.87686    
## CITYCITY OF OAKDALE            2.15e-01   1.20e-01    1.79  0.07384 .  
## CITYCITY OF OAK PARK HEIGHTS   1.21e-02   1.21e-01    0.10  0.92029    
## CITYCITY OF STILLWATER         4.83e-02   1.20e-01    0.40  0.68669    
## CITYCITY OF ST PAUL PARK      -1.61e-02   1.20e-01   -0.13  0.89346    
## CITYCITY OF WHITE BEAR LAKE    8.40e-02   1.32e-01    0.63  0.52572    
## CITYCITY OF WILLERNIE         -1.40e-01   1.24e-01   -1.13  0.25804    
## CITYCITY OF WOODBURY           1.55e-01   1.20e-01    1.29  0.19545    
## CITYEAGAN                      1.16e-01   8.40e-03   13.84  < 2e-16 ***
## CITYEMPIRE TOWNSHIP            2.95e-02   2.55e-02    1.16  0.24690    
## CITYFALCON HEIGHTS             3.99e-01   2.20e-02   18.10  < 2e-16 ***
## CITYFARMINGTON                 5.44e-02   1.02e-02    5.36  8.3e-08 ***
## CITYGEM LAKE                   6.28e-02   1.20e-01    0.52  0.60039    
## CITYINVER GROVE HEIGHTS        2.02e-01   1.22e-02   16.53  < 2e-16 ***
## CITYLAKEVILLE                  3.20e-02   8.86e-03    3.61  0.00031 ***
## CITYLAUDERDALE                 2.83e-01   2.68e-02   10.55  < 2e-16 ***
## CITYLITTLE CANADA              2.04e-01   2.04e-02   10.02  < 2e-16 ***
## CITYMAPLEWOOD                  1.78e-01   1.42e-02   12.58  < 2e-16 ***
## CITYMENDOTA                    6.95e-02   1.69e-01    0.41  0.68108    
## CITYMENDOTA HEIGHTS            3.91e-01   1.78e-02   21.96  < 2e-16 ***
## CITYMOUNDS VIEW                1.20e-02   1.56e-02    0.77  0.44232    
## CITYNEW BRIGHTON               9.65e-02   1.42e-02    6.79  1.1e-11 ***
## CITYNORTH ST. PAUL             1.23e-01   1.53e-02    8.03  9.9e-16 ***
## CITYROSEMOUNT                  4.61e-02   7.94e-03    5.81  6.3e-09 ***
## CITYROSEVILLE                  2.34e-01   1.44e-02   16.29  < 2e-16 ***
## CITYSHOREVIEW                  1.04e-01   1.33e-02    7.82  5.6e-15 ***
## CITYSOUTH ST PAUL              2.74e-01   1.37e-02   20.09  < 2e-16 ***
## CITYSPRING LAKE PARK          -1.25e-01   1.69e-01   -0.74  0.45975    
## CITYST. ANTHONY                1.25e-01   4.69e-02    2.65  0.00799 ** 
## CITYST. PAUL                   3.90e-01   1.42e-02   27.42  < 2e-16 ***
## CITYTOWN OF BAYTOWN                  NA         NA      NA       NA    
## CITYVADNAIS HEIGHTS            9.76e-02   1.61e-02    6.07  1.3e-09 ***
## CITYWEST ST PAUL               4.29e-01   1.59e-02   26.88  < 2e-16 ***
## CITYWHITE BEAR LAKE            6.76e-02   1.33e-02    5.09  3.7e-07 ***
## CITYWHITE BEAR TOWNSHIP              NA         NA      NA       NA    
## factor(SALE_YR)2006            1.05e-02   2.43e-03    4.34  1.4e-05 ***
## factor(SALE_YR)2007           -1.74e-02   2.68e-03   -6.49  8.5e-11 ***
## ACRES_POLY                     2.46e+00   1.62e-01   15.18  < 2e-16 ***
## CBD_dist                       8.31e-05   2.93e-06   28.33  < 2e-16 ***
## I(ACRES_POLY^2)               -3.23e+00   2.97e-01  -10.84  < 2e-16 ***
## I(CBD_dist^2)                 -1.93e-09   8.72e-11  -22.12  < 2e-16 ***
## HOMESTEADY                     2.70e-02   3.29e-03    8.19  2.8e-16 ***
## FIN_SQ_FT                      3.45e-04   2.08e-06  166.09  < 2e-16 ***
## YEAR_BUILT                     1.30e-03   5.52e-05   23.46  < 2e-16 ***
## logMAX                        -1.04e-01   8.39e-03  -12.36  < 2e-16 ***
## logPARK                       -8.15e-04   1.47e-03   -0.56  0.57869    
## MCA3                           5.06e-03   1.96e-04   25.78  < 2e-16 ***
## logSHOP                        1.78e-02   1.82e-03    9.79  < 2e-16 ***
## SALE_SEASO2                    2.10e-02   3.04e-03    6.90  5.2e-12 ***
## SALE_SEASO3                    2.22e-02   3.07e-03    7.21  5.5e-13 ***
## SALE_SEASO4                    5.98e-03   3.38e-03    1.77  0.07714 .  
## ACRES_POLY:CBD_dist           -2.93e-04   2.22e-05  -13.16  < 2e-16 ***
## CBD_dist:I(ACRES_POLY^2)       4.06e-04   3.82e-05   10.64  < 2e-16 ***
## ACRES_POLY:I(CBD_dist^2)       7.81e-09   6.24e-10   12.51  < 2e-16 ***
## I(ACRES_POLY^2):I(CBD_dist^2) -1.08e-08   1.04e-09  -10.36  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 0.169 on 25930 degrees of freedom
## Multiple R-squared: 0.749,	Adjusted R-squared: 0.749 
## F-statistic: 1.23e+03 on 63 and 25930 DF,  p-value: <2e-16
```



