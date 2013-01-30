Max's Analysis -- 2005-2007 SalesData 1/30/13
========================================================

This is a test run of analysis through RMarkdown using 2005-2007 Sales dataset.

Preparation for regression analysis...

```r
getwd()
```

```
## [1] "/home/timmm/Noise Hedonic Project/Noise-Hedonic/analysis/SandboxMax"
```

```r

library(foreign)
workingdata <- read.dbf("../../../Data//R2GIS/CleanData/Sales20052007.dbf")
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


