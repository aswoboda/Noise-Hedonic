Max's Analysis -- 2005-2007 SalesData 1/30/13
========================================================

This is a test run of analysis through RMarkdown using 2005-2007 sales dataset.

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

(3) Included more fixed effect variables related to the timing of the sale (either by month or season)


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


The first regression will be a baseline without any log transformations.


```r
model.SaleValue1 <- lm(SALE_VALUE ~ COUNTY_ID + CITY + SALE_YR + SDNUM + ACRES_POLY + 
    HOMESTEAD + FIN_SQ_FT + YEAR_BUILT + MAX + PARK_dist + LAKE_dist + MCA3 + 
    MCA5 + SHOP_dist + CBD_dist + SALE_SEASO + SALE_MO, data = workingdata)
summary(model.SaleValue1)
```

```
## 
## Call:
## lm(formula = SALE_VALUE ~ COUNTY_ID + CITY + SALE_YR + SDNUM + 
##     ACRES_POLY + HOMESTEAD + FIN_SQ_FT + YEAR_BUILT + MAX + PARK_dist + 
##     LAKE_dist + MCA3 + MCA5 + SHOP_dist + CBD_dist + SALE_SEASO + 
##     SALE_MO, data = workingdata)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -374802  -31709   -5293   22372  372497 
## 
## Coefficients: (5 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   1.42e+06   8.53e+05    1.66  0.09622 .  
## COUNTY_ID123                  5.35e+04   9.39e+03    5.69  1.3e-08 ***
## COUNTY_ID163                  4.86e+04   4.04e+04    1.20  0.22864    
## CITYARDEN HILLS               3.74e+04   5.89e+03    6.35  2.2e-10 ***
## CITYBURNSVILLE               -7.67e+03   2.36e+03   -3.25  0.00115 ** 
## CITYCITY OF BAYPORT          -2.21e+04   3.91e+04   -0.57  0.57066    
## CITYCITY OF BIRCHWOOD         1.06e+05   4.05e+04    2.62  0.00890 ** 
## CITYCITY OF COTTAGE GROVE     1.66e+04   3.84e+04    0.43  0.66483    
## CITYCITY OF HUGO             -1.93e+04   3.89e+04   -0.50  0.61917    
## CITYCITY OF LAKE ELMO         3.54e+04   3.94e+04    0.90  0.36908    
## CITYCITY OF MAHTOMEDI         8.89e+04   3.87e+04    2.30  0.02166 *  
## CITYCITY OF NEWPORT           2.36e+04   3.89e+04    0.61  0.54396    
## CITYCITY OF OAKDALE           7.71e+04   3.87e+04    1.99  0.04655 *  
## CITYCITY OF OAK PARK HEIGHTS  5.28e+03   3.88e+04    0.14  0.89188    
## CITYCITY OF STILLWATER        2.43e+04   3.85e+04    0.63  0.52756    
## CITYCITY OF ST PAUL PARK      2.01e+04   3.86e+04    0.52  0.60262    
## CITYCITY OF WHITE BEAR LAKE   3.84e+04   4.25e+04    0.90  0.36605    
## CITYCITY OF WILLERNIE         1.68e+04   3.99e+04    0.42  0.67426    
## CITYCITY OF WOODBURY          6.46e+04   3.85e+04    1.68  0.09328 .  
## CITYEAGAN                     5.11e+04   2.68e+03   19.09  < 2e-16 ***
## CITYEMPIRE TOWNSHIP          -2.94e+04   8.00e+03   -3.67  0.00024 ***
## CITYFALCON HEIGHTS            9.22e+04   6.93e+03   13.31  < 2e-16 ***
## CITYFARMINGTON               -2.15e+04   3.08e+03   -6.97  3.2e-12 ***
## CITYGEM LAKE                  3.54e+04   3.85e+04    0.92  0.35854    
## CITYINVER GROVE HEIGHTS       7.89e+04   3.89e+03   20.29  < 2e-16 ***
## CITYLAKEVILLE                -1.84e+04   2.60e+03   -7.07  1.5e-12 ***
## CITYLAUDERDALE                7.32e+04   8.48e+03    8.62  < 2e-16 ***
## CITYLITTLE CANADA             5.05e+04   6.44e+03    7.84  4.7e-15 ***
## CITYMAPLEWOOD                 4.50e+04   4.33e+03   10.37  < 2e-16 ***
## CITYMENDOTA                   2.74e+04   5.44e+04    0.50  0.61378    
## CITYMENDOTA HEIGHTS           1.11e+05   5.60e+03   19.89  < 2e-16 ***
## CITYMOUNDS VIEW               9.01e+03   5.04e+03    1.79  0.07391 .  
## CITYNEW BRIGHTON              2.50e+04   4.49e+03    5.58  2.5e-08 ***
## CITYNORTH ST. PAUL            4.27e+04   4.85e+03    8.79  < 2e-16 ***
## CITYROSEMOUNT                 2.21e+04   2.55e+03    8.66  < 2e-16 ***
## CITYROSEVILLE                 5.67e+04   4.42e+03   12.84  < 2e-16 ***
## CITYSHOREVIEW                 3.10e+04   4.26e+03    7.28  3.5e-13 ***
## CITYSOUTH ST PAUL             1.01e+05   5.40e+03   18.67  < 2e-16 ***
## CITYSPRING LAKE PARK         -1.42e+03   5.43e+04   -0.03  0.97914    
## CITYST. ANTHONY               3.44e+04   1.64e+04    2.10  0.03589 *  
## CITYST. PAUL                  9.56e+04   4.28e+03   22.33  < 2e-16 ***
## CITYTOWN OF BAYTOWN                 NA         NA      NA       NA    
## CITYVADNAIS HEIGHTS           3.05e+04   5.15e+03    5.94  3.0e-09 ***
## CITYWEST ST PAUL              1.09e+05   4.94e+03   22.05  < 2e-16 ***
## CITYWHITE BEAR LAKE           2.91e+04   4.28e+03    6.80  1.1e-11 ***
## CITYWHITE BEAR TOWNSHIP             NA         NA      NA       NA    
## SALE_YR                      -1.33e+03   4.25e+02   -3.14  0.00168 ** 
## SDNUM                         2.89e+01   1.95e+01    1.48  0.13806    
## ACRES_POLY                    7.80e+04   4.45e+03   17.52  < 2e-16 ***
## HOMESTEADY                    5.33e+03   1.06e+03    5.05  4.5e-07 ***
## FIN_SQ_FT                     1.10e+02   6.68e-01  164.99  < 2e-16 ***
## YEAR_BUILT                    3.43e+02   1.75e+01   19.60  < 2e-16 ***
## MAX                          -5.78e+02   4.50e+01  -12.85  < 2e-16 ***
## PARK_dist                    -1.04e+00   3.39e-01   -3.06  0.00219 ** 
## LAKE_dist                     5.51e+00   6.44e-01    8.55  < 2e-16 ***
## MCA3                          1.35e+03   6.29e+01   21.44  < 2e-16 ***
## MCA5                          4.47e+01   1.15e+01    3.90  9.5e-05 ***
## SHOP_dist                     2.81e+00   3.68e-01    7.65  2.1e-14 ***
## CBD_dist                      4.24e+00   1.89e-01   22.46  < 2e-16 ***
## SALE_SEASO2                   8.37e+03   1.76e+03    4.77  1.9e-06 ***
## SALE_SEASO3                   5.06e+03   1.87e+03    2.71  0.00677 ** 
## SALE_SEASO4                   2.28e+03   2.06e+03    1.11  0.26708    
## SALE_MO10                     2.73e+03   1.89e+03    1.44  0.14891    
## SALE_MO11                     1.80e+03   1.96e+03    0.92  0.35721    
## SALE_MO12                           NA         NA      NA       NA    
## SALE_MO2                      1.60e+03   2.04e+03    0.78  0.43259    
## SALE_MO3                      1.96e+03   1.88e+03    1.04  0.29858    
## SALE_MO4                     -3.58e+03   1.49e+03   -2.41  0.01614 *  
## SALE_MO5                     -2.90e+03   1.41e+03   -2.05  0.04028 *  
## SALE_MO6                            NA         NA      NA       NA    
## SALE_MO7                      3.77e+03   1.55e+03    2.43  0.01528 *  
## SALE_MO8                      2.70e+03   1.54e+03    1.76  0.07872 .  
## SALE_MO9                            NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 54200 on 25926 degrees of freedom
## Multiple R-squared: 0.715,	Adjusted R-squared: 0.714 
## F-statistic:  969 on 67 and 25926 DF,  p-value: <2e-16
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
    logSHOP + logCBD + SALE_MO + SALE_SEASO, data = workingdata)
summary(model.logSaleValue2)
```

```
## 
## Call:
## lm(formula = logSALE_VA ~ COUNTY_ID + CITY + SALE_YR + ACRES_POLY + 
##     ACRES2 + HOMESTEAD + logFIN_SQ_ + YEAR_BUILT + logMAX + logPARK + 
##     MCA3 + logSHOP + logCBD + SALE_MO + SALE_SEASO, data = workingdata)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.2652 -0.1076 -0.0108  0.0960  1.0285 
## 
## Coefficients: (5 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   1.72e+01   2.68e+00    6.40  1.6e-10 ***
## COUNTY_ID123                  1.70e-01   1.24e-02   13.64  < 2e-16 ***
## COUNTY_ID163                  1.92e-01   1.21e-01    1.59   0.1122    
## CITYARDEN HILLS               1.48e-01   1.83e-02    8.05  9.0e-16 ***
## CITYBURNSVILLE               -1.92e-02   7.42e-03   -2.58   0.0098 ** 
## CITYCITY OF BAYPORT          -9.56e-02   1.23e-01   -0.78   0.4366    
## CITYCITY OF BIRCHWOOD         2.84e-01   1.26e-01    2.25   0.0247 *  
## CITYCITY OF COTTAGE GROVE     5.42e-02   1.21e-01    0.45   0.6539    
## CITYCITY OF HUGO             -5.21e-03   1.21e-01   -0.04   0.9657    
## CITYCITY OF LAKE ELMO         6.83e-02   1.24e-01    0.55   0.5817    
## CITYCITY OF MAHTOMEDI         2.38e-01   1.22e-01    1.96   0.0502 .  
## CITYCITY OF NEWPORT           2.26e-02   1.22e-01    0.19   0.8530    
## CITYCITY OF OAKDALE           2.44e-01   1.21e-01    2.02   0.0435 *  
## CITYCITY OF OAK PARK HEIGHTS  2.47e-02   1.22e-01    0.20   0.8397    
## CITYCITY OF STILLWATER        6.35e-02   1.21e-01    0.52   0.5998    
## CITYCITY OF ST PAUL PARK      3.01e-02   1.21e-01    0.25   0.8041    
## CITYCITY OF WHITE BEAR LAKE   1.45e-01   1.34e-01    1.08   0.2781    
## CITYCITY OF WILLERNIE        -2.01e-02   1.25e-01   -0.16   0.8724    
## CITYCITY OF WOODBURY          1.77e-01   1.21e-01    1.46   0.1431    
## CITYEAGAN                     1.34e-01   7.33e-03   18.23  < 2e-16 ***
## CITYEMPIRE TOWNSHIP          -6.85e-02   2.47e-02   -2.78   0.0055 ** 
## CITYFALCON HEIGHTS            3.94e-01   2.15e-02   18.35  < 2e-16 ***
## CITYFARMINGTON               -4.05e-02   8.28e-03   -4.89  1.0e-06 ***
## CITYGEM LAKE                  8.77e-02   1.21e-01    0.72   0.4689    
## CITYINVER GROVE HEIGHTS       2.11e-01   1.05e-02   20.08  < 2e-16 ***
## CITYLAKEVILLE                -1.64e-02   7.01e-03   -2.34   0.0191 *  
## CITYLAUDERDALE                3.15e-01   2.63e-02   11.99  < 2e-16 ***
## CITYLITTLE CANADA             1.93e-01   1.99e-02    9.70  < 2e-16 ***
## CITYMAPLEWOOD                 1.90e-01   1.31e-02   14.48  < 2e-16 ***
## CITYMENDOTA                   2.79e-02   1.71e-01    0.16   0.8700    
## CITYMENDOTA HEIGHTS           4.25e-01   1.51e-02   28.18  < 2e-16 ***
## CITYMOUNDS VIEW               2.62e-02   1.58e-02    1.66   0.0961 .  
## CITYNEW BRIGHTON              1.03e-01   1.38e-02    7.47  8.4e-14 ***
## CITYNORTH ST. PAUL            1.51e-01   1.47e-02   10.23  < 2e-16 ***
## CITYROSEMOUNT                 6.29e-02   7.97e-03    7.89  3.2e-15 ***
## CITYROSEVILLE                 2.45e-01   1.34e-02   18.19  < 2e-16 ***
## CITYSHOREVIEW                 1.04e-01   1.34e-02    7.77  8.4e-15 ***
## CITYSOUTH ST PAUL             2.83e-01   1.04e-02   27.18  < 2e-16 ***
## CITYSPRING LAKE PARK         -7.68e-02   1.71e-01   -0.45   0.6530    
## CITYST. ANTHONY               1.16e-01   4.71e-02    2.47   0.0135 *  
## CITYST. PAUL                  4.05e-01   1.30e-02   31.11  < 2e-16 ***
## CITYTOWN OF BAYTOWN                 NA         NA      NA       NA    
## CITYVADNAIS HEIGHTS           8.83e-02   1.60e-02    5.50  3.8e-08 ***
## CITYWEST ST PAUL              4.52e-01   1.29e-02   34.99  < 2e-16 ***
## CITYWHITE BEAR LAKE           9.25e-02   1.33e-02    6.96  3.4e-12 ***
## CITYWHITE BEAR TOWNSHIP             NA         NA      NA       NA    
## SALE_YR                      -7.73e-03   1.34e-03   -5.78  7.4e-09 ***
## ACRES_POLY                    4.33e-01   5.42e-02    7.99  1.4e-15 ***
## ACRES2                       -2.63e-01   8.25e-02   -3.19   0.0014 ** 
## HOMESTEADY                    2.41e-02   3.32e-03    7.25  4.4e-13 ***
## logFIN_SQ_                    6.04e-01   3.70e-03  163.03  < 2e-16 ***
## YEAR_BUILT                    1.28e-03   5.57e-05   22.93  < 2e-16 ***
## logMAX                       -1.20e-01   8.47e-03  -14.13  < 2e-16 ***
## logPARK                      -4.24e-03   1.48e-03   -2.87   0.0041 ** 
## MCA3                          4.80e-03   2.00e-04   24.01  < 2e-16 ***
## logSHOP                       2.42e-02   1.83e-03   13.19  < 2e-16 ***
## logCBD                        2.15e-01   5.14e-03   41.76  < 2e-16 ***
## SALE_MO10                     1.77e-02   6.04e-03    2.93   0.0034 ** 
## SALE_MO11                     8.42e-03   6.23e-03    1.35   0.1768    
## SALE_MO12                     1.06e-05   6.47e-03    0.00   0.9987    
## SALE_MO2                      3.51e-03   6.43e-03    0.55   0.5848    
## SALE_MO3                      9.97e-03   5.92e-03    1.68   0.0921 .  
## SALE_MO4                      1.60e-02   5.85e-03    2.73   0.0063 ** 
## SALE_MO5                      2.32e-02   5.66e-03    4.10  4.2e-05 ***
## SALE_MO6                      3.14e-02   5.52e-03    5.68  1.3e-08 ***
## SALE_MO7                      2.97e-02   5.67e-03    5.25  1.6e-07 ***
## SALE_MO8                      2.71e-02   5.62e-03    4.82  1.4e-06 ***
## SALE_MO9                      1.77e-02   5.88e-03    3.02   0.0026 ** 
## SALE_SEASO2                         NA         NA      NA       NA    
## SALE_SEASO3                         NA         NA      NA       NA    
## SALE_SEASO4                         NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 0.17 on 25928 degrees of freedom
## Multiple R-squared: 0.744,	Adjusted R-squared: 0.744 
## F-statistic: 1.16e+03 on 65 and 25928 DF,  p-value: <2e-16
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
    MCA3 + logSHOP + logCBD + SALE_MO + SALE_SEASO, data = workingdata)
summary(model.logSaleValue3)
```

```
## 
## Call:
## lm(formula = logSALE_VA ~ COUNTY_ID + CITY + SALE_YR + GARAGE + 
##     ACRES_POLY + ACRES2 + HOMESTEAD + logFIN_SQ_ + YEAR_BUILT + 
##     logMAX + logPARK + MCA3 + logSHOP + logCBD + SALE_MO + SALE_SEASO, 
##     data = workingdata)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.2459 -0.1068 -0.0124  0.0953  0.9976 
## 
## Coefficients: (5 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   2.15e+01   2.89e+00    7.45  9.8e-14 ***
## COUNTY_ID123                  1.75e-01   1.36e-02   12.83  < 2e-16 ***
## COUNTY_ID163                  1.86e-01   1.19e-01    1.56  0.11880    
## CITYARDEN HILLS               1.24e-01   1.98e-02    6.26  3.8e-10 ***
## CITYBURNSVILLE               -2.02e-02   7.54e-03   -2.67  0.00750 ** 
## CITYCITY OF BAYPORT          -8.55e-02   1.22e-01   -0.70  0.48295    
## CITYCITY OF BIRCHWOOD         2.60e-01   1.26e-01    2.06  0.03943 *  
## CITYCITY OF COTTAGE GROVE     4.89e-02   1.19e-01    0.41  0.68191    
## CITYCITY OF HUGO             -1.02e-02   1.20e-01   -0.09  0.93222    
## CITYCITY OF LAKE ELMO         9.54e-02   1.24e-01    0.77  0.44089    
## CITYCITY OF MAHTOMEDI         2.60e-01   1.20e-01    2.16  0.03067 *  
## CITYCITY OF NEWPORT           2.21e-02   1.21e-01    0.18  0.85529    
## CITYCITY OF OAKDALE           2.41e-01   1.19e-01    2.02  0.04343 *  
## CITYCITY OF OAK PARK HEIGHTS  3.69e-02   1.21e-01    0.30  0.76061    
## CITYCITY OF STILLWATER        8.30e-02   1.19e-01    0.69  0.48719    
## CITYCITY OF ST PAUL PARK      2.58e-02   1.20e-01    0.21  0.82991    
## CITYCITY OF WHITE BEAR LAKE   1.47e-01   1.32e-01    1.11  0.26610    
## CITYCITY OF WILLERNIE        -1.54e-02   1.30e-01   -0.12  0.90555    
## CITYCITY OF WOODBURY          1.69e-01   1.19e-01    1.41  0.15780    
## CITYEAGAN                     1.29e-01   7.41e-03   17.41  < 2e-16 ***
## CITYEMPIRE TOWNSHIP          -6.91e-02   2.44e-02   -2.83  0.00467 ** 
## CITYFALCON HEIGHTS            3.77e-01   2.35e-02   16.04  < 2e-16 ***
## CITYFARMINGTON               -3.85e-02   8.45e-03   -4.56  5.3e-06 ***
## CITYGEM LAKE                  1.57e-02   1.69e-01    0.09  0.92595    
## CITYINVER GROVE HEIGHTS       2.03e-01   1.08e-02   18.75  < 2e-16 ***
## CITYLAKEVILLE                -1.34e-02   7.09e-03   -1.89  0.05917 .  
## CITYLAUDERDALE                3.03e-01   2.97e-02   10.22  < 2e-16 ***
## CITYLITTLE CANADA             1.98e-01   2.27e-02    8.72  < 2e-16 ***
## CITYMAPLEWOOD                 1.70e-01   1.49e-02   11.41  < 2e-16 ***
## CITYMENDOTA                   1.41e-02   1.68e-01    0.08  0.93341    
## CITYMENDOTA HEIGHTS           3.99e-01   1.59e-02   25.05  < 2e-16 ***
## CITYMOUNDS VIEW               2.36e-02   1.73e-02    1.36  0.17239    
## CITYNEW BRIGHTON              8.39e-02   1.54e-02    5.46  4.7e-08 ***
## CITYNORTH ST. PAUL            1.33e-01   1.67e-02    7.94  2.1e-15 ***
## CITYROSEMOUNT                 6.01e-02   7.97e-03    7.54  5.0e-14 ***
## CITYROSEVILLE                 2.31e-01   1.50e-02   15.43  < 2e-16 ***
## CITYSHOREVIEW                 9.54e-02   1.49e-02    6.42  1.4e-10 ***
## CITYSOUTH ST PAUL             2.76e-01   1.13e-02   24.45  < 2e-16 ***
## CITYSPRING LAKE PARK         -7.76e-02   1.69e-01   -0.46  0.64557    
## CITYST. ANTHONY               8.78e-02   5.04e-02    1.74  0.08166 .  
## CITYST. PAUL                  3.89e-01   1.47e-02   26.50  < 2e-16 ***
## CITYTOWN OF BAYTOWN                 NA         NA      NA       NA    
## CITYVADNAIS HEIGHTS           6.87e-02   1.82e-02    3.78  0.00016 ***
## CITYWEST ST PAUL              4.33e-01   1.43e-02   30.26  < 2e-16 ***
## CITYWHITE BEAR LAKE           8.29e-02   1.48e-02    5.59  2.3e-08 ***
## CITYWHITE BEAR TOWNSHIP             NA         NA      NA       NA    
## SALE_YR                      -9.87e-03   1.44e-03   -6.85  7.5e-12 ***
## GARAGEY                       7.94e-02   8.74e-03    9.08  < 2e-16 ***
## ACRES_POLY                    4.14e-01   5.98e-02    6.92  4.5e-12 ***
## ACRES2                       -2.24e-01   8.99e-02   -2.49  0.01290 *  
## HOMESTEADY                    2.55e-02   3.92e-03    6.51  7.8e-11 ***
## logFIN_SQ_                    6.12e-01   4.09e-03  149.48  < 2e-16 ***
## YEAR_BUILT                    1.36e-03   6.32e-05   21.46  < 2e-16 ***
## logMAX                       -1.36e-01   9.20e-03  -14.82  < 2e-16 ***
## logPARK                      -5.23e-03   1.62e-03   -3.23  0.00122 ** 
## MCA3                          4.27e-03   2.24e-04   19.09  < 2e-16 ***
## logSHOP                       2.38e-02   1.99e-03   11.99  < 2e-16 ***
## logCBD                        2.02e-01   5.99e-03   33.77  < 2e-16 ***
## SALE_MO10                     1.91e-02   6.63e-03    2.89  0.00391 ** 
## SALE_MO11                     7.39e-03   6.79e-03    1.09  0.27634    
## SALE_MO12                     2.05e-03   7.07e-03    0.29  0.77191    
## SALE_MO2                      7.61e-03   7.05e-03    1.08  0.28038    
## SALE_MO3                      1.28e-02   6.49e-03    1.97  0.04940 *  
## SALE_MO4                      1.73e-02   6.41e-03    2.70  0.00694 ** 
## SALE_MO5                      2.39e-02   6.22e-03    3.84  0.00012 ***
## SALE_MO6                      3.22e-02   6.05e-03    5.33  1.0e-07 ***
## SALE_MO7                      2.92e-02   6.20e-03    4.71  2.4e-06 ***
## SALE_MO8                      2.96e-02   6.17e-03    4.80  1.6e-06 ***
## SALE_MO9                      2.29e-02   6.42e-03    3.56  0.00037 ***
## SALE_SEASO2                         NA         NA      NA       NA    
## SALE_SEASO3                         NA         NA      NA       NA    
## SALE_SEASO4                         NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 0.168 on 21389 degrees of freedom
##   (4538 observations deleted due to missingness)
## Multiple R-squared: 0.746,	Adjusted R-squared: 0.745 
## F-statistic:  952 on 66 and 21389 DF,  p-value: <2e-16
```


