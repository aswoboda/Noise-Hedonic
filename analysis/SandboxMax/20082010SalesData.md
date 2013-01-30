Max's Analysis -- 2008-2010 SalesData 1/30/13
========================================================

This is a test run of analysis through RMarkdown using 2008-2010 Sales dataset.

Preparation for regression analysis...

```r
getwd()
```

```
## [1] "/home/timmm/Noise Hedonic Project/Noise-Hedonic/analysis/SandboxMax"
```

```r

library(foreign)
workingdata <- read.dbf("../../../Data//R2GIS/CleanData/Sales20082010.dbf")
require(zoo)
require(lmtest)
require(car)
```


In order to improve the model, I did three things:

(1) After normality issues arose in preliminary regressions, used more restrictive constraints on outliers to make distributions to key variables more normal.
Limited parcels to under 4000 square feet in living area, less than .6 acres, experiencing more than 25 dBA in traffic noise and sales values between $100k and $675k
Sample size went from ~19k to 15,587

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
## -368730  -34557   -4039   27011  437368 
## 
## Coefficients: (1 not defined because of singularities)
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)              2.44e+07   1.85e+06   13.21  < 2e-16 ***
## COUNTY_ID123             7.56e+04   2.62e+04    2.89  0.00391 ** 
## COUNTY_ID163             1.05e+05   1.77e+04    5.90  3.6e-09 ***
## CITYARDEN HILLS          3.33e+04   2.40e+04    1.39  0.16416    
## CITYBAYPORT             -6.01e+04   8.50e+03   -7.07  1.6e-12 ***
## CITYBIRCHWOOD VILLAGE   -6.02e+03   1.39e+04   -0.43  0.66549    
## CITYBURNSVILLE          -1.44e+04   3.52e+03   -4.09  4.3e-05 ***
## CITYCOTTAGE GROVE       -4.97e+04   2.83e+03  -17.57  < 2e-16 ***
## CITYDELLWOOD            -8.16e+04   6.06e+04   -1.35  0.17826    
## CITYEAGAN                5.73e+04   4.05e+03   14.16  < 2e-16 ***
## CITYEMPIRE TOWNSHIP     -6.32e+04   1.26e+04   -5.02  5.3e-07 ***
## CITYFALCON HEIGHTS       9.82e+04   2.43e+04    4.04  5.3e-05 ***
## CITYFARMINGTON          -5.41e+04   4.54e+03  -11.90  < 2e-16 ***
## CITYGEM LAKE            -6.36e+04   6.48e+04   -0.98  0.32648    
## CITYHUGO                -7.31e+04   8.40e+03   -8.71  < 2e-16 ***
## CITYINVER GROVE HEIGHTS  6.31e+04   5.97e+03   10.58  < 2e-16 ***
## CITYLAKE ELMO           -3.30e+03   1.05e+04   -0.31  0.75412    
## CITYLAKEVILLE           -2.22e+04   3.80e+03   -5.85  5.0e-09 ***
## CITYLAUDERDALE           5.29e+04   2.60e+04    2.04  0.04169 *  
## CITYLITTLE CANADA        2.36e+04   2.43e+04    0.97  0.33119    
## CITYMAHTOMEDI            2.55e+04   5.67e+03    4.50  6.8e-06 ***
## CITYMAPLEWOOD            1.39e+04   2.34e+04    0.60  0.55106    
## CITYMENDOTA              1.13e+05   3.53e+04    3.19  0.00142 ** 
## CITYMENDOTA HEIGHTS      1.19e+05   7.64e+03   15.61  < 2e-16 ***
## CITYMOUNDS VIEW         -3.00e+04   2.37e+04   -1.27  0.20539    
## CITYNEW BRIGHTON         1.41e+04   2.35e+04    0.60  0.54752    
## CITYNEWPORT             -4.82e+04   7.80e+03   -6.17  6.9e-10 ***
## CITYNORTH ST. PAUL       2.45e+03   2.36e+04    0.10  0.91723    
## CITYOAKDALE              4.84e+03   6.27e+03    0.77  0.44003    
## CITYOAK PARK HEIGHTS    -4.22e+04   8.15e+03   -5.18  2.3e-07 ***
## CITYROSEMOUNT            7.98e+03   4.11e+03    1.94  0.05231 .  
## CITYROSEVILLE            4.07e+04   2.34e+04    1.74  0.08184 .  
## CITYSHOREVIEW            1.12e+04   2.34e+04    0.48  0.63052    
## CITYSOUTH ST PAUL        7.61e+04   7.81e+03    9.74  < 2e-16 ***
## CITYSPRING LAKE PARK    -7.83e+03   4.87e+04   -0.16  0.87217    
## CITYST. ANTHONY          2.78e+04   3.14e+04    0.89  0.37555    
## CITYSTILLWATER          -4.32e+04   4.21e+03  -10.25  < 2e-16 ***
## CITYST. PAUL             6.00e+04   2.33e+04    2.58  0.01000 *  
## CITYST. PAUL PARK       -5.62e+04   5.30e+03  -10.60  < 2e-16 ***
## CITYVADNAIS HEIGHTS      5.27e+03   2.39e+04    0.22  0.82514    
## CITYWEST ST PAUL         1.02e+05   7.12e+03   14.26  < 2e-16 ***
## CITYWHITE BEAR LAKE      9.23e+03   2.30e+04    0.40  0.68795    
## CITYWHITE BEAR TOWNSHIP -1.33e+04   2.37e+04   -0.56  0.57408    
## CITYWILLERNIE           -3.39e+04   1.70e+04   -1.99  0.04618 *  
## CITYWOODBURY                   NA         NA      NA       NA    
## SALE_YR                 -1.23e+04   9.18e+02  -13.45  < 2e-16 ***
## SDNUM                   -9.67e+00   2.73e+01   -0.35  0.72276    
## ACRES_POLY               8.35e+04   6.18e+03   13.51  < 2e-16 ***
## HOMESTEADY               6.43e+03   1.31e+03    4.91  9.3e-07 ***
## FIN_SQ_FT                9.61e+01   9.30e-01  103.35  < 2e-16 ***
## YEAR_BUILT               1.28e+02   2.46e+01    5.19  2.1e-07 ***
## MAX                     -9.47e+02   7.11e+01  -13.32  < 2e-16 ***
## PARK_dist                1.22e+00   4.68e-01    2.61  0.00895 ** 
## LAKE_dist                3.96e+00   9.18e-01    4.31  1.6e-05 ***
## MCA3                     3.31e+02   3.86e+01    8.56  < 2e-16 ***
## MCA5                     1.59e+01   7.76e+00    2.05  0.04048 *  
## SHOP_dist                1.99e+00   5.52e-01    3.60  0.00032 ***
## CBD_dist                 4.31e+00   2.68e-01   16.06  < 2e-16 ***
## SALE_SEASO2              2.65e+03   1.43e+03    1.85  0.06419 .  
## SALE_SEASO3              1.76e+03   1.46e+03    1.20  0.22987    
## SALE_SEASO4             -6.66e+03   1.62e+03   -4.10  4.1e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 60500 on 15528 degrees of freedom
## Multiple R-squared: 0.629,	Adjusted R-squared: 0.627 
## F-statistic:  446 on 59 and 15528 DF,  p-value: <2e-16
```


Looking at the residual and qqplot, notice that there is a greater threat to homoscedasticity than in the 2005-2007 model and normality issues are still a problem.


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
## -1.4684 -0.1297  0.0029  0.1373  1.4047 
## 
## Coefficients: (1 not defined because of singularities)
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)              1.25e+02   4.90e+00   25.57  < 2e-16 ***
## COUNTY_ID123             1.23e-01   8.95e-02    1.38  0.16904    
## COUNTY_ID163             3.23e-01   1.31e-02   24.61  < 2e-16 ***
## CITYARDEN HILLS          2.45e-01   9.19e-02    2.67  0.00763 ** 
## CITYBAYPORT             -2.85e-01   3.06e-02   -9.31  < 2e-16 ***
## CITYBIRCHWOOD VILLAGE   -1.13e-02   4.90e-02   -0.23  0.81792    
## CITYBURNSVILLE          -5.40e-02   1.35e-02   -4.01  6.1e-05 ***
## CITYCOTTAGE GROVE       -1.81e-01   1.01e-02  -17.92  < 2e-16 ***
## CITYDELLWOOD            -3.38e-01   2.32e-01   -1.45  0.14630    
## CITYEAGAN                1.80e-01   1.36e-02   13.21  < 2e-16 ***
## CITYEMPIRE TOWNSHIP     -1.75e-01   4.76e-02   -3.68  0.00023 ***
## CITYFALCON HEIGHTS       5.76e-01   9.32e-02    6.18  6.6e-10 ***
## CITYFARMINGTON          -1.64e-01   1.53e-02  -10.66  < 2e-16 ***
## CITYGEM LAKE            -2.83e-01   2.49e-01   -1.14  0.25541    
## CITYHUGO                -2.26e-01   1.73e-02  -13.08  < 2e-16 ***
## CITYINVER GROVE HEIGHTS  1.96e-01   2.01e-02    9.73  < 2e-16 ***
## CITYLAKE ELMO           -7.99e-02   4.00e-02   -2.00  0.04596 *  
## CITYLAKEVILLE           -3.38e-02   1.27e-02   -2.65  0.00794 ** 
## CITYLAUDERDALE           4.09e-01   9.95e-02    4.11  3.9e-05 ***
## CITYLITTLE CANADA        2.03e-01   9.31e-02    2.18  0.02939 *  
## CITYMAHTOMEDI            5.79e-02   2.12e-02    2.73  0.00631 ** 
## CITYMAPLEWOOD            1.99e-01   8.97e-02    2.21  0.02682 *  
## CITYMENDOTA              4.41e-01   1.35e-01    3.27  0.00108 ** 
## CITYMENDOTA HEIGHTS      5.14e-01   2.62e-02   19.61  < 2e-16 ***
## CITYMOUNDS VIEW         -4.38e-02   9.11e-02   -0.48  0.63016    
## CITYNEW BRIGHTON         1.65e-01   9.00e-02    1.83  0.06727 .  
## CITYNEWPORT             -2.47e-01   2.98e-02   -8.27  < 2e-16 ***
## CITYNORTH ST. PAUL       1.13e-01   9.05e-02    1.25  0.21311    
## CITYOAKDALE              5.06e-02   1.27e-02    3.99  6.6e-05 ***
## CITYOAK PARK HEIGHTS    -1.53e-01   2.96e-02   -5.16  2.4e-07 ***
## CITYROSEMOUNT            2.77e-02   1.57e-02    1.77  0.07671 .  
## CITYROSEVILLE            3.29e-01   8.97e-02    3.66  0.00025 ***
## CITYSHOREVIEW            1.28e-01   8.98e-02    1.42  0.15464    
## CITYSOUTH ST PAUL        2.66e-01   1.99e-02   13.38  < 2e-16 ***
## CITYSPRING LAKE PARK     2.45e-02   1.87e-01    0.13  0.89552    
## CITYST. ANTHONY          2.99e-01   1.16e-01    2.58  0.00975 ** 
## CITYSTILLWATER          -1.71e-01   1.35e-02  -12.61  < 2e-16 ***
## CITYST. PAUL             4.14e-01   8.95e-02    4.63  3.7e-06 ***
## CITYST. PAUL PARK       -2.65e-01   2.02e-02  -13.10  < 2e-16 ***
## CITYVADNAIS HEIGHTS      9.71e-02   9.16e-02    1.06  0.28948    
## CITYWEST ST PAUL         4.92e-01   2.42e-02   20.32  < 2e-16 ***
## CITYWHITE BEAR LAKE      1.31e-01   8.82e-02    1.49  0.13723    
## CITYWHITE BEAR TOWNSHIP  2.88e-02   9.10e-02    0.32  0.75161    
## CITYWILLERNIE           -1.64e-01   6.50e-02   -2.52  0.01167 *  
## CITYWOODBURY                   NA         NA      NA       NA    
## SALE_YR                 -6.05e-02   2.43e-03  -24.85  < 2e-16 ***
## ACRES_POLY               1.80e-01   9.46e-02    1.90  0.05687 .  
## ACRES2                   1.28e-01   1.43e-01    0.90  0.36999    
## HOMESTEADY               3.97e-02   4.96e-03    8.00  1.3e-15 ***
## logFIN_SQ_               6.24e-01   6.42e-03   97.15  < 2e-16 ***
## YEAR_BUILT               5.59e-04   9.47e-05    5.91  3.6e-09 ***
## logMAX                  -1.96e-01   1.43e-02  -13.67  < 2e-16 ***
## logPARK                  1.65e-03   2.53e-03    0.65  0.51537    
## MCA3                     1.47e-03   1.46e-04   10.05  < 2e-16 ***
## logSHOP                  2.46e-02   3.34e-03    7.37  1.8e-13 ***
## logCBD                   2.81e-01   9.37e-03   30.03  < 2e-16 ***
## SALE_SEASO2              1.38e-02   5.50e-03    2.51  0.01214 *  
## SALE_SEASO3              4.17e-03   5.60e-03    0.74  0.45709    
## SALE_SEASO4             -3.03e-02   6.20e-03   -4.89  1.0e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 0.232 on 15530 degrees of freedom
## Multiple R-squared: 0.636,	Adjusted R-squared: 0.635 
## F-statistic:  477 on 57 and 15530 DF,  p-value: <2e-16
```


Looking at the residual and qqplot, note non constant variance and normality conditions improve, yet there are still some issues. I am joining the residuals with the shapefile in order to see if these observations with either large over- or under-estimates are clustered.


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


The last regression inputs the dummy variable for GARAGE. The inclusion of this variable excludes ~ 1500 parcels mostly in Dakota County which brings the sample size to ~14k. This regression has similar looking residual and qqplots.


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
## -1.4606 -0.1292  0.0061  0.1392  1.3993 
## 
## Coefficients: (1 not defined because of singularities)
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)              1.28e+02   5.19e+00   24.69  < 2e-16 ***
## COUNTY_ID123             1.38e-01   9.06e-02    1.53  0.12717    
## COUNTY_ID163             3.44e-01   1.46e-02   23.65  < 2e-16 ***
## CITYARDEN HILLS          2.41e-01   9.29e-02    2.59  0.00955 ** 
## CITYBAYPORT             -2.53e-01   3.31e-02   -7.64  2.2e-14 ***
## CITYBIRCHWOOD VILLAGE   -7.46e-02   5.58e-02   -1.34  0.18169    
## CITYBURNSVILLE          -3.85e-02   1.58e-02   -2.44  0.01481 *  
## CITYCOTTAGE GROVE       -1.87e-01   1.03e-02  -18.21  < 2e-16 ***
## CITYDELLWOOD            -3.72e-01   2.35e-01   -1.58  0.11317    
## CITYEAGAN                1.91e-01   1.55e-02   12.29  < 2e-16 ***
## CITYEMPIRE TOWNSHIP     -1.54e-01   6.39e-02   -2.40  0.01619 *  
## CITYFALCON HEIGHTS       5.60e-01   9.43e-02    5.94  3.0e-09 ***
## CITYFARMINGTON          -1.73e-01   1.79e-02   -9.64  < 2e-16 ***
## CITYGEM LAKE            -2.86e-01   2.51e-01   -1.14  0.25551    
## CITYHUGO                -2.21e-01   1.76e-02  -12.53  < 2e-16 ***
## CITYINVER GROVE HEIGHTS  2.10e-01   2.37e-02    8.85  < 2e-16 ***
## CITYLAKE ELMO           -4.92e-02   4.23e-02   -1.16  0.24458    
## CITYLAKEVILLE           -6.48e-02   1.51e-02   -4.28  1.8e-05 ***
## CITYLAUDERDALE           3.98e-01   1.01e-01    3.96  7.6e-05 ***
## CITYLITTLE CANADA        2.02e-01   9.41e-02    2.15  0.03161 *  
## CITYMAHTOMEDI            3.91e-02   2.21e-02    1.77  0.07756 .  
## CITYMAPLEWOOD            1.97e-01   9.07e-02    2.17  0.03009 *  
## CITYMENDOTA              3.96e-01   1.67e-01    2.38  0.01753 *  
## CITYMENDOTA HEIGHTS      5.18e-01   3.61e-02   14.36  < 2e-16 ***
## CITYMOUNDS VIEW         -5.15e-02   9.20e-02   -0.56  0.57559    
## CITYNEW BRIGHTON         1.59e-01   9.10e-02    1.75  0.08002 .  
## CITYNEWPORT             -2.65e-01   3.14e-02   -8.45  < 2e-16 ***
## CITYNORTH ST. PAUL       1.04e-01   9.15e-02    1.14  0.25406    
## CITYOAKDALE              4.67e-02   1.30e-02    3.59  0.00034 ***
## CITYOAK PARK HEIGHTS    -1.41e-01   3.19e-02   -4.43  9.4e-06 ***
## CITYROSEMOUNT            4.10e-03   1.93e-02    0.21  0.83172    
## CITYROSEVILLE            3.22e-01   9.07e-02    3.55  0.00039 ***
## CITYSHOREVIEW            1.28e-01   9.08e-02    1.41  0.15970    
## CITYSOUTH ST PAUL        2.84e-01   2.43e-02   11.72  < 2e-16 ***
## CITYSPRING LAKE PARK     9.68e-03   1.89e-01    0.05  0.95912    
## CITYST. ANTHONY          2.93e-01   1.17e-01    2.51  0.01220 *  
## CITYSTILLWATER          -1.71e-01   1.41e-02  -12.11  < 2e-16 ***
## CITYST. PAUL             4.03e-01   9.05e-02    4.45  8.6e-06 ***
## CITYST. PAUL PARK       -2.81e-01   2.08e-02  -13.51  < 2e-16 ***
## CITYVADNAIS HEIGHTS      1.01e-01   9.26e-02    1.09  0.27487    
## CITYWEST ST PAUL         5.15e-01   2.77e-02   18.54  < 2e-16 ***
## CITYWHITE BEAR LAKE      1.22e-01   8.91e-02    1.37  0.17164    
## CITYWHITE BEAR TOWNSHIP  2.92e-02   9.19e-02    0.32  0.75042    
## CITYWILLERNIE           -1.13e-01   7.87e-02   -1.44  0.15088    
## CITYWOODBURY                   NA         NA      NA       NA    
## SALE_YR                 -6.16e-02   2.58e-03  -23.87  < 2e-16 ***
## GARAGEY                  1.36e-01   1.53e-02    8.90  < 2e-16 ***
## ACRES_POLY               1.21e-01   1.01e-01    1.20  0.23027    
## ACRES2                   2.13e-01   1.54e-01    1.38  0.16650    
## HOMESTEADY               3.94e-02   5.11e-03    7.70  1.5e-14 ***
## logFIN_SQ_               6.22e-01   6.74e-03   92.33  < 2e-16 ***
## YEAR_BUILT               1.19e-04   9.98e-05    1.19  0.23280    
## logMAX                  -1.96e-01   1.51e-02  -12.93  < 2e-16 ***
## logPARK                  3.52e-03   2.65e-03    1.33  0.18518    
## MCA3                     1.45e-03   1.49e-04    9.74  < 2e-16 ***
## logSHOP                  2.18e-02   3.52e-03    6.20  5.7e-10 ***
## logCBD                   2.84e-01   9.75e-03   29.15  < 2e-16 ***
## SALE_SEASO2              1.73e-02   5.80e-03    2.98  0.00286 ** 
## SALE_SEASO3              6.43e-03   5.92e-03    1.09  0.27743    
## SALE_SEASO4             -3.43e-02   6.59e-03   -5.20  2.0e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 0.235 on 14015 degrees of freedom
##   (1514 observations deleted due to missingness)
## Multiple R-squared: 0.628,	Adjusted R-squared: 0.626 
## F-statistic:  407 on 58 and 14015 DF,  p-value: <2e-16
```


