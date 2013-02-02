Max's Analysis -- 2008-2010 SalesData 1/31/13
========================================================
Today we are looking at interaction terms and whether attributes such as land size and traffic noise have a marginal effect that varies over space. Our goal is to map these marginal effects to show the need for locally-weighted regression.

Although using a linear dependent variable would be easier to interpret, the model assumptions are violated and some of the explanatory variables do not fit/explain the variation in sales value as well as they do when the sales value is transformed into log.

To find the marginal effect of an attribute that includes quadratic relationships and interaction terms requires a little more math. 

Lets look at an example equation: log (y) = a + b1(x) + b2 (x^2) + b3 (x*z)
In order to get y by itself, we need to take the exponent of both sides:
y = exp^[a + b1(x) + b2 (x^2) + b3 (x*z)]

Now, to estimate the marginal effect x has on y, we take the derivative of both sides with respect to x:
dy/dx = (b1 + 2(b2) + b3(z)) * exp ^ [a + b1(x) + b2 (x^2) + b3 (x*z)] 


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


After the Thursday, 1/31 morning meeting with Aaron, found that most of the extreme over/under estimation of sales values occur in the city of St. Paul, where one neighborhood is 'good' (Hihgland Park) and another is riddled with crime/lower income. In order to account for these features we added three variables to the model:

(1) Distance to nearest residential college (2) Median income ($2011) in Census Tract based on American Community Survey administered through US Census Bureau (3) Distance to Minneapolis & St. Paul, not just nearest CBD

In this first model, we include these three variables as well as exclude CITY variable so we can assess if there is multicollinearity present through vif(). We see that the inclusion of distance to St. Paul, Minneapolis, and nearest CBD are all closely correlated.


```r
model.SaleValue1 <- lm(logSALE_VA ~ COUNTY_ID + factor(SALE_YR) + SDNUM + ACRES_POLY + 
    I(ACRES_POLY^2) + HOMESTEAD + log(FIN_SQ_FT) + YEAR_BUILT + MAX + PARK_dist + 
    I(PARK_dist^2) + LAKE_dist + I(LAKE_dist^2) + MCA3 + MCA5 + SHOP_dist + 
    I(SHOP_dist^2) + COLLEGE_di + SP_dist + MPS_dist + CBD_dist + I(CBD_dist^2) + 
    MED_INCOME + SALE_SEASO, data = workingdata)

vif(model.SaleValue1)
```

```
##                    GVIF Df GVIF^(1/(2*Df))
## COUNTY_ID       170.874  2           3.616
## factor(SALE_YR)   1.014  2           1.004
## SDNUM            35.440  1           5.953
## ACRES_POLY       25.997  1           5.099
## I(ACRES_POLY^2)  21.662  1           4.654
## HOMESTEAD         1.029  1           1.014
## log(FIN_SQ_FT)    1.915  1           1.384
## YEAR_BUILT        2.646  1           1.627
## MAX               1.231  1           1.110
## PARK_dist        13.757  1           3.709
## I(PARK_dist^2)   15.071  1           3.882
## LAKE_dist         8.918  1           2.986
## I(LAKE_dist^2)    9.098  1           3.016
## MCA3              2.145  1           1.464
## MCA5              1.829  1           1.352
## SHOP_dist        11.140  1           3.338
## I(SHOP_dist^2)   12.015  1           3.466
## COLLEGE_di       80.869  1           8.993
## SP_dist         130.378  1          11.418
## MPS_dist         60.720  1           7.792
## CBD_dist        218.326  1          14.776
## I(CBD_dist^2)    48.516  1           6.965
## MED_INCOME        2.596  1           1.611
## SALE_SEASO        1.008  3           1.001
```


The second model will exclude the St. Paul and Minneapolis distances. We see that there is no large violation of multicollinearity anymore.


```r
model.SaleValue2 <- lm(logSALE_VA ~ COUNTY_ID + factor(SALE_YR) + ACRES_POLY + 
    I(ACRES_POLY^2) + HOMESTEAD + log(FIN_SQ_FT) + YEAR_BUILT + MAX + PARK_dist + 
    I(PARK_dist^2) + LAKE_dist + I(LAKE_dist^2) + MCA3 + MCA5 + SHOP_dist + 
    I(SHOP_dist^2) + COLLEGE_di + CBD_dist + I(CBD_dist^2) + MED_INCOME + SALE_SEASO, 
    data = workingdata)

vif(model.SaleValue2)
```

```
##                   GVIF Df GVIF^(1/(2*Df))
## COUNTY_ID        9.006  2           1.732
## factor(SALE_YR)  1.014  2           1.003
## ACRES_POLY      25.767  1           5.076
## I(ACRES_POLY^2) 21.519  1           4.639
## HOMESTEAD        1.028  1           1.014
## log(FIN_SQ_FT)   1.895  1           1.377
## YEAR_BUILT       2.616  1           1.617
## MAX              1.225  1           1.107
## PARK_dist       13.667  1           3.697
## I(PARK_dist^2)  14.950  1           3.867
## LAKE_dist        8.910  1           2.985
## I(LAKE_dist^2)   9.079  1           3.013
## MCA3             2.139  1           1.463
## MCA5             1.628  1           1.276
## SHOP_dist       10.590  1           3.254
## I(SHOP_dist^2)  10.640  1           3.262
## COLLEGE_di      11.130  1           3.336
## CBD_dist        44.905  1           6.701
## I(CBD_dist^2)   42.390  1           6.511
## MED_INCOME       2.521  1           1.588
## SALE_SEASO       1.008  3           1.001
```


Now we include interaction terms to view if there is an interaction effect between distance to the CBD and (1) Land Size and (2) Traffic Noise. Note that all non linear relationships are significant except for CBD distance. Also, both interaction effects are insignificant however I believe that getting rid of the insignificant quadratic term for CBD will yield significant results for the interaction between land size and location.


```r
model.SaleValue3 <- lm(logSALE_VA ~ COUNTY_ID + CITY + factor(SALE_YR) + ACRES_POLY * 
    CBD_dist + I(CBD_dist^2) + I(ACRES_POLY^2) + log(MAX) * CBD_dist + HOMESTEAD + 
    log(FIN_SQ_FT) + YEAR_BUILT + LAKE_dist + I(LAKE_dist^2) + PARK_dist + I(PARK_dist^2) + 
    MCA3 + SHOP_dist + I(SHOP_dist^2) + MED_INCOME + COLLEGE_di + SALE_SEASO, 
    data = workingdata)

summary(model.SaleValue3)
```

```
## 
## Call:
## lm(formula = logSALE_VA ~ COUNTY_ID + CITY + factor(SALE_YR) + 
##     ACRES_POLY * CBD_dist + I(CBD_dist^2) + I(ACRES_POLY^2) + 
##     log(MAX) * CBD_dist + HOMESTEAD + log(FIN_SQ_FT) + YEAR_BUILT + 
##     LAKE_dist + I(LAKE_dist^2) + PARK_dist + I(PARK_dist^2) + 
##     MCA3 + SHOP_dist + I(SHOP_dist^2) + MED_INCOME + COLLEGE_di + 
##     SALE_SEASO, data = workingdata)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.2153 -0.0997 -0.0106  0.0884  0.9967 
## 
## Coefficients: (2 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   3.60e+00   1.35e-01   26.65  < 2e-16 ***
## COUNTY_ID123                  6.21e-02   1.22e-02    5.10  3.5e-07 ***
## COUNTY_ID163                  4.89e-01   1.13e-01    4.34  1.4e-05 ***
## CITYARDEN HILLS              -7.93e-02   1.75e-02   -4.53  5.9e-06 ***
## CITYBURNSVILLE               -1.08e-02   6.86e-03   -1.58  0.11524    
## CITYCITY OF BAYPORT          -9.00e-02   1.14e-01   -0.79  0.43163    
## CITYCITY OF BIRCHWOOD        -6.32e-02   1.18e-01   -0.54  0.59229    
## CITYCITY OF COTTAGE GROVE    -1.15e-01   1.13e-01   -1.02  0.30769    
## CITYCITY OF HUGO             -2.22e-01   1.15e-01   -1.93  0.05351 .  
## CITYCITY OF LAKE ELMO        -4.76e-02   1.15e-01   -0.41  0.67963    
## CITYCITY OF MAHTOMEDI        -4.15e-02   1.13e-01   -0.37  0.71423    
## CITYCITY OF NEWPORT          -1.77e-01   1.14e-01   -1.55  0.12123    
## CITYCITY OF OAKDALE           2.38e-02   1.13e-01    0.21  0.83334    
## CITYCITY OF OAK PARK HEIGHTS -3.50e-02   1.14e-01   -0.31  0.75820    
## CITYCITY OF STILLWATER       -3.94e-02   1.13e-01   -0.35  0.72678    
## CITYCITY OF ST PAUL PARK     -1.48e-01   1.13e-01   -1.30  0.19208    
## CITYCITY OF WHITE BEAR LAKE  -1.93e-01   1.25e-01   -1.54  0.12254    
## CITYCITY OF WILLERNIE        -2.69e-01   1.17e-01   -2.30  0.02142 *  
## CITYCITY OF WOODBURY         -2.67e-03   1.13e-01   -0.02  0.98112    
## CITYEAGAN                     4.86e-02   8.05e-03    6.04  1.5e-09 ***
## CITYEMPIRE TOWNSHIP           2.44e-02   2.40e-02    1.02  0.31000    
## CITYFALCON HEIGHTS            2.98e-01   2.04e-02   14.63  < 2e-16 ***
## CITYFARMINGTON                3.66e-02   9.88e-03    3.70  0.00021 ***
## CITYGEM LAKE                  9.44e-02   1.13e-01    0.84  0.40278    
## CITYINVER GROVE HEIGHTS       2.58e-01   1.14e-02   22.66  < 2e-16 ***
## CITYLAKEVILLE                -1.03e-02   8.54e-03   -1.21  0.22660    
## CITYLAUDERDALE                2.09e-01   2.52e-02    8.26  < 2e-16 ***
## CITYLITTLE CANADA             1.84e-01   1.87e-02    9.82  < 2e-16 ***
## CITYMAPLEWOOD                 3.00e-01   1.28e-02   23.43  < 2e-16 ***
## CITYMENDOTA                  -1.73e-01   1.59e-01   -1.09  0.27599    
## CITYMENDOTA HEIGHTS           2.61e-01   1.66e-02   15.73  < 2e-16 ***
## CITYMOUNDS VIEW              -6.84e-02   1.48e-02   -4.63  3.7e-06 ***
## CITYNEW BRIGHTON             -4.96e-03   1.33e-02   -0.37  0.70813    
## CITYNORTH ST. PAUL            3.40e-01   1.42e-02   23.89  < 2e-16 ***
## CITYROSEMOUNT                 9.07e-02   7.49e-03   12.11  < 2e-16 ***
## CITYROSEVILLE                 1.56e-01   1.32e-02   11.80  < 2e-16 ***
## CITYSHOREVIEW                -7.00e-02   1.25e-02   -5.60  2.2e-08 ***
## CITYSOUTH ST PAUL             3.25e-01   1.23e-02   26.35  < 2e-16 ***
## CITYSPRING LAKE PARK         -1.57e-01   1.59e-01   -0.98  0.32531    
## CITYST. ANTHONY               9.11e-02   4.15e-02    2.20  0.02815 *  
## CITYST. PAUL                  3.50e-01   1.30e-02   26.90  < 2e-16 ***
## CITYTOWN OF BAYTOWN                 NA         NA      NA       NA    
## CITYVADNAIS HEIGHTS           3.96e-03   1.49e-02    0.27  0.79033    
## CITYWEST ST PAUL              3.30e-01   1.48e-02   22.35  < 2e-16 ***
## CITYWHITE BEAR LAKE           1.77e-01   1.23e-02   14.32  < 2e-16 ***
## CITYWHITE BEAR TOWNSHIP             NA         NA      NA       NA    
## factor(SALE_YR)2006           7.70e-03   2.27e-03    3.39  0.00070 ***
## factor(SALE_YR)2007          -2.51e-02   2.50e-03  -10.04  < 2e-16 ***
## ACRES_POLY                    7.75e-01   5.12e-02   15.13  < 2e-16 ***
## CBD_dist                      3.27e-05   3.55e-06    9.22  < 2e-16 ***
## I(CBD_dist^2)                -1.52e-11   2.96e-11   -0.51  0.60803    
## I(ACRES_POLY^2)              -6.54e-01   7.83e-02   -8.35  < 2e-16 ***
## log(MAX)                     -1.30e-01   1.47e-02   -8.83  < 2e-16 ***
## HOMESTEADY                    1.77e-02   3.08e-03    5.76  8.7e-09 ***
## log(FIN_SQ_FT)                5.50e-01   3.51e-03  156.55  < 2e-16 ***
## YEAR_BUILT                    2.02e-03   5.37e-05   37.60  < 2e-16 ***
## LAKE_dist                    -8.72e-05   4.71e-06  -18.52  < 2e-16 ***
## I(LAKE_dist^2)                3.48e-08   1.70e-09   20.43  < 2e-16 ***
## PARK_dist                    -7.70e-06   2.11e-06   -3.65  0.00026 ***
## I(PARK_dist^2)                8.03e-10   3.18e-10    2.53  0.01149 *  
## MCA3                          2.15e-03   1.95e-04   11.02  < 2e-16 ***
## SHOP_dist                     3.79e-05   2.48e-06   15.25  < 2e-16 ***
## I(SHOP_dist^2)               -5.21e-09   4.09e-10  -12.74  < 2e-16 ***
## MED_INCOME                    1.69e-06   7.33e-08   23.12  < 2e-16 ***
## COLLEGE_di                   -2.85e-05   5.05e-07  -56.57  < 2e-16 ***
## SALE_SEASO2                   1.66e-02   2.84e-03    5.86  4.7e-09 ***
## SALE_SEASO3                   1.79e-02   2.87e-03    6.22  5.2e-10 ***
## SALE_SEASO4                   1.95e-04   3.16e-03    0.06  0.95094    
## ACRES_POLY:CBD_dist          -2.05e-06   1.44e-06   -1.43  0.15383    
## CBD_dist:log(MAX)            -3.83e-07   8.23e-07   -0.47  0.64193    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 0.159 on 26357 degrees of freedom
## Multiple R-squared: 0.781,	Adjusted R-squared: 0.78 
## F-statistic: 1.4e+03 on 67 and 26357 DF,  p-value: <2e-16
```


Excluding the quadratic term for CBD distance and the interaction effect between traffic noise and CBD distance did not make the interaction effect between land size and distance significant.

```r
model.SaleValue4 <- lm(logSALE_VA ~ COUNTY_ID + CITY + factor(SALE_YR) + ACRES_POLY * 
    CBD_dist + I(ACRES_POLY^2) + log(MAX) + HOMESTEAD + log(FIN_SQ_FT) + YEAR_BUILT + 
    LAKE_dist + I(LAKE_dist^2) + PARK_dist + I(PARK_dist^2) + MCA3 + SHOP_dist + 
    I(SHOP_dist^2) + MED_INCOME + COLLEGE_di + SALE_SEASO, data = workingdata)

summary(model.SaleValue4)
```

```
## 
## Call:
## lm(formula = logSALE_VA ~ COUNTY_ID + CITY + factor(SALE_YR) + 
##     ACRES_POLY * CBD_dist + I(ACRES_POLY^2) + log(MAX) + HOMESTEAD + 
##     log(FIN_SQ_FT) + YEAR_BUILT + LAKE_dist + I(LAKE_dist^2) + 
##     PARK_dist + I(PARK_dist^2) + MCA3 + SHOP_dist + I(SHOP_dist^2) + 
##     MED_INCOME + COLLEGE_di + SALE_SEASO, data = workingdata)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.2153 -0.0997 -0.0106  0.0883  0.9963 
## 
## Coefficients: (2 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   3.61e+00   1.26e-01   28.79  < 2e-16 ***
## COUNTY_ID123                  6.25e-02   1.22e-02    5.14  2.8e-07 ***
## COUNTY_ID163                  4.90e-01   1.13e-01    4.35  1.4e-05 ***
## CITYARDEN HILLS              -8.06e-02   1.73e-02   -4.66  3.2e-06 ***
## CITYBURNSVILLE               -1.08e-02   6.86e-03   -1.57  0.11529    
## CITYCITY OF BAYPORT          -8.97e-02   1.14e-01   -0.78  0.43304    
## CITYCITY OF BIRCHWOOD        -6.35e-02   1.18e-01   -0.54  0.59082    
## CITYCITY OF COTTAGE GROVE    -1.15e-01   1.13e-01   -1.02  0.30864    
## CITYCITY OF HUGO             -2.22e-01   1.15e-01   -1.93  0.05305 .  
## CITYCITY OF LAKE ELMO        -4.74e-02   1.15e-01   -0.41  0.68062    
## CITYCITY OF MAHTOMEDI        -4.16e-02   1.13e-01   -0.37  0.71370    
## CITYCITY OF NEWPORT          -1.77e-01   1.14e-01   -1.55  0.12031    
## CITYCITY OF OAKDALE           2.30e-02   1.13e-01    0.20  0.83849    
## CITYCITY OF OAK PARK HEIGHTS -3.45e-02   1.14e-01   -0.30  0.76178    
## CITYCITY OF STILLWATER       -3.92e-02   1.13e-01   -0.35  0.72827    
## CITYCITY OF ST PAUL PARK     -1.48e-01   1.13e-01   -1.30  0.19219    
## CITYCITY OF WHITE BEAR LAKE  -1.93e-01   1.25e-01   -1.55  0.12156    
## CITYCITY OF WILLERNIE        -2.69e-01   1.17e-01   -2.30  0.02156 *  
## CITYCITY OF WOODBURY         -2.92e-03   1.13e-01   -0.03  0.97934    
## CITYEAGAN                     4.90e-02   8.03e-03    6.11  1.0e-09 ***
## CITYEMPIRE TOWNSHIP           2.31e-02   2.38e-02    0.97  0.33015    
## CITYFALCON HEIGHTS            2.97e-01   2.01e-02   14.75  < 2e-16 ***
## CITYFARMINGTON                3.51e-02   9.11e-03    3.86  0.00011 ***
## CITYGEM LAKE                  9.44e-02   1.13e-01    0.84  0.40281    
## CITYINVER GROVE HEIGHTS       2.58e-01   1.14e-02   22.76  < 2e-16 ***
## CITYLAKEVILLE                -1.20e-02   7.78e-03   -1.55  0.12229    
## CITYLAUDERDALE                2.06e-01   2.49e-02    8.30  < 2e-16 ***
## CITYLITTLE CANADA             1.83e-01   1.86e-02    9.84  < 2e-16 ***
## CITYMAPLEWOOD                 2.99e-01   1.26e-02   23.65  < 2e-16 ***
## CITYMENDOTA                  -1.75e-01   1.59e-01   -1.10  0.27194    
## CITYMENDOTA HEIGHTS           2.59e-01   1.62e-02   15.96  < 2e-16 ***
## CITYMOUNDS VIEW              -6.87e-02   1.47e-02   -4.66  3.1e-06 ***
## CITYNEW BRIGHTON             -6.02e-03   1.31e-02   -0.46  0.64544    
## CITYNORTH ST. PAUL            3.40e-01   1.42e-02   23.89  < 2e-16 ***
## CITYROSEMOUNT                 9.13e-02   7.45e-03   12.25  < 2e-16 ***
## CITYROSEVILLE                 1.54e-01   1.28e-02   12.02  < 2e-16 ***
## CITYSHOREVIEW                -7.07e-02   1.24e-02   -5.69  1.3e-08 ***
## CITYSOUTH ST PAUL             3.25e-01   1.23e-02   26.51  < 2e-16 ***
## CITYSPRING LAKE PARK         -1.57e-01   1.59e-01   -0.98  0.32464    
## CITYST. ANTHONY               8.96e-02   4.14e-02    2.16  0.03048 *  
## CITYST. PAUL                  3.48e-01   1.24e-02   27.93  < 2e-16 ***
## CITYTOWN OF BAYTOWN                 NA         NA      NA       NA    
## CITYVADNAIS HEIGHTS           3.16e-03   1.48e-02    0.21  0.83112    
## CITYWEST ST PAUL              3.29e-01   1.42e-02   23.09  < 2e-16 ***
## CITYWHITE BEAR LAKE           1.77e-01   1.23e-02   14.34  < 2e-16 ***
## CITYWHITE BEAR TOWNSHIP             NA         NA      NA       NA    
## factor(SALE_YR)2006           7.72e-03   2.27e-03    3.40  0.00067 ***
## factor(SALE_YR)2007          -2.51e-02   2.50e-03  -10.03  < 2e-16 ***
## ACRES_POLY                    7.78e-01   5.09e-02   15.29  < 2e-16 ***
## CBD_dist                      3.07e-05   7.10e-07   43.17  < 2e-16 ***
## I(ACRES_POLY^2)              -6.55e-01   7.82e-02   -8.38  < 2e-16 ***
## log(MAX)                     -1.36e-01   7.79e-03  -17.44  < 2e-16 ***
## HOMESTEADY                    1.77e-02   3.08e-03    5.76  8.3e-09 ***
## log(FIN_SQ_FT)                5.50e-01   3.50e-03  156.98  < 2e-16 ***
## YEAR_BUILT                    2.03e-03   5.26e-05   38.52  < 2e-16 ***
## LAKE_dist                    -8.71e-05   4.70e-06  -18.53  < 2e-16 ***
## I(LAKE_dist^2)                3.47e-08   1.70e-09   20.48  < 2e-16 ***
## PARK_dist                    -7.66e-06   2.11e-06   -3.63  0.00028 ***
## I(PARK_dist^2)                7.93e-10   3.17e-10    2.50  0.01241 *  
## MCA3                          2.14e-03   1.94e-04   11.02  < 2e-16 ***
## SHOP_dist                     3.80e-05   2.48e-06   15.32  < 2e-16 ***
## I(SHOP_dist^2)               -5.22e-09   4.08e-10  -12.80  < 2e-16 ***
## MED_INCOME                    1.70e-06   7.26e-08   23.41  < 2e-16 ***
## COLLEGE_di                   -2.86e-05   4.72e-07  -60.62  < 2e-16 ***
## SALE_SEASO2                   1.66e-02   2.84e-03    5.86  4.7e-09 ***
## SALE_SEASO3                   1.79e-02   2.87e-03    6.22  5.1e-10 ***
## SALE_SEASO4                   1.89e-04   3.16e-03    0.06  0.95235    
## ACRES_POLY:CBD_dist          -2.17e-06   1.43e-06   -1.52  0.12786    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 0.159 on 26359 degrees of freedom
## Multiple R-squared: 0.781,	Adjusted R-squared: 0.78 
## F-statistic: 1.44e+03 on 65 and 26359 DF,  p-value: <2e-16
```


Including all possible interaction effects between land size and distance to CBD yields significant results.

```r
model.SaleValue5 <- lm(logSALE_VA ~ COUNTY_ID + CITY + factor(SALE_YR) + ACRES_POLY * 
    CBD_dist + I(ACRES_POLY^2) * CBD_dist + I(ACRES_POLY^2) * I(CBD_dist^2) + 
    ACRES_POLY * I(CBD_dist^2) + log(MAX) + HOMESTEAD + log(FIN_SQ_FT) + YEAR_BUILT + 
    LAKE_dist + I(LAKE_dist^2) + PARK_dist + I(PARK_dist^2) + MCA3 + SHOP_dist + 
    I(SHOP_dist^2) + MED_INCOME + COLLEGE_di + SALE_SEASO, data = workingdata)

summary(model.SaleValue5)
```

```
## 
## Call:
## lm(formula = logSALE_VA ~ COUNTY_ID + CITY + factor(SALE_YR) + 
##     ACRES_POLY * CBD_dist + I(ACRES_POLY^2) * CBD_dist + I(ACRES_POLY^2) * 
##     I(CBD_dist^2) + ACRES_POLY * I(CBD_dist^2) + log(MAX) + HOMESTEAD + 
##     log(FIN_SQ_FT) + YEAR_BUILT + LAKE_dist + I(LAKE_dist^2) + 
##     PARK_dist + I(PARK_dist^2) + MCA3 + SHOP_dist + I(SHOP_dist^2) + 
##     MED_INCOME + COLLEGE_di + SALE_SEASO, data = workingdata)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.2342 -0.0989 -0.0101  0.0878  0.9682 
## 
## Coefficients: (2 not defined because of singularities)
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    3.64e+00   1.26e-01   28.95  < 2e-16 ***
## COUNTY_ID123                   5.38e-02   1.22e-02    4.42  9.9e-06 ***
## COUNTY_ID163                   4.84e-01   1.12e-01    4.31  1.7e-05 ***
## CITYARDEN HILLS               -9.28e-02   1.75e-02   -5.30  1.2e-07 ***
## CITYBURNSVILLE                -9.98e-03   6.84e-03   -1.46  0.14491    
## CITYCITY OF BAYPORT           -9.26e-02   1.14e-01   -0.81  0.41731    
## CITYCITY OF BIRCHWOOD         -7.97e-02   1.18e-01   -0.68  0.49849    
## CITYCITY OF COTTAGE GROVE     -1.22e-01   1.12e-01   -1.09  0.27592    
## CITYCITY OF HUGO              -2.14e-01   1.14e-01   -1.87  0.06113 .  
## CITYCITY OF LAKE ELMO         -4.92e-02   1.15e-01   -0.43  0.66891    
## CITYCITY OF MAHTOMEDI         -5.90e-02   1.13e-01   -0.52  0.60201    
## CITYCITY OF NEWPORT           -2.05e-01   1.14e-01   -1.81  0.07099 .  
## CITYCITY OF OAKDALE           -5.12e-03   1.13e-01   -0.05  0.96378    
## CITYCITY OF OAK PARK HEIGHTS  -3.34e-02   1.14e-01   -0.29  0.76885    
## CITYCITY OF STILLWATER        -4.45e-02   1.12e-01   -0.40  0.69206    
## CITYCITY OF ST PAUL PARK      -1.67e-01   1.13e-01   -1.48  0.13875    
## CITYCITY OF WHITE BEAR LAKE   -2.12e-01   1.24e-01   -1.70  0.08920 .  
## CITYCITY OF WILLERNIE         -3.17e-01   1.17e-01   -2.71  0.00664 ** 
## CITYCITY OF WOODBURY          -2.56e-02   1.13e-01   -0.23  0.82022    
## CITYEAGAN                      4.01e-02   8.07e-03    4.97  6.7e-07 ***
## CITYEMPIRE TOWNSHIP            6.09e-03   2.41e-02    0.25  0.80035    
## CITYFALCON HEIGHTS             2.63e-01   2.06e-02   12.78  < 2e-16 ***
## CITYFARMINGTON                 3.67e-02   9.85e-03    3.73  0.00019 ***
## CITYGEM LAKE                   9.18e-02   1.13e-01    0.82  0.41489    
## CITYINVER GROVE HEIGHTS        2.34e-01   1.16e-02   20.21  < 2e-16 ***
## CITYLAKEVILLE                 -2.09e-02   8.62e-03   -2.42  0.01549 *  
## CITYLAUDERDALE                 1.77e-01   2.53e-02    7.00  2.7e-12 ***
## CITYLITTLE CANADA              1.55e-01   1.89e-02    8.20  2.5e-16 ***
## CITYMAPLEWOOD                  2.64e-01   1.32e-02   20.06  < 2e-16 ***
## CITYMENDOTA                   -2.13e-01   1.59e-01   -1.34  0.18053    
## CITYMENDOTA HEIGHTS            2.15e-01   1.71e-02   12.57  < 2e-16 ***
## CITYMOUNDS VIEW               -7.22e-02   1.47e-02   -4.90  9.6e-07 ***
## CITYNEW BRIGHTON              -2.67e-02   1.34e-02   -2.00  0.04539 *  
## CITYNORTH ST. PAUL             3.05e-01   1.45e-02   21.01  < 2e-16 ***
## CITYROSEMOUNT                  9.04e-02   7.45e-03   12.13  < 2e-16 ***
## CITYROSEVILLE                  1.23e-01   1.35e-02    9.10  < 2e-16 ***
## CITYSHOREVIEW                 -7.31e-02   1.25e-02   -5.86  4.8e-09 ***
## CITYSOUTH ST PAUL              2.78e-01   1.30e-02   21.46  < 2e-16 ***
## CITYSPRING LAKE PARK          -1.51e-01   1.59e-01   -0.95  0.34242    
## CITYST. ANTHONY                5.95e-02   4.15e-02    1.43  0.15191    
## CITYST. PAUL                   3.18e-01   1.33e-02   23.99  < 2e-16 ***
## CITYTOWN OF BAYTOWN                  NA         NA      NA       NA    
## CITYVADNAIS HEIGHTS           -6.82e-03   1.49e-02   -0.46  0.64661    
## CITYWEST ST PAUL               2.83e-01   1.53e-02   18.48  < 2e-16 ***
## CITYWHITE BEAR LAKE            1.62e-01   1.24e-02   13.07  < 2e-16 ***
## CITYWHITE BEAR TOWNSHIP              NA         NA      NA       NA    
## factor(SALE_YR)2006            7.55e-03   2.26e-03    3.33  0.00086 ***
## factor(SALE_YR)2007           -2.54e-02   2.49e-03  -10.18  < 2e-16 ***
## ACRES_POLY                     1.94e+00   1.50e-01   12.99  < 2e-16 ***
## CBD_dist                       5.57e-05   2.75e-06   20.27  < 2e-16 ***
## I(ACRES_POLY^2)               -2.20e+00   2.73e-01   -8.04  9.1e-16 ***
## I(CBD_dist^2)                 -7.13e-10   8.27e-11   -8.62  < 2e-16 ***
## log(MAX)                      -1.36e-01   7.78e-03  -17.53  < 2e-16 ***
## HOMESTEADY                     1.72e-02   3.07e-03    5.61  2.1e-08 ***
## log(FIN_SQ_FT)                 5.51e-01   3.51e-03  157.26  < 2e-16 ***
## YEAR_BUILT                     1.97e-03   5.37e-05   36.73  < 2e-16 ***
## LAKE_dist                     -8.50e-05   4.70e-06  -18.09  < 2e-16 ***
## I(LAKE_dist^2)                 3.42e-08   1.70e-09   20.13  < 2e-16 ***
## PARK_dist                     -7.57e-06   2.11e-06   -3.60  0.00032 ***
## I(PARK_dist^2)                 8.98e-10   3.17e-10    2.83  0.00465 ** 
## MCA3                           2.00e-03   1.94e-04   10.28  < 2e-16 ***
## SHOP_dist                      3.86e-05   2.48e-06   15.56  < 2e-16 ***
## I(SHOP_dist^2)                -5.36e-09   4.08e-10  -13.12  < 2e-16 ***
## MED_INCOME                     1.72e-06   7.32e-08   23.55  < 2e-16 ***
## COLLEGE_di                    -2.82e-05   5.04e-07  -55.98  < 2e-16 ***
## SALE_SEASO2                    1.66e-02   2.83e-03    5.86  4.6e-09 ***
## SALE_SEASO3                    1.75e-02   2.87e-03    6.11  1.0e-09 ***
## SALE_SEASO4                   -2.02e-04   3.15e-03   -0.06  0.94895    
## ACRES_POLY:CBD_dist           -1.82e-04   2.06e-05   -8.85  < 2e-16 ***
## CBD_dist:I(ACRES_POLY^2)       2.29e-04   3.51e-05    6.52  7.3e-11 ***
## I(ACRES_POLY^2):I(CBD_dist^2) -5.66e-09   9.60e-10   -5.89  3.8e-09 ***
## ACRES_POLY:I(CBD_dist^2)       4.61e-09   5.79e-10    7.97  1.7e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 0.158 on 26355 degrees of freedom
## Multiple R-squared: 0.782,	Adjusted R-squared: 0.781 
## F-statistic: 1.37e+03 on 69 and 26355 DF,  p-value: <2e-16
```


