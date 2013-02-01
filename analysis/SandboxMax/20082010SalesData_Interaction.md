Max's Analysis -- 2008-2010 SalesData 1/31/13
========================================================
Today we are looking at interaction terms and whether attributes such as land size and traffic noise have a marginal effect that varies over space. Our goal is to map these marginal effects to show the need for locally-weighted regression.

Although using a linear dependent variable would be easier to interpret, the model assumptions are violated and some of the explanatory variables do not fit/explain the variation in sales value as well as they do when the sales value is transformed into log.

To find the marginal effect of an attribute that includes quadratic relationships and interaction terms requires a little more math. 

Lets look at an example equation: log (y) = a + b1(x) + b2 (x^2) + b3 (x*z)
In order to get y by itself, we need to take the exponent of both sides:
y = exp^[a + b1(x) + b2 (x^2) + b3 (x*z)]

Now, to estimate the marginal effect x has on y, we take the derivative of both sides with respect to x:
dy/dx = (b1 + 2(b2) + b3(z)) * exp^[a + b1(x) + b2 (x^2) + b3 (x*z)] 


Preparation for regression analysis...

```r
getwd()
```

```
## [1] "/home/timmm/Noise Hedonic Project/Noise-Hedonic/analysis/SandboxMax"
```

```r

library(foreign)
workingdata <- read.dbf("../../../Data/R2GIS/CleanData/Sales20082010.dbf")
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
## COUNTY_ID       151.912  2           3.511
## factor(SALE_YR)   3.639  2           1.381
## SDNUM            30.852  1           5.554
## ACRES_POLY       24.494  1           4.949
## I(ACRES_POLY^2)  21.451  1           4.631
## HOMESTEAD         1.456  1           1.207
## log(FIN_SQ_FT)    1.778  1           1.333
## YEAR_BUILT        2.291  1           1.514
## MAX               1.369  1           1.170
## PARK_dist        13.989  1           3.740
## I(PARK_dist^2)   14.471  1           3.804
## LAKE_dist         9.170  1           3.028
## I(LAKE_dist^2)    9.298  1           3.049
## MCA3              1.168  1           1.081
## MCA5              3.445  1           1.856
## SHOP_dist        10.353  1           3.218
## I(SHOP_dist^2)   10.289  1           3.208
## COLLEGE_di       76.276  1           8.734
## SP_dist         112.001  1          10.583
## MPS_dist         62.292  1           7.893
## CBD_dist        187.801  1          13.704
## I(CBD_dist^2)    41.040  1           6.406
## MED_INCOME        2.202  1           1.484
## SALE_SEASO        1.081  3           1.013
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
## COUNTY_ID       10.310  2           1.792
## factor(SALE_YR)  3.499  2           1.368
## ACRES_POLY      24.212  1           4.921
## I(ACRES_POLY^2) 21.255  1           4.610
## HOMESTEAD        1.456  1           1.207
## log(FIN_SQ_FT)   1.756  1           1.325
## YEAR_BUILT       2.251  1           1.500
## MAX              1.352  1           1.163
## PARK_dist       13.895  1           3.728
## I(PARK_dist^2)  14.369  1           3.791
## LAKE_dist        9.110  1           3.018
## I(LAKE_dist^2)   9.236  1           3.039
## MCA3             1.161  1           1.077
## MCA5             3.268  1           1.808
## SHOP_dist       10.106  1           3.179
## I(SHOP_dist^2)   9.424  1           3.070
## COLLEGE_di      10.319  1           3.212
## CBD_dist        36.400  1           6.033
## I(CBD_dist^2)   36.684  1           6.057
## MED_INCOME       2.135  1           1.461
## SALE_SEASO       1.080  3           1.013
```


Now we include interaction terms to view if there is an interaction effect between distance to the CBD and (1) Land Size and (2) Traffic Noise. Note that all quadratic relationships and interaction terms are significant.


```r
model.SaleValue3 <- lm(logSALE_VA ~ COUNTY_ID + CITY + factor(SALE_YR) + ACRES_POLY * 
    CBD_dist + log(MAX) * CBD_dist + HOMESTEAD + log(FIN_SQ_FT) + YEAR_BUILT + 
    LAKE_dist + I(LAKE_dist^2) + PARK_dist + I(PARK_dist^2) + MCA3 + SHOP_dist + 
    I(SHOP_dist^2) + MED_INCOME + COLLEGE_di + SALE_SEASO, data = workingdata)

summary(model.SaleValue3)
```

```
## 
## Call:
## lm(formula = logSALE_VA ~ COUNTY_ID + CITY + factor(SALE_YR) + 
##     ACRES_POLY * CBD_dist + log(MAX) * CBD_dist + HOMESTEAD + 
##     log(FIN_SQ_FT) + YEAR_BUILT + LAKE_dist + I(LAKE_dist^2) + 
##     PARK_dist + I(PARK_dist^2) + MCA3 + SHOP_dist + I(SHOP_dist^2) + 
##     MED_INCOME + COLLEGE_di + SALE_SEASO, data = workingdata)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.5851 -0.1189  0.0022  0.1270  1.3999 
## 
## Coefficients: (1 not defined because of singularities)
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)              5.42e+00   2.07e-01   26.15  < 2e-16 ***
## COUNTY_ID123             3.17e-01   8.25e-02    3.84  0.00012 ***
## COUNTY_ID163             4.63e-01   1.63e-02   28.36  < 2e-16 ***
## CITYARDEN HILLS         -4.45e-01   8.51e-02   -5.23  1.7e-07 ***
## CITYBAYPORT              2.75e-02   3.07e-02    0.90  0.37036    
## CITYBIRCHWOOD VILLAGE   -2.27e-01   4.53e-02   -5.00  5.7e-07 ***
## CITYBURNSVILLE          -3.35e-02   1.24e-02   -2.70  0.00686 ** 
## CITYCOTTAGE GROVE       -1.30e-01   1.02e-02  -12.66  < 2e-16 ***
## CITYDELLWOOD            -5.95e-01   2.13e-01   -2.80  0.00516 ** 
## CITYEAGAN                6.01e-02   1.46e-02    4.11  4.0e-05 ***
## CITYEMPIRE TOWNSHIP      1.13e-02   4.47e-02    0.25  0.79991    
## CITYFALCON HEIGHTS       4.02e-02   8.58e-02    0.47  0.63958    
## CITYFARMINGTON          -3.71e-02   1.64e-02   -2.26  0.02395 *  
## CITYGEM LAKE            -6.32e-01   2.28e-01   -2.78  0.00551 ** 
## CITYHUGO                -2.60e-01   3.66e-02   -7.11  1.2e-12 ***
## CITYINVER GROVE HEIGHTS  2.57e-01   2.11e-02   12.20  < 2e-16 ***
## CITYLAKE ELMO           -3.17e-02   3.71e-02   -0.85  0.39332    
## CITYLAKEVILLE           -1.94e-02   1.37e-02   -1.42  0.15614    
## CITYLAUDERDALE          -1.42e-01   9.17e-02   -1.54  0.12261    
## CITYLITTLE CANADA       -1.76e-01   8.55e-02   -2.06  0.03935 *  
## CITYMAHTOMEDI           -1.02e-01   2.10e-02   -4.85  1.2e-06 ***
## CITYMAPLEWOOD           -2.19e-02   8.22e-02   -0.27  0.79029    
## CITYMENDOTA              1.02e-01   1.24e-01    0.82  0.41215    
## CITYMENDOTA HEIGHTS      2.46e-01   2.80e-02    8.78  < 2e-16 ***
## CITYMOUNDS VIEW         -5.25e-01   8.39e-02   -6.26  4.0e-10 ***
## CITYNEW BRIGHTON        -3.62e-01   8.31e-02   -4.36  1.3e-05 ***
## CITYNEWPORT             -2.65e-01   2.78e-02   -9.53  < 2e-16 ***
## CITYNORTH ST. PAUL       9.78e-03   8.30e-02    0.12  0.90621    
## CITYOAKDALE              1.61e-02   1.23e-02    1.30  0.19207    
## CITYOAK PARK HEIGHTS     8.32e-02   2.93e-02    2.84  0.00453 ** 
## CITYROSEMOUNT            6.19e-02   1.45e-02    4.27  2.0e-05 ***
## CITYROSEVILLE           -1.74e-01   8.26e-02   -2.10  0.03537 *  
## CITYSHOREVIEW           -4.85e-01   8.30e-02   -5.85  5.0e-09 ***
## CITYSOUTH ST PAUL        3.05e-01   2.24e-02   13.59  < 2e-16 ***
## CITYSPRING LAKE PARK    -4.73e-01   1.71e-01   -2.76  0.00572 ** 
## CITYST. ANTHONY         -1.79e-01   1.06e-01   -1.68  0.09218 .  
## CITYSTILLWATER          -2.80e-02   1.57e-02   -1.78  0.07548 .  
## CITYST. PAUL            -2.49e-02   8.22e-02   -0.30  0.76188    
## CITYST. PAUL PARK       -2.25e-01   1.91e-02  -11.73  < 2e-16 ***
## CITYVADNAIS HEIGHTS     -3.69e-01   8.43e-02   -4.38  1.2e-05 ***
## CITYWEST ST PAUL         3.12e-01   2.57e-02   12.15  < 2e-16 ***
## CITYWHITE BEAR LAKE     -1.19e-01   8.09e-02   -1.47  0.14082    
## CITYWHITE BEAR TOWNSHIP -3.26e-01   8.35e-02   -3.91  9.3e-05 ***
## CITYWILLERNIE           -3.07e-01   6.04e-02   -5.09  3.7e-07 ***
## CITYWOODBURY                   NA         NA      NA       NA    
## factor(SALE_YR)2009     -7.85e-02   4.15e-03  -18.95  < 2e-16 ***
## factor(SALE_YR)2010     -1.14e-01   4.46e-03  -25.55  < 2e-16 ***
## ACRES_POLY               3.68e-01   4.54e-02    8.11  5.4e-16 ***
## CBD_dist                 1.91e-05   6.35e-06    3.01  0.00264 ** 
## log(MAX)                -2.75e-01   2.75e-02  -10.03  < 2e-16 ***
## HOMESTEADY               2.66e-02   4.67e-03    5.70  1.2e-08 ***
## log(FIN_SQ_FT)           5.56e-01   5.98e-03   92.93  < 2e-16 ***
## YEAR_BUILT               1.66e-03   8.95e-05   18.58  < 2e-16 ***
## LAKE_dist               -7.33e-05   8.41e-06   -8.72  < 2e-16 ***
## I(LAKE_dist^2)           2.73e-08   3.04e-09    8.98  < 2e-16 ***
## PARK_dist               -9.74e-06   3.73e-06   -2.61  0.00898 ** 
## I(PARK_dist^2)           2.34e-09   5.54e-10    4.22  2.5e-05 ***
## MCA3                     4.36e-04   1.36e-04    3.20  0.00136 ** 
## SHOP_dist                4.26e-05   4.19e-06   10.18  < 2e-16 ***
## I(SHOP_dist^2)          -6.30e-09   6.86e-10   -9.19  < 2e-16 ***
## MED_INCOME               2.27e-06   1.20e-07   18.96  < 2e-16 ***
## COLLEGE_di              -4.07e-05   8.45e-07  -48.18  < 2e-16 ***
## SALE_SEASO2              1.31e-02   5.02e-03    2.60  0.00930 ** 
## SALE_SEASO3              3.36e-03   5.12e-03    0.66  0.51150    
## SALE_SEASO4             -1.93e-02   5.73e-03   -3.37  0.00076 ***
## ACRES_POLY:CBD_dist     -4.37e-06   2.50e-06   -1.75  0.08023 .  
## CBD_dist:log(MAX)        5.25e-06   1.57e-06    3.35  0.00080 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 0.212 on 15604 degrees of freedom
## Multiple R-squared: 0.696,	Adjusted R-squared: 0.695 
## F-statistic:  550 on 65 and 15604 DF,  p-value: <2e-16
```


Since all interaction terms were significant, we include more interaction terms that resemble the nonlinear relationship land size and distance to the central business district have with logged sales value. Note that the inclusion of more interaction terms that reflect the nonlinear relationship makes all interactions non-significant, perhaps multicollinearity is present.


```r
model.SaleValue4 <- lm(logSALE_VA ~ COUNTY_ID + CITY + factor(SALE_YR) + ACRES_POLY * 
    CBD_dist + ACRES_POLY * I(CBD_dist^2) + I(ACRES_POLY^2) * I(CBD_dist^2) + 
    I(ACRES_POLY^2) * CBD_dist + log(MAX) * CBD_dist + log(MAX) * I(CBD_dist^2) + 
    HOMESTEAD + log(FIN_SQ_FT) + YEAR_BUILT + LAKE_dist + I(LAKE_dist^2) + PARK_dist + 
    I(PARK_dist^2) + MCA3 + SHOP_dist + I(SHOP_dist^2) + MED_INCOME + COLLEGE_di + 
    SALE_SEASO, data = workingdata)

summary(model.SaleValue4)
```

```
## 
## Call:
## lm(formula = logSALE_VA ~ COUNTY_ID + CITY + factor(SALE_YR) + 
##     ACRES_POLY * CBD_dist + ACRES_POLY * I(CBD_dist^2) + I(ACRES_POLY^2) * 
##     I(CBD_dist^2) + I(ACRES_POLY^2) * CBD_dist + log(MAX) * CBD_dist + 
##     log(MAX) * I(CBD_dist^2) + HOMESTEAD + log(FIN_SQ_FT) + YEAR_BUILT + 
##     LAKE_dist + I(LAKE_dist^2) + PARK_dist + I(PARK_dist^2) + 
##     MCA3 + SHOP_dist + I(SHOP_dist^2) + MED_INCOME + COLLEGE_di + 
##     SALE_SEASO, data = workingdata)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.5867 -0.1191  0.0023  0.1262  1.3952 
## 
## Coefficients: (1 not defined because of singularities)
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    5.45e+00   2.62e-01   20.80  < 2e-16 ***
## COUNTY_ID123                   3.03e-01   8.26e-02    3.67  0.00024 ***
## COUNTY_ID163                   4.48e-01   1.72e-02   26.10  < 2e-16 ***
## CITYARDEN HILLS               -4.40e-01   8.53e-02   -5.16  2.4e-07 ***
## CITYBAYPORT                    3.73e-02   3.09e-02    1.21  0.22719    
## CITYBIRCHWOOD VILLAGE         -2.21e-01   4.53e-02   -4.89  1.0e-06 ***
## CITYBURNSVILLE                -3.44e-02   1.24e-02   -2.77  0.00557 ** 
## CITYCOTTAGE GROVE             -1.26e-01   1.05e-02  -11.98  < 2e-16 ***
## CITYDELLWOOD                  -5.73e-01   2.13e-01   -2.69  0.00709 ** 
## CITYEAGAN                      5.34e-02   1.48e-02    3.61  0.00030 ***
## CITYEMPIRE TOWNSHIP            4.65e-03   4.54e-02    0.10  0.91851    
## CITYFALCON HEIGHTS             4.02e-02   8.61e-02    0.47  0.64033    
## CITYFARMINGTON                -2.87e-02   1.84e-02   -1.56  0.11868    
## CITYGEM LAKE                  -6.08e-01   2.28e-01   -2.67  0.00759 ** 
## CITYHUGO                      -2.47e-01   3.69e-02   -6.70  2.2e-11 ***
## CITYINVER GROVE HEIGHTS        2.44e-01   2.15e-02   11.39  < 2e-16 ***
## CITYLAKE ELMO                 -1.65e-02   3.72e-02   -0.44  0.65771    
## CITYLAKEVILLE                 -2.11e-02   1.56e-02   -1.35  0.17651    
## CITYLAUDERDALE                -1.33e-01   9.21e-02   -1.45  0.14766    
## CITYLITTLE CANADA             -1.75e-01   8.57e-02   -2.05  0.04087 *  
## CITYMAHTOMEDI                 -1.00e-01   2.10e-02   -4.77  1.8e-06 ***
## CITYMAPLEWOOD                 -2.75e-02   8.24e-02   -0.33  0.73830    
## CITYMENDOTA                    9.04e-02   1.24e-01    0.73  0.46716    
## CITYMENDOTA HEIGHTS            2.34e-01   2.93e-02    7.99  1.4e-15 ***
## CITYMOUNDS VIEW               -5.20e-01   8.40e-02   -6.18  6.4e-10 ***
## CITYNEW BRIGHTON              -3.63e-01   8.32e-02   -4.36  1.3e-05 ***
## CITYNEWPORT                   -2.65e-01   2.78e-02   -9.52  < 2e-16 ***
## CITYNORTH ST. PAUL             3.74e-03   8.30e-02    0.05  0.96409    
## CITYOAKDALE                    1.47e-02   1.25e-02    1.18  0.23957    
## CITYOAK PARK HEIGHTS           9.28e-02   2.95e-02    3.15  0.00166 ** 
## CITYROSEMOUNT                  5.92e-02   1.45e-02    4.07  4.7e-05 ***
## CITYROSEVILLE                 -1.78e-01   8.29e-02   -2.14  0.03198 *  
## CITYSHOREVIEW                 -4.77e-01   8.31e-02   -5.74  9.7e-09 ***
## CITYSOUTH ST PAUL              2.94e-01   2.33e-02   12.61  < 2e-16 ***
## CITYSPRING LAKE PARK          -4.59e-01   1.71e-01   -2.68  0.00737 ** 
## CITYST. ANTHONY               -1.80e-01   1.06e-01   -1.70  0.08999 .  
## CITYSTILLWATER                -2.00e-02   1.60e-02   -1.25  0.21013    
## CITYST. PAUL                  -1.26e-02   8.26e-02   -0.15  0.87844    
## CITYST. PAUL PARK             -2.27e-01   1.92e-02  -11.81  < 2e-16 ***
## CITYVADNAIS HEIGHTS           -3.67e-01   8.44e-02   -4.34  1.4e-05 ***
## CITYWEST ST PAUL               3.07e-01   2.69e-02   11.40  < 2e-16 ***
## CITYWHITE BEAR LAKE           -1.21e-01   8.09e-02   -1.49  0.13641    
## CITYWHITE BEAR TOWNSHIP       -3.19e-01   8.36e-02   -3.82  0.00013 ***
## CITYWILLERNIE                 -3.15e-01   6.06e-02   -5.20  2.0e-07 ***
## CITYWOODBURY                         NA         NA      NA       NA    
## factor(SALE_YR)2009           -7.85e-02   4.14e-03  -18.95  < 2e-16 ***
## factor(SALE_YR)2010           -1.14e-01   4.46e-03  -25.59  < 2e-16 ***
## ACRES_POLY                     1.08e+00   2.77e-01    3.91  9.1e-05 ***
## CBD_dist                       2.38e-05   2.58e-05    0.92  0.35632    
## I(CBD_dist^2)                 -6.68e-11   7.17e-10   -0.09  0.92570    
## I(ACRES_POLY^2)               -8.26e-01   4.93e-01   -1.68  0.09369 .  
## log(MAX)                      -2.84e-01   4.91e-02   -5.79  7.0e-09 ***
## HOMESTEADY                     2.63e-02   4.67e-03    5.62  1.9e-08 ***
## log(FIN_SQ_FT)                 5.55e-01   6.00e-03   92.58  < 2e-16 ***
## YEAR_BUILT                     1.61e-03   9.10e-05   17.64  < 2e-16 ***
## LAKE_dist                     -7.37e-05   8.43e-06   -8.74  < 2e-16 ***
## I(LAKE_dist^2)                 2.79e-08   3.06e-09    9.13  < 2e-16 ***
## PARK_dist                     -9.49e-06   3.73e-06   -2.54  0.01101 *  
## I(PARK_dist^2)                 2.41e-09   5.56e-10    4.34  1.4e-05 ***
## MCA3                           4.44e-04   1.36e-04    3.26  0.00110 ** 
## SHOP_dist                      4.28e-05   4.20e-06   10.21  < 2e-16 ***
## I(SHOP_dist^2)                -6.30e-09   6.88e-10   -9.16  < 2e-16 ***
## MED_INCOME                     2.26e-06   1.21e-07   18.62  < 2e-16 ***
## COLLEGE_di                    -4.03e-05   8.95e-07  -45.09  < 2e-16 ***
## SALE_SEASO2                    1.30e-02   5.02e-03    2.58  0.00976 ** 
## SALE_SEASO3                    3.05e-03   5.12e-03    0.60  0.55147    
## SALE_SEASO4                   -1.94e-02   5.73e-03   -3.38  0.00072 ***
## ACRES_POLY:CBD_dist           -6.48e-05   3.73e-05   -1.74  0.08268 .  
## ACRES_POLY:I(CBD_dist^2)       1.29e-09   1.04e-09    1.24  0.21568    
## I(CBD_dist^2):I(ACRES_POLY^2) -5.31e-10   1.72e-09   -0.31  0.75729    
## CBD_dist:I(ACRES_POLY^2)       4.51e-05   6.29e-05    0.72  0.47377    
## CBD_dist:log(MAX)              7.23e-06   6.29e-06    1.15  0.25052    
## I(CBD_dist^2):log(MAX)        -6.81e-11   1.75e-10   -0.39  0.69686    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 0.212 on 15598 degrees of freedom
## Multiple R-squared: 0.697,	Adjusted R-squared: 0.695 
## F-statistic:  505 on 71 and 15598 DF,  p-value: <2e-16
```


The last model keeps the interaction terms as well as Acres * CBD^2, which was closest to signficance level in the preceding model. We see that all interaction terms are significant. These coefficients suggest that the value of land increases as a parcel is closer to the central business district, at a decreasing rate. Also, the value of traffic noise disturbance increases as a parcel get farther away from the central business district.


```r
model.SaleValue5 <- lm(logSALE_VA ~ COUNTY_ID + CITY + factor(SALE_YR) + ACRES_POLY * 
    CBD_dist + I(ACRES_POLY^2) + ACRES_POLY * I(CBD_dist^2) + log(MAX) * CBD_dist + 
    HOMESTEAD + log(FIN_SQ_FT) + YEAR_BUILT + LAKE_dist + I(LAKE_dist^2) + PARK_dist + 
    I(PARK_dist^2) + MCA3 + SHOP_dist + I(SHOP_dist^2) + MED_INCOME + COLLEGE_di + 
    SALE_SEASO, data = workingdata)

summary(model.SaleValue5)
```

```
## 
## Call:
## lm(formula = logSALE_VA ~ COUNTY_ID + CITY + factor(SALE_YR) + 
##     ACRES_POLY * CBD_dist + I(ACRES_POLY^2) + ACRES_POLY * I(CBD_dist^2) + 
##     log(MAX) * CBD_dist + HOMESTEAD + log(FIN_SQ_FT) + YEAR_BUILT + 
##     LAKE_dist + I(LAKE_dist^2) + PARK_dist + I(PARK_dist^2) + 
##     MCA3 + SHOP_dist + I(SHOP_dist^2) + MED_INCOME + COLLEGE_di + 
##     SALE_SEASO, data = workingdata)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.5843 -0.1192  0.0025  0.1262  1.3959 
## 
## Coefficients: (1 not defined because of singularities)
##                           Estimate Std. Error t value Pr(>|t|)    
## (Intercept)               5.41e+00   2.07e-01   26.11  < 2e-16 ***
## COUNTY_ID123              3.05e-01   8.26e-02    3.70  0.00022 ***
## COUNTY_ID163              4.49e-01   1.70e-02   26.38  < 2e-16 ***
## CITYARDEN HILLS          -4.41e-01   8.52e-02   -5.18  2.3e-07 ***
## CITYBAYPORT               3.68e-02   3.08e-02    1.19  0.23218    
## CITYBIRCHWOOD VILLAGE    -2.22e-01   4.53e-02   -4.91  9.3e-07 ***
## CITYBURNSVILLE           -3.52e-02   1.24e-02   -2.84  0.00451 ** 
## CITYCOTTAGE GROVE        -1.27e-01   1.04e-02  -12.16  < 2e-16 ***
## CITYDELLWOOD             -5.70e-01   2.13e-01   -2.68  0.00734 ** 
## CITYEAGAN                 5.43e-02   1.48e-02    3.67  0.00024 ***
## CITYEMPIRE TOWNSHIP       4.22e-03   4.53e-02    0.09  0.92587    
## CITYFALCON HEIGHTS        4.08e-02   8.61e-02    0.47  0.63564    
## CITYFARMINGTON           -2.65e-02   1.81e-02   -1.47  0.14281    
## CITYGEM LAKE             -6.10e-01   2.28e-01   -2.68  0.00737 ** 
## CITYHUGO                 -2.45e-01   3.68e-02   -6.67  2.7e-11 ***
## CITYINVER GROVE HEIGHTS   2.46e-01   2.14e-02   11.48  < 2e-16 ***
## CITYLAKE ELMO            -1.62e-02   3.72e-02   -0.43  0.66432    
## CITYLAKEVILLE            -2.19e-02   1.56e-02   -1.41  0.15991    
## CITYLAUDERDALE           -1.34e-01   9.21e-02   -1.46  0.14521    
## CITYLITTLE CANADA        -1.78e-01   8.56e-02   -2.08  0.03768 *  
## CITYMAHTOMEDI            -1.00e-01   2.10e-02   -4.78  1.7e-06 ***
## CITYMAPLEWOOD            -2.75e-02   8.24e-02   -0.33  0.73805    
## CITYMENDOTA               9.01e-02   1.24e-01    0.72  0.46851    
## CITYMENDOTA HEIGHTS       2.32e-01   2.92e-02    7.93  2.3e-15 ***
## CITYMOUNDS VIEW          -5.21e-01   8.40e-02   -6.20  5.7e-10 ***
## CITYNEW BRIGHTON         -3.63e-01   8.32e-02   -4.37  1.2e-05 ***
## CITYNEWPORT              -2.65e-01   2.78e-02   -9.54  < 2e-16 ***
## CITYNORTH ST. PAUL        3.74e-03   8.30e-02    0.05  0.96405    
## CITYOAKDALE               1.52e-02   1.24e-02    1.22  0.22112    
## CITYOAK PARK HEIGHTS      9.01e-02   2.94e-02    3.06  0.00220 ** 
## CITYROSEMOUNT             5.85e-02   1.45e-02    4.02  5.7e-05 ***
## CITYROSEVILLE            -1.77e-01   8.29e-02   -2.14  0.03234 *  
## CITYSHOREVIEW            -4.79e-01   8.30e-02   -5.77  8.2e-09 ***
## CITYSOUTH ST PAUL         2.95e-01   2.31e-02   12.76  < 2e-16 ***
## CITYSPRING LAKE PARK     -4.61e-01   1.71e-01   -2.69  0.00712 ** 
## CITYST. ANTHONY          -1.80e-01   1.06e-01   -1.69  0.09100 .  
## CITYSTILLWATER           -2.05e-02   1.59e-02   -1.29  0.19679    
## CITYST. PAUL             -1.47e-02   8.26e-02   -0.18  0.85904    
## CITYST. PAUL PARK        -2.27e-01   1.92e-02  -11.85  < 2e-16 ***
## CITYVADNAIS HEIGHTS      -3.68e-01   8.43e-02   -4.37  1.3e-05 ***
## CITYWEST ST PAUL          3.08e-01   2.66e-02   11.56  < 2e-16 ***
## CITYWHITE BEAR LAKE      -1.22e-01   8.09e-02   -1.50  0.13308    
## CITYWHITE BEAR TOWNSHIP  -3.21e-01   8.35e-02   -3.85  0.00012 ***
## CITYWILLERNIE            -3.14e-01   6.05e-02   -5.18  2.2e-07 ***
## CITYWOODBURY                    NA         NA      NA       NA    
## factor(SALE_YR)2009      -7.85e-02   4.14e-03  -18.95  < 2e-16 ***
## factor(SALE_YR)2010      -1.14e-01   4.46e-03  -25.58  < 2e-16 ***
## ACRES_POLY                8.00e-01   1.02e-01    7.84  4.8e-15 ***
## CBD_dist                  3.05e-05   7.01e-06    4.35  1.4e-05 ***
## I(ACRES_POLY^2)          -2.98e-01   1.38e-01   -2.16  0.03114 *  
## I(CBD_dist^2)            -3.28e-10   8.76e-11   -3.74  0.00019 ***
## log(MAX)                 -2.70e-01   2.75e-02   -9.82  < 2e-16 ***
## HOMESTEADY                2.62e-02   4.67e-03    5.62  2.0e-08 ***
## log(FIN_SQ_FT)            5.55e-01   6.00e-03   92.62  < 2e-16 ***
## YEAR_BUILT                1.61e-03   9.09e-05   17.75  < 2e-16 ***
## LAKE_dist                -7.39e-05   8.42e-06   -8.78  < 2e-16 ***
## I(LAKE_dist^2)            2.79e-08   3.06e-09    9.13  < 2e-16 ***
## PARK_dist                -9.76e-06   3.73e-06   -2.62  0.00889 ** 
## I(PARK_dist^2)            2.43e-09   5.55e-10    4.38  1.2e-05 ***
## MCA3                      4.40e-04   1.36e-04    3.24  0.00120 ** 
## SHOP_dist                 4.28e-05   4.19e-06   10.20  < 2e-16 ***
## I(SHOP_dist^2)           -6.31e-09   6.87e-10   -9.18  < 2e-16 ***
## MED_INCOME                2.25e-06   1.21e-07   18.58  < 2e-16 ***
## COLLEGE_di               -4.03e-05   8.94e-07  -45.13  < 2e-16 ***
## SALE_SEASO2               1.30e-02   5.02e-03    2.60  0.00944 ** 
## SALE_SEASO3               3.15e-03   5.12e-03    0.61  0.53890    
## SALE_SEASO4              -1.93e-02   5.73e-03   -3.37  0.00076 ***
## ACRES_POLY:CBD_dist      -4.12e-05   1.05e-05   -3.93  8.6e-05 ***
## ACRES_POLY:I(CBD_dist^2)  1.07e-09   2.81e-10    3.82  0.00013 ***
## CBD_dist:log(MAX)         4.94e-06   1.57e-06    3.15  0.00162 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 0.212 on 15601 degrees of freedom
## Multiple R-squared: 0.697,	Adjusted R-squared: 0.695 
## F-statistic:  527 on 68 and 15601 DF,  p-value: <2e-16
```

