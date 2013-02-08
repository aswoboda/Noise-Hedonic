Global Regression Models -- 2005 to 2010
========================================================

This file displays two global regression models from the 2005 to 2010 dataset: (1) that does not include beds, baths, and measurement of building quality (2) includes beds, baths, and measurement of building quality. The latter model only contains properties in Dakota County.


```r
library(foreign)
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

```r
workingdata20052010 <- read.dbf("../../../Data/R2GIS/CleanData/Sales20052010.dbf")
workingdata20052010.Dakota <- read.dbf("../../../Data/R2GIS/CleanData/Sales20052010_Dakota.dbf")
model.SaleValue20052010 <- lm(logSALE_VA ~ COUNTY_ID + CITY + factor(SALE_YR) + 
    ACRES_POLY * CBD_dist + I(ACRES_POLY^2) * I(CBD_dist^2) + ACRES_POLY * I(CBD_dist^2) + 
    I(ACRES_POLY^2) * CBD_dist + log(MAX) + HOMESTEAD + log(FIN_SQ_FT) + YEAR_BUILT + 
    LAKE_dist + I(LAKE_dist^2) + PARK_dist + I(PARK_dist^2) + MCA3 + SHOP_dist + 
    I(SHOP_dist^2) + MED_INCOME + COLLEGE_di + SALE_MO, data = workingdata20052010)
model.SaleValue20052010.Dakota <- lm(logSALE_VA ~ CITY + factor(SALE_YR) + (ACRES_POLY * 
    CBD_dist) + I(ACRES_POLY^2) * I(CBD_dist^2) + ACRES_POLY * I(CBD_dist^2) + 
    I(ACRES_POLY^2) * CBD_dist + HOMESTEAD + log(FIN_SQ_FT) + YEAR_BUILT + log(MAX) * 
    I(CBD_dist^2) + log(MAX) * CBD_dist + PARK_dist + LAKE_dist + I(LAKE_dist^2) + 
    MCA3 + MED_INCOME + COLLEGE_di + I(COLLEGE_di^2) + SHOP_dist + SALE_MO + 
    BEDS + BATH + I(BATH^2) + BLDG_QUAL, data = workingdata20052010.Dakota)
mfx.TRAFFIC.20052010 = (model.SaleValue20052010$coefficients["log(MAX)"]/workingdata20052010$MAX) * 
    (workingdata20052010$SALE_VALUE)
mfx.LAND.20052010 = (model.SaleValue20052010$coefficients["ACRES_POLY"] + (2 * 
    workingdata20052010$ACRES_POLY * model.SaleValue20052010$coefficients["I(ACRES_POLY^2)"]) + 
    (model.SaleValue20052010$coefficients["ACRES_POLY:CBD_dist"] * workingdata20052010$CBD_dist) + 
    (model.SaleValue20052010$coefficients["ACRES_POLY:I(CBD_dist^2)"] * (workingdata20052010$CBD_dist^2)) + 
    (model.SaleValue20052010$coefficients["CBD_dist:I(ACRES_POLY^2)"] * 2 * 
        workingdata20052010$CBD_dist * workingdata20052010$ACRES_POLY) + (model.SaleValue20052010$coefficients["I(ACRES_POLY^2):I(CBD_dist^2)"] * 
    2 * workingdata20052010$ACRES_POLY * (workingdata20052010$CBD_dist^2))) * 
    (workingdata20052010$SALE_VALUE)
mfx.TRAFFIC.20052010.Dakota = ((model.SaleValue20052010.Dakota$coefficients["log(MAX)"]/workingdata20052010.Dakota$MAX) + 
    ((workingdata20052010.Dakota$CBD_dist * model.SaleValue20052010.Dakota$coefficients["CBD_dist:log(MAX)"])/workingdata20052010.Dakota$MAX) + 
    ((workingdata20052010.Dakota$CBD_dist^2 * model.SaleValue20052010.Dakota$coefficients["I(CBD_dist^2):log(MAX)"])/workingdata20052010.Dakota$MAX)) * 
    (workingdata20052010.Dakota$SALE_VALUE)
mfx.LAND.20052010.Dakota = (model.SaleValue20052010.Dakota$coefficients["ACRES_POLY"] + 
    (2 * workingdata20052010.Dakota$ACRES_POLY * model.SaleValue20052010.Dakota$coefficients["I(ACRES_POLY^2)"]) + 
    (model.SaleValue20052010.Dakota$coefficients["ACRES_POLY:CBD_dist"] * workingdata20052010.Dakota$CBD_dist) + 
    (model.SaleValue20052010.Dakota$coefficients["ACRES_POLY:I(CBD_dist^2)"] * 
        (workingdata20052010.Dakota$CBD_dist^2)) + (model.SaleValue20052010.Dakota$coefficients["CBD_dist:I(ACRES_POLY^2)"] * 
    2 * workingdata20052010.Dakota$CBD_dist * workingdata20052010.Dakota$ACRES_POLY) + 
    (model.SaleValue20052010.Dakota$coefficients["I(ACRES_POLY^2):I(CBD_dist^2)"] * 
        2 * workingdata20052010.Dakota$ACRES_POLY * (workingdata20052010.Dakota$CBD_dist^2))) * 
    (workingdata20052010.Dakota$SALE_VALUE)
```


Here we compare the two outputs from the regression models. Note that the inclusion of beds, baths, and building quality causes a large increase in the amount of variance in log(Sale Value) explained by the model. Also, the model that includes beds and baths has significant interaction terms with traffic noise whereas the other model does not.


```r
summary(model.SaleValue20052010)
```

```
## 
## Call:
## lm(formula = logSALE_VA ~ COUNTY_ID + CITY + factor(SALE_YR) + 
##     ACRES_POLY * CBD_dist + I(ACRES_POLY^2) * I(CBD_dist^2) + 
##     ACRES_POLY * I(CBD_dist^2) + I(ACRES_POLY^2) * CBD_dist + 
##     log(MAX) + HOMESTEAD + log(FIN_SQ_FT) + YEAR_BUILT + LAKE_dist + 
##     I(LAKE_dist^2) + PARK_dist + I(PARK_dist^2) + MCA3 + SHOP_dist + 
##     I(SHOP_dist^2) + MED_INCOME + COLLEGE_di + SALE_MO, data = workingdata20052010)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -1.547 -0.106 -0.005  0.103  1.369 
## 
## Coefficients: (1 not defined because of singularities)
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    4.57e+00   1.02e-01   44.76  < 2e-16 ***
## COUNTY_ID123                   2.63e-01   6.95e-02    3.79  0.00015 ***
## COUNTY_ID163                   4.39e-01   9.60e-03   45.68  < 2e-16 ***
## CITYARDEN HILLS               -3.38e-01   7.05e-02   -4.79  1.7e-06 ***
## CITYBAYPORT                   -4.15e-03   2.42e-02   -0.17  0.86394    
## CITYBIRCHWOOD VILLAGE         -1.61e-01   3.84e-02   -4.21  2.6e-05 ***
## CITYBURNSVILLE                -2.03e-02   6.30e-03   -3.22  0.00128 ** 
## CITYCITY OF BAYPORT           -2.70e-02   2.60e-02   -1.04  0.29968    
## CITYCITY OF BIRCHWOOD         -8.09e-02   4.02e-02   -2.01  0.04398 *  
## CITYCITY OF COTTAGE GROVE     -7.73e-02   8.19e-03   -9.44  < 2e-16 ***
## CITYCITY OF HUGO              -1.81e-01   2.17e-02   -8.32  < 2e-16 ***
## CITYCITY OF LAKE ELMO          5.44e-03   2.88e-02    0.19  0.85024    
## CITYCITY OF MAHTOMEDI         -4.19e-02   1.50e-02   -2.79  0.00535 ** 
## CITYCITY OF NEWPORT           -1.63e-01   2.06e-02   -7.90  2.9e-15 ***
## CITYCITY OF OAKDALE            4.51e-02   9.42e-03    4.79  1.7e-06 ***
## CITYCITY OF OAK PARK HEIGHTS   2.29e-02   2.23e-02    1.03  0.30416    
## CITYCITY OF STILLWATER         4.61e-03   1.10e-02    0.42  0.67534    
## CITYCITY OF ST PAUL PARK      -1.29e-01   1.51e-02   -8.59  < 2e-16 ***
## CITYCITY OF WHITE BEAR LAKE   -2.03e-01   6.09e-02   -3.33  0.00088 ***
## CITYCITY OF WILLERNIE         -3.02e-01   3.54e-02   -8.54  < 2e-16 ***
## CITYCITY OF WOODBURY           2.32e-02   6.48e-03    3.58  0.00034 ***
## CITYCOTTAGE GROVE             -1.31e-01   8.04e-03  -16.33  < 2e-16 ***
## CITYDELLWOOD                  -5.08e-01   1.82e-01   -2.79  0.00524 ** 
## CITYEAGAN                      4.13e-02   7.46e-03    5.54  3.1e-08 ***
## CITYEMPIRE TOWNSHIP           -2.63e-03   2.25e-02   -0.12  0.90693    
## CITYFALCON HEIGHTS             7.42e-02   7.09e-02    1.05  0.29575    
## CITYFARMINGTON                 3.39e-03   9.02e-03    0.38  0.70722    
## CITYGEM LAKE                  -2.79e-01   1.26e-01   -2.22  0.02663 *  
## CITYHUGO                      -2.23e-01   2.18e-02  -10.23  < 2e-16 ***
## CITYINVER GROVE HEIGHTS        2.32e-01   1.07e-02   21.67  < 2e-16 ***
## CITYLAKE ELMO                 -3.05e-02   3.14e-02   -0.97  0.33169    
## CITYLAKEVILLE                 -2.69e-02   7.93e-03   -3.39  0.00069 ***
## CITYLAUDERDALE                -5.24e-02   7.25e-02   -0.72  0.46991    
## CITYLITTLE CANADA             -8.75e-02   7.06e-02   -1.24  0.21515    
## CITYMAHTOMEDI                 -5.16e-02   1.70e-02   -3.04  0.00239 ** 
## CITYMAPLEWOOD                  3.67e-02   6.95e-02    0.53  0.59751    
## CITYMENDOTA                    5.54e-02   9.15e-02    0.61  0.54469    
## CITYMENDOTA HEIGHTS            2.25e-01   1.54e-02   14.59  < 2e-16 ***
## CITYMOUNDS VIEW               -3.59e-01   7.00e-02   -5.12  3.0e-07 ***
## CITYNEW BRIGHTON              -2.67e-01   6.97e-02   -3.83  0.00013 ***
## CITYNEWPORT                   -2.71e-01   2.33e-02  -11.64  < 2e-16 ***
## CITYNORTH ST. PAUL             6.98e-02   6.97e-02    1.00  0.31636    
## CITYOAKDALE                    7.83e-04   9.85e-03    0.08  0.93658    
## CITYOAK PARK HEIGHTS           5.89e-02   2.34e-02    2.52  0.01181 *  
## CITYROSEMOUNT                  7.66e-02   7.01e-03   10.93  < 2e-16 ***
## CITYROSEVILLE                 -1.04e-01   6.97e-02   -1.49  0.13706    
## CITYSHOREVIEW                 -3.38e-01   6.97e-02   -4.85  1.2e-06 ***
## CITYSOUTH ST PAUL              2.79e-01   1.18e-02   23.55  < 2e-16 ***
## CITYSPRING LAKE PARK          -3.67e-01   1.26e-01   -2.92  0.00349 ** 
## CITYST. ANTHONY               -1.41e-01   7.81e-02   -1.80  0.07210 .  
## CITYSTILLWATER                -2.65e-02   1.11e-02   -2.38  0.01718 *  
## CITYST. PAUL                   7.05e-02   6.96e-02    1.01  0.31070    
## CITYST. PAUL PARK             -2.25e-01   1.59e-02  -14.20  < 2e-16 ***
## CITYTOWN OF BAYTOWN            6.12e-02   1.29e-01    0.47  0.63502    
## CITYVADNAIS HEIGHTS           -2.59e-01   7.00e-02   -3.70  0.00021 ***
## CITYWEST ST PAUL               2.84e-01   1.39e-02   20.53  < 2e-16 ***
## CITYWHITE BEAR LAKE           -6.22e-02   6.90e-02   -0.90  0.36709    
## CITYWHITE BEAR TOWNSHIP       -2.37e-01   6.98e-02   -3.40  0.00068 ***
## CITYWILLERNIE                 -2.63e-01   5.11e-02   -5.16  2.5e-07 ***
## CITYWOODBURY                         NA         NA      NA       NA    
## factor(SALE_YR)2006            6.56e-03   2.60e-03    2.53  0.01151 *  
## factor(SALE_YR)2007           -2.75e-02   2.85e-03   -9.63  < 2e-16 ***
## factor(SALE_YR)2008           -1.38e-01   3.29e-03  -41.83  < 2e-16 ***
## factor(SALE_YR)2009           -2.20e-01   3.31e-03  -66.33  < 2e-16 ***
## factor(SALE_YR)2010           -2.52e-01   3.59e-03  -70.29  < 2e-16 ***
## ACRES_POLY                     1.56e+00   1.38e-01   11.30  < 2e-16 ***
## CBD_dist                       5.37e-05   2.55e-06   21.10  < 2e-16 ***
## I(ACRES_POLY^2)               -1.59e+00   2.50e-01   -6.35  2.2e-10 ***
## I(CBD_dist^2)                 -5.53e-10   7.63e-11   -7.24  4.6e-13 ***
## log(MAX)                      -1.61e-01   6.97e-03  -23.10  < 2e-16 ***
## HOMESTEADY                     2.88e-02   2.56e-03   11.25  < 2e-16 ***
## log(FIN_SQ_FT)                 5.52e-01   3.16e-03  174.74  < 2e-16 ***
## YEAR_BUILT                     1.81e-03   4.82e-05   37.62  < 2e-16 ***
## LAKE_dist                     -7.80e-05   4.30e-06  -18.16  < 2e-16 ***
## I(LAKE_dist^2)                 3.09e-08   1.56e-09   19.83  < 2e-16 ***
## PARK_dist                     -8.80e-06   1.92e-06   -4.59  4.5e-06 ***
## I(PARK_dist^2)                 1.64e-09   2.87e-10    5.70  1.2e-08 ***
## MCA3                           6.66e-04   1.02e-04    6.56  5.4e-11 ***
## SHOP_dist                      4.01e-05   2.23e-06   18.01  < 2e-16 ***
## I(SHOP_dist^2)                -5.73e-09   3.66e-10  -15.66  < 2e-16 ***
## MED_INCOME                     2.00e-06   6.42e-08   31.20  < 2e-16 ***
## COLLEGE_di                    -3.26e-05   4.60e-07  -70.88  < 2e-16 ***
## SALE_MO10                      6.12e-03   5.08e-03    1.21  0.22803    
## SALE_MO11                     -2.09e-03   5.30e-03   -0.39  0.69401    
## SALE_MO12                     -1.60e-02   5.57e-03   -2.88  0.00401 ** 
## SALE_MO2                       3.65e-03   5.46e-03    0.67  0.50353    
## SALE_MO3                       9.61e-03   5.04e-03    1.91  0.05673 .  
## SALE_MO4                       1.19e-02   4.96e-03    2.41  0.01604 *  
## SALE_MO5                       2.07e-02   4.78e-03    4.34  1.5e-05 ***
## SALE_MO6                       2.67e-02   4.68e-03    5.70  1.2e-08 ***
## SALE_MO7                       2.15e-02   4.81e-03    4.48  7.7e-06 ***
## SALE_MO8                       1.47e-02   4.78e-03    3.08  0.00208 ** 
## SALE_MO9                       1.41e-02   4.97e-03    2.84  0.00448 ** 
## ACRES_POLY:CBD_dist           -1.33e-04   1.89e-05   -7.02  2.2e-12 ***
## I(ACRES_POLY^2):I(CBD_dist^2) -3.54e-09   8.78e-10   -4.03  5.5e-05 ***
## ACRES_POLY:I(CBD_dist^2)       3.23e-09   5.30e-10    6.09  1.1e-09 ***
## CBD_dist:I(ACRES_POLY^2)       1.52e-04   3.21e-05    4.74  2.2e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 0.182 on 41999 degrees of freedom
## Multiple R-squared: 0.752,	Adjusted R-squared: 0.751 
## F-statistic: 1.34e+03 on 95 and 41999 DF,  p-value: <2e-16
```

```r
summary(model.SaleValue20052010.Dakota)
```

```
## 
## Call:
## lm(formula = logSALE_VA ~ CITY + factor(SALE_YR) + (ACRES_POLY * 
##     CBD_dist) + I(ACRES_POLY^2) * I(CBD_dist^2) + ACRES_POLY * 
##     I(CBD_dist^2) + I(ACRES_POLY^2) * CBD_dist + HOMESTEAD + 
##     log(FIN_SQ_FT) + YEAR_BUILT + log(MAX) * I(CBD_dist^2) + 
##     log(MAX) * CBD_dist + PARK_dist + LAKE_dist + I(LAKE_dist^2) + 
##     MCA3 + MED_INCOME + COLLEGE_di + I(COLLEGE_di^2) + SHOP_dist + 
##     SALE_MO + BEDS + BATH + I(BATH^2) + BLDG_QUAL, data = workingdata20052010.Dakota)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -0.8013 -0.0706 -0.0012  0.0662  0.9212 
## 
## Coefficients:
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    6.07e+00   2.65e-01   22.90  < 2e-16 ***
## CITYBURNSVILLE                 2.73e-03   4.35e-03    0.63  0.53021    
## CITYEAGAN                      4.29e-02   6.44e-03    6.66  2.8e-11 ***
## CITYEMPIRE TOWNSHIP           -4.54e-02   1.67e-02   -2.72  0.00647 ** 
## CITYFARMINGTON                -3.69e-02   7.97e-03   -4.63  3.7e-06 ***
## CITYINVER GROVE HEIGHTS        4.92e-02   1.10e-02    4.48  7.6e-06 ***
## CITYLAKEVILLE                  1.37e-02   5.75e-03    2.39  0.01702 *  
## CITYMENDOTA                    2.30e-01   5.97e-02    3.85  0.00012 ***
## CITYMENDOTA HEIGHTS            1.45e-01   1.62e-02    8.93  < 2e-16 ***
## CITYROSEMOUNT                  1.65e-02   5.73e-03    2.88  0.00394 ** 
## CITYSOUTH ST PAUL              1.96e-02   1.58e-02    1.24  0.21501    
## CITYWEST ST PAUL               4.35e-02   1.85e-02    2.35  0.01898 *  
## factor(SALE_YR)2006            1.05e-02   2.77e-03    3.80  0.00014 ***
## factor(SALE_YR)2007           -1.54e-02   2.98e-03   -5.17  2.4e-07 ***
## factor(SALE_YR)2008           -1.04e-01   3.88e-03  -26.80  < 2e-16 ***
## factor(SALE_YR)2009           -1.92e-01   4.16e-03  -46.16  < 2e-16 ***
## factor(SALE_YR)2010           -2.20e-01   4.46e-03  -49.39  < 2e-16 ***
## ACRES_POLY                    -1.16e-01   2.31e-01   -0.50  0.61460    
## CBD_dist                       8.05e-05   1.62e-05    4.98  6.6e-07 ***
## I(ACRES_POLY^2)                1.00e+00   3.83e-01    2.61  0.00911 ** 
## I(CBD_dist^2)                 -2.47e-09   3.93e-10   -6.29  3.3e-10 ***
## HOMESTEADY                     7.02e-03   3.97e-03    1.77  0.07713 .  
## log(FIN_SQ_FT)                 3.11e-01   6.42e-03   48.50  < 2e-16 ***
## YEAR_BUILT                     1.96e-03   9.06e-05   21.60  < 2e-16 ***
## log(MAX)                      -8.96e-03   3.71e-02   -0.24  0.80931    
## PARK_dist                     -6.75e-06   1.05e-06   -6.42  1.4e-10 ***
## LAKE_dist                     -3.58e-05   4.46e-06   -8.03  1.1e-15 ***
## I(LAKE_dist^2)                 1.19e-08   1.48e-09    8.05  8.7e-16 ***
## MCA3                          -9.59e-04   2.87e-04   -3.35  0.00082 ***
## MED_INCOME                     5.17e-07   7.49e-08    6.90  5.3e-12 ***
## COLLEGE_di                    -9.52e-06   3.65e-06   -2.61  0.00909 ** 
## I(COLLEGE_di^2)                2.17e-10   1.05e-10    2.07  0.03866 *  
## SHOP_dist                      3.93e-06   1.03e-06    3.80  0.00015 ***
## SALE_MO10                     -1.23e-03   5.81e-03   -0.21  0.83283    
## SALE_MO11                      3.44e-03   6.05e-03    0.57  0.56909    
## SALE_MO12                     -1.41e-02   6.46e-03   -2.18  0.02914 *  
## SALE_MO2                      -2.41e-03   6.23e-03   -0.39  0.69926    
## SALE_MO3                       8.47e-03   5.65e-03    1.50  0.13376    
## SALE_MO4                       1.09e-02   5.61e-03    1.93  0.05317 .  
## SALE_MO5                       9.25e-03   5.43e-03    1.70  0.08848 .  
## SALE_MO6                       1.55e-02   5.28e-03    2.94  0.00333 ** 
## SALE_MO7                       1.68e-02   5.46e-03    3.09  0.00203 ** 
## SALE_MO8                       1.59e-02   5.45e-03    2.91  0.00360 ** 
## SALE_MO9                       1.31e-02   5.62e-03    2.32  0.02031 *  
## BEDS                           4.17e-03   1.63e-03    2.55  0.01070 *  
## BATH                          -3.32e-02   1.08e-02   -3.07  0.00213 ** 
## I(BATH^2)                      1.35e-02   2.23e-03    6.04  1.6e-09 ***
## BLDG_QUAL                      1.51e-01   2.04e-03   74.30  < 2e-16 ***
## ACRES_POLY:CBD_dist            6.26e-05   2.67e-05    2.35  0.01896 *  
## I(ACRES_POLY^2):I(CBD_dist^2)  2.80e-09   1.02e-09    2.75  0.00602 ** 
## ACRES_POLY:I(CBD_dist^2)      -1.38e-09   6.57e-10   -2.10  0.03603 *  
## CBD_dist:I(ACRES_POLY^2)      -1.33e-04   4.20e-05   -3.17  0.00151 ** 
## I(CBD_dist^2):log(MAX)         6.20e-10   9.31e-11    6.66  2.9e-11 ***
## CBD_dist:log(MAX)             -2.00e-05   3.87e-06   -5.16  2.5e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 0.115 on 12699 degrees of freedom
## Multiple R-squared: 0.862,	Adjusted R-squared: 0.861 
## F-statistic: 1.49e+03 on 53 and 12699 DF,  p-value: <2e-16
```


Looking at the marginal effects of the two models, note that the marginal effect for both land and traffic noise decreases on average in the model that includes beds, baths, and building quality.


```r
# Traffic Noise
summary(mfx.TRAFFIC.20052010)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   -3760    -936    -692    -783    -537    -199
```

```r
summary(mfx.TRAFFIC.20052010.Dakota)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   -2520    -681    -450    -473    -255    1080
```

```r
# Land Size
summary(mfx.LAND.20052010)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  -77100   63900   88600  100000  125000  663000
```

```r
summary(mfx.LAND.20052010.Dakota)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  -53400   54500   69700   72000   86400  406000
```


