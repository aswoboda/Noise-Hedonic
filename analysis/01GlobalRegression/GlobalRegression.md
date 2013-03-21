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
    I(ACRES_POLY^2) * CBD_dist + MAX + HOMESTEAD + log(FIN_SQ_FT) + YEAR_BUILT + 
    LAKE_dist + I(LAKE_dist^2) + PARK_dist + I(PARK_dist^2) + MCA3 + SHOP_dist + 
    I(SHOP_dist^2) + MED_INCOME + COLLEGE_di + SALE_MO, data = workingdata20052010)
model.SaleValue20052010.Dakota <- lm(logSALE_VA ~ CITY + factor(SALE_YR) + (ACRES_POLY * 
    CBD_dist) + I(ACRES_POLY^2) * I(CBD_dist^2) + ACRES_POLY * I(CBD_dist^2) + 
    I(ACRES_POLY^2) * CBD_dist + HOMESTEAD + log(FIN_SQ_FT) + YEAR_BUILT + MAX * 
    I(CBD_dist^2) + MAX * CBD_dist + PARK_dist + LAKE_dist + I(LAKE_dist^2) + 
    MCA3 + MED_INCOME + COLLEGE_di + I(COLLEGE_di^2) + SHOP_dist + SALE_MO + 
    BEDS + BATH + I(BATH^2) + BLDG_QUAL, data = workingdata20052010.Dakota)
mfx.TRAFFIC.20052010 = (model.SaleValue20052010$coefficients["MAX"]) * (workingdata20052010$SALE_VALUE)
mfx.LAND.20052010 = (model.SaleValue20052010$coefficients["ACRES_POLY"] + (2 * 
    workingdata20052010$ACRES_POLY * model.SaleValue20052010$coefficients["I(ACRES_POLY^2)"]) + 
    (model.SaleValue20052010$coefficients["ACRES_POLY:CBD_dist"] * workingdata20052010$CBD_dist) + 
    (model.SaleValue20052010$coefficients["ACRES_POLY:I(CBD_dist^2)"] * (workingdata20052010$CBD_dist^2)) + 
    (model.SaleValue20052010$coefficients["CBD_dist:I(ACRES_POLY^2)"] * 2 * 
        workingdata20052010$CBD_dist * workingdata20052010$ACRES_POLY) + (model.SaleValue20052010$coefficients["I(ACRES_POLY^2):I(CBD_dist^2)"] * 
    2 * workingdata20052010$ACRES_POLY * (workingdata20052010$CBD_dist^2))) * 
    (workingdata20052010$SALE_VALUE)
mfx.TRAFFIC.20052010.Dakota = ((model.SaleValue20052010.Dakota$coefficients["MAX"]) + 
    ((workingdata20052010.Dakota$CBD_dist * model.SaleValue20052010.Dakota$coefficients["CBD_dist:MAX"]) + 
        ((workingdata20052010.Dakota$CBD_dist^2 * model.SaleValue20052010.Dakota$coefficients["I(CBD_dist^2):MAX"])))) * 
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
model.SaleValue20052010.Dakota.NoBeds <- lm(logSALE_VA ~ CITY + factor(SALE_YR) + 
    (ACRES_POLY * CBD_dist) + I(ACRES_POLY^2) * I(CBD_dist^2) + ACRES_POLY * 
    I(CBD_dist^2) + I(ACRES_POLY^2) * CBD_dist + HOMESTEAD + log(FIN_SQ_FT) + 
    YEAR_BUILT + MAX * I(CBD_dist^2) + MAX * CBD_dist + PARK_dist + LAKE_dist + 
    I(LAKE_dist^2) + MCA3 + MED_INCOME + COLLEGE_di + I(COLLEGE_di^2) + SHOP_dist + 
    SALE_MO, data = workingdata20052010.Dakota)
mfx.TRAFFIC.20052010.Dakota.NoBeds = ((model.SaleValue20052010.Dakota.NoBeds$coefficients["MAX"]) + 
    ((workingdata20052010.Dakota$CBD_dist * model.SaleValue20052010.Dakota.NoBeds$coefficients["CBD_dist:MAX"]) + 
        ((workingdata20052010.Dakota$CBD_dist^2 * model.SaleValue20052010.Dakota.NoBeds$coefficients["I(CBD_dist^2):MAX"])))) * 
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
##     MAX + HOMESTEAD + log(FIN_SQ_FT) + YEAR_BUILT + LAKE_dist + 
##     I(LAKE_dist^2) + PARK_dist + I(PARK_dist^2) + MCA3 + SHOP_dist + 
##     I(SHOP_dist^2) + MED_INCOME + COLLEGE_di + SALE_MO, data = workingdata20052010)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.5378 -0.1063 -0.0046  0.1037  1.3713 
## 
## Coefficients: (1 not defined because of singularities)
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    4.07e+00   9.67e-02   42.08  < 2e-16 ***
## COUNTY_ID123                   3.50e-01   4.66e-02    7.52  5.7e-14 ***
## COUNTY_ID163                   4.55e-01   8.93e-03   50.93  < 2e-16 ***
## CITYARDEN HILLS               -4.24e-01   4.81e-02   -8.82  < 2e-16 ***
## CITYBAYPORT                   -2.97e-02   1.87e-02   -1.59  0.11120    
## CITYBIRCHWOOD VILLAGE         -1.32e-01   2.79e-02   -4.74  2.2e-06 ***
## CITYBURNSVILLE                -2.00e-02   6.31e-03   -3.17  0.00152 ** 
## CITYCOTTAGE GROVE             -1.16e-01   6.07e-03  -19.07  < 2e-16 ***
## CITYDELLWOOD                  -5.11e-01   1.82e-01   -2.81  0.00502 ** 
## CITYEAGAN                      4.08e-02   7.46e-03    5.47  4.6e-08 ***
## CITYEMPIRE TOWNSHIP           -1.58e-03   2.25e-02   -0.07  0.94397    
## CITYFALCON HEIGHTS            -1.09e-02   4.87e-02   -0.22  0.82230    
## CITYFARMINGTON                 4.40e-03   9.03e-03    0.49  0.62565    
## CITYGEM LAKE                  -3.64e-01   1.15e-01   -3.17  0.00152 ** 
## CITYHUGO                      -2.12e-01   2.03e-02  -10.47  < 2e-16 ***
## CITYINVER GROVE HEIGHTS        2.32e-01   1.07e-02   21.65  < 2e-16 ***
## CITYLAKE ELMO                 -2.60e-02   2.14e-02   -1.21  0.22445    
## CITYLAKEVILLE                 -2.68e-02   7.94e-03   -3.38  0.00072 ***
## CITYLAUDERDALE                -1.38e-01   5.09e-02   -2.72  0.00651 ** 
## CITYLITTLE CANADA             -1.74e-01   4.82e-02   -3.61  0.00030 ***
## CITYMAHTOMEDI                 -5.95e-02   1.16e-02   -5.14  2.7e-07 ***
## CITYMAPLEWOOD                 -4.96e-02   4.66e-02   -1.06  0.28699    
## CITYMENDOTA                    5.88e-02   9.16e-02    0.64  0.52104    
## CITYMENDOTA HEIGHTS            2.26e-01   1.54e-02   14.61  < 2e-16 ***
## CITYMOUNDS VIEW               -4.45e-01   4.73e-02   -9.41  < 2e-16 ***
## CITYNEW BRIGHTON              -3.53e-01   4.69e-02   -7.52  5.5e-14 ***
## CITYNEWPORT                   -2.24e-01   1.56e-02  -14.35  < 2e-16 ***
## CITYNORTH ST. PAUL            -1.66e-02   4.68e-02   -0.35  0.72309    
## CITYOAKDALE                    1.09e-02   7.00e-03    1.56  0.11886    
## CITYOAK PARK HEIGHTS           2.66e-02   1.70e-02    1.57  0.11702    
## CITYROSEMOUNT                  7.67e-02   7.02e-03   10.93  < 2e-16 ***
## CITYROSEVILLE                 -1.89e-01   4.68e-02   -4.05  5.2e-05 ***
## CITYSHOREVIEW                 -4.24e-01   4.69e-02   -9.04  < 2e-16 ***
## CITYSOUTH ST PAUL              2.80e-01   1.19e-02   23.64  < 2e-16 ***
## CITYSPRING LAKE PARK          -4.49e-01   1.15e-01   -3.91  9.1e-05 ***
## CITYST. ANTHONY               -2.27e-01   5.87e-02   -3.87  0.00011 ***
## CITYSTILLWATER                -2.44e-02   8.87e-03   -2.75  0.00594 ** 
## CITYST. PAUL                  -1.49e-02   4.67e-02   -0.32  0.74883    
## CITYST. PAUL PARK             -1.86e-01   1.11e-02  -16.74  < 2e-16 ***
## CITYTOWN OF BAYTOWN            4.51e-02   1.29e-01    0.35  0.72652    
## CITYVADNAIS HEIGHTS           -3.45e-01   4.74e-02   -7.29  3.2e-13 ***
## CITYWEST ST PAUL               2.86e-01   1.39e-02   20.64  < 2e-16 ***
## CITYWHITE BEAR LAKE           -1.48e-01   4.58e-02   -3.24  0.00119 ** 
## CITYWHITE BEAR TOWNSHIP       -3.23e-01   4.71e-02   -6.86  6.8e-12 ***
## CITYWILLERNIE                 -3.07e-01   2.94e-02  -10.44  < 2e-16 ***
## CITYWOODBURY                         NA         NA      NA       NA    
## factor(SALE_YR)2006            6.22e-03   2.60e-03    2.39  0.01669 *  
## factor(SALE_YR)2007           -2.83e-02   2.85e-03   -9.91  < 2e-16 ***
## factor(SALE_YR)2008           -1.46e-01   3.10e-03  -47.24  < 2e-16 ***
## factor(SALE_YR)2009           -2.29e-01   3.11e-03  -73.64  < 2e-16 ***
## factor(SALE_YR)2010           -2.61e-01   3.41e-03  -76.34  < 2e-16 ***
## ACRES_POLY                     1.59e+00   1.38e-01   11.48  < 2e-16 ***
## CBD_dist                       5.44e-05   2.55e-06   21.36  < 2e-16 ***
## I(ACRES_POLY^2)               -1.62e+00   2.50e-01   -6.47  9.7e-11 ***
## I(CBD_dist^2)                 -5.67e-10   7.64e-11   -7.43  1.1e-13 ***
## MAX                           -2.77e-03   1.22e-04  -22.66  < 2e-16 ***
## HOMESTEADY                     2.60e-02   2.53e-03   10.25  < 2e-16 ***
## log(FIN_SQ_FT)                 5.52e-01   3.16e-03  174.62  < 2e-16 ***
## YEAR_BUILT                     1.81e-03   4.82e-05   37.61  < 2e-16 ***
## LAKE_dist                     -7.89e-05   4.30e-06  -18.34  < 2e-16 ***
## I(LAKE_dist^2)                 3.11e-08   1.56e-09   19.96  < 2e-16 ***
## PARK_dist                     -8.80e-06   1.92e-06   -4.58  4.6e-06 ***
## I(PARK_dist^2)                 1.62e-09   2.88e-10    5.64  1.7e-08 ***
## MCA3                           6.84e-04   1.02e-04    6.73  1.7e-11 ***
## SHOP_dist                      4.00e-05   2.23e-06   17.99  < 2e-16 ***
## I(SHOP_dist^2)                -5.71e-09   3.66e-10  -15.60  < 2e-16 ***
## MED_INCOME                     2.02e-06   6.43e-08   31.38  < 2e-16 ***
## COLLEGE_di                    -3.26e-05   4.61e-07  -70.72  < 2e-16 ***
## SALE_MO10                      6.29e-03   5.08e-03    1.24  0.21593    
## SALE_MO11                     -1.96e-03   5.31e-03   -0.37  0.71198    
## SALE_MO12                     -1.61e-02   5.57e-03   -2.89  0.00382 ** 
## SALE_MO2                       3.59e-03   5.47e-03    0.66  0.51133    
## SALE_MO3                       9.71e-03   5.05e-03    1.92  0.05455 .  
## SALE_MO4                       1.19e-02   4.96e-03    2.40  0.01620 *  
## SALE_MO5                       2.10e-02   4.79e-03    4.38  1.2e-05 ***
## SALE_MO6                       2.71e-02   4.68e-03    5.78  7.4e-09 ***
## SALE_MO7                       2.19e-02   4.81e-03    4.54  5.5e-06 ***
## SALE_MO8                       1.49e-02   4.78e-03    3.12  0.00181 ** 
## SALE_MO9                       1.42e-02   4.97e-03    2.85  0.00435 ** 
## ACRES_POLY:CBD_dist           -1.36e-04   1.89e-05   -7.20  6.3e-13 ***
## I(ACRES_POLY^2):I(CBD_dist^2) -3.61e-09   8.78e-10   -4.11  4.0e-05 ***
## ACRES_POLY:I(CBD_dist^2)       3.29e-09   5.31e-10    6.21  5.5e-10 ***
## CBD_dist:I(ACRES_POLY^2)       1.56e-04   3.21e-05    4.86  1.2e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 0.182 on 42013 degrees of freedom
## Multiple R-squared: 0.751,	Adjusted R-squared: 0.751 
## F-statistic: 1.56e+03 on 81 and 42013 DF,  p-value: <2e-16
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
##     log(FIN_SQ_FT) + YEAR_BUILT + MAX * I(CBD_dist^2) + MAX * 
##     CBD_dist + PARK_dist + LAKE_dist + I(LAKE_dist^2) + MCA3 + 
##     MED_INCOME + COLLEGE_di + I(COLLEGE_di^2) + SHOP_dist + SALE_MO + 
##     BEDS + BATH + I(BATH^2) + BLDG_QUAL, data = workingdata20052010.Dakota)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -0.7894 -0.0709 -0.0017  0.0664  0.9220 
## 
## Coefficients:
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    5.91e+00   2.20e-01   26.93  < 2e-16 ***
## CITYBURNSVILLE                 3.32e-03   4.47e-03    0.74  0.45705    
## CITYEAGAN                      4.31e-02   6.58e-03    6.55  5.8e-11 ***
## CITYEMPIRE TOWNSHIP           -3.84e-02   1.69e-02   -2.28  0.02286 *  
## CITYFARMINGTON                -3.27e-02   8.14e-03   -4.02  5.9e-05 ***
## CITYINVER GROVE HEIGHTS        5.09e-02   1.12e-02    4.52  6.1e-06 ***
## CITYLAKEVILLE                  1.65e-02   5.82e-03    2.83  0.00472 ** 
## CITYMENDOTA                    2.42e-01   8.26e-02    2.93  0.00342 ** 
## CITYMENDOTA HEIGHTS            1.39e-01   1.67e-02    8.32  < 2e-16 ***
## CITYROSEMOUNT                  1.84e-02   5.82e-03    3.16  0.00159 ** 
## CITYSOUTH ST PAUL              2.69e-02   1.65e-02    1.63  0.10327    
## CITYWEST ST PAUL               4.51e-02   1.94e-02    2.33  0.02008 *  
## factor(SALE_YR)2006            1.15e-02   2.83e-03    4.04  5.3e-05 ***
## factor(SALE_YR)2007           -1.61e-02   3.05e-03   -5.26  1.5e-07 ***
## factor(SALE_YR)2008           -1.05e-01   3.97e-03  -26.36  < 2e-16 ***
## factor(SALE_YR)2009           -1.91e-01   4.26e-03  -44.98  < 2e-16 ***
## factor(SALE_YR)2010           -2.19e-01   4.54e-03  -48.20  < 2e-16 ***
## ACRES_POLY                    -1.41e-01   2.48e-01   -0.57  0.56820    
## CBD_dist                       1.63e-05   6.44e-06    2.53  0.01142 *  
## I(ACRES_POLY^2)                9.86e-01   4.13e-01    2.39  0.01693 *  
## I(CBD_dist^2)                 -5.17e-10   1.64e-10   -3.14  0.00167 ** 
## HOMESTEADY                     5.64e-03   4.08e-03    1.38  0.16699    
## log(FIN_SQ_FT)                 3.11e-01   6.60e-03   47.05  < 2e-16 ***
## YEAR_BUILT                     2.05e-03   9.43e-05   21.74  < 2e-16 ***
## MAX                           -5.47e-04   6.46e-04   -0.85  0.39694    
## PARK_dist                     -7.51e-06   1.07e-06   -7.00  2.7e-12 ***
## LAKE_dist                     -3.55e-05   4.56e-06   -7.79  7.5e-15 ***
## I(LAKE_dist^2)                 1.21e-08   1.50e-09    8.05  9.0e-16 ***
## MCA3                          -9.73e-04   2.94e-04   -3.31  0.00093 ***
## MED_INCOME                     5.23e-07   7.67e-08    6.82  9.6e-12 ***
## COLLEGE_di                    -1.02e-05   3.79e-06   -2.70  0.00687 ** 
## I(COLLEGE_di^2)                2.32e-10   1.09e-10    2.13  0.03309 *  
## SHOP_dist                      3.69e-06   1.05e-06    3.51  0.00046 ***
## SALE_MO10                     -2.63e-03   5.95e-03   -0.44  0.65823    
## SALE_MO11                      1.52e-03   6.18e-03    0.25  0.80514    
## SALE_MO12                     -1.64e-02   6.62e-03   -2.47  0.01350 *  
## SALE_MO2                      -3.21e-03   6.37e-03   -0.50  0.61414    
## SALE_MO3                       6.36e-03   5.77e-03    1.10  0.27025    
## SALE_MO4                       8.42e-03   5.72e-03    1.47  0.14099    
## SALE_MO5                       7.49e-03   5.54e-03    1.35  0.17601    
## SALE_MO6                       1.52e-02   5.38e-03    2.83  0.00469 ** 
## SALE_MO7                       1.41e-02   5.56e-03    2.53  0.01141 *  
## SALE_MO8                       1.36e-02   5.57e-03    2.44  0.01487 *  
## SALE_MO9                       9.92e-03   5.73e-03    1.73  0.08356 .  
## BEDS                           3.61e-03   1.67e-03    2.16  0.03042 *  
## BATH                          -3.29e-02   1.12e-02   -2.95  0.00317 ** 
## I(BATH^2)                      1.34e-02   2.29e-03    5.84  5.5e-09 ***
## BLDG_QUAL                      1.51e-01   2.08e-03   72.61  < 2e-16 ***
## ACRES_POLY:CBD_dist            6.46e-05   2.81e-05    2.30  0.02136 *  
## I(ACRES_POLY^2):I(CBD_dist^2)  2.72e-09   1.06e-09    2.55  0.01072 *  
## ACRES_POLY:I(CBD_dist^2)      -1.41e-09   6.85e-10   -2.06  0.03943 *  
## CBD_dist:I(ACRES_POLY^2)      -1.30e-04   4.43e-05   -2.94  0.00329 ** 
## I(CBD_dist^2):MAX              9.92e-12   1.62e-12    6.12  9.6e-10 ***
## CBD_dist:MAX                  -3.04e-07   6.74e-08   -4.51  6.4e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 0.114 on 11983 degrees of freedom
## Multiple R-squared: 0.863,	Adjusted R-squared: 0.863 
## F-statistic: 1.43e+03 on 53 and 11983 DF,  p-value: <2e-16
```

```r
summary(model.SaleValue20052010.Dakota.NoBeds)
```

```
## 
## Call:
## lm(formula = logSALE_VA ~ CITY + factor(SALE_YR) + (ACRES_POLY * 
##     CBD_dist) + I(ACRES_POLY^2) * I(CBD_dist^2) + ACRES_POLY * 
##     I(CBD_dist^2) + I(ACRES_POLY^2) * CBD_dist + HOMESTEAD + 
##     log(FIN_SQ_FT) + YEAR_BUILT + MAX * I(CBD_dist^2) + MAX * 
##     CBD_dist + PARK_dist + LAKE_dist + I(LAKE_dist^2) + MCA3 + 
##     MED_INCOME + COLLEGE_di + I(COLLEGE_di^2) + SHOP_dist + SALE_MO, 
##     data = workingdata20052010.Dakota)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -0.7703 -0.0913 -0.0160  0.0758  0.8880 
## 
## Coefficients:
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -2.20e-01   2.50e-01   -0.88  0.37966    
## CITYBURNSVILLE                -2.75e-02   5.48e-03   -5.02  5.3e-07 ***
## CITYEAGAN                      2.88e-02   8.11e-03    3.55  0.00038 ***
## CITYEMPIRE TOWNSHIP           -1.10e-01   2.08e-02   -5.27  1.4e-07 ***
## CITYFARMINGTON                -4.03e-02   1.00e-02   -4.02  5.7e-05 ***
## CITYINVER GROVE HEIGHTS        7.92e-02   1.39e-02    5.72  1.1e-08 ***
## CITYLAKEVILLE                 -4.97e-03   7.17e-03   -0.69  0.48840    
## CITYMENDOTA                    1.70e-01   1.02e-01    1.67  0.09535 .  
## CITYMENDOTA HEIGHTS            1.17e-01   2.06e-02    5.69  1.3e-08 ***
## CITYROSEMOUNT                  2.17e-02   7.18e-03    3.03  0.00246 ** 
## CITYSOUTH ST PAUL              7.92e-02   2.04e-02    3.89  0.00010 ***
## CITYWEST ST PAUL               4.40e-02   2.39e-02    1.84  0.06597 .  
## factor(SALE_YR)2006            1.22e-02   3.50e-03    3.50  0.00047 ***
## factor(SALE_YR)2007           -1.76e-02   3.77e-03   -4.66  3.1e-06 ***
## factor(SALE_YR)2008           -1.19e-01   4.89e-03  -24.35  < 2e-16 ***
## factor(SALE_YR)2009           -2.05e-01   5.25e-03  -39.00  < 2e-16 ***
## factor(SALE_YR)2010           -2.29e-01   5.61e-03  -40.93  < 2e-16 ***
## ACRES_POLY                    -2.36e+00   3.04e-01   -7.76  9.5e-15 ***
## CBD_dist                      -1.07e-05   7.94e-06   -1.35  0.17863    
## I(ACRES_POLY^2)                4.12e+00   5.07e-01    8.13  4.7e-16 ***
## I(CBD_dist^2)                  9.31e-11   2.03e-10    0.46  0.64620    
## HOMESTEADY                     1.01e-02   5.04e-03    2.01  0.04445 *  
## log(FIN_SQ_FT)                 5.40e-01   5.80e-03   93.06  < 2e-16 ***
## YEAR_BUILT                     4.64e-03   1.07e-04   43.30  < 2e-16 ***
## MAX                            9.07e-04   7.97e-04    1.14  0.25527    
## PARK_dist                     -1.52e-05   1.32e-06  -11.56  < 2e-16 ***
## LAKE_dist                     -4.24e-05   5.63e-06   -7.54  5.1e-14 ***
## I(LAKE_dist^2)                 1.98e-08   1.84e-09   10.71  < 2e-16 ***
## MCA3                          -5.00e-04   3.62e-04   -1.38  0.16748    
## MED_INCOME                     6.85e-07   9.46e-08    7.24  4.9e-13 ***
## COLLEGE_di                    -1.64e-05   4.67e-06   -3.50  0.00047 ***
## I(COLLEGE_di^2)                2.89e-10   1.34e-10    2.15  0.03161 *  
## SHOP_dist                      3.56e-06   1.30e-06    2.73  0.00627 ** 
## SALE_MO10                     -9.77e-03   7.34e-03   -1.33  0.18325    
## SALE_MO11                      1.41e-03   7.63e-03    0.19  0.85305    
## SALE_MO12                     -2.69e-02   8.17e-03   -3.29  0.00101 ** 
## SALE_MO2                      -6.02e-03   7.86e-03   -0.77  0.44420    
## SALE_MO3                      -8.51e-04   7.12e-03   -0.12  0.90491    
## SALE_MO4                      -6.57e-04   7.06e-03   -0.09  0.92593    
## SALE_MO5                      -6.85e-04   6.84e-03   -0.10  0.92020    
## SALE_MO6                       9.85e-03   6.64e-03    1.48  0.13803    
## SALE_MO7                       7.60e-03   6.86e-03    1.11  0.26778    
## SALE_MO8                       7.71e-03   6.88e-03    1.12  0.26223    
## SALE_MO9                       4.86e-03   7.08e-03    0.69  0.49226    
## ACRES_POLY:CBD_dist            2.66e-04   3.45e-05    7.72  1.3e-14 ***
## I(ACRES_POLY^2):I(CBD_dist^2)  7.92e-09   1.31e-09    6.04  1.6e-09 ***
## ACRES_POLY:I(CBD_dist^2)      -5.09e-09   8.43e-10   -6.04  1.6e-09 ***
## CBD_dist:I(ACRES_POLY^2)      -4.05e-04   5.45e-05   -7.44  1.1e-13 ***
## I(CBD_dist^2):MAX              1.32e-11   2.00e-12    6.62  3.7e-11 ***
## CBD_dist:MAX                  -4.84e-07   8.31e-08   -5.82  6.0e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 0.141 on 11987 degrees of freedom
## Multiple R-squared: 0.791,	Adjusted R-squared: 0.791 
## F-statistic:  928 on 49 and 11987 DF,  p-value: <2e-16
```


Looking at the marginal effects of the two models, note that the marginal effect for both land and traffic noise decreases on average in the model that includes beds, baths, and building quality.


```r
# Traffic Noise
summary(mfx.TRAFFIC.20052010)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   -1870    -871    -667    -734    -539    -273
```

```r
summary(mfx.TRAFFIC.20052010.Dakota)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   -1930    -692    -458    -460    -251     981
```

```r
summary(mfx.TRAFFIC.20052010.Dakota.NoBeds)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   -2330    -889    -656    -663    -391     604
```

```r
# Land Size
summary(mfx.LAND.20052010)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  -80800   63400   88200  100000  126000  672000
```

```r
summary(mfx.LAND.20052010.Dakota)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  -45000   55100   70800   72800   87800  394000
```


