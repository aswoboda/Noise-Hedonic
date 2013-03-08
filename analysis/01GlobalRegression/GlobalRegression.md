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
##     Min      1Q  Median      3Q     Max 
## -1.5425 -0.1061 -0.0045  0.1037  1.3727 
## 
## Coefficients: (1 not defined because of singularities)
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    4.55e+00   1.02e-01   44.56  < 2e-16 ***
## COUNTY_ID123                   3.54e-01   4.66e-02    7.60  3.1e-14 ***
## COUNTY_ID163                   4.53e-01   8.94e-03   50.62  < 2e-16 ***
## CITYARDEN HILLS               -4.29e-01   4.81e-02   -8.93  < 2e-16 ***
## CITYBAYPORT                   -2.71e-02   1.87e-02   -1.45  0.14698    
## CITYBIRCHWOOD VILLAGE         -1.35e-01   2.79e-02   -4.83  1.4e-06 ***
## CITYBURNSVILLE                -2.03e-02   6.31e-03   -3.22  0.00126 ** 
## CITYCOTTAGE GROVE             -1.15e-01   6.07e-03  -18.98  < 2e-16 ***
## CITYDELLWOOD                  -5.14e-01   1.82e-01   -2.83  0.00472 ** 
## CITYEAGAN                      4.04e-02   7.46e-03    5.41  6.2e-08 ***
## CITYEMPIRE TOWNSHIP           -1.86e-03   2.25e-02   -0.08  0.93403    
## CITYFALCON HEIGHTS            -1.65e-02   4.87e-02   -0.34  0.73432    
## CITYFARMINGTON                 3.88e-03   9.03e-03    0.43  0.66739    
## CITYGEM LAKE                  -3.70e-01   1.15e-01   -3.22  0.00128 ** 
## CITYHUGO                      -2.12e-01   2.03e-02  -10.47  < 2e-16 ***
## CITYINVER GROVE HEIGHTS        2.32e-01   1.07e-02   21.62  < 2e-16 ***
## CITYLAKE ELMO                 -2.43e-02   2.14e-02   -1.13  0.25707    
## CITYLAKEVILLE                 -2.65e-02   7.94e-03   -3.34  0.00083 ***
## CITYLAUDERDALE                -1.44e-01   5.09e-02   -2.82  0.00478 ** 
## CITYLITTLE CANADA             -1.79e-01   4.82e-02   -3.71  0.00021 ***
## CITYMAHTOMEDI                 -5.87e-02   1.16e-02   -5.07  3.9e-07 ***
## CITYMAPLEWOOD                 -5.44e-02   4.66e-02   -1.17  0.24229    
## CITYMENDOTA                    5.82e-02   9.16e-02    0.64  0.52538    
## CITYMENDOTA HEIGHTS            2.25e-01   1.54e-02   14.55  < 2e-16 ***
## CITYMOUNDS VIEW               -4.50e-01   4.73e-02   -9.50  < 2e-16 ***
## CITYNEW BRIGHTON              -3.58e-01   4.69e-02   -7.63  2.3e-14 ***
## CITYNEWPORT                   -2.23e-01   1.56e-02  -14.27  < 2e-16 ***
## CITYNORTH ST. PAUL            -2.11e-02   4.68e-02   -0.45  0.65307    
## CITYOAKDALE                    1.24e-02   7.01e-03    1.77  0.07746 .  
## CITYOAK PARK HEIGHTS           2.73e-02   1.70e-02    1.61  0.10762    
## CITYROSEMOUNT                  7.63e-02   7.02e-03   10.87  < 2e-16 ***
## CITYROSEVILLE                 -1.94e-01   4.68e-02   -4.15  3.3e-05 ***
## CITYSHOREVIEW                 -4.29e-01   4.69e-02   -9.15  < 2e-16 ***
## CITYSOUTH ST PAUL              2.78e-01   1.18e-02   23.47  < 2e-16 ***
## CITYSPRING LAKE PARK          -4.55e-01   1.15e-01   -3.96  7.4e-05 ***
## CITYST. ANTHONY               -2.32e-01   5.87e-02   -3.96  7.6e-05 ***
## CITYSTILLWATER                -2.29e-02   8.87e-03   -2.58  0.00995 ** 
## CITYST. PAUL                  -2.03e-02   4.67e-02   -0.43  0.66415    
## CITYST. PAUL PARK             -1.86e-01   1.11e-02  -16.74  < 2e-16 ***
## CITYTOWN OF BAYTOWN            4.41e-02   1.29e-01    0.34  0.73246    
## CITYVADNAIS HEIGHTS           -3.51e-01   4.74e-02   -7.40  1.4e-13 ***
## CITYWEST ST PAUL               2.84e-01   1.39e-02   20.48  < 2e-16 ***
## CITYWHITE BEAR LAKE           -1.53e-01   4.58e-02   -3.34  0.00084 ***
## CITYWHITE BEAR TOWNSHIP       -3.28e-01   4.71e-02   -6.97  3.1e-12 ***
## CITYWILLERNIE                 -3.05e-01   2.94e-02  -10.36  < 2e-16 ***
## CITYWOODBURY                         NA         NA      NA       NA    
## factor(SALE_YR)2006            6.18e-03   2.60e-03    2.38  0.01741 *  
## factor(SALE_YR)2007           -2.83e-02   2.85e-03   -9.91  < 2e-16 ***
## factor(SALE_YR)2008           -1.47e-01   3.10e-03  -47.36  < 2e-16 ***
## factor(SALE_YR)2009           -2.29e-01   3.11e-03  -73.69  < 2e-16 ***
## factor(SALE_YR)2010           -2.61e-01   3.42e-03  -76.40  < 2e-16 ***
## ACRES_POLY                     1.60e+00   1.38e-01   11.56  < 2e-16 ***
## CBD_dist                       5.45e-05   2.55e-06   21.40  < 2e-16 ***
## I(ACRES_POLY^2)               -1.64e+00   2.50e-01   -6.56  5.5e-11 ***
## I(CBD_dist^2)                 -5.70e-10   7.64e-11   -7.47  8.3e-14 ***
## log(MAX)                      -1.58e-01   6.96e-03  -22.65  < 2e-16 ***
## HOMESTEADY                     2.60e-02   2.54e-03   10.24  < 2e-16 ***
## log(FIN_SQ_FT)                 5.52e-01   3.16e-03  174.57  < 2e-16 ***
## YEAR_BUILT                     1.81e-03   4.82e-05   37.62  < 2e-16 ***
## LAKE_dist                     -7.88e-05   4.30e-06  -18.33  < 2e-16 ***
## I(LAKE_dist^2)                 3.11e-08   1.56e-09   19.97  < 2e-16 ***
## PARK_dist                     -8.86e-06   1.92e-06   -4.61  4.1e-06 ***
## I(PARK_dist^2)                 1.62e-09   2.88e-10    5.65  1.7e-08 ***
## MCA3                           6.90e-04   1.02e-04    6.80  1.1e-11 ***
## SHOP_dist                      3.99e-05   2.23e-06   17.93  < 2e-16 ***
## I(SHOP_dist^2)                -5.71e-09   3.66e-10  -15.60  < 2e-16 ***
## MED_INCOME                     2.01e-06   6.43e-08   31.30  < 2e-16 ***
## COLLEGE_di                    -3.26e-05   4.61e-07  -70.79  < 2e-16 ***
## SALE_MO10                      6.36e-03   5.08e-03    1.25  0.21072    
## SALE_MO11                     -1.89e-03   5.31e-03   -0.36  0.72178    
## SALE_MO12                     -1.60e-02   5.57e-03   -2.87  0.00409 ** 
## SALE_MO2                       3.63e-03   5.47e-03    0.66  0.50665    
## SALE_MO3                       9.76e-03   5.05e-03    1.93  0.05325 .  
## SALE_MO4                       1.20e-02   4.96e-03    2.42  0.01545 *  
## SALE_MO5                       2.10e-02   4.79e-03    4.40  1.1e-05 ***
## SALE_MO6                       2.71e-02   4.68e-03    5.80  6.8e-09 ***
## SALE_MO7                       2.19e-02   4.81e-03    4.55  5.4e-06 ***
## SALE_MO8                       1.50e-02   4.78e-03    3.13  0.00176 ** 
## SALE_MO9                       1.42e-02   4.97e-03    2.87  0.00415 ** 
## ACRES_POLY:CBD_dist           -1.38e-04   1.89e-05   -7.29  3.3e-13 ***
## I(ACRES_POLY^2):I(CBD_dist^2) -3.68e-09   8.78e-10   -4.19  2.7e-05 ***
## ACRES_POLY:I(CBD_dist^2)       3.34e-09   5.31e-10    6.29  3.1e-10 ***
## CBD_dist:I(ACRES_POLY^2)       1.59e-04   3.21e-05    4.95  7.6e-07 ***
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
##   -3680    -918    -678    -768    -526    -195
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
##  -85500   63100   87900   99900  126000  675000
```

```r
summary(mfx.LAND.20052010.Dakota)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  -53400   54500   69700   72000   86400  406000
```


