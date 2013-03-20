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
## -0.8016 -0.0707 -0.0013  0.0665  0.9219 
## 
## Coefficients:
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    6.03e+00   2.12e-01   28.44  < 2e-16 ***
## CITYBURNSVILLE                 2.87e-03   4.36e-03    0.66  0.51037    
## CITYEAGAN                      4.24e-02   6.44e-03    6.58  5.0e-11 ***
## CITYEMPIRE TOWNSHIP           -4.50e-02   1.67e-02   -2.70  0.00693 ** 
## CITYFARMINGTON                -3.64e-02   7.97e-03   -4.57  4.9e-06 ***
## CITYINVER GROVE HEIGHTS        4.85e-02   1.10e-02    4.41  1.0e-05 ***
## CITYLAKEVILLE                  1.42e-02   5.75e-03    2.46  0.01384 *  
## CITYMENDOTA                    2.32e-01   5.98e-02    3.87  0.00011 ***
## CITYMENDOTA HEIGHTS            1.44e-01   1.62e-02    8.86  < 2e-16 ***
## CITYROSEMOUNT                  1.75e-02   5.73e-03    3.06  0.00218 ** 
## CITYSOUTH ST PAUL              1.84e-02   1.58e-02    1.16  0.24512    
## CITYWEST ST PAUL               4.24e-02   1.85e-02    2.29  0.02215 *  
## factor(SALE_YR)2006            1.07e-02   2.77e-03    3.85  0.00012 ***
## factor(SALE_YR)2007           -1.54e-02   2.98e-03   -5.15  2.6e-07 ***
## factor(SALE_YR)2008           -1.04e-01   3.88e-03  -26.72  < 2e-16 ***
## factor(SALE_YR)2009           -1.92e-01   4.16e-03  -46.06  < 2e-16 ***
## factor(SALE_YR)2010           -2.20e-01   4.46e-03  -49.31  < 2e-16 ***
## ACRES_POLY                    -1.24e-01   2.31e-01   -0.54  0.59070    
## CBD_dist                       1.86e-05   6.14e-06    3.03  0.00248 ** 
## I(ACRES_POLY^2)                1.01e+00   3.83e-01    2.65  0.00815 ** 
## I(CBD_dist^2)                 -5.47e-10   1.58e-10   -3.46  0.00055 ***
## HOMESTEADY                     7.00e-03   3.97e-03    1.76  0.07792 .  
## log(FIN_SQ_FT)                 3.12e-01   6.42e-03   48.53  < 2e-16 ***
## YEAR_BUILT                     1.95e-03   9.07e-05   21.55  < 2e-16 ***
## MAX                           -2.12e-05   6.04e-04   -0.04  0.97203    
## PARK_dist                     -6.83e-06   1.05e-06   -6.49  8.7e-11 ***
## LAKE_dist                     -3.60e-05   4.46e-06   -8.07  7.6e-16 ***
## I(LAKE_dist^2)                 1.20e-08   1.48e-09    8.09  6.3e-16 ***
## MCA3                          -9.47e-04   2.87e-04   -3.30  0.00095 ***
## MED_INCOME                     5.20e-07   7.50e-08    6.93  4.4e-12 ***
## COLLEGE_di                    -9.50e-06   3.65e-06   -2.60  0.00923 ** 
## I(COLLEGE_di^2)                2.19e-10   1.05e-10    2.08  0.03728 *  
## SHOP_dist                      4.05e-06   1.03e-06    3.91  9.1e-05 ***
## SALE_MO10                     -1.25e-03   5.81e-03   -0.21  0.82996    
## SALE_MO11                      3.42e-03   6.05e-03    0.57  0.57200    
## SALE_MO12                     -1.41e-02   6.47e-03   -2.18  0.02944 *  
## SALE_MO2                      -2.38e-03   6.23e-03   -0.38  0.70293    
## SALE_MO3                       8.58e-03   5.65e-03    1.52  0.12897    
## SALE_MO4                       1.10e-02   5.62e-03    1.95  0.05081 .  
## SALE_MO5                       9.26e-03   5.43e-03    1.70  0.08845 .  
## SALE_MO6                       1.55e-02   5.29e-03    2.94  0.00328 ** 
## SALE_MO7                       1.68e-02   5.46e-03    3.08  0.00205 ** 
## SALE_MO8                       1.59e-02   5.46e-03    2.91  0.00366 ** 
## SALE_MO9                       1.31e-02   5.63e-03    2.32  0.02012 *  
## BEDS                           4.16e-03   1.64e-03    2.54  0.01096 *  
## BATH                          -3.32e-02   1.08e-02   -3.07  0.00214 ** 
## I(BATH^2)                      1.35e-02   2.24e-03    6.03  1.7e-09 ***
## BLDG_QUAL                      1.52e-01   2.04e-03   74.34  < 2e-16 ***
## ACRES_POLY:CBD_dist            6.32e-05   2.67e-05    2.37  0.01793 *  
## I(ACRES_POLY^2):I(CBD_dist^2)  2.82e-09   1.02e-09    2.77  0.00566 ** 
## ACRES_POLY:I(CBD_dist^2)      -1.39e-09   6.57e-10   -2.11  0.03488 *  
## CBD_dist:I(ACRES_POLY^2)      -1.34e-04   4.20e-05   -3.20  0.00137 ** 
## I(CBD_dist^2):MAX              1.01e-11   1.56e-12    6.48  9.5e-11 ***
## CBD_dist:MAX                  -3.30e-07   6.41e-08   -5.15  2.6e-07 ***
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
##   -1870    -871    -667    -734    -539    -273
```

```r
summary(mfx.TRAFFIC.20052010.Dakota)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   -1810    -660    -445    -450    -246     874
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
##  -54200   54300   69600   71800   86300  409000
```


