Global Regression Model Results
========================================================

#This script runs the most up to date global regression models that include interaction effects.

```r
require(foreign)
```

```
## Loading required package: foreign
```

```r
workingdata20052010 <- read.dbf("../../../Data/R2GIS/CleanData/Sales20052010.dbf")
workingdata20052010$PostCrash = 0
crashobs <- which(workingdata20052010$TimePeriod > 46)
workingdata20052010$PostCrash[crashobs] = 1

myModel = "logSALE_VA ~ FIN_SQ_FT + ACRES_POLY +  HOMESTEAD +  YEAR_BUILT + MAX +\nMED_INCOME + MCA3 + CBD_dist + LAKE_dist + PARK_dist + SHOP_dist  + COUNTY_ID +  factor(SALE_YR) + SALE_MO"

lm.mini = lm(myModel, data = workingdata20052010)
summary(lm.mini)
```

```
## 
## Call:
## lm(formula = myModel, data = workingdata20052010)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -1.506 -0.118 -0.010  0.108  1.318 
## 
## Coefficients:
##                      Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          9.37e+00   9.48e-02   98.78  < 2e-16 ***
## FIN_SQ_FT            3.38e-04   1.85e-06  182.39  < 2e-16 ***
## ACRES_POLY           1.42e-01   1.14e-02   12.51  < 2e-16 ***
## HOMESTEADY           3.22e-02   2.80e-03   11.52  < 2e-16 ***
## YEAR_BUILT           7.82e-04   4.57e-05   17.11  < 2e-16 ***
## MAX                 -2.02e-03   1.32e-04  -15.27  < 2e-16 ***
## MED_INCOME           3.30e-06   5.87e-08   56.20  < 2e-16 ***
## MCA3                 1.88e-03   1.06e-04   17.79  < 2e-16 ***
## CBD_dist            -1.56e-06   1.97e-07   -7.96  1.8e-15 ***
## LAKE_dist            2.12e-05   1.68e-06   12.60  < 2e-16 ***
## PARK_dist           -2.38e-07   6.82e-07   -0.35  0.72699    
## SHOP_dist            3.01e-06   7.67e-07    3.92  8.9e-05 ***
## COUNTY_ID123         1.36e-01   3.52e-03   38.72  < 2e-16 ***
## COUNTY_ID163         1.46e-01   3.29e-03   44.26  < 2e-16 ***
## factor(SALE_YR)2006  9.46e-03   2.87e-03    3.30  0.00097 ***
## factor(SALE_YR)2007 -1.53e-02   3.15e-03   -4.87  1.1e-06 ***
## factor(SALE_YR)2008 -1.29e-01   3.41e-03  -37.99  < 2e-16 ***
## factor(SALE_YR)2009 -2.12e-01   3.42e-03  -62.09  < 2e-16 ***
## factor(SALE_YR)2010 -2.43e-01   3.76e-03  -64.67  < 2e-16 ***
## SALE_MO10            1.84e-02   5.61e-03    3.29  0.00102 ** 
## SALE_MO11            7.97e-03   5.86e-03    1.36  0.17358    
## SALE_MO12           -2.35e-03   6.15e-03   -0.38  0.70178    
## SALE_MO2             1.09e-02   6.04e-03    1.80  0.07111 .  
## SALE_MO3             1.75e-02   5.57e-03    3.15  0.00165 ** 
## SALE_MO4             2.54e-02   5.48e-03    4.64  3.5e-06 ***
## SALE_MO5             3.20e-02   5.28e-03    6.06  1.4e-09 ***
## SALE_MO6             4.24e-02   5.17e-03    8.21  2.3e-16 ***
## SALE_MO7             3.85e-02   5.31e-03    7.24  4.5e-13 ***
## SALE_MO8             3.19e-02   5.28e-03    6.04  1.5e-09 ***
## SALE_MO9             2.50e-02   5.49e-03    4.56  5.2e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 0.201 on 42065 degrees of freedom
## Multiple R-squared: 0.696,	Adjusted R-squared: 0.696 
## F-statistic: 3.32e+03 on 29 and 42065 DF,  p-value: <2e-16 
## 
```

```r

ModelBig = paste0(myModel, " + ACRES_POLY * CBD_dist")
lm.big = lm(ModelBig, data = workingdata20052010)
summary(lm.big)
```

```
## 
## Call:
## lm(formula = ModelBig, data = workingdata20052010)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.5044 -0.1187 -0.0104  0.1081  1.3162 
## 
## Coefficients:
##                      Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          9.49e+00   9.62e-02   98.68  < 2e-16 ***
## FIN_SQ_FT            3.39e-04   1.86e-06  182.60  < 2e-16 ***
## ACRES_POLY           2.74e-01   2.12e-02   12.95  < 2e-16 ***
## HOMESTEADY           3.23e-02   2.79e-03   11.56  < 2e-16 ***
## YEAR_BUILT           7.17e-04   4.65e-05   15.42  < 2e-16 ***
## MAX                 -2.00e-03   1.32e-04  -15.13  < 2e-16 ***
## MED_INCOME           3.31e-06   5.87e-08   56.41  < 2e-16 ***
## MCA3                 1.81e-03   1.06e-04   17.05  < 2e-16 ***
## CBD_dist             7.46e-07   3.70e-07    2.02  0.04359 *  
## LAKE_dist            2.36e-05   1.71e-06   13.78  < 2e-16 ***
## PARK_dist           -5.48e-07   6.83e-07   -0.80  0.42267    
## SHOP_dist            3.12e-06   7.66e-07    4.07  4.6e-05 ***
## COUNTY_ID123         1.35e-01   3.52e-03   38.39  < 2e-16 ***
## COUNTY_ID163         1.42e-01   3.33e-03   42.71  < 2e-16 ***
## factor(SALE_YR)2006  9.54e-03   2.87e-03    3.33  0.00088 ***
## factor(SALE_YR)2007 -1.57e-02   3.14e-03   -4.99  6.0e-07 ***
## factor(SALE_YR)2008 -1.30e-01   3.41e-03  -38.14  < 2e-16 ***
## factor(SALE_YR)2009 -2.13e-01   3.41e-03  -62.26  < 2e-16 ***
## factor(SALE_YR)2010 -2.43e-01   3.75e-03  -64.74  < 2e-16 ***
## SALE_MO10            1.82e-02   5.61e-03    3.24  0.00119 ** 
## SALE_MO11            7.99e-03   5.86e-03    1.36  0.17228    
## SALE_MO12           -2.12e-03   6.14e-03   -0.35  0.72992    
## SALE_MO2             1.06e-02   6.03e-03    1.75  0.08004 .  
## SALE_MO3             1.76e-02   5.57e-03    3.15  0.00163 ** 
## SALE_MO4             2.53e-02   5.47e-03    4.62  3.9e-06 ***
## SALE_MO5             3.19e-02   5.28e-03    6.04  1.6e-09 ***
## SALE_MO6             4.22e-02   5.17e-03    8.17  3.1e-16 ***
## SALE_MO7             3.84e-02   5.31e-03    7.23  4.9e-13 ***
## SALE_MO8             3.17e-02   5.28e-03    6.01  1.8e-09 ***
## SALE_MO9             2.48e-02   5.48e-03    4.52  6.1e-06 ***
## ACRES_POLY:CBD_dist -8.81e-06   1.19e-06   -7.38  1.6e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 0.201 on 42064 degrees of freedom
## Multiple R-squared: 0.696,	Adjusted R-squared: 0.696 
## F-statistic: 3.21e+03 on 30 and 42064 DF,  p-value: <2e-16 
## 
```


Now run model pre- and post- crash

```r
lm.bigPre = lm(ModelBig, data = workingdata20052010, subset = (PostCrash == 
    0))
summary(lm.bigPre)
```

```
## 
## Call:
## lm(formula = ModelBig, data = workingdata20052010, subset = (PostCrash == 
##     0))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.4188 -0.1108 -0.0142  0.0965  1.2875 
## 
## Coefficients:
##                      Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          8.55e+00   1.11e-01   77.15  < 2e-16 ***
## FIN_SQ_FT            3.35e-04   1.99e-06  167.80  < 2e-16 ***
## ACRES_POLY           2.31e-01   2.29e-02   10.09  < 2e-16 ***
## HOMESTEADY           3.38e-02   3.24e-03   10.42  < 2e-16 ***
## YEAR_BUILT           9.68e-04   5.06e-05   19.14  < 2e-16 ***
## MAX                 -1.63e-03   1.38e-04  -11.79  < 2e-16 ***
## MED_INCOME           2.86e-06   6.56e-08   43.64  < 2e-16 ***
## MCA3                 3.11e-03   1.75e-04   17.79  < 2e-16 ***
## CBD_dist            -1.57e-07   3.95e-07   -0.40  0.69208    
## LAKE_dist            2.22e-05   1.83e-06   12.16  < 2e-16 ***
## PARK_dist           -6.37e-07   7.33e-07   -0.87  0.38480    
## SHOP_dist            4.16e-06   8.17e-07    5.09  3.6e-07 ***
## COUNTY_ID123         1.42e-01   3.67e-03   38.59  < 2e-16 ***
## COUNTY_ID163         1.58e-01   3.56e-03   44.44  < 2e-16 ***
## factor(SALE_YR)2006  9.95e-03   2.63e-03    3.78  0.00015 ***
## factor(SALE_YR)2007 -1.50e-02   2.89e-03   -5.21  1.9e-07 ***
## factor(SALE_YR)2008 -1.20e-01   3.32e-03  -36.07  < 2e-16 ***
## SALE_MO10            1.89e-02   5.91e-03    3.19  0.00141 ** 
## SALE_MO11            1.50e-02   6.38e-03    2.36  0.01841 *  
## SALE_MO12            7.28e-03   6.64e-03    1.10  0.27311    
## SALE_MO2             7.81e-03   6.30e-03    1.24  0.21522    
## SALE_MO3             1.90e-02   5.82e-03    3.27  0.00109 ** 
## SALE_MO4             2.57e-02   5.75e-03    4.47  7.9e-06 ***
## SALE_MO5             3.13e-02   5.54e-03    5.64  1.7e-08 ***
## SALE_MO6             4.07e-02   5.41e-03    7.53  5.2e-14 ***
## SALE_MO7             3.74e-02   5.54e-03    6.76  1.4e-11 ***
## SALE_MO8             3.29e-02   5.49e-03    5.99  2.1e-09 ***
## SALE_MO9             2.17e-02   5.72e-03    3.80  0.00014 ***
## ACRES_POLY:CBD_dist -6.66e-06   1.27e-06   -5.26  1.5e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 0.184 on 31224 degrees of freedom
## Multiple R-squared: 0.714,	Adjusted R-squared: 0.713 
## F-statistic: 2.78e+03 on 28 and 31224 DF,  p-value: <2e-16 
## 
```

```r

lm.bigPost = lm(ModelBig, data = workingdata20052010, subset = (PostCrash == 
    1))
summary(lm.bigPost)
```

```
## 
## Call:
## lm(formula = ModelBig, data = workingdata20052010, subset = (PostCrash == 
##     1))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.5009 -0.1454 -0.0021  0.1388  1.3217 
## 
## Coefficients:
##                      Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          1.10e+01   2.12e-01   51.61  < 2e-16 ***
## FIN_SQ_FT            3.50e-04   4.26e-06   82.10  < 2e-16 ***
## ACRES_POLY           2.74e-01   4.99e-02    5.50  3.9e-08 ***
## HOMESTEADY           2.61e-02   6.41e-03    4.08  4.6e-05 ***
## YEAR_BUILT           2.15e-06   1.05e-04    0.02  0.98365    
## MAX                 -3.08e-03   3.29e-04   -9.37  < 2e-16 ***
## MED_INCOME           4.06e-06   1.35e-07   30.16  < 2e-16 ***
## MCA3                 1.21e-03   1.55e-04    7.81  6.5e-15 ***
## CBD_dist             1.40e-06   8.98e-07    1.56  0.11863    
## LAKE_dist            2.79e-05   4.03e-06    6.92  4.9e-12 ***
## PARK_dist           -4.01e-07   1.58e-06   -0.25  0.79975    
## SHOP_dist            1.88e-06   1.82e-06    1.03  0.30169    
## COUNTY_ID123         1.13e-01   9.30e-03   12.20  < 2e-16 ***
## COUNTY_ID163         9.96e-02   7.85e-03   12.69  < 2e-16 ***
## factor(SALE_YR)2009 -5.18e-02   1.22e-02   -4.23  2.4e-05 ***
## factor(SALE_YR)2010 -8.01e-02   1.31e-02   -6.10  1.1e-09 ***
## SALE_MO10            1.52e-02   1.37e-02    1.11  0.26841    
## SALE_MO11            9.48e-03   1.42e-02    0.67  0.50550    
## SALE_MO12           -7.03e-04   1.57e-02   -0.04  0.96430    
## SALE_MO2             2.09e-02   1.51e-02    1.39  0.16552    
## SALE_MO3             1.43e-02   1.40e-02    1.02  0.30635    
## SALE_MO4             2.54e-02   1.35e-02    1.88  0.06004 .  
## SALE_MO5             3.18e-02   1.31e-02    2.44  0.01481 *  
## SALE_MO6             4.49e-02   1.28e-02    3.50  0.00047 ***
## SALE_MO7             3.98e-02   1.34e-02    2.98  0.00285 ** 
## SALE_MO8             2.67e-02   1.33e-02    2.00  0.04508 *  
## SALE_MO9             3.36e-02   1.38e-02    2.43  0.01500 *  
## ACRES_POLY:CBD_dist -9.55e-06   2.94e-06   -3.25  0.00115 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 0.24 on 10814 degrees of freedom
## Multiple R-squared: 0.613,	Adjusted R-squared: 0.612 
## F-statistic:  635 on 27 and 10814 DF,  p-value: <2e-16 
## 
```

