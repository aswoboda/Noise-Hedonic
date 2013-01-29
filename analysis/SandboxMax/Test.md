Max's First Analysis
========================================================

This is an R Markdown document. Markdown is a simple formatting syntax for authoring web pages (click the **MD** toolbar button for help on Markdown).

When you click the **Knit HTML** button a web page will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


```r
getwd()
```

```
## [1] "/home/timmm/Noise Hedonic Project/Noise-Hedonic/analysis/SandboxMax"
```

```r

library(foreign)
workingdata <- read.dbf("../../../Data//R2GIS/CleanData/Sales20052007.dbf")
names(workingdata)
```

```
##  [1] "COUNTY_ID"  "PIN"        "CITY"       "ZIP"        "ACRES_POLY"
##  [6] "HOMESTEAD"  "TOTAL_TAX"  "HOME_STYLE" "FIN_SQ_FT"  "GARAGE"    
## [11] "YEAR_BUILT" "SALE_VALUE" "SALE_YR"    "BEDS"       "BATH"      
## [16] "MAX"        "TRACTCE10"  "BLDG_QUAL"  "PARK_dist"  "LAKE_dist" 
## [21] "SALE_QRT"   "SDNUM"      "MCA3"       "MCA5"       "UNIQID"    
## [26] "Long_X"     "Lat_Y"      "SHOP_dist"  "CBD_dist"   "GARSQFT"   
## [31] "logSALE_VA" "logFIN_SQ_" "logMAX"     "ACRES2"
```

```r
summary(workingdata)
```

```
##  COUNTY_ID                 PIN                      CITY      
##  037: 9989   037-010220002050:    1   ST. PAUL        : 7133  
##  123:12063   037-011040001001:    1   LAKEVILLE       : 2114  
##  163: 5500   037-011100001001:    1   CITY OF WOODBURY: 2005  
##              037-011126001001:    1   EAGAN           : 1538  
##              037-011165002005:    1   BURNSVILLE      : 1233  
##              037-011165003003:    1   APPLE VALLEY    : 1158  
##              (Other)         :27546   (Other)         :12371  
##       ZIP          ACRES_POLY    HOMESTEAD   TOTAL_TAX     
##  55044  : 1745   Min.   :0.020   N: 3267   Min.   :     0  
##  55106  : 1574   1st Qu.:0.160   Y:24285   1st Qu.:  2223  
##  55024  : 1294   Median :0.250             Median :  2884  
##  55125  : 1255   Mean   :0.265             Mean   : 61929  
##  55124  : 1158   3rd Qu.:0.320             3rd Qu.:  4993  
##  (Other):20525   Max.   :1.000             Max.   :729500  
##  NA's   :    1                                             
##          HOME_STYLE     FIN_SQ_FT     GARAGE        YEAR_BUILT  
##  SPLIT LEVL   :4017   Min.   : 390   N   :  439   Min.   :1858  
##  RAMBLER      :3991   1st Qu.:1159   Y   :22365   1st Qu.:1949  
##  BUNGALOW     :3188   Median :1639   NA's: 4748   Median :1973  
##  TWO STORY    :3188   Mean   :1771                Mean   :1966  
##  2 Story Frame:2117   3rd Qu.:2204                3rd Qu.:1993  
##  ONE STORY    :2083   Max.   :4987                Max.   :2008  
##  (Other)      :8968                                             
##    SALE_VALUE        SALE_YR          BEDS          BATH      
##  Min.   : 21000   Min.   :2005   Min.   :0.0   Min.   :0.000  
##  1st Qu.:210000   1st Qu.:2005   1st Qu.:0.0   1st Qu.:0.000  
##  Median :254000   Median :2006   Median :0.0   Median :0.000  
##  Mean   :282878   Mean   :2006   Mean   :1.3   Mean   :0.811  
##  3rd Qu.:332508   3rd Qu.:2006   3rd Qu.:3.0   3rd Qu.:1.750  
##  Max.   :675000   Max.   :2007   Max.   :9.0   Max.   :4.500  
##                                                               
##       MAX         TRACTCE10       BLDG_QUAL      PARK_dist   
##  Min.   :28.4   071018 :  489   Min.   :0.00   Min.   :   0  
##  1st Qu.:52.3   060907 :  457   1st Qu.:0.00   1st Qu.:1085  
##  Median :57.1   060820 :  366   Median :0.00   Median :2097  
##  Mean   :58.7   061009 :  358   Mean   :1.35   Mean   :2602  
##  3rd Qu.:64.8   060819 :  317   3rd Qu.:3.00   3rd Qu.:3776  
##  Max.   :91.5   060817 :  286   Max.   :7.00   Max.   :9688  
##                 (Other):25279                                
##    LAKE_dist       SALE_QRT       SDNUM          MCA3          MCA5    
##  Min.   :   0   Min.   :1.0   Min.   :  6   Min.   :338   Min.   :  0  
##  1st Qu.: 400   1st Qu.:2.0   1st Qu.:196   1st Qu.:360   1st Qu.:355  
##  Median : 734   Median :2.0   Median :623   Median :367   Median :359  
##  Mean   : 898   Mean   :2.5   Mean   :496   Mean   :365   Mean   :370  
##  3rd Qu.:1267   3rd Qu.:3.0   3rd Qu.:625   3rd Qu.:372   3rd Qu.:362  
##  Max.   :4391   Max.   :4.0   Max.   :834   Max.   :378   Max.   :562  
##                                                                        
##      UNIQID          Long_X          Lat_Y        SHOP_dist    
##  Min.   :    0   Min.   :-93.3   Min.   :44.6   Min.   :    6  
##  1st Qu.: 7093   1st Qu.:-93.2   1st Qu.:44.8   1st Qu.:  932  
##  Median :14242   Median :-93.1   Median :44.9   Median : 1498  
##  Mean   :14306   Mean   :-93.1   Mean   :44.9   Mean   : 1896  
##  3rd Qu.:21562   3rd Qu.:-93.0   3rd Qu.:45.0   3rd Qu.: 2244  
##  Max.   :28744   Max.   :-92.8   Max.   :45.2   Max.   :10854  
##                                                                
##     CBD_dist        GARSQFT       logSALE_VA      logFIN_SQ_  
##  Min.   : 1113   Min.   :   1   Min.   : 9.95   Min.   :5.97  
##  1st Qu.: 6759   1st Qu.: 310   1st Qu.:12.26   1st Qu.:7.05  
##  Median :13239   Median : 537   Median :12.45   Median :7.40  
##  Mean   :14785   Mean   : 521   Mean   :12.49   Mean   :7.39  
##  3rd Qu.:22289   3rd Qu.: 732   3rd Qu.:12.71   3rd Qu.:7.70  
##  Max.   :37040   Max.   :1411   Max.   :13.42   Max.   :8.52  
##                                                               
##      logMAX         ACRES2      
##  Min.   :3.35   Min.   :0.0004  
##  1st Qu.:3.96   1st Qu.:0.0256  
##  Median :4.04   Median :0.0625  
##  Mean   :4.06   Mean   :0.0896  
##  3rd Qu.:4.17   3rd Qu.:0.1024  
##  Max.   :4.52   Max.   :1.0000  
## 
```

```r

### Preparation for regression analysis, load packages for tests

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


You can also embed plots, for example:


```r
hist(workingdata$SALE_VALUE)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


Here, let's go ahead and run a regression.


```r
model.SaleValue1 <- lm(SALE_VALUE ~ COUNTY_ID + CITY + SALE_YR + SDNUM + GARAGE + 
    ACRES_POLY + HOMESTEAD + FIN_SQ_FT + YEAR_BUILT + MAX + PARK_dist + LAKE_dist + 
    MCA3 + MCA5 + SHOP_dist + CBD_dist, data = workingdata)
summary(model.SaleValue1)
```

```
## 
## Call:
## lm(formula = SALE_VALUE ~ COUNTY_ID + CITY + SALE_YR + SDNUM + 
##     GARAGE + ACRES_POLY + HOMESTEAD + FIN_SQ_FT + YEAR_BUILT + 
##     MAX + PARK_dist + LAKE_dist + MCA3 + MCA5 + SHOP_dist + CBD_dist, 
##     data = workingdata)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -494315  -33301   -5946   23135  371413 
## 
## Coefficients: (2 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   2.90e+06   9.57e+05    3.04  0.00240 ** 
## COUNTY_ID123                  4.78e+04   9.98e+03    4.79  1.7e-06 ***
## COUNTY_ID163                  1.36e+05   2.90e+04    4.68  2.9e-06 ***
## CITYARDEN HILLS               2.35e+04   6.42e+03    3.67  0.00025 ***
## CITYBURNSVILLE               -6.77e+03   2.44e+03   -2.78  0.00548 ** 
## CITYCITY OF BAYPORT          -1.21e+05   2.72e+04   -4.45  8.4e-06 ***
## CITYCITY OF BIRCHWOOD        -1.79e+03   2.93e+04   -0.06  0.95136    
## CITYCITY OF COTTAGE GROVE    -9.05e+04   2.59e+04   -3.50  0.00047 ***
## CITYCITY OF DELLWOOD         -1.17e+03   6.31e+04   -0.02  0.98523    
## CITYCITY OF HUGO             -1.17e+05   2.66e+04   -4.40  1.1e-05 ***
## CITYCITY OF LAKE ELMO        -2.22e+04   2.71e+04   -0.82  0.41159    
## CITYCITY OF MAHTOMEDI        -7.02e+03   2.64e+04   -0.27  0.79008    
## CITYCITY OF NEWPORT          -8.35e+04   2.70e+04   -3.10  0.00196 ** 
## CITYCITY OF OAKDALE          -2.76e+04   2.64e+04   -1.05  0.29574    
## CITYCITY OF OAK PARK HEIGHTS -9.63e+04   2.68e+04   -3.59  0.00033 ***
## CITYCITY OF PINE SPRINGS     -1.44e+05   4.83e+04   -2.98  0.00290 ** 
## CITYCITY OF STILLWATER       -7.33e+04   2.60e+04   -2.82  0.00474 ** 
## CITYCITY OF ST PAUL PARK     -8.76e+04   2.63e+04   -3.33  0.00088 ***
## CITYCITY OF WHITE BEAR LAKE  -6.69e+04   3.23e+04   -2.07  0.03838 *  
## CITYCITY OF WILLERNIE        -8.85e+04   3.13e+04   -2.83  0.00467 ** 
## CITYCITY OF WOODBURY         -4.11e+04   2.60e+04   -1.58  0.11334    
## CITYEAGAN                     4.49e+04   2.76e+03   16.26  < 2e-16 ***
## CITYEMPIRE TOWNSHIP          -2.57e+04   8.41e+03   -3.05  0.00227 ** 
## CITYFALCON HEIGHTS            8.28e+04   7.91e+03   10.48  < 2e-16 ***
## CITYFARMINGTON               -1.89e+04   3.28e+03   -5.77  8.0e-09 ***
## CITYGEM LAKE                  1.42e+04   5.77e+04    0.25  0.80501    
## CITYINVER GROVE HEIGHTS       6.89e+04   4.11e+03   16.76  < 2e-16 ***
## CITYLAKEVILLE                -1.23e+04   2.73e+03   -4.50  6.9e-06 ***
## CITYLAUDERDALE                6.13e+04   1.01e+04    6.07  1.3e-09 ***
## CITYLITTLE CANADA             4.12e+04   7.09e+03    5.81  6.1e-09 ***
## CITYMAPLEWOOD                 3.05e+04   4.95e+03    6.17  6.9e-10 ***
## CITYMENDOTA                   1.67e+04   5.77e+04    0.29  0.77230    
## CITYMENDOTA HEIGHTS           9.73e+04   5.88e+03   16.54  < 2e-16 ***
## CITYMOUNDS VIEW              -2.85e+02   5.61e+03   -0.05  0.95952    
## CITYNEW BRIGHTON              1.51e+04   5.09e+03    2.96  0.00304 ** 
## CITYNORTH OAKS                4.35e+04   2.22e+04    1.97  0.04936 *  
## CITYNORTH ST. PAUL            3.14e+04   5.66e+03    5.54  3.0e-08 ***
## CITYROSEMOUNT                 1.91e+04   2.64e+03    7.26  4.0e-13 ***
## CITYROSEVILLE                 4.58e+04   4.99e+03    9.18  < 2e-16 ***
## CITYSHOREVIEW                 2.44e+04   4.79e+03    5.10  3.4e-07 ***
## CITYSOUTH ST PAUL             9.31e+04   5.79e+03   16.09  < 2e-16 ***
## CITYSPRING LAKE PARK         -9.90e+03   5.77e+04   -0.17  0.86369    
## CITYST. ANTHONY               4.36e+04   1.74e+04    2.50  0.01226 *  
## CITYST. PAUL                  8.28e+04   4.81e+03   17.22  < 2e-16 ***
## CITYTOWN OF BAYTOWN                 NA         NA      NA       NA    
## CITYVADNAIS HEIGHTS           2.20e+04   5.95e+03    3.70  0.00022 ***
## CITYWEST ST PAUL              9.42e+04   5.32e+03   17.71  < 2e-16 ***
## CITYWHITE BEAR LAKE           2.07e+04   4.90e+03    4.23  2.3e-05 ***
## CITYWHITE BEAR TOWNSHIP             NA         NA      NA       NA    
## SALE_YR                      -2.06e+03   4.76e+02   -4.32  1.5e-05 ***
## SDNUM                         4.62e+01   2.05e+01    2.25  0.02428 *  
## GARAGEY                       2.52e+04   2.89e+03    8.70  < 2e-16 ***
## ACRES_POLY                    6.88e+04   3.53e+03   19.48  < 2e-16 ***
## HOMESTEADY                    5.82e+03   1.30e+03    4.47  7.8e-06 ***
## FIN_SQ_FT                     1.09e+02   6.94e-01  157.54  < 2e-16 ***
## YEAR_BUILT                    3.65e+02   2.08e+01   17.56  < 2e-16 ***
## MAX                          -7.36e+02   5.05e+01  -14.59  < 2e-16 ***
## PARK_dist                    -1.30e+00   3.69e-01   -3.52  0.00043 ***
## LAKE_dist                     2.70e+00   7.36e-01    3.66  0.00025 ***
## MCA3                          1.17e+03   7.48e+01   15.59  < 2e-16 ***
## MCA5                          4.52e+01   1.22e+01    3.69  0.00022 ***
## SHOP_dist                     2.93e+00   4.02e-01    7.29  3.3e-13 ***
## CBD_dist                      3.68e+00   2.07e-01   17.77  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 57500 on 22743 degrees of freedom
##   (4748 observations deleted due to missingness)
## Multiple R-squared: 0.716,	Adjusted R-squared: 0.715 
## F-statistic:  955 on 60 and 22743 DF,  p-value: <2e-16
```




