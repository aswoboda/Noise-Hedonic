require(foreign)
require(multicore, quietly = TRUE)
require(fields, quietly = TRUE)

# the following command loads up some functions we'll use
source("helper/LWRfunctions.R")

# Basic idea of the algorithm is:
  # n observations
  # a variables believed to have stationary effect over space
  # b variables believed to have non-stationary effect over space
  
# for each column of Xa:
# regress the column against Xb using basic GWR
# compute the residuals from the above regression
# regress y against Xb using basic GWR
# compute the residuals from the above regression
# regress the y residuals against the Xa residuals using OLS -> 
#     this yields ahat, the stationary coefficients
# subtract Xa*ahat from y. regress this against Xb using basic GWR 
#    to obtain the geographically varying coefficients

# currently, our model 3 is 
# logSALE_VA = dependent variable as a function of
# Air_Mean+FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+OWNOCC+PercWhite+PercU18+
# MED_INCOME+MCA3+LAKE_dist+PARK_dist+SHOP_dist+CBD_dist+factor(TimePeriod)+CITY

# for the mixed model
# dep var = logSALE_VA
# stationary = Air_Mean + OWNOCC + PercWhite + PercU18
# non-stationary = FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+MED_INCOME+MCA3+
# LAKE_dist+PARK_dist+SHOP_dist+CBD_dist+factor(TimePeriod)+CITY

MYMODELS = c("logSALE_VA ~ FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+MED_INCOME+MCA3+LAKE_dist+PARK_dist+SHOP_dist+CBD_dist+factor(TimePeriod)+CITY",
             "Air_Mean ~ FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+MED_INCOME+MCA3+LAKE_dist+PARK_dist+SHOP_dist+CBD_dist+factor(TimePeriod)+CITY",
             "OWNOCC ~ FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+MED_INCOME+MCA3+LAKE_dist+PARK_dist+SHOP_dist+CBD_dist+factor(TimePeriod)+CITY",
             "PercWhite ~ FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+MED_INCOME+MCA3+LAKE_dist+PARK_dist+SHOP_dist+CBD_dist+factor(TimePeriod)+CITY",
             "PercU18 ~ FIN_SQ_FT+ACRES_POLY+YEAR_BUILT+HOME_STYLE+MED_INCOME+MCA3+LAKE_dist+PARK_dist+SHOP_dist+CBD_dist+factor(TimePeriod)+CITY")

KVECTOR = c(650)

filePrefix = "../Data/R2GIS/CleanData/"
inputFile = "Sales20052010.dbf"
dataSource = strsplit(inputFile, "\\.")[[1]][1]

DATAFRAME = read.dbf(paste0(filePrefix, inputFile))
N =  dim(DATAFRAME)[1]
obs2run = which(DATAFRAME$TimePeriod>1)

print(Sys.time())
for (modelNum in 1:5) { #
  MYMODEL = MYMODELS[modelNum]
  start = print(Sys.time())
  output.raw = mclapply(obs2run,
                        LWRtimelag,
                        Data.Frame = DATAFRAME,
                        my.model = MYMODEL, 
                        kvector = KVECTOR,
                        timelag = 12,
                        mc.cores = 16
  )
  end = print(Sys.time())
  print(end - start)
  names(output.raw) = DATAFRAME$UNIQID[obs2run]
  output = Reorganizer(output.raw)
  save(output, inputFile, MYMODEL, file = paste0(filePrefix, "MixedLWR/", "tempmodel", modelNum, "-", Sys.Date(), ".RData"))
  rm(output.raw, output)
  gc()
  print(gc())
}