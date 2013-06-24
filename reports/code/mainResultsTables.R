# Goal is to make a table of LWR results for the paper...

# columns
# global model coefficients and se's
# mean LWR Beta hats
# sd of LWR beta hats
# monte carlo p-value

# load the dataframe and helper functions
require(foreign)
DATAFRAME = read.dbf("~/NoiseHedonicProject/Data/R2GIS/CleanData/Sales20052010.dbf")
obs2run = which(DATAFRAME$TimePeriod>11)
source("~/NoiseHedonicProject/Noise-Hedonic/helper/LWRfunctions.R")

# load the LWR results
# load the Monte Carlo Results
####################
# Kitchen Sink Model
####################
load("~/NoiseHedonicProject/Noise-Hedonic/analysis/04MonteCarloSim/ModelBigCity/CopyOfSales20052010LWRmodel18-2013-04-13.RData")
LWRMCstats = read.csv("~/NoiseHedonicProject/Noise-Hedonic/analysis/04MonteCarloSim/ModelBigCity/bandwidth200/LWRMonteCarloStats2013-05-10.csv")

lm.global = lm(paste0(MYMODEL, "+factor(SALE_YR)"), data = DATAFRAME[obs2run, ])
GlobalModel = summary(lm.global)
global.leverages = lm.influence(lm.global, do.coef = FALSE)$hat

GCV(leverages = matrix(global.leverages, ncol = 1), 
    yhats = matrix(lm.global$fitted.values, ncol = 1), 
    dep.var = DATAFRAME$logSALE_VA[obs2run])

vars = c("(Intercept)", "MAX", "FIN_SQ_FT", "ACRES_POLY", "YEAR_BUILT", "OWNOCC", 
         "MCA3", "MED_INCOME", "CBD_dist", "PARK_dist", "LAKE_dist", "SHOP_dist" )

LWRtable = matrix(NA, length(vars), 6)
rownames(LWRtable) = vars
LWRtable[, 1:2] = signif(GlobalModel$coefficients[vars , 1:2], 2)

# populate the rest of the table
for (i in 1:length(vars)) {
  # put the mean and sd of LWR beta hats in the table
  LWRtable[i, 3] = signif(mean(output[[paste0("beta.", vars[i])]][, "k200"], na.rm = T), 2)
  LWRtable[i, 5] = signif( sd(output[[paste0("beta.", vars[i])]][, "k200"], na.rm = T), 2)
  
  # what percent of the coefficients are significantly different from zero at 10% level?
  tstats = output[[paste0("beta.", vars[i])]][, "k200"]/output[[paste0("ses.", vars[i])]][, "k200"]
  LWRtable[i, 4] = signif(length(which(abs(tstats) > qnorm(.95)))/sum(!is.na(tstats)), 2)
  
  # add the Monte Carlo p-value
  # pick a variable
  # grab the sd of the LWR betahats
  # grab the 100 sds from the Monte Carlo simulations for the variable
  # rank the real LWR betahat sd within the Monte Carlo simulation values
  mysd = LWRtable[i, 5]
  varid = sub("\\(", "", vars[i])
  varid = sub("\\)", "", varid)
  othersds = LWRMCstats[, paste0("sterBeta.", varid)]
  LWRtable[i, 6] = (101-rank(c(mysd, othersds))[1])/100
}

LWRtable
require(xtable)
colnames(LWRtable) = paste0("(", 1:6, ")")
xtable(LWRtable, digits = c(0, -1, -1, -1, 2, -1, 2))


###################
# Structural Model
###################
load("~/NoiseHedonicProject/Noise-Hedonic/analysis/04MonteCarloSim/ModelStructural/Sales20052010LWRoutput2013-03-21.RData")
LWRMCstats = read.csv("~/NoiseHedonicProject/Noise-Hedonic/analysis/04MonteCarloSim/ModelStructural/bandwidthK100/LWRMonteCarloStatsMasterNearest100obs.csv")


lm.global = lm(paste0(MYMODEL, "+factor(SALE_YR)"), data = DATAFRAME[obs2run, ])
GlobalModel = summary(lm.global)
global.leverages = lm.influence(lm.global, do.coef = FALSE)$hat

GCV(leverages = matrix(global.leverages, ncol = 1), 
    yhats = matrix(lm.global$fitted.values, ncol = 1), 
    dep.var = DATAFRAME$logSALE_VA[obs2run])

vars = c("(Intercept)", "MAX", "FIN_SQ_FT", "ACRES_POLY", "YEAR_BUILT")

LWRtable = matrix(NA, length(vars), 6)
rownames(LWRtable) = vars
LWRtable[, 1:2] = signif(GlobalModel$coefficients[vars , 1:2], 2)

# populate the rest of the table
for (i in 1:length(vars)) {
  # put the mean and sd of LWR beta hats in the table
  LWRtable[i, 3] = signif(mean(output[[paste0("beta.", vars[i])]][, "k200"], na.rm = T), 2)
  LWRtable[i, 5] = signif( sd(output[[paste0("beta.", vars[i])]][, "k200"], na.rm = T), 2)
  
  # what percent of the coefficients are significantly different from zero at 10% level?
  tstats = output[[paste0("beta.", vars[i])]][, "k200"]/output[[paste0("ses.", vars[i])]][, "k200"]
  LWRtable[i, 4] = signif(length(which(abs(tstats) > qnorm(.95)))/sum(!is.na(tstats)), 2)
  
  # add the Monte Carlo p-value
  # pick a variable
  # grab the sd of the LWR betahats
  # grab the 100 sds from the Monte Carlo simulations for the variable
  # rank the real LWR betahat sd within the Monte Carlo simulation values
  mysd = LWRtable[i, 5]
  varid = sub("\\(", "", vars[i])
  varid = sub("\\)", "", varid)
  othersds = LWRMCstats[, paste0("sterBeta.", varid)]
  LWRtable[i, 6] = (101-rank(c(mysd, othersds))[1])/100
}

LWRtable
require(xtable)
colnames(LWRtable) = paste0("(", 1:6, ")")
xtable(LWRtable, digits = c(0, -1, -1, -1, 2, -1, 2))

######################
# Big (No City) Model
######################
load("~/NoiseHedonicProject/Noise-Hedonic/analysis/04MonteCarloSim/ModelBigNoCity/bandwidth200/CopyOfSales20052010LWRmodel17-2013-04-13.RData")
LWRMCstats = read.csv("~/NoiseHedonicProject/Noise-Hedonic/analysis/04MonteCarloSim/ModelBigNoCity/bandwidth200/LWRMonteCarloStatsMasterBigModelK200.csv")

lm.global = lm(paste0(MYMODEL, "+factor(SALE_YR)"), data = DATAFRAME[obs2run, ])
GlobalModel = summary(lm.global)
global.leverages = lm.influence(lm.global, do.coef = FALSE)$hat

GCV(leverages = matrix(global.leverages, ncol = 1), 
    yhats = matrix(lm.global$fitted.values, ncol = 1), 
    dep.var = DATAFRAME$logSALE_VA[obs2run])

vars = c("(Intercept)", "MAX", "FIN_SQ_FT", "ACRES_POLY", "YEAR_BUILT", "OWNOCC", 
         "MCA3", "MED_INCOME", "CBD_dist", "PARK_dist", "LAKE_dist", "SHOP_dist" )

LWRtable = matrix(NA, length(vars), 6)
rownames(LWRtable) = vars
LWRtable[, 1:2] = signif(GlobalModel$coefficients[vars , 1:2], 2)

# populate the rest of the table
for (i in 1:length(vars)) {
  # put the mean and sd of LWR beta hats in the table
  LWRtable[i, 3] = signif(mean(output[[paste0("beta.", vars[i])]][, "k200"], na.rm = T), 2)
  LWRtable[i, 5] = signif( sd(output[[paste0("beta.", vars[i])]][, "k200"], na.rm = T), 2)
  
  # what percent of the coefficients are significantly different from zero at 10% level?
  tstats = output[[paste0("beta.", vars[i])]][, "k200"]/output[[paste0("ses.", vars[i])]][, "k200"]
  LWRtable[i, 4] = signif(length(which(abs(tstats) > qnorm(.95)))/sum(!is.na(tstats)), 2)
  
  # add the Monte Carlo p-value
  # pick a variable
  # grab the sd of the LWR betahats
  # grab the 100 sds from the Monte Carlo simulations for the variable
  # rank the real LWR betahat sd within the Monte Carlo simulation values
  mysd = LWRtable[i, 5]
  varid = sub("\\(", "", vars[i])
  varid = sub("\\)", "", varid)
  othersds = LWRMCstats[, paste0("sterBeta.", varid)]
  LWRtable[i, 6] = (101-rank(c(mysd, othersds))[1])/100
}

LWRtable
require(xtable)
colnames(LWRtable) = paste0("(", 1:6, ")")
xtable(LWRtable, digits = c(0, -1, -1, -1, 2, -1, 2))
