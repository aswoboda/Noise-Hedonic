
# Goal here is to create the GCV plots by model and time lag

# 3 plots (one for each time lag) with 3 lines in the plot (one for each model)
# 3 plots (one for each model) with 3 lines in the plot (one for each time lag)


# need to create a matrix/data.frame/array for the nine different time lag and model combinations

setwd("~/NoiseHedonicProject/Data/R2GIS/CleanData/")
file.list = list.files()

myfiles = file.list[which(substr(file.list, nchar(file.list)-15, nchar(file.list)-6 ) == "2013-06-24")]

source("~/NoiseHedonicProject/Noise-Hedonic/helper/LWRfunctions.R")
require(foreign)
DATAFRAME = read.dbf("~/NoiseHedonicProject/Data/R2GIS/CleanData/Sales20052010.dbf")

gcvs.global = c()
gcvs = c()
for (i in 4:6) {
  load(myfiles[i])
  
  obs2run = which(DATAFRAME$TimePeriod>(TIMELAG-1))
  gcv.vec = GCV(output$leverages, output$yhats, DATAFRAME$logSALE_VA[obs2run])
  gcvs = cbind(gcvs, gcv.vec)
  ks = as.numeric(substr(names(gcv.vec), 2, 6))
  
  lm.global = lm(paste0(MYMODEL, "+factor(SALE_YR)"), data = DATAFRAME[obs2run, ])
  GlobalModel = summary(lm.global)
  global.leverages = lm.influence(lm.global, do.coef = FALSE)$hat
  
  gcvs.global = c(gcvs.global, GCV(leverages = matrix(global.leverages, ncol = 1),
                                   yhats = matrix(lm.global$fitted.values, ncol = 1),
                                   dep.var = DATAFRAME$logSALE_VA[obs2run]))
}


pdf("~/NoiseHedonicProject/Noise-Hedonic/graphs/GCVbyModel.pdf", 
    family = "Palatino", height = 6, width = 8)
matplot(ks, gcvs, type = "o", pch = 16, cex = .5, las = 1,
        ylim = c(.025, .052), xlim = c(0, 2000), lty = 1,
        col = c("black", "blue", "red"),
        ylab = "Generalized Cross Validation (GCV) Score",
        xlab = "Bandwidth Size (# of houses sold within last 12 months inluded in LWR)")

abline(h = gcvs.global, col = c("black", "blue", "red"), lty = 2)
lines(c(200, 200), c(0, min(gcvs)), col = "gray80")
lines(c(200, -200), rep(min(gcvs), 2), col = "gray80")
axis(1, at = 200)
axis(2, at = round(min(gcvs), 4), las = 1)
title("Generalized Cross Validation Scores Across Models")

text(rep(900, 3), c(.034, .03, .026),
     c("LWR Model (1) = Structural Variables", 
       "LWR Model (2) = LWR Model (1) + Locational Variables",
       "LWR Model (3) = LWR Model (2) + City Fixed Effects"), 
     srt = 3, pos = 4, col = c("black", "blue", "red"), cex = .8)

text(rep(200, 3), gcvs.global+.001,
     c("Global Model (1) = Structural Variables + Year Fixed Effects", 
       "Global Model (2) = Global Model (1) + Locational Variables", 
       "Global Model (3) = Global Model (2) + City Fixed Effects"),
     pos = 4, col = c("black", "blue", "red"), cex = .8)
dev.off()