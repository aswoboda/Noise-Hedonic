
# Goal here is to create the GCV plots by model and time lag

# 3 plots (one for each time lag) with 3 lines in the plot (one for each model)
# 3 plots (one for each model) with 3 lines in the plot (one for each time lag)


# need to create a matrix/data.frame/array for the nine different time lag and model combinations

setwd("~/NoiseHedonicProject/Data/R2GIS/CleanData/TimeLag12months/")
file.list = list.files()

myfiles = file.list[which(substr(file.list, 1, 28) == "Sales20052010LWRmodelAirMean")]

source("~/NoiseHedonicProject/Noise-Hedonic/helper/LWRfunctions.R")
require(foreign)
DATAFRAME = read.dbf("~/NoiseHedonicProject/Data/R2GIS/CleanData/Sales20052010.dbf")

gcvs.global = c()
gcvs = c()
for (i in 1:3) {
  load(myfiles[i])
  
  obs2run = which(DATAFRAME$TimePeriod>11)
  gcv.vec = print(GCV(output$leverages, output$yhats, DATAFRAME$logSALE_VA[obs2run]))
  #gcvs = cbind(gcvs, gcv.vec)
  ks = print(as.numeric(substr(names(gcv.vec), 2, 6)))
  
  lm.global = lm(MYMODEL, data = DATAFRAME[obs2run, ])
  GlobalModel = summary(lm.global)
  global.leverages = lm.influence(lm.global, do.coef = FALSE)$hat
  
  gcvs.global = c(gcvs.global, GCV(leverages = matrix(global.leverages, ncol = 1),
                                   yhats = matrix(lm.global$fitted.values, ncol = 1),
                                   dep.var = DATAFRAME$logSALE_VA[obs2run]))
}

ks = c(100,  200,  300,  350,  400,  450,  500,  550,  600,  650,  700,  800, 1000, 1500, 2000, 3000, 4000)
gcv1 = c(0.03390767, 0.02991290, 0.02941916, 0.02938722, 0.02944322, 0.02953900, 0.02964363, 0.02977173, 
         0.02990581, 0.03005645, 0.03020745, 0.03049779, 0.03105754, 0.03245082, 0.03364512, 0.03614031, 
         0.03840753)
gcv2 = c(0.03512758, 0.02889986, 0.02774813, 0.02751932, 0.02742405, 0.02739631, 0.02739526, 0.02742459,
         0.02746483, 0.02752084, 0.02757967, 0.02769759, 0.02792871, 0.02856684, 0.02921754, 0.03072560,
         0.03230943)
gcv3 = c(0.03515802, 0.02876113, 0.02754652, 0.02728218, 0.02715732, 0.02709864, 0.02707271, 0.02707393,
         0.02708750, 0.02711570, 0.02715047, 0.02722432, 0.02737575, 0.02783357, 0.02829078, 0.02932899,
         0.03065546)
gcvs = cbind(gcv1, gcv2, gcv3)
  
pdf("~/NoiseHedonicProject/Noise-Hedonic/graphs/GCVbyModel.pdf", 
    family = "Palatino", height = 6, width = 8)
matplot(ks, gcvs, type = "o", pch = 16, cex = .5, las = 1,
        ylim = c(.025, .05), xlim = c(0, 4000), lty = 1,
        col = c("black", "blue", "red"),
        ylab = "Generalized Cross Validation (GCV) Score",
        xlab = "Bandwidth Size (# of houses sold within last 12 months inluded in LWR)")

abline(h = gcvs.global, col = c("black", "blue", "red"), lty = 2)
lines(c(500, 500), c(0, min(gcvs)), col = "gray80")
lines(c(500, -200), rep(min(gcvs), 2), col = "gray80")
axis(1, at = 500)
axis(2, at = round(min(gcvs), 3), las = 1)
title("Generalized Cross Validation Scores Across Models")

text(500, .031, "LWR Model (1) = Structural Variables + Month Fixed Effects",  
     srt = 13, pos = 4, col = "black", cex = .8)

text(500, .028, "LWR Model (2) = LWR Model (1) + Locational Variables",
     srt = 7, pos = 4, col = "blue", cex = .8)

text(500, .026, "LWR Model (3) = LWR Model (2) + City Fixed Effects", 
     srt = 5, pos = 4, col = "red", cex = .8)

text(rep(200, 3), gcvs.global+.001,
     c("Global Model (1) = Structural Variables + Month*Year Fixed Effects", 
       "Global Model (2) = Global Model (1) + Locational Variables", 
       "Global Model (3) = Global Model (2) + City Fixed Effects"),
     pos = 4, col = c("black", "blue", "red"), cex = .8)
dev.off()