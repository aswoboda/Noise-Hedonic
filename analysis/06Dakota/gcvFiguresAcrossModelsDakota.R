
# The goal of this script is to create a figure showing the GCV plots for Dakota county
# when we run LWR using only Dakota county and the additional variables available
# vs. when we ran the model without those additional variables

# Do we get different results if we run LWR on Dakota county alone using only the variables
# we have globally vs. running LWR on the entire study area?


setwd("~/NoiseHedonicProject/Noise-Hedonic/analysis/06Dakota/") #assume I'm in the project default working directory "Noise Hedonic Project"

require(foreign)
source("../../helper/LWRfunctions.R")

DATAFRAME = read.dbf("~/NoiseHedonicProject/Data/R2GIS/CleanData/Sales20052010.dbf")
obs2run = which(DATAFRAME$COUNTY_ID == "037" & DATAFRAME$BATH>.8 & DATAFRAME$TimePeriod>11)

#dataPath = "../Data/R2GIS/CleanData/"
filelist = list.files()
files2open = filelist[which(substr(filelist, 1, 6) == "Dakota")]
# mymat = c()
pdf("GCVplotsAll.pdf")
for (myFile in 1:length(files2open)) {
  load(files2open[myFile])
  if (dim(output[[1]])[1] == 9140) gcvs = GCV(output$leverages, output$yhats, DATAFRAME$logSALE_VA[obs2run])
  ks = as.numeric(substr(colnames(output[[1]]), 2, 5))
  plot(ks, gcvs, type = "l", main = "")
  title(MYMODEL, cex.main = .5)
  title(paste0("N = ", dim(output[[1]])[1]), line = .5)
  title(paste("data: ", substr(files2open[myFile], 22, nchar(files2open[myFile]))), line = -1)
  lmreg = lm(MYMODEL, data = DATAFRAME[obs2run, ])
  levs = lm.influence(lmreg, do.coef = FALSE)$hat
  globalGCV = GCV(cbind(levs), cbind(lmreg$fitted.values), DATAFRAME$logSALE_VA[obs2run])
  title(paste("global model gcv:", round(globalGCV, 3)), line = -2)
  title(paste("min gcv:", round(min(gcvs), 5)), line = -3)
  title(paste("at bandwidth:", names(gcvs)[which.min(gcvs)]), line = -4)
  #mymat = cbind(mymat, gcvs)
}
dev.off()


# mymat = cbind(as.numeric(substr(rownames(mymat), 2, 6)), mymat)
# matplot(mymat)
# matplot(mymat[, 1], mymat[, -1], type = "o")
