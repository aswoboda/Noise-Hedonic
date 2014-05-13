

load("~/NoiseHedonicProject/Data/R2GIS/CleanData/MixedLWR/step3aoutput-2014-05-04.RData")
# grab the important info from ste3a output
temp = output$yhats[, 1]
tempData = data.frame(yhat = temp,
                      UNIQID = as.numeric(names(temp)))
tempData$Noisebeta = output$beta.noiseHAT[, 1]
tempData$Noisese = output$ses.noiseHAT[, 1]

tempData$OwnOccbeta = output$beta.ownoccHAT[, 1]
tempData$OwnOccse = output$ses.ownoccHAT[, 1]

tempData$percWhitebeta = output$beta.percwhiteHAT[, 1]
tempData$percWhitese = output$ses.percwhiteHAT[, 1]

tempData$perc18beta = output$beta.perc18HAT[, 1]
tempData$perc18se = output$ses.perc18HAT[, 1]

tempData$Constantbeta = output[["beta.(Intercept)"]][, 1]

tempData$Xaahat = tempData$yhat - tempData$Constantbeta

# now bring up the full dataset, merge it, and run our final LWR...
load("../Data/R2GIS/CleanData/MixedLWR/mixedDATA.RData")

newdata = merge(DATAFRAME, tempData, sort = FALSE)

summary(newdata$UNIQID - DATAFRAME$UNIQID) # should be all zeros!

DATAFRAME = newdata
DATAFRAME$YminusXaahat = DATAFRAME$logSALE_VA - DATAFRAME$Xaahat

save(DATAFRAME, file = "~/NoiseHedonicProject/Data/R2GIS/CleanData/MixedLWR/step3bOutput.RData")

