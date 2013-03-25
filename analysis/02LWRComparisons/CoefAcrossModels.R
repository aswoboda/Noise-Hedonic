#Array script that compares coefficients across nested models

getwd() #assume I'm in the project default working directory "Noise Hedonic Project"
require(foreign)
source("helper/LWRfunctions.R")
filePrefix = "../Data/R2GIS/CleanData/"
inputFile = "Sales20052010.dbf"
DATAFRAME = read.dbf(paste0(filePrefix, inputFile))
obs2run = which(DATAFRAME$TimePeriod>11)

#Create an empty array for 3 variables, 3 time lags, using all obs used in LWR, across 2 models
my.array = array(NA, c(3, 3, length(obs2run), 2), 
                 dimnames=list(c("Noise", "Lot", "Home"), 
                               c("6 month lag", "12 month lag", "24 month lag"),
                               DATAFRAME$UNIQID[obs2run],
                               c("Small model", "Big model")))

####Load each model and populate the empty array (There are six models to upload)
dataPath = "../Data/R2GIS/LWRoutput/"
filelist = list.files(path = dataPath)
files2open = filelist[which(substr(filelist, 1, 26) == "Sales20052010LWRoutput2013")]
# files are ordered first by lag, then by big vs. small
# [1] "Sales20052010LWRoutput2013-03-21-Lag06-big.RData"  
# [2] "Sales20052010LWRoutput2013-03-21-Lag06-small.RData"
# [3] "Sales20052010LWRoutput2013-03-21-Lag12-big.RData"  
# [4] "Sales20052010LWRoutput2013-03-21-Lag12-small.RData"
# [5] "Sales20052010LWRoutput2013-03-21-Lag24-big.RData"  
# [6] "Sales20052010LWRoutput2013-03-21-Lag24-small.RData"

Lag = c(rep("6 month lag", 2), rep("12 month lag", 2), rep("24 month lag", 2))
ModelSize = rep(c("Small model", "Big model"), 3)

for (myFile in 1:6) {# Determines which model we are opening
  load(paste0(dataPath, files2open[myFile], sep = ""))
  #Choose bandwidth by finding minimum GCV score
  gcv = GCV(output$leverages, output$yhats, DATAFRAME$logSALE_VA[obs2run])
  min.gcv <- min(gcv)
  myBandwidth = which(gcv == min.gcv)
  my.array["Noise", Lag[myFile], , ModelSize[myFile]] = output[["beta.MAX"]][, myBandwidth]
  my.array["Lot", Lag[myFile], , ModelSize[myFile]] = output[["beta.ACRES_POLY"]][, myBandwidth]
  my.array["Home", Lag[myFile], , ModelSize[myFile]] = output[["beta.FIN_SQ_FT"]][, myBandwidth]
}

###########################################################
### Create a .pdf file and plot each model against each other by coefficient and time lag
png("analysis/02LWRComparisons/CoefAcrossModels.png", height = 1200, width = 900)
par(mfrow = c(4, 3))
notebooks = c(1:3) #How many coefficients are we analyzing
pages = c(1:3) #How many time lags are we analyzing
for (i in notebooks) {
  min.coef = min(my.array[i, , , ]) #Finds minimum value for the given coefficient across all models and time lags
  max.coef = max(my.array[i, , , ]) #Finds maximum value for the given coefficient across all models and time lags
  for (j in pages) {
    myX = my.array[i, j, , "Small model"] #Extracts given coef from small model in given time lag
    myY = my.array[i, j, , "Big model"] #Extracts given coef from big model in given time lag
    reg = lm(myY ~ myX) #Runs a linear regression on the two coefficients from different models
    plot(myX, myY, las = 1, cex = .2,
         xlim = c(min.coef, max.coef),
         ylim = c(min.coef, max.coef),
         xlab = "Small model coefs",
         ylab = "Big model coefs",
         main = paste(c("Noise", "Lot", "Home")[i], Lag[2*j]))
    abline(reg, col = "red")
    abline(0, 1) #plots 45 deg. angle
    title(paste("R Squared", round(summary(reg)$r.squared, 2)), line = -1)
  }
}

# Now I want to make a single plot of the coefficient densities, 
# with a line for each lag/model combination

#Create an array for 3 variables, 3 time lags, 2 models, using all obs used in LWR, and density (x and y values)
dens.arr = array(NA, c(3, 3, 2, 512, 2), 
                 dimnames = list(c("Noise", "Lot", "Home"), 
                                 c("6 month lag", "12 month lag", "24 month lag"),
                                 c("Small model", "Big model"),
                                 1:512,
                                 c("x", "y")))

for (myvar in c("Noise", "Lot", "Home")) {
  for (mylag in c("6 month lag", "12 month lag", "24 month lag")) {
    for (mymodel in c("Small model", "Big model")) {
      temp = density(my.array[myvar, mylag, , mymodel])
      dens.arr[myvar, mylag, mymodel, , "x"] = temp$x
      dens.arr[myvar, mylag, mymodel, , "y"] = temp$y
    }
  }
}

for (k in 1:3) {
  minx = min(dens.arr[k, , , , "x"])
  maxx = max(dens.arr[k, , , , "x"])
  maxy = max(dens.arr[k, , , , "y"])
  
  cols = c("black", "blue", "red")
  plot(c(minx, maxx), c(0, maxy), type = "n", axes = F,
       xlim = c(minx, maxx),
       ylim = c(0, maxy),
       xlab = "coef values",
       ylab = "",
       main = c("Noise", "Lot", "Home")[k])
  axis(1)
  for (i in 1:3) {
    for (j in 1:2) {
      lines(dens.arr[k, i, j, , "x"], dens.arr[k, i, j, , "y"], col = cols[i], lw = 2, lty = j)
    }
  }
}
dev.off()
