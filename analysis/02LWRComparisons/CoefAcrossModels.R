#Array script that compares coefficients across nested models

getwd() #assume I'm in the project default working directory "Noise Hedonic Project"
require(foreign)
source("helper/LWRfunctions.R")
filePrefix = "../Data/R2GIS/CleanData/"
inputFile = "Sales20052010.dbf"
DATAFRAME = read.dbf(paste0(filePrefix, inputFile))
obs2run = which(DATAFRAME$TimePeriod>11)
lwr.obs = dim(DATAFRAME[obs2run,])[1]#Selects how many obs are run in LWR

#Create an empty array for 3 variables, 3 time lags, using all obs used in LWR, across 2 models
my.array = array(1:571464, c(3,3,lwr.obs,2), 
                 dimnames=list(c("Noise", "Lot", "Home"), 
                               c("6 month lag", "12 month lag", "24 month lag"),
                               1:lwr.obs,
                               c("Small model", "Big model")))


####Load each model and populate the empty array (There are six models to upload)

###TIME LAG 6 MONTHS
dataPath = "../Data/R2GIS/LWRoutput/"
filelist = list.files(path = dataPath)
files2open = filelist[which(substr(filelist, 1, 26) == "Sales20052010LWRoutput2013")]
##BIG MODEL
myFile = 1 # Determines which model we are opening
load(paste0(dataPath, files2open[myFile], sep = ""))
#Choose bandwidth by finding minimum GCV score
gcv = GCV(output$leverages, output$yhats, DATAFRAME$logSALE_VA[obs2run])
min.gcv <- min(gcv)
myBandwidth = which(gcv == min.gcv)
#Noise
beta.noise = which(names(output)=="beta.MAX") #find which column beta noise is in
myCoeff.beta = names(output)[beta.noise]# choose a coefficient to work with
coef.table <- data.frame(output[myCoeff.beta])
myNoise = coef.table[myBandwidth]
my.array["Noise", "6 month lag", , "Big model"] = myNoise[1:lwr.obs,1] #Populate array field
#Lot size
beta.lot = which(names(output)=="beta.ACRES_POLY") #find which column beta noise is in
myCoeff.beta = names(output)[beta.lot]# choose a coefficient to work with
coef.table <- data.frame(output[myCoeff.beta])
myLot = coef.table[myBandwidth]
my.array["Lot", "6 month lag", , "Big model"] = myLot[1:lwr.obs, 1] #Populate array field
#Home size
beta.home = which(names(output)=="beta.FIN_SQ_FT") #find which column beta noise is in
myCoeff.beta = names(output)[beta.home]# choose a coefficient to work with
coef.table <- data.frame(output[myCoeff.beta])
myHome = coef.table[myBandwidth]
my.array["Home", "6 month lag", , "Big model"] = myHome[1:lwr.obs, 1] #Populate array field
##SMALL MODEL
myFile = 2 # Determines which model we are opening
load(paste0(dataPath, files2open[myFile], sep = ""))
#Choose bandwidth by finding minimum GCV score
gcv = GCV(output$leverages, output$yhats, DATAFRAME$logSALE_VA[obs2run])
min.gcv <- min(gcv)
myBandwidth = which(gcv == min.gcv)
#Noise
beta.noise = which(names(output)=="beta.MAX") #find which column beta noise is in
myCoeff.beta = names(output)[beta.noise]# choose a coefficient to work with
coef.table <- data.frame(output[myCoeff.beta])
myNoise = coef.table[myBandwidth]
my.array["Noise", "6 month lag", , "Small model"] = myNoise[1:lwr.obs,1] #Populate array field
#Lot size
beta.lot = which(names(output)=="beta.ACRES_POLY") #find which column beta noise is in
myCoeff.beta = names(output)[beta.lot]# choose a coefficient to work with
coef.table <- data.frame(output[myCoeff.beta])
myLot = coef.table[myBandwidth]
my.array["Lot", "6 month lag", , "Small model"] = myLot[1:lwr.obs, 1] #Populate array field
#Home size
beta.home = which(names(output)=="beta.FIN_SQ_FT") #find which column beta noise is in
myCoeff.beta = names(output)[beta.home]# choose a coefficient to work with
coef.table <- data.frame(output[myCoeff.beta])
myHome = coef.table[myBandwidth]
my.array["Home", "6 month lag", , "Small model"] = myHome[1:lwr.obs, 1] #Populate array field



###TIME LAG 12 MONTHS
##BIG MODEL
myFile = 3 # Determines which model we are opening
load(paste0(dataPath, files2open[myFile], sep = ""))
#Choose bandwidth by finding minimum GCV score
gcv = GCV(output$leverages, output$yhats, DATAFRAME$logSALE_VA[obs2run])
min.gcv <- min(gcv)
myBandwidth = which(gcv == min.gcv)
#Noise
beta.noise = which(names(output)=="beta.MAX") #find which column beta noise is in
myCoeff.beta = names(output)[beta.noise]# choose a coefficient to work with
coef.table <- data.frame(output[myCoeff.beta])
myNoise = coef.table[myBandwidth]
my.array["Noise", "12 month lag", , "Big model"] = myNoise[1:lwr.obs,1] #Populate array field
#Lot size
beta.lot = which(names(output)=="beta.ACRES_POLY") #find which column beta noise is in
myCoeff.beta = names(output)[beta.lot]# choose a coefficient to work with
coef.table <- data.frame(output[myCoeff.beta])
myLot = coef.table[myBandwidth]
my.array["Lot", "12 month lag", , "Big model"] = myLot[1:lwr.obs, 1] #Populate array field
#Home size
beta.home = which(names(output)=="beta.FIN_SQ_FT") #find which column beta noise is in
myCoeff.beta = names(output)[beta.home]# choose a coefficient to work with
coef.table <- data.frame(output[myCoeff.beta])
myHome = coef.table[myBandwidth]
my.array["Home", "12 month lag", , "Big model"] = myHome[1:lwr.obs, 1] #Populate array field
##SMALL MODEL
myFile = 2 # Determines which model we are opening
load(paste0(dataPath, files2open[myFile], sep = ""))
#Choose bandwidth by finding minimum GCV score
gcv = GCV(output$leverages, output$yhats, DATAFRAME$logSALE_VA[obs2run])
min.gcv <- min(gcv)
myBandwidth = which(gcv == min.gcv)
#Noise
beta.noise = which(names(output)=="beta.MAX") #find which column beta noise is in
myCoeff.beta = names(output)[beta.noise]# choose a coefficient to work with
coef.table <- data.frame(output[myCoeff.beta])
myNoise = coef.table[myBandwidth]
my.array["Noise", "12 month lag", , "Small model"] = myNoise[1:lwr.obs,1] #Populate array field
#Lot size
beta.lot = which(names(output)=="beta.ACRES_POLY") #find which column beta noise is in
myCoeff.beta = names(output)[beta.lot]# choose a coefficient to work with
coef.table <- data.frame(output[myCoeff.beta])
myLot = coef.table[myBandwidth]
my.array["Lot", "12 month lag", , "Small model"] = myLot[1:lwr.obs, 1] #Populate array field
#Home size
beta.home = which(names(output)=="beta.FIN_SQ_FT") #find which column beta noise is in
myCoeff.beta = names(output)[beta.home]# choose a coefficient to work with
coef.table <- data.frame(output[myCoeff.beta])
myHome = coef.table[myBandwidth]
my.array["Home", "12 month lag", , "Small model"] = myHome[1:lwr.obs, 1] #Populate array field



###TIME LAG 24 MONTHS
##BIG MODEL
myFile = 4 # Determines which model we are opening
load(paste0(dataPath, files2open[myFile], sep = ""))
#Choose bandwidth by finding minimum GCV score
gcv = GCV(output$leverages, output$yhats, DATAFRAME$logSALE_VA[obs2run])
min.gcv <- min(gcv)
myBandwidth = which(gcv == min.gcv)
#Noise
beta.noise = which(names(output)=="beta.MAX") #find which column beta noise is in
myCoeff.beta = names(output)[beta.noise]# choose a coefficient to work with
coef.table <- data.frame(output[myCoeff.beta])
myNoise = coef.table[myBandwidth]
my.array["Noise", "24 month lag", , "Big model"] = myNoise[1:lwr.obs,1] #Populate array field
#Lot size
beta.lot = which(names(output)=="beta.ACRES_POLY") #find which column beta noise is in
myCoeff.beta = names(output)[beta.lot]# choose a coefficient to work with
coef.table <- data.frame(output[myCoeff.beta])
myLot = coef.table[myBandwidth]
my.array["Lot", "24 month lag", , "Big model"] = myLot[1:lwr.obs, 1] #Populate array field
#Home size
beta.home = which(names(output)=="beta.FIN_SQ_FT") #find which column beta noise is in
myCoeff.beta = names(output)[beta.home]# choose a coefficient to work with
coef.table <- data.frame(output[myCoeff.beta])
myHome = coef.table[myBandwidth]
my.array["Home", "24 month lag", , "Big model"] = myHome[1:lwr.obs, 1] #Populate array field
##SMALL MODEL
myFile = 2 # Determines which model we are opening
load(paste0(dataPath, files2open[myFile], sep = ""))
#Choose bandwidth by finding minimum GCV score
gcv = GCV(output$leverages, output$yhats, DATAFRAME$logSALE_VA[obs2run])
min.gcv <- min(gcv)
myBandwidth = which(gcv == min.gcv)
#Noise
beta.noise = which(names(output)=="beta.MAX") #find which column beta noise is in
myCoeff.beta = names(output)[beta.noise]# choose a coefficient to work with
coef.table <- data.frame(output[myCoeff.beta])
myNoise = coef.table[myBandwidth]
my.array["Noise", "24 month lag", , "Small model"] = myNoise[1:lwr.obs,1] #Populate array field
#Lot size
beta.lot = which(names(output)=="beta.ACRES_POLY") #find which column beta noise is in
myCoeff.beta = names(output)[beta.lot]# choose a coefficient to work with
coef.table <- data.frame(output[myCoeff.beta])
myLot = coef.table[myBandwidth]
my.array["Lot", "24 month lag", , "Small model"] = myLot[1:lwr.obs, 1] #Populate array field
#Home size
beta.home = which(names(output)=="beta.FIN_SQ_FT") #find which column beta noise is in
myCoeff.beta = names(output)[beta.home]# choose a coefficient to work with
coef.table <- data.frame(output[myCoeff.beta])
myHome = coef.table[myBandwidth]
my.array["Home", "24 month lag", , "Small model"] = myHome[1:lwr.obs, 1] #Populate array field

###########################################################
### Create a .pdf file and plot each model against each other by coefficient and time lag
pdf("analysis/02LWRComparisons/CoefacrossModelsTest.pdf", height = 8, width = 12)
par(mfrow = c(3, 3))
notebooks = c(1:3) #How many coefficients are we analyzing
pages = c(1:3) #How many time lags are we analyzing
for (i in notebooks) {
  min.coef = min(my.array[i, , ,]) #Finds minimum value for the given coefficient across all models and time lags
  max.coef = max(my.array[i, , ,]) #Finds minimum value for the given coefficient across all models and time lags
  for (j in pages) {
    myX = my.array[i,j, ,"Small model"] #Extracts given coef from small model in given time lag
    myY = my.array[i,j, ,"Big model"] #Extracts given coef from big model in given time lag
    reg = lm(myY ~ myX) #Runs a linear regression on the two coefficients from different models
    plot(myX, myY,
         xlim = c(min.coef,max.coef),
         ylim = c(min.coef, max.coef),
         xlab = "Small coef",
         ylab = "Big coef",
         main = "test")
    abline(reg, col = "red")
    abline(0,1) #plots 45 deg. angle
  }
}
dev.off()

#Trying to print r.squared result -- error of invalid graphic parameter
#title("R.square",(summary(reg)[8]), line = -1) #prints r-squared result from regression