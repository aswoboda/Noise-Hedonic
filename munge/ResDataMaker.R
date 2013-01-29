##Issue: In a model that excludes some observations (such as if we included GARAGE dummy) we had trouble
#creating the .dbf file because: arguments imply differing number of rows. This script grabs row names and merges with working data
#in order to create a field with the PIN (now we can join resids with parcels in GIS)

#Grab residual from model and row name from the model
resData = data.frame(Res = model.SaleValue3$residuals, RowName = names(model.SaleValue3$residuals))

#Add column of row names to the workingdata
workingdata$RowName = row.names(workingdata)

#Merge the resData table from model with workingdata table
temp = merge(resData, workingdata, all = TRUE)

#From temporary table that merged resData table and workingdata table, extract residuals and PIN
names(temp)
temp = temp[, c(2, 4)]
write.dbf(temp, "../Data/R2GIS/Model3.dbf")