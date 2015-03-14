
#Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a 
#specified list of monitors. The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. 
#Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory 
#specified in the 'directory' argument and returns the mean of the pollutant across all of the monitors, ignoring any 
#missing values coded as NA.



> setwd("~/R_Coursera")
> 
> pollutantmean <- function(directory, pollutant, id = 1:332) {
+      #set the path
+      path = directory
+      
+      #get the file List in that directory
+      fileList = list.files(path)
+      
+      #extract the file names and store as numeric for comparison
+      file.names = as.numeric(sub("\\.csv$","",fileList))
+      
+      #select files to be imported based on the user input or default
+      selected.files = fileList[match(id,file.names)]
+      
+      #import data and convert into a data frame
+      Data = lapply(file.path(path,selected.files),read.csv)
+      Data = do.call(rbind.data.frame,Data)
+      
+      #Return mean to 3 dec place
+      result <- mean(Data[,pollutant],na.rm=TRUE)
+      return(round(result, 3))
+  }
> 
> 
> pollutantmean("specdata", "sulfate", 1:10)
[1] 4.064
> pollutantmean("specdata", "nitrate", 20:30)
[1] 3.881
