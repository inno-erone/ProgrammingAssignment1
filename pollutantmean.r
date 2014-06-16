##'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id' calculates the mean of a pollutant (sulfate or nitrate) across a 
##'specified list of monitors.  Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory 
## specified in the 'directory argument and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA.

## 'directory' is a character vector of length 1 indicating the location of the CSV files
## 'pollutant' is a character vector of length 1 indicating  the name of the pollutant for which we will calculate the mean; either "sulfate" or "nitrate".
## 'id' is an integer vector indicating the monitor ID numbers to be used




require("foreign")
remove(list=ls())
#setwd("C:/1/DataScienceCourse/r prog")
setwd("G:/r prog")

pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  id <- as.numeric(id)  
  i <- 1
  obs <- numeric()  
  
  if(is.numeric(directory) || is.numeric(pollutant)  ) stop("One or more parameters does not match!")
  
  
  ## format the names of variable direc0tory to reflect in the path to be read
  while(i <= length(as.character(id)) ){
    
    if(id[i]>=1 & id[i]<10)   filename <- paste(getwd(), "/", directory,  "/", "00", id[i], ".csv", sep="")    
    else if(id[i]>=10 & id[i]<100)   filename <- paste(getwd(), "/", directory,  "/", "0", id[i], ".csv", sep="")  
    else if(id[i]>=100 & id[i]<=332)      filename <- paste(getwd(), "/", directory,  "/", id[i], ".csv", sep="")   
    else     return("You entered an id out of range!!!")   
    
    m <- read.csv(filename, as.is=T, header=T)
    m <- m[ , pollutant]
    
    obs <- c(obs, m)
    i <- i+1
  }
  
  ## Return the mean of the pollutant across all monitors list in the 'id' vector (ignoring NA values)
  return( mean(obs, na.rm=T, trim=0) )
}

#pollutantmean("specdata", "nitrate", 1:10)