
## Write a function that reads a directory full of files and reports the number of completely observed cases in each data file. 
## The function should return a data frame where the first column is the name of the file and the second column is the number 
## of complete cases.

## Return a data frame of the form:
## id nobs
## 1 117
## 2 1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the
## number of complete cases



require("foreign")
remove(list=ls())
#setwd("C:/1/DataScienceCourse/r prog")
setwd("G:/r prog")

complete <- function(directory, id = 1:332) {
  
  id <- as.numeric(id)    
  i <- 1
  nobs <- numeric()
  my_ids <- numeric()
  
  if(is.numeric(directory) ) stop("the directory does not match!")
  
  while(i <= length(as.character(id)) )
  {
    if(id[i]>=1 & id[i]<10)   filename <- paste(getwd(), "/", directory,  "/", "00", id[i], ".csv", sep="")    
    else if(id[i]>=10 & id[i]<100)   filename <- paste(getwd(), "/", directory,  "/", "0", id[i], ".csv", sep="")  
    else if(id[i]>=100 & id[i]<=332)      filename <- paste(getwd(), "/", directory,  "/", id[i], ".csv", sep="")   
    else return("You entered an id out of range!!!")   
    
    my_ids <- c(my_ids, id[i])
    nobs <- c(nobs, sum(complete.cases(read.csv(filename, as.is=T, header=T))))
    i <- i+1
  }
  
  my_df <- as.data.frame(cbind(id = my_ids, nobs = nobs))
  return(my_df)
}

#complete("specdata", 30:25)