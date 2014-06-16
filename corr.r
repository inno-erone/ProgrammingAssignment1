
require("foreign")
remove(list=ls())
setwd("G:/r prog")

corr <- function(directory, threshold=0)
{
  
  count <- length(list.files(paste( getwd(), "/", directory, sep="")))
  id  <- 1:count
  i <- 1
  cum <- numeric()
  
  while(i <= count)  
  {
    
    if(id[i]>0    &   id[i]<=9 )    filename <- paste(getwd(), "/", directory,  "/", "00", id[i], ".csv", sep="")  
    else if (id[i]>9   &   id[i]<100  )    filename <- paste(getwd(), "/", directory,  "/", "0", id[i], ".csv", sep="")
    else  filename <- paste(getwd(), "/", directory,  "/", id[i], ".csv", sep="")
    
    a <- read.csv(filename)
    
    if(sum(complete.cases(a))  > threshold)   cum <- c(cum,  cor(a$nitrate, a$sulfate, use="complete.obs") )
    i <- i+1
  }
  
  return(cum)
}

cr <- corr("specdata", 5000)
summary(cr)
length(cr)
print("..........................................")

cr <- corr("specdata")
summary(cr)
length(cr)