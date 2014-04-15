
# Example run:
# source("pollutantmean.R")
# pollutantmean("specdata", "sulfate", 1:10)
# ## [1] 4.064
# pollutantmean("specdata", "nitrate", 70:72)
# ## [1] 1.706
# pollutantmean("specdata", "nitrate", 23)
# ## [1] 1.281


pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  len <- length(id);
  numobsvec <- rep(0, len)
  meanvec <- rep(0, len)

  for (i in seq_along(id)){
    thisid <- id[i];
    if(thisid<1 | thisid>332)
      stop("Error: input id vector out of range");
    idstring <- if(thisid >= 100)  {
      toString(thisid);
    } else if (thisid >= 10) {
      paste("0", toString(thisid), sep="");
    } else {
      paste("00", toString(thisid), sep="");
    };


    filename <- paste(directory, "/", idstring, ".csv", sep="");

    thisdata <- read.csv(filename);
    pollutant.data <- thisdata[[pollutant]];
    
    numobsvec[i] <- sum(!is.na(pollutant.data));
    meanvec[i] <- mean(pollutant.data, na.rm=T);
    
  }
  
  weighted.mean(meanvec, numobsvec)
}





