corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  
  MAX <- 332;
  len <- length(MAX);
  nobs <- rep(0, MAX)
  correls <- rep(-2, MAX)
  
  for (i in 1:MAX){
    thisid <- i;
    
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
    
    data <- read.csv(filename);
    nobs[i] <- sum(!is.na(data$sulfate) & !is.na(data$nitrate))
    
    if(nobs[i]>threshold) {
      correls[i] <- cor(data$sulfate, data$nitrate, use = "complete");
    }
  }
  
  correls[correls >= -1.5]
  
}