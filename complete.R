# Example run: 
#
# source("complete.R")
# complete("specdata", 1)
# ##   id nobs
# ## 1  1  117
# complete("specdata", c(2, 4, 8, 10, 12))
# ##   id nobs
# ## 1  2 1041
# ## 2  4  474
# ## 3  8  192
# ## 4 10  148
# ## 5 12   96
# complete("specdata", 30:25)
# ##   id nobs
# ## 1 30  932
# ## 2 29  711
# ## 3 28  475
# ## 4 27  338
# ## 5 26  586
# ## 6 25  463
# complete("specdata", 3)
# ##   id nobs
# ## 1  3  243




complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  
  len <- length(id);
  nobs <- rep(0, len)
  
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
    
    data <- read.csv(filename);
    nobs[i] <- sum(!is.na(data$sulfate) & !is.na(data$nitrate))
  }
  
  data.frame(id, nobs)
}