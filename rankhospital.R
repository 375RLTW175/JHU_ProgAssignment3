## Anthony Smith
## JHU R Programming Week 4 
## 7 June 2020

library(dplyr)


rankhospital <- function(state, outcome, num = "best") {
      ## Read outcome data
      hospdf <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      ## Check that state and outcome are valid
      outcomes <- c("Heart Attack", "Heart Failure", "Pneumonia")
      if(state %in% hospdf$State == FALSE) {
            stop("Invalid state")
      }
      if (outcome %in% outcomes == FALSE) {
            stop("Invalid outcome")
      }
      # make dataframes based on which outcomes the user inputs
      if (outcome == "Heart Attack"){
            hospdf <- hospdf[,c(2,7,11)] 
      }
      if (outcome == "Heart Failure"){
            hospdf <- hospdf[,c(2,7,17)]
      }
      if (outcome == "Pneumonia"){
            hospdf <- hospdf[,c(2,7,23)]
      }
      #make the outcome a numeric
      hospdf[,3] <- as.numeric(hospdf[,3])
      # split the states out
      x <- split(hospdf, hospdf$State)
      # only look at the state that the user inputs
      y <- x[[state]]
      # grab the 1st and third columns and all rows of y
      z <- y[,c(1,3)]
      # add the column names
      colnames(z)<-cbind('Hospital.Name', outcome)
      
      # only return the rank the user input as num
      ## convert num argument to valid rank
      
      if(num == "best") {
            num <- 1 
      }
      
      if (num == "worst") {
            num <- nrow(x) 
      }
      
      ## Order by outcome rate
      z <- z[order(z[, outcome], z[, 'Hospital.Name'], na.last = NA), ]
      
      ## Return hospital name in that state with num rank
      z[num,1]
}  

rankhospital("MN", "Pneumonia", 113)
rankhospital("MN", "Pneumonia", "worst")




      