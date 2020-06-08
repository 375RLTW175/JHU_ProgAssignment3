## Anthony Smith
## JHU R Programming Week 4 
## 7 June 2020

library(dplyr)

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

## make a simple histogram of the 30-day death rates from heart attack (column 11 in the outcome dataset),
run

outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])
names(outcome)
# Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
# outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
# in that state. 

best <- function(state, outcome) {
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
      # find the smallest number and exclude NAs
      best<- z[which(z[,2] == min(z[,2],na.rm=TRUE)),]
      
      best<-best$Hospital.Name
      best<-sort(best)[1]
      
      ## Return hospital name in that state with lowest 30-day death
      best

      }

best("MN", "Pneumonia")


