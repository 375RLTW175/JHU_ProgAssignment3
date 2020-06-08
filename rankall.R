## Anthony Smith
## JHU R Programming Week 4 
## 7 June 2020

library(dplyr)


rankall <- function(outcome, num = "best") {
      ## Read outcome data
      hospdf <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      ## Check that outcome is valid
      outcomes <- c("Heart Attack", "Heart Failure", "Pneumonia")
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
      
      
      #make a shell dataframe outside of the for loop
      rankdf <- data.frame()
      
      for(state in sort(unique(hospdf[,2]))){
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
                  rnum <- 1 
            } else if (num == "worst") {
                  rnum <- nrow(x) 
            }
            else {rnum = num}
            
            ## Order by outcome rate
            z <- z[order(z[, outcome], z[, 'Hospital.Name'], na.last = NA), ]
            
            hospName <- z[rnum,1]
            
            # the row from that state to the dataframe
            rankdf <- rbind(rankdf,
                            data.frame(Hospital = hospName, State = state))
      }
      
      ## Return the dataframe
      rankdf
      
      
}  

head(rankall("Heart Attack", 20), 10)
tail(rankall("Pneumonia", "worst"), 3)
tail(rankall("Heart Failure"), 10)

best("SC", "Heart Attack")
best("NY", "Pneumonia")
best("AK", "Pneumonia")
rankhospital("NY", "Heart Attack", 7)
rankhospital("TX", "Pneumonia", 10)
r <- rankall("Heart Attack", 4)
as.character(subset(r, state == "HI")$hospital)
r
rankhospital("NJ", "Pneumonia", 65)
r <- rankall("Heart Failure", 10)
as.character(subset(r, state == "NV")$hospital)
r
