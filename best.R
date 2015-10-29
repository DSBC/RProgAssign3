
best <- function(state, outcome) {
  ## Read outcome data
  
  # read the data file
  setwd("C:/Users/chenb1/Desktop/DSci/rprog-data-ProgAssignment3-data")
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  # change data type from character to numeric
  outcomes[, 11] <- suppressWarnings(as.numeric(outcomes[, 11])) # heart attack
  outcomes[, 17] <- suppressWarnings(as.numeric(outcomes[, 17])) # heart failure
  outcomes[, 23] <- suppressWarnings(as.numeric(outcomes[, 23])) # pneumonia
  
  target_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if (!state %in% outcomes$State) {
    stop("invalid state")
  } else if(!outcome %in% target_outcomes) {
    stop("invalid outcome")
  } else {  ## Return hospital name in that state with lowest 30-day death rate
    # all outcomes for the specific state; a dataframe
    states <- outcomes[outcomes$State==state, ]
    
      if(outcome == "heart attack") {
        col_num <- 11
      } else if(outcome == "heart failure") {
        col_num <- 17
      } else {
        col_num <- 23
      }
    # order it by outcome and hospital, discard NA
    states <- states[order(states[,col_num], states$Hospital.Name, na.last=NA),]
    # return first hospital
    return (states$Hospital.Name[1])
  }
  
}
