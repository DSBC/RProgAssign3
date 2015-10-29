
rankall <- function(outcome, num ="best") {
  ## Read outcome data
  
  # read the data file
  setwd("C:/Users/chenb1/Desktop/DSci/rprog-data-ProgAssignment3-data")
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  # change data type from character to numeric
  outcomes[, 11] <- suppressWarnings(as.numeric(outcomes[, 11])) # heart attack
  outcomes[, 17] <- suppressWarnings(as.numeric(outcomes[, 17])) # heart failure
  outcomes[, 23] <- suppressWarnings(as.numeric(outcomes[, 23])) # pneumonia
  
  # the outcomes we are investigating 
  target_outcomes <- c("heart attack", "heart failure", "pneumonia")
  # states - a vector of char
  states_v <- sort(unique(outcomes$State))
  # hosiptals
  hospitals <- vector(mode = "character", length=length(states_v))
  
  
  for (i in 1:length(states_v)) {
    
  ## Check that outcome are valid
  if(!outcome %in% target_outcomes) {
    stop("invalid outcome")
  } else {  
    # all outcomes for all state_v[i]; a dataframe
    states <- outcomes[outcomes$State==state_v[i],]
   
    if(outcome == "heart attack") {
      col_num <- 11
    } else if(outcome == "heart failure") {
      col_num <- 17
    } else {
      col_num <- 23
    }
    # order it by outcome and hospital, discard NA
    states <- states[order(states[,col_num], states$Hospital.Name, na.last=NA),]
    
    if (num == "best") {
      hospitals[i] <- (states$Hospital.Name[1])
    } else if (num == "worst") { 
      hospitals[i] <- (states$Hospital.Name[nrow(states)])
    } else if (is.numeric(num)) {
      hospitals[i] <- (states$Hospital.Name[num])
    } else {
      stop("invalid num")
      
    }
  }
  
  } # end for loop
  
  return(data.frame(hospital=hospitals,state=states_v,row.names=states_v))
  
}