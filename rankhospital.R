############# Programming Assignment 3 #####################

####### Ranking hospitals by outcome in a state ############

# Load packages

library(dplyr)


rankhospital <- function(state, outcome, num = "best") {
  
  ## Read outcome data
  
  outcome_data <- read.csv("outcome-of-care-measures.csv")
  
  
  ## Put outcome in lower case 
  
  outcome <- tolower(outcome)
  
  
  ## Put the State in capital letters
  
  state <- toupper(state)
  
  
  ## Check that state and outcome are valid
  
  if (!state %in% unique(outcome_data[["State"]])) {
    
    stop("Invalid State", call. = TRUE)
    
  } 
  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    
    stop("Invalid Outcome", call. = TRUE)
    
  } 
  
  
  ## Filter by state
  
  outcome_data <- outcome_data[grep(state, outcome_data$State), ]
  
  
  ## Getting the column for the selected outcome
  
  col <- if (outcome == "heart attack") {
    
    11
    
  } else if (outcome == "heart failure") {
    
    17
    
  } else {
    
    23
    
  }
  
  
  ## Getting the rate ordered from lowest to highest in the State and outcome selected
  
  outcome_data <- select(outcome_data, 2, all_of(col))
  
  colnames(outcome_data) <- c("Hospital", "Rate")
  
  suppressWarnings(outcome_data$Rate <- as.numeric(as.character(outcome_data$Rate)))
  
  outcome_data <- outcome_data[order(outcome_data$Rate, outcome_data$Hospital), ] 
  
  
  ## Select one option accordingly to num specified in the function
  
  num <-  if(num == "best"){
    
    1 
    
  }
  
  else if(num == "worst"){
    
    nrow(na.omit(outcome_data))
    
  }
  
  else if(num > nrow(outcome_data)){
    
    stop("NA")
    
  } else {
    
    num
    
  }
  
  
  ## Return hospital name in that state with the given rank 30-day death rate
  
  return(outcome_data$Hospital[num])
  
}

####### Test ############

rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
#rankhospital("MN", "heart attack", 5000)
rankhospital("AL", "heart attack", 20)
