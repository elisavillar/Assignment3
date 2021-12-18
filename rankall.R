############# Programming Assignment 3 #####################

########## Ranking hospitals in all states #################

# Load packages

library(dplyr)

rankall <- function(outcome, num = "best") { 
  
  ## Read outcome data
  
  outcome_data <- read.csv("outcome-of-care-measures.csv")
  
  
  ## Put outcome in lower case 
  
  outcome <- tolower(outcome)
  
  
  ## Check that outcome is valid
  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    
    stop("Invalid Outcome", call. = TRUE)
    
  } 
  
  
  ## Getting the column for the selected outcome
  
  col <- if (outcome == "heart attack") {
    
    11
    
  } else if (outcome == "heart failure") {
    
    17
    
  } else {
    
    23
    
  }
  
  
  ## Getting the rate ordered from lowest to highest in the State and outcome selected
  
  outcome_data <- select(outcome_data, 2, 7, all_of(col))
  
  colnames(outcome_data) <- c("Hospital", "States", "Rate")
  
  suppressWarnings(outcome_data$Rate <- as.numeric(as.character(outcome_data$Rate)))
  
  output <- vector()
  
  states <- sort(unique(outcome_data[ , 2]))
  
  
  ## Getting the hospital name for each State on the given ranking
  
  for(i in 1:length(states)) {
    
    state_data <- outcome_data[grep(states[i], outcome_data$State), ]
    
    order_data <- state_data[order(state_data[, 3], state_data[, 1], na.last = NA), ]
    
    hospital <- if(num == "best") {
      
      order_data[1, 1]
      
    } else if(num == "worst") {
      
      order_data[nrow(order_data), 1]
      
    } else{
      
      order_data[num, 1]
      
    }
    
    output <- append(output, c(hospital, states[i]))
    
  }
  
  ## Return a data frame with the hospital names and the (abbreviated) state name
  
  output <- as.data.frame(matrix(output, length(states), 2, byrow = TRUE))
  
  colnames(output) <- c("hospital", "state")
  
  rownames(output) <- states
  
  output
  
}

####### Test ############

head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)
