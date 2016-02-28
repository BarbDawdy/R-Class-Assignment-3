##
##  goal : Find the best hospital in US by state for a specific outcome
##  input : 2 character US state abbreviation ("TX", "NM", etc...)
##          outcome name from set of outcomes ("heart attack", "heart failure", "pneumonia")
##          rank of a hospital in that state for that outcome (num)
##          example ->  rankhospital("TX", "heart failure", 4) OR rankhospital("TX", "heart failure", "worst")
##  output: character vector containing the name of the hospital with the 5th lowest 30-day death rate
##          for heart failure
##          example -> "DETAR HOSPITAL NAVARRO"  OR "ETMC CARTHAGE"
##
##  description : read in the file outcome-of-care-measures.csv (provided) and return a character vector with
##                the name of the hospital that has the rank 30-day mortality for the defined outcome in that state.
##
##
##
##  author: Barb Dawdy for Coursera.org R programming course
##
#######################################################################################

rankhospital <- function(state, outcome, num = "best") {
  
  # read in the outcome data file - yeah hard coded but input specified by assignment did not allow for dir/file name future improvement to fix this
  outcomeData <- read.csv("E:\\dev\\R\\rprog-data-ProgAssignment3-data\\outcome-of-care-measures.csv",header=TRUE,sep=",",colClasses = "character",na.strings="Not Available")
  
  # check input for invalid US state code and invalid outcome value
  validState = unique(outcomeData[,"State"])
  if (!state %in% validState) {
    stop("invalid state")
  }
  
  validOutcome <- c("heart attack","heart failure","pneumonia")
  if (!outcome %in% validOutcome) {
    stop("invalid Outcome - valid choices are heart attack,heart failure, or pneumonia")
  }
  
  # column mappings for outcomes
  sheetColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  colName <- sheetColName[match(outcome,validOutcome)]
  
  #colName
  
  # return the hospital name in the US state with the lowest 30 day death rate for the specified outcome
  outcomeData.state <- outcomeData[outcomeData$State==state,]
  #outcomeData
  
  # order data by outcome to use with rank numbers
  sorted.outcomeData.state <- outcomeData.state[order(as.numeric(outcomeData.state[[colName]]),outcomeData.state[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ]
  
  #handle the num rank words and the number passed in will work
  #if num rank < 1 (0 or neg) they will get what they wanted
  #if num rank > number rows of data they will get NA
  #if best get top of sorted list
  #if worst then get last in sorted list

  if (num=="best") num = 1
  if (num=='worst') num = nrow(sorted.outcomeData.state)
  
  sorted.outcomeData.state[num,"Hospital.Name"]
}