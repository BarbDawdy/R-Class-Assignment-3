##
##  goal : Find the best hospital in US by state for a specific outcome
##  input : 2 character US state abbreviation ("TX", "NM", etc...)
##          outcome name from set of outcomes ("heart attack", "heart failure", "pneumonia")
##          example ->  best("TX", "heart attack")
##  output: character vector with name of hospital
##          example -> "CYPRESS FAIRBANKS MEDICAL CENTER"
##
##  description : read in the file outcome-of-care-measures.csv (provided) and return a character vector with
##                the name of the hospital that has the best (lowest) 30-day mortality for the defined outcome in that state.
##
##
##
##  author: Barb Dawdy for Coursera.org R programming course
##
#######################################################################################

best <- function(state, outcome) {
  
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
  #outcomeData.state
  
  idHospital <- which.min(as.double(outcomeData.state[,colName]))
  #idHospital
  
  outcomeData.state[idHospital,"Hospital.Name"]
}