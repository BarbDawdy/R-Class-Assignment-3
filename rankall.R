##
##  goal : Find the best hospital in US by state for a specific outcome
##  input : outcome name from set of outcomes ("heart attack", "heart failure", "pneumonia")
##          rank of a hospital in that state for that outcome (num)
##          example ->  rankall("heart failure", 4) OR rankall("heart failure", "worst") OR rankall("heart attack", "best")
##  output: return a value for every state (some may be NA) in a dataframe with columns - hospital name and state
##          for heart failure
##          example -> 
##                 > head(rankall("heart attack", 20), 10)
##                 hospitals state
##                 AL      D W MCMILLAN MEMORIAL HOSPITAL    AL
##                 AK                                <NA>    AK
##                 AZ JOHN C LINCOLN DEER VALLEY HOSPITAL    AZ
##                 AR   ARKANSAS METHODIST MEDICAL CENTER    AR
##                 CA               SHERMAN OAKS HOSPITAL    CA
##                 CO            SKY RIDGE MEDICAL CENTER    CO
##                 CT             MIDSTATE MEDICAL CENTER    CT
##                 DE                                <NA>    DE
##                 DC                                <NA>    DC
##                 FL      SOUTH FLORIDA BAPTIST HOSPITAL    FL
##
##  description : read in the file outcome-of-care-measures.csv (provided) and return a dataframe with columns - hospital name and state for that outcome
##
##  from assignment : NOTE: For the purpose of this part of the assignment (and for efficiency), your function should NOT call
##                          the rankhospital function from the previous section.
##
##
##
##  author: Barb Dawdy for Coursera.org R programming course
##
#######################################################################################

rankall <- function(outcome, num = "best") {
  
  # read in the outcome data file - yeah hard coded but input specified by assignment did not allow for dir/file name future improvement to fix this
  outcomeData <- read.csv("E:\\dev\\R\\rprog-data-ProgAssignment3-data\\outcome-of-care-measures.csv",header=TRUE,sep=",",colClasses = "character",na.strings="Not Available")

  # get the states and check input for valid outcome value
  # sort unique states so dataframe presorted by state
  validState = sort(unique(outcomeData[,"State"]))  
  
  validOutcome <- c("heart attack","heart failure","pneumonia")
  if (!outcome %in% validOutcome) {
    stop("invalid Outcome - valid choices are heart attack,heart failure, or pneumonia")
  }
  
  # column mappings for outcomes
  sheetColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  colName <- sheetColName[match(outcome,validOutcome)]
  
  #colName
  
  #init
  hospitals <-character(0)  
  
  ## For each state, find the hospital of the given rank  
  for (i in seq_along(validState)) {
    # return the hospital name in the US state with the lowest 30 day death rate for the specified outcome
    outcomeData.state <- outcomeData[outcomeData$State==validState[i],]
    #outcomeData
    ## For each state, find the hospital of the given rank
    # order data by outcome to use with rank numbers
    sorted.outcomeData.state <- outcomeData.state[order(as.numeric(outcomeData.state[[colName]]),outcomeData.state[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ]
    
    #handle the num rank words and the number passed in will work
    #if num rank < 1 (0 or neg) they will get what they wanted
    #if num rank > number rows of data they will get NA
    #if best get top of sorted list
    #if worst then get last in sorted list
    
    this.num = num
    if (this.num=="best") this.num = 1
    if (this.num=='worst') this.num = nrow(sorted.outcomeData.state)
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    hospitals[i] <- sorted.outcomeData.state[this.num,"Hospital.Name"]   
    
  }

  data.frame(hospitals=hospitals,state=validState,row.names=validState)

}