#Plot the 30-day mortality rates for heart attack

outcome <- read.csv("E:\\dev\\R\\rprog-data-ProgAssignment3-data\\outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])
