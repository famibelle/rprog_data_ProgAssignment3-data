#Finding the best hospital in a state
# Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
# outcome name. The function reads the outcome-of-care-measures.csv le and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specied outcome
# in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
# be one of \heart attack", \heart failure", or \pneumonia". Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the rankings.
# Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
# be sorted in alphabetical order and the rst hospital in that set should be chosen (i.e. if hospitals \b", \c",
# and \f" are tied for best, then hospital \b" should be returned).

best <- function(state="TX", outcome="heart attack") {
	## Read outcome data
	## Possible values are 
	#		Heart Attack
	#		Heart Failure
	#		Pneumonia
	outcome_of_care_measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character",check.names=FALSE) ## with check.names=FALSE to avoid spaces to be converted into "."

	## Check that state and outcome are valid
	if (!(toupper(state) %in% outcome_of_care_measures$State)) stop("invalid state")
	if (!(toupper(outcome) %in% toupper(c("Heart Attack", "Heart Failure", "Pneumonia")))) stop("invalid outcome")

	## Return hospital name in that state with lowest 30-day death rate
	
	elt <- grep(pattern = outcome,x = names(outcome_of_care_measures), ignore.case=TRUE) ## elt[1] gives the column number,
	
	# find out the lowest 30-day death rate line number in column elt[1]
	outcome_of_care_measures[,elt[1]] <- suppressWarnings(as.numeric(outcome_of_care_measures[,elt[1]])) #make it numeric and suppres the warning message
	lowest <- min(outcome_of_care_measures[ outcome_of_care_measures$State == state ,elt[1]],na.rm=TRUE)
	
	line_lowest <- which(outcome_of_care_measures[,elt[1]] == lowest)
	col_lowest <- which( outcome_of_care_measures[line_lowest,"State"] == state)
	Hospital <- outcome_of_care_measures[line_lowest[col_lowest],"Hospital Name"]
	
	return(Hospital)
}

# The function should check the validity of its arguments. If an invalid state value is passed to best, the
# function should throw an error via the stop function with the exact message \invalid state". If an invalid
# outcome value is passed to best, the function should throw an error via the stop function with the exact
# message \invalid outcome".
# Here is some sample output from the function.
# best("TX", "heart attack")
# best("TX", "heart failure")
# best("MD", "heart attack")
# best("MD", "pneumonia")

# > source("best.R")
# > best("TX", "heart attack")
# [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
# > best("TX", "heart failure")
# [1] "FORT DUNCAN MEDICAL CENTER"
# > best("MD", "heart attack")
# [1] "JOHNS HOPKINS HOSPITAL, THE"
# > best("MD", "pneumonia")
# [1] "GREATER BALTIMORE MEDICAL CENTER"
# > best("BB", "heart attack")
# Error in best("BB", "heart attack") : invalid state
# >best("BB", "heart attack")
# Error in best("NY", "hert attack") : invalid outcome
# 
