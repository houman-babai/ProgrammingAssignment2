best <- function(state, outcome) {
	supportedOutcome <- list('heart attack'=11, 'heart failure'=17, 'pneumonia'=23)
	measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	groupedByState <- split(measures, measures$State)
	
	if (!(state %in% names(groupedByState))) {
		stop("Invalid State")
	}
	
	if (!(outcome %in% names(supportedOutcome))) {
		stop("Invalid outcome")
	}
	
	colNum <- supportedOutcome[[outcome]][1]
	measurementForState <- groupedByState[[state]]
	measurementForState[, colNum] <- as.numeric(measurementForState[, colNum])
	orderedRows <- order(measurementForState[, colNum], measurementForState[,2])
	measurementForState <- measurementForState[orderedRows, ]
	measurementForState[1,2]
}