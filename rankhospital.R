rankhospital <- function(state, outcome, num) {
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
	
	measurementForState  <- measurementForState[complete.cases(measurementForState[, colNum]),]
	
	if ('best' == num) {
		num = 1
	}
	
	if ('worst' == num) {
		num = nrow(measurementForState)
	}	
	
	if (num > nrow(measurementForState)) {
		return (NA)
	}
	
	
	orderedRows <- order(measurementForState[, colNum], measurementForState[,2])
	measurementForState <- measurementForState[orderedRows, ]
	
	
	measurementForState[num,2]
}