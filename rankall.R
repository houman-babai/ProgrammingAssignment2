rankall <- function(outcome, num) {
	supportedOutcome <- list('heart attack'=11, 'heart failure'=17, 'pneumonia'=23)
	measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	#groupedByState <- split(measures, measures$State)
	
	if (!(outcome %in% names(supportedOutcome))) {
		stop("Invalid outcome")
	}
	print(names(measures))
	colNum <- supportedOutcome[[outcome]][1]
	measures[, colNum] <- as.numeric(measures[, colNum])
	measures <- measures[complete.cases(measures[, colNum]),]	
	measures <- measures[,c(7, 2, colNum)]
	listed <- lapply(split(measures, measures$State), 
	function (measuresByState) {
		orderedRows <- order(measuresByState[, 3], measuresByState[,2])
		measuresByState <- measuresByState[orderedRows, ]
		numForState <- num
		
		if ('best' == numForState) {
			numForState = 1
		}
	
		if ('worst' == numForState) {
			numForState = nrow(measuresByState)
		}	
	
		if (numForState > nrow(measuresByState)) {
			c (NA, measuresByState[1,1])
		} else {
			c (measuresByState[numForState, 2], measuresByState[numForState,1])	
		}
	})
	
	unlisted <- unlist(listed, use.names=FALSE)
	rows <- length(listed)
	m <- matrix(unlisted, nrow=54, byrow=TRUE)
	df <- data.frame(m)
	names(df)[1] <- 'hospital'
	names(df)[2] <- 'state'
	df
}