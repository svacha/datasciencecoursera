rankhospital <- function(state, outcome, num = "best") {
	## Read outcome data
	outcomes <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses="character")

	## Get a vector of the states in the dataset
	## fyi: states.abb won't work because it doesn't include 'DC'
	states <- unique(outcomes$State)


	## Check that state and outcome are valid
	if(!(state %in% states)) {
		## message("invalid state")
		stop("invalid state")
	}
	ok_outcomes = c("pneumonia", "heart attack", "heart failure")
	if(!(outcome %in% ok_outcomes)) {
		## message("invalid outcome")
		stop("invalid outcome")
	}

	## Return hospital name in that state with lowest 30-day death
	## rate
	
	## Define variables for the column names we need
	hospital_name <- "Hospital.Name"
	
	heart_attack <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
	heart_failure <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
	pneumonia <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
	
	if(outcome == "heart attack") outcome_column <- heart_attack
	if(outcome == "heart failure") outcome_column <- heart_failure
	if(outcome == "pneumonia") outcome_column <- pneumonia
	
	## Narrow by state, only keep the hospital name and desired outcome
	state_outcomes <- outcomes[outcomes$State==state, c(hospital_name, outcome_column)]
	
	## Make sure the outcome column is numeric before ordering
	state_outcomes[, outcome_column] <- suppressWarnings(as.numeric(state_outcomes[, outcome_column]))
	
	## Remove hospitals where there is no data
	state_outcomes <- state_outcomes[complete.cases(state_outcomes),]
	
	## Order by rate in outcome column, then by hospital name
	ranked_hospitals <- state_outcomes[order(state_outcomes[,2], state_outcomes[,1]), hospital_name]
	
	## Select the rank based on the input "best", "worst", or a number value
	if (num == "best") num <- 1
	if (num == "worst") num <- length(ranked_hospitals)
	
	## Return the ranked hospital name for a specific ranking
	ranked_hospitals[num]


}
