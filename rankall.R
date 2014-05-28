rankall <- function( outcome, num = "best") {
	## Read outcome data
	outcomes <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses="character")

	## Get a vector of the states in the dataset and order them by abbreviation
	## fyi: states.abb won't work because it doesn't include 'DC'
	states <- unique(outcomes$State)
	states <- states[order(states)]

	## Check that outcome is valid

	ok_outcomes = c("pneumonia", "heart attack", "heart failure")
	if(!(outcome %in% ok_outcomes)) {
		## message("invalid outcome")
		stop("invalid outcome")
	}

	## Return hospital name in that state with lowest 30-day death rate
	
	## Define variables for the column names we need
	
	hospital_name <- "Hospital.Name"
	
	heart_attack <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
	heart_failure <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
	pneumonia <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
	
	if(outcome == "heart attack") outcome_column <- heart_attack
	if(outcome == "heart failure") outcome_column <- heart_failure
	if(outcome == "pneumonia") outcome_column <- pneumonia

	## Set the desired outcome column to numeric
	outcomes[, outcome_column] <- suppressWarnings(as.numeric(outcomes[, outcome_column]))

		
	## Create a new data frame to store each state's nth ranked hospital
	ranked_hospitals_all_states <- data.frame()

	
	for (state in states) {
		
		## Narrow by state, only keep the hospital name and desired outcome
		state_outcomes <- outcomes[outcomes$State==state, c(hospital_name, outcome_column)]
		
		## Remove hospitals where there is no data
		state_outcomes <- state_outcomes[complete.cases(state_outcomes),]
		
		## Order by rate in outcome column, then by hospital name
		ranked_hospitals <- state_outcomes[order(state_outcomes[,2], state_outcomes[,1]), hospital_name]
		
		## Select the rank based on the input "best", "worst", or a number value
		if (num == "best") rank <- 1
		else if (num == "worst") rank <- length(ranked_hospitals)
		else rank <- num
		
		## Get the ranked hospital name for a specific ranking
		ranked_hospital <- ranked_hospitals[rank]		
		
		## Create a data frame row for this state and append it to full list of states
		ranked_hospital_this_state <- data.frame(hospital=ranked_hospital, state=state, row.names=state)
		ranked_hospitals_all_states <- rbind(ranked_hospitals_all_states, ranked_hospital_this_state)
	}
	
	## Return the data frame for hospitals with a specific ranking across all states
	ranked_hospitals_all_states
}
