complete <- function(directory, id = 1:332) {
	mergedpollutantdata <- data.frame()
	for(i in id) {
		data_i <- loadfile(directory, i)
		newrow <- data.frame(id=i,nobs=sum(complete.cases(data_i)))
		mergedpollutantdata <- rbind(mergedpollutantdata, newrow)
	}
	mergedpollutantdata
}

loadfile <- function(directory, id) {
	idstring <- sprintf("%03d", id)
	csvfile <- paste(directory, "/", idstring, ".csv", sep="")
	# print(csvfile)
	data_id <- read.csv(csvfile)	
	data_id
}