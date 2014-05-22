pollutantmean <- function(directory, pollutant, id = 1:332) {
	mergedpollutantdata <- data.frame()
	for(i in id) {
		data_i <- loadfile(directory, i)
		mergedpollutantdata <- rbind(mergedpollutantdata, na.omit(data_i[pollutant]))
	}
	mean(mergedpollutantdata[[pollutant]])
}

loadfile <- function(directory, id) {
	idstring <- sprintf("%03d", id)
	csvfile <- paste(directory, "/", idstring, ".csv", sep="")
	# print(csvfile)
	data_id <- read.csv(csvfile)	
	data_id
}