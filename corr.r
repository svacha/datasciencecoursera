corr <- function(directory, threshold = 0) {
	completeobs <- completecors(directory)
	# print(head(completeobs))
	# subset(completeobs, nobs > threshold, cor)
	completeobs[completeobs$nobs > threshold, "cor"]		
}

completecors <- function(directory, id = 1:332) {
	mergedpollutantdata <- data.frame()
	for(i in id) {
		# print(i)
		data_i <- loadfile(directory, i)
		completes <- sum(complete.cases(data_i))
		if(completes == 0) {
			next
		}
		cor_i <- cor(data_i["sulfate"],data_i["nitrate"], use="complete.obs")
		newrow <- data.frame(id=i,nobs=completes, cor=as.vector(cor_i))
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