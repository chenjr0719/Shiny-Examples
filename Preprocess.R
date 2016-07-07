##############################################################################################
#Preprocess of clustering

dataPreprocess_Clustering <- function(data) {

	col_types <- getColumnsType(data)
	numeric_list <- unlist(col_types[1])
	non_numeric_list <- unlist(col_types[2])
	others_list <- unlist(col_types[3])

	for(col in non_numeric_list) {
		data[, col] <- NULL
	}

	for(col in others_list) {
		data[, col] <- NULL
	}
	
	data <- data[complete.cases(data),]
	raw_data <- data
	data <- scale(data)

	return(list(raw_data, data))

}

##############################################################################################
#Preprocess of classification

dataPreprocess_Classification <- function(data) {

	col_types <- getColumnsType(data)
	numeric_list <- unlist(col_types[1])
	non_numeric_list <- unlist(col_types[2])
	others_list <- unlist(col_types[3])

	data <- data[complete.cases(data),]

	sample_number <- sample(1:nrow(data), nrow(data)/3*2, replace=FALSE)
	training_dataset <- data[sample_number, ]
	testing_dataset <- data[-sample_number, ]

	return(list(training_dataset, testing_dataset))

}

##############################################################################################
