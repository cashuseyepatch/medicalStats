##  Reads in the relevant data.  

q3_data_raw <- read.csv("q3_data_raw.csv")
question2_data <- read.csv("question2_data.csv")

q3_data_raw <- q3_data_raw[1:349,1:11]

colnames(q3_data_raw) <- c("id","med","UPDRS","Golbe","MDRS","Gender",
                           "Age_Today")

##  Creates matching id:

q3_data_raw$match_id <- substr(q3_data_raw$id,9,12)

##  Drops enrollment id and re-orders:

q3_data_raw <- q3_data_raw[,c(8,2:7)]

##  Merges the two data frames:

question3_data <- merge(question2_data, q3_data_raw, by="match_id", 
                        all.x=TRUE, all.y=TRUE)

##  Id: 1654 is again missing the data.  

##  This confirms that med and statin columns are both identical.  

identical(question3_data$statin,question3_data$med)

str(question3_data$statin)
str(question3_data$med)

class(question3_data$statin)
class(question3_data$med)

mode(question3_data$statin)
mode(question3_data$med)

question3_data$statin==question3_data$med

not_equal <- question3_data[question3_data$statin!=question3_data$med,]
row.names(not_equal) <- NULL

##  Organize the data and delete duplicate columns:

question3_data <- question3_data[,c(9,1:8,10:17,19)]
not_equal <- not_equal[,c(9,1:8,10:17,19)]

##  Creates a new column "symptom_years".  

question3_data$symptom_years <- with(question3_data, Age_Today-Age_first_symptoms)

##  Have the data, now write it to a csv file.  

write.csv(question3_data, file="question3_data.csv", row.names=FALSE)
write.csv(not_equal, file="not_equal.csv", row.names=FALSE)
