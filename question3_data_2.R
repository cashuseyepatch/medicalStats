##  Reads in the relevant data.  

q3_data_raw <- read.csv("q3_data_raw.csv")
question2_data <- read.csv("question2_data.csv")
case_full_data <- read.csv("case_full_data.csv")

q3_data_raw <- q3_data_raw[1:349,1:11]

colnames(case_full_data)
colnames(q3_data_raw)

case_full_data <- case_full_data[,c(3,8,115)]
q3_data_raw <- q3_data_raw[,c(1,2,4,5,6,9,11)]

colnames(case_full_data) <- c("id","year_symptoms_start","test_date_full")

colnames(q3_data_raw) <- c("id","med","UPDRS","Golbe","test_date_q3","MDRS",
                           "DRS_date")

##  Creates matching id:

q3_data_raw$match_id <- substr(q3_data_raw$id,9,12)
case_full_data$match_id <- substr(case_full_data$id,9,12)

##  Reorganizes data frames and drops "id".  

q3_data_raw <- q3_data_raw[,c(8,5,7,2:4,6)]
case_full_data <- case_full_data[,c(4,2,3)]


##  Merges "q3_data_raw" and "case_full_data" to make q3_data_edited.  

q3_data_edited <- merge(case_full_data, q3_data_raw, by="match_id", 
                        all.x=TRUE, all.y=TRUE)

q3_data_edited <- q3_data_edited[order(q3_data_edited$match_id),]
    
##  Below checks to see if the dates line up.  

q3_data_edited$test_date_full <- as.character(q3_data_edited$test_date_full)
q3_data_edited$test_date_q3 <- as.character(q3_data_edited$test_date_q3)

test_date_not_equal <- q3_data_edited[q3_data_edited$test_date_full!=
                                          q3_data_edited$test_date_q3,]

row.names(test_date_not_equal) <- NULL

##  Id: 1836 has only the DRS test date.  This takes that into account.  

q3_data_edited$test_date_full <- as.character(q3_data_edited$test_date_full)
q3_data_edited$test_date_q3 <- as.character(q3_data_edited$test_date_q3)
q3_data_edited$DRS_date <- as.character(q3_data_edited$DRS_date)


q3_data_edited$date_used <- ifelse(q3_data_edited$test_date_full=="",
                                   q3_data_edited$DRS_date,
                                   q3_data_edited$test_date_full)

##  Extracts year from "date_used".  

q3_data_edited$test_year <- substr(q3_data_edited$date_used,7,10)

##  Calculates the number of years with symptoms.  

q3_data_edited$test_year <- as.numeric(q3_data_edited$test_year)
q3_data_edited$symptom_years <- q3_data_edited$test_year - 
                                q3_data_edited$year_symptoms_start

##  Merges "q3_data_edited" with "question2_data" to create 
##  "question3_data":

question3_data <- merge(question2_data, q3_data_edited, by="match_id", 
                        all.x=TRUE, all.y=TRUE)

##  Id: 1654 is again missing the data.  

##  This confirms that med and statin columns are both identical.  

identical(question3_data$statin,question3_data$med)

##  Have the data, now write it to a csv file.  

write.csv(question3_data, file="question3_data.csv", row.names=FALSE)

