##  Reads in the relevant dataset.  

q3_data <- read.csv("question3_data.csv")

##  Creates a new column indicating if test scores are missing.  

q3_data$missing <- ifelse(is.na(q3_data$UPDRS)==TRUE,1,0)
q3_data$missing <- ifelse(is.na(q3_data$Golbe)==TRUE,1,q3_data$missing)
q3_data$missing <- ifelse(is.na(q3_data$MDRS)==TRUE,1,q3_data$missing)

sum(q3_data$missing)

q3_data_missing <- q3_data[q3_data$missing==1,]

write.csv(q3_data_missing, file="missing_test_scores.csv", row.names=FALSE)
