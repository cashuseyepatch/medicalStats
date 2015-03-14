##  Reads in relevant data.  

question2_data <- read.csv("case_data_q2.csv")
case_full_smoke <- read.csv("case_full_data_smoke.csv")

colnames(case_full_smoke)
colnames(question2_data)

data <- question2_data

str(data$Income_group)

data$Income_group <- as.character(data$Income_group)

##  This converts "<50 K" to "<50K".  

data$Income_group <- ifelse(data$Income_group=="<50 K","<50K",
                             data$Income_group)

##  This sets DKN values to NA.  

data$Income_group <- ifelse(data$Income_group=="DKN",NA,data$Income_group)

##  "Re-factors" the Income_groups.  

data$Income_group <- factor(data$Income_group, levels=c("<50K",">80K","50-80K"))
summary(data$Income_group)

##  Changes Column names in 'data'.

colnames(data) <- c("id", "type", "gender", "dob", "dosymptoms", 
                    "Age_first_symptoms", "college", "ethnic", "income", 
                    "agri")

##  Creates matching column in data.  

data$match_id <- substr(data$id,9,12)

##  Extracts smoking.  

smoke <- case_full_smoke[,c(3,94)]
colnames(smoke) <- c("id","smoke")

##  Creates matching column in smoke.  

smoke$match_id <- substr(smoke$id,9,12)

##  Merges two data frames.  

data_full <- merge(data, smoke, by="match_id", all.x=TRUE, all.y=TRUE)

