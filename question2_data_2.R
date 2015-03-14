##  Reads in relevant data.  

question2_data <- read.csv("case_data_q2.csv")
case_full_smoke <- read.csv("case_full_data_smoke.csv")

colnames(case_full_smoke)
colnames(question2_data)

data <- question2_data

str(data$Income_group)

data <- data[1:349,]

##  This labels the columns in 'data' correctly.  Notice that we are missing 
##  gender and smoking.  

colnames(data) <- c("match_id", "statin", "Age_first_symptoms", "college", "agri", 
                    "ethnic", "income", "alphabet")

str(data$income)
str(data$ethnic)

##  This cleans up ethnicity using the codes: 

##  Convert '1's to "White".  
data$ethnic <- ifelse(data$ethnic==1,"White",data$ethnic)

##  Convert '2's to "Asian".
data$ethnic <- ifelse(data$ethnic==2,"Asian",data$ethnic)

##  Convert '3's to "Latino".
data$ethnic <- ifelse(data$ethnic==3,"Latino",data$ethnic)

##  Convert '4's to "Black".
data$ethnic <- ifelse(data$ethnic==4,"Black",data$ethnic)

##  Convert '5's to "Native American".
data$ethnic <- ifelse(data$ethnic==5,"Native American",data$ethnic)

##  The following cleans up income using the codes:

##  Convert '1's to "<50K".  
data$income <- ifelse(data$income==1,"<50K",data$income)

##  Convert '2's to "50-80K".  
data$income <- ifelse(data$income==2,"50-80K",data$income)

##  Convert '3's to ">80K".  
data$income <- ifelse(data$income==3,">80K",data$income)

##  "Re-factors" the Income_groups.  

data$income <- factor(data$income, levels=c("<50K","50-80K",">80K"))
summary(data$income)

##  "Re-factors" the ethnicity.  

data$ethnic <- factor(data$ethnic, levels=c("White", "Asian", "Latino", 
                                            "Black", "Native American"))

##  Extracts smoking, and gender..  

smoke <- case_full_smoke[,c(3,6,94)]
colnames(smoke) <- c("id","gender","smoke")

##  Creates matching column in smoke.  

smoke$match_id <- substr(smoke$id,9,12)

##  Merges two data frames.  

data_full <- merge(data, smoke, by="match_id", all.x=TRUE, all.y=TRUE)

##  Note: id# 1654 is missing question 2 data.  

##  Also want to include whether subject used zocor or lipitor.  

##  This reads in the zocor users, changes the column names, creates
##  a matching id, and creates two columns "match_id" and "zocor":

zocor <- read.csv("psp_zocor.csv")
zocor <- zocor[1:349,1:2]

colnames(zocor) <- c("id","zocor")

zocor$match_id <- substr(zocor$id,9,12)

zocor <- zocor[,c(3,2)]

##  Merges the zocor data with the full data set:

data_full <- merge(data_full, zocor, by="match_id", all.x=TRUE, all.y=TRUE)

##  Note: id# 1654 is missing the zocor data.  

##  This reads in the lipitor users, changes the column names, creates
##  a matching id, and creates two columns "match_id" and "lipitor":

lipitor <- read.csv("psp_lipitor.csv")
lipitor <- lipitor[1:349,1:2]

colnames(lipitor) <- c("id","lipitor")

lipitor$match_id <- substr(lipitor$id,9,12)

lipitor <- lipitor[,c(3,2)]

##  Merges the lipitor data with the full data set:

data_full <- merge(data_full, lipitor, by="match_id", all.x=TRUE, all.y=TRUE)


##  I have all the data I want to do analysis on.  Going to write it out to
##  a csv file.  

write.csv(data_full, file="question2_data.csv", row.names=FALSE)
