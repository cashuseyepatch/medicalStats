##  Question 2.  

##  Reads in the relevant dataset.  

q2_data <- read.csv("question2_data.csv")

##  Re-directs output to a file.  

sink(file="question2_results_maleandfemale.txt")

##  Performs linear regression.  

cat("\nAge of First Symptoms vs. Statin and All Variables\n")

age_statin_all <- lm(Age_first_symptoms~college+agri+statin+gender+ethnic+income+
                       smoke, data=q2_data)
summary(age_statin_all)
confint(age_statin_all)

cat("\nAge of First Symptoms vs. Statin\n")

age_statin <- lm(Age_first_symptoms~statin, data=q2_data)
summary(age_statin)
confint(age_statin)

cat("\nAge of First Symptoms vs. College\n")

age_college <- lm(Age_first_symptoms~college, data=q2_data)
summary(age_college)
confint(age_college)

cat("\nAge of First Symptoms vs. Agri\n")

age_agri <- lm(Age_first_symptoms~agri, data=q2_data)
summary(age_agri)
confint(age_agri)

cat("\nAge of First Symptoms vs. Gender\n")

age_gender <- lm(Age_first_symptoms~gender, data=q2_data)
summary(age_gender)
confint(age_gender)

cat("\nAge of First Symptoms vs. Ethnic\n")

age_ethnic <- lm(Age_first_symptoms~ethnic, data=q2_data)
summary(age_ethnic)
confint(age_ethnic)

cat("\nAge of First Symptoms vs. Income\n")

age_income <- lm(Age_first_symptoms~income, data=q2_data)
summary(age_income)
confint(age_income)

cat("\nAge of First Symptoms vs. Smoke\n")

age_smoke <- lm(Age_first_symptoms~smoke, data=q2_data)
summary(age_smoke)
confint(age_smoke)

cat("\nAge of First Symptoms vs. Zocor and All Variables\n")

age_zocor_all <- lm(Age_first_symptoms~college+agri+zocor+gender+ethnic+income+
                        smoke, data=q2_data)
summary(age_zocor_all)
confint(age_zocor_all)

cat("\nAge of First Symptoms vs. Zocor\n")

age_zocor <- lm(Age_first_symptoms~zocor, data=q2_data)
summary(age_zocor)
confint(age_zocor)

cat("\nAge of First Symptoms vs. Lipitor and All Variables\n")

age_lipitor_all <- lm(Age_first_symptoms~college+agri+lipitor+gender+ethnic+income+
                          smoke, data=q2_data)
summary(age_lipitor_all)
confint(age_lipitor_all)

cat("\nAge of First Symptoms vs. Lipitor\n")

age_lipitor <- lm(Age_first_symptoms~lipitor, data=q2_data)
summary(age_lipitor)
confint(age_lipitor)

##  Ends output to "question2_results_maleandfemale.txt".  

sink()

##  Separates the males and females.  

male <- q2_data$gender=="Male"

q2_data_male <- q2_data[male,]
q2_data_female <- q2_data[!male,]

##  Repeats above for Males.

sink(file="question2_results_male.txt")

cat("\nAge of First Symptoms vs. Statin and All Variables\n")

age_statin_all_male <- lm(Age_first_symptoms~college+agri+statin+ethnic+income+
                         smoke, data=q2_data_male)
summary(age_statin_all_male)
confint(age_statin_all_male)

cat("\nAge of First Symptoms vs. Statin\n")

age_statin_male <- lm(Age_first_symptoms~statin, data=q2_data_male)
summary(age_statin_male)
confint(age_statin_male)

cat("\nAge of First Symptoms vs. College\n")

age_college_male <- lm(Age_first_symptoms~college, data=q2_data_male)
summary(age_college_male)
confint(age_college_male)

cat("\nAge of First Symptoms vs. Agri\n")

age_agri_male <- lm(Age_first_symptoms~agri, data=q2_data_male)
summary(age_agri_male)
confint(age_agri_male)

cat("\nAge of First Symptoms vs. Ethnic\n")

age_ethnic_male <- lm(Age_first_symptoms~ethnic, data=q2_data_male)
summary(age_ethnic_male)
confint(age_ethnic_male)

cat("\nAge of First Symptoms vs. Income\n")

age_income_male <- lm(Age_first_symptoms~income, data=q2_data_male)
summary(age_income_male)
confint(age_income_male)

cat("\nAge of First Symptoms vs. Smoke\n")

age_smoke_male <- lm(Age_first_symptoms~smoke, data=q2_data_male)
summary(age_smoke_male)
confint(age_smoke_male)

cat("\nAge of First Symptoms vs. Zocor and All Variables\n")

age_zocor_all_male <- lm(Age_first_symptoms~college+agri+zocor+ethnic+income+
                        smoke, data=q2_data_male)
summary(age_zocor_all_male)
confint(age_zocor_all_male)

cat("\nAge of First Symptoms vs. Zocor\n")

age_zocor_male <- lm(Age_first_symptoms~zocor, data=q2_data_male)
summary(age_zocor_male)
confint(age_zocor_male)

cat("\nAge of First Symptoms vs. Lipitor and All Variables\n")

age_lipitor_all_male <- lm(Age_first_symptoms~college+agri+lipitor+ethnic+income+
                          smoke, data=q2_data_male)
summary(age_lipitor_all_male)
confint(age_lipitor_all_male)

cat("\nAge of First Symptoms vs. Lipitor\n")

age_lipitor_male <- lm(Age_first_symptoms~lipitor, data=q2_data_male)
summary(age_lipitor_male)
confint(age_lipitor_male)

sink()

##  Repeats above for Females.

sink(file="question2_results_female.txt")

cat("\nAge of First Symptoms vs. Statin and All Variables\n")

age_statin_all_female <- lm(Age_first_symptoms~college+agri+statin+ethnic+income+
                              smoke, data=q2_data_female)
summary(age_statin_all_female)
confint(age_statin_all_female)

cat("\nAge of First Symptoms vs. Statin\n")

age_statin_female <- lm(Age_first_symptoms~statin, data=q2_data_female)
summary(age_statin_female)
confint(age_statin_female)

cat("\nAge of First Symptoms vs. College\n")

age_college_female <- lm(Age_first_symptoms~college, data=q2_data_female)
summary(age_college_female)
confint(age_college_female)

cat("\nAge of First Symptoms vs. Agri\n")

age_agri_female <- lm(Age_first_symptoms~agri, data=q2_data_female)
summary(age_agri_female)
confint(age_agri_female)

cat("\nAge of First Symptoms vs. Ethnic\n")

age_ethnic_female <- lm(Age_first_symptoms~ethnic, data=q2_data_female)
summary(age_ethnic_female)
confint(age_ethnic_female)

cat("\nAge of First Symptoms vs. Income\n")

age_income_female <- lm(Age_first_symptoms~income, data=q2_data_female)
summary(age_income_female)
confint(age_income_female)

cat("\nAge of First Symptoms vs. Smoke\n")

age_smoke_female <- lm(Age_first_symptoms~smoke, data=q2_data_female)
summary(age_smoke_female)
confint(age_smoke_female)

cat("\nAge of First Symptoms vs. Zocor and All Variables\n")

age_zocor_all_female <- lm(Age_first_symptoms~college+agri+zocor+ethnic+income+
                             smoke, data=q2_data_female)
summary(age_zocor_all_female)
confint(age_zocor_all_female)

cat("\nAge of First Symptoms vs. Zocor\n")

age_zocor_female <- lm(Age_first_symptoms~zocor, data=q2_data_female)
summary(age_zocor_female)
confint(age_zocor_female)

cat("\nAge of First Symptoms vs. Lipitor and All Variables\n")

age_lipitor_all_female <- lm(Age_first_symptoms~college+agri+lipitor+ethnic+income+
                               smoke, data=q2_data_female)
summary(age_lipitor_all_female)
confint(age_lipitor_all_female)

cat("\nAge of First Symptoms vs. Lipitor\n")

age_lipitor_female <- lm(Age_first_symptoms~lipitor, data=q2_data_female)
summary(age_lipitor_female)
confint(age_lipitor_female)

sink()







