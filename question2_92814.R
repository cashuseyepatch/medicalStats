##  Question 2.  

##  Reads in the relevant dataset.  

q2_data <- read.csv("question2_data.csv")

##  Re-directs output to a file.  

sink(file="question2_results_mandf_92814.txt")

##  Performs linear regression.  

cat("\nAge of First Symptoms vs. Statin, Income, and Smoking\n")
age_statin_income_smoke <- lm(Age_first_symptoms~statin+income+
                                       smoke, data=q2_data)
summary(age_statin_income_smoke)
confint(age_statin_income_smoke)

sink()

##  Separates the males and females.  

male <- q2_data$gender=="Male"

q2_data_male <- q2_data[male,]
q2_data_female <- q2_data[!male,]

#  Repeats above for Males.

sink(file="question2_results_male_92814.txt")

cat("\nAge of First Symptoms vs. Statin, Income, and Smoking\n")
age_male_statin_income_smoke <- lm(Age_first_symptoms~statin+income+
                                  smoke, data=q2_data_male)
summary(age_male_statin_income_smoke)
confint(age_male_statin_income_smoke)

sink()

#  Repeats above for Females.

sink(file="question2_results_female_92814.txt")

cat("\nAge of First Symptoms vs. Statin, Income, and Smoking\n")
age_female_statin_income_smoke <- lm(Age_first_symptoms~statin+income+
                                       smoke, data=q2_data_female)
summary(age_female_statin_income_smoke)
confint(age_female_statin_income_smoke)

sink()

