##  Question 2.  

##  Reads in the relevant dataset.  

q2_data <- read.csv("question2_data.csv")

##  Re-directs output to a file.  

sink(file="question2_results_mandf_91614.txt")

##  Performs linear regression.  

cat("\nAge of First Symptoms vs. Statin and Agri, Income, and Smoking\n")
age_statin_agri_income_smoke <- lm(Age_first_symptoms~agri+statin+income+
                       smoke, data=q2_data)
summary(age_statin_agri_income_smoke)
confint(age_statin_agri_income_smoke)

cat("\nAge of First Symptoms vs. Statin and Agri, College, and Smoking\n")
age_statin_agri_college_smoke <- lm(Age_first_symptoms~
                                        statin+agri+college+smoke, data=q2_data)
summary(age_statin_agri_college_smoke)
confint(age_statin_agri_college_smoke)

cat("\nAge of First Symptoms vs. Statin, Agri, and College\n")
age_statin_agri_college <- lm(Age_first_symptoms~
                                  statin+agri+college, data=q2_data)
summary(age_statin_agri_college)
confint(age_statin_agri_college)

#

cat("\nAge of First Symptoms vs. Zocor and Agri, Income, and Smoking\n")
age_zocor_agri_income_smoke <- lm(Age_first_symptoms~
                                      agri+zocor+income+ smoke, data=q2_data)
summary(age_zocor_agri_income_smoke)
confint(age_zocor_agri_income_smoke)

cat("\nAge of First Symptoms vs. Zocor and Agri, College, and Smoking\n")
age_zocor_agri_college_smoke <- lm(Age_first_symptoms~
                                       zocor+agri+college+smoke, data=q2_data)
summary(age_zocor_agri_college_smoke)
confint(age_zocor_agri_college_smoke)

cat("\nAge of First Symptoms vs. Zocor, Agri, and College\n")
age_zocor_agri_college <- lm(Age_first_symptoms~
                                 zocor+agri+college, data=q2_data)
summary(age_zocor_agri_college)
confint(age_zocor_agri_college)

#

cat("\nAge of First Symptoms vs. Lipitor and Agri, Income, and Smoking\n")
age_lipitor_agri_income_smoke <- lm(Age_first_symptoms~
                                        agri+lipitor+income+smoke, data=q2_data)
summary(age_lipitor_agri_income_smoke)
confint(age_lipitor_agri_income_smoke)

cat("\nAge of First Symptoms vs. Lipitor and Agri, College, and Smoking\n")
age_lipitor_agri_college_smoke <- lm(Age_first_symptoms~
                                         lipitor+agri+college+smoke, data=q2_data)
summary(age_lipitor_agri_college_smoke)
confint(age_lipitor_agri_college_smoke)

cat("\nAge of First Symptoms vs. Lipitor, Agri, and College\n")
age_lipitor_agri_college <- lm(Age_first_symptoms~
                                   lipitor+agri+college, data=q2_data)
summary(age_lipitor_agri_college)
confint(age_lipitor_agri_college)


##  Ends output to "question2_results_maleandfemale.txt".  

sink()

##  Separates the males and females.  

male <- q2_data$gender=="Male"

q2_data_male <- q2_data[male,]
q2_data_female <- q2_data[!male,]

#  Repeats above for Males.

sink(file="question2_results_male_91614.txt")

cat("\nAge of First Symptoms vs. Statin and Agri, Income, and Smoking\n")
age_male_statin_agri_income_smoke <- lm(Age_first_symptoms~agri+statin+income+
          smoke, data=q2_data_male)
summary(age_male_statin_agri_income_smoke)
confint(age_male_statin_agri_income_smoke)

cat("\nAge of First Symptoms vs. Statin and Agri, College, and Smoking\n")
age_male_statin_agri_college_smoke <- lm(Age_first_symptoms~
                                             statin+agri+college+smoke, data=q2_data_male)
summary(age_male_statin_agri_college_smoke)
confint(age_male_statin_agri_college_smoke)

cat("\nAge of First Symptoms vs. Statin, Agri, and College\n")
age_male_statin_agri_college <- lm(Age_first_symptoms~
                                       statin+agri+college, data=q2_data_male)
summary(age_male_statin_agri_college)
confint(age_male_statin_agri_college)

#

cat("\nAge of First Symptoms vs. Zocor and Agri, Income, and Smoking\n")
age_male_zocor_agri_income_smoke <- lm(Age_first_symptoms~
                                           agri+zocor+income+
          smoke, data=q2_data_male)
summary(age_male_zocor_agri_income_smoke)
confint(age_male_zocor_agri_income_smoke)

cat("\nAge of First Symptoms vs. Zocor and Agri, College, and Smoking\n")
age_male_zocor_agri_college_smoke <- lm(Age_first_symptoms~
                                            zocor+agri+college+smoke, data=q2_data_male)
summary(age_male_zocor_agri_college_smoke)
confint(age_male_zocor_agri_college_smoke)

cat("\nAge of First Symptoms vs. Zocor, Agri, and College\n")
age_male_zocor_agri_college <- lm(Age_first_symptoms~
                                      zocor+agri+college, data=q2_data_male)
summary(age_male_zocor_agri_college)
confint(age_male_zocor_agri_college)

#

cat("\nAge of First Symptoms vs. Lipitor and Agri, Income, and Smoking\n")
age_male_lipitor_agri_income_smoke <- lm(Age_first_symptoms~
                                             agri+lipitor+income+smoke, data=q2_data_male)
summary(age_male_lipitor_agri_income_smoke)
confint(age_male_lipitor_agri_income_smoke)

cat("\nAge of First Symptoms vs. Lipitor and Agri, College, and Smoking\n")
age_male_lipitor_agri_college_smoke <- lm(Age_first_symptoms~
                                              lipitor+agri+college+smoke, data=q2_data_male)
summary(age_male_lipitor_agri_college_smoke)
confint(age_male_lipitor_agri_college_smoke)

cat("\nAge of First Symptoms vs. Lipitor, Agri, and College\n")
age_male_lipitor_agri_college <- lm(Age_first_symptoms~
                                        lipitor+agri+college, data=q2_data_male)
summary(age_male_lipitor_agri_college)
confint(age_male_lipitor_agri_college)

sink()

##  Repeats above for Females.

sink(file="question2_results_female_91614.txt")

cat("\nAge of First Symptoms vs. Statin and Agri, Income, and Smoking\n")
age_female_statin_agri_income_smoke <- lm(Age_first_symptoms~
                                              agri+statin+income+smoke, data=q2_data_female)
summary(age_female_statin_agri_income_smoke)
confint(age_female_statin_agri_income_smoke)

cat("\nAge of First Symptoms vs. Statin and Agri, College, and Smoking\n")
age_female_statin_agri_college_smoke <- lm(Age_first_symptoms~
                                        statin+agri+college+smoke, data=q2_data_female)
summary(age_female_statin_agri_college_smoke)
confint(age_female_statin_agri_college_smoke)

cat("\nAge of First Symptoms vs. Statin, Agri, and College\n")
age_female_statin_agri_college <- lm(Age_first_symptoms~
                                    statin+agri+college, data=q2_data_female)
summary(age_female_statin_agri_college)
confint(age_female_statin_agri_college)

#

cat("\nAge of First Symptoms vs. Zocor and Agri, Income, and Smoking\n")
age_female_zocor_agri_income_smoke <- lm(Age_first_symptoms~
                                             agri+zocor+income+smoke, data=q2_data_female)
summary(age_female_zocor_agri_income_smoke)
confint(age_female_zocor_agri_income_smoke)

cat("\nAge of First Symptoms vs. Zocor and Agri, College, and Smoking\n")
age_female_zocor_agri_college_smoke <- lm(Age_first_symptoms~
                                              zocor+agri+college+smoke, data=q2_data_female)
summary(age_female_zocor_agri_college_smoke)
confint(age_female_zocor_agri_college_smoke)

cat("\nAge of First Symptoms vs. Zocor, Agri, and College\n")
age_female_zocor_agri_college <- lm(Age_first_symptoms~
                                        zocor+agri+college, data=q2_data_female)
summary(age_female_zocor_agri_college)
confint(age_female_zocor_agri_college)

#

cat("\nAge of First Symptoms vs. Lipitor and Agri, Income, and Smoking\n")
age_female_lipitor_agri_income_smoke <- lm(Age_first_symptoms~agri+lipitor+income+
          smoke, data=q2_data_female)
summary(age_female_lipitor_agri_income_smoke)
confint(age_female_lipitor_agri_income_smoke)

cat("\nAge of First Symptoms vs. Lipitor and Agri, College, and Smoking\n")
age_female_lipitor_agri_college_smoke <- lm(Age_first_symptoms~
                                        lipitor+agri+college+smoke, data=q2_data_female)
summary(age_female_lipitor_agri_college_smoke)
confint(age_female_lipitor_agri_college_smoke)

cat("\nAge of First Symptoms vs. Lipitor, Agri, and College\n")
age_female_lipitor_agri_college <- lm(Age_first_symptoms~
                                          lipitor+agri+college, data=q2_data_female)
summary(age_female_lipitor_agri_college)
confint(age_female_lipitor_agri_college)

sink()





