##  Question 3.  

##  Reads in the relevant dataset.  

q3_data <- read.csv("question3_data.csv")

##  Re-directs output to a file.  

sink(file="question3_results_UPDRS_maleandfemale.txt")

##  Performs linear regression.  

##  Output = UPDRS.  

cat("\nUPDRS vs. Statin and All Variables\n")
UPDRS_all_statin <- lm(UPDRS~college+agri+statin+gender+ethnic+income+
                    smoke+symptom_years, data=q3_data)
summary(UPDRS_all_statin)
confint(UPDRS_all_statin)

cat("\nUPDRS vs. Statin\n")
UPDRS_statin <- lm(UPDRS~statin+symptom_years, data=q3_data)
summary(UPDRS_statin)
confint(UPDRS_statin)

cat("\nUPDRS vs. college\n")
UPDRS_college <- lm(UPDRS~college+symptom_years, data=q3_data)
summary(UPDRS_college)
confint(UPDRS_college)

cat("\nUPDRS vs. agri\n")
UPDRS_agri <- lm(UPDRS~agri+symptom_years, data=q3_data)
summary(UPDRS_statin)
confint(UPDRS_statin)

cat("\nUPDRS vs. gender\n")
UPDRS_gender <- lm(UPDRS~gender+symptom_years, data=q3_data)
summary(UPDRS_gender)
confint(UPDRS_gender)

cat("\nUPDRS vs. ethnic\n")
UPDRS_ethnic <- lm(UPDRS~ethnic+symptom_years, data=q3_data)
summary(UPDRS_ethnic)
confint(UPDRS_ethnic)

cat("\nUPDRS vs. income\n")
UPDRS_income <- lm(UPDRS~income+symptom_years, data=q3_data)
summary(UPDRS_income)
confint(UPDRS_income)

cat("\nUPDRS vs. smoke\n")
UPDRS_smoke <- lm(UPDRS~smoke+symptom_years, data=q3_data)
summary(UPDRS_smoke)
confint(UPDRS_smoke)

sink()

##  Output = Golbe.  

sink(file="question3_results_Golbe_maleandfemale.txt")

cat("\nGolbe vs. Statin and All Variables\n")
Golbe_all_statin <- lm(Golbe~college+agri+statin+gender+ethnic+income+
                           smoke+symptom_years, data=q3_data)
summary(Golbe_all_statin)
confint(Golbe_all_statin)

cat("\nGolbe vs. Statin\n")
Golbe_statin <- lm(Golbe~statin+symptom_years, data=q3_data)
summary(Golbe_statin)
confint(Golbe_statin)

cat("\nGolbe vs. college\n")
Golbe_college <- lm(Golbe~college+symptom_years, data=q3_data)
summary(Golbe_college)
confint(Golbe_college)

cat("\nGolbe vs. agri\n")
Golbe_agri <- lm(Golbe~agri+symptom_years, data=q3_data)
summary(Golbe_statin)
confint(Golbe_statin)

cat("\nGolbe vs. gender\n")
Golbe_gender <- lm(Golbe~gender+symptom_years, data=q3_data)
summary(Golbe_gender)
confint(Golbe_gender)

cat("\nGolbe vs. ethnic\n")
Golbe_ethnic <- lm(Golbe~ethnic+symptom_years, data=q3_data)
summary(Golbe_ethnic)
confint(Golbe_ethnic)

cat("\nGolbe vs. income\n")
Golbe_income <- lm(Golbe~income+symptom_years, data=q3_data)
summary(Golbe_income)
confint(Golbe_income)

cat("\nGolbe vs. smoke\n")
Golbe_smoke <- lm(Golbe~smoke+symptom_years, data=q3_data)
summary(Golbe_smoke)
confint(Golbe_smoke)

sink()

##  Output = MDRS.  

sink(file="question3_results_MDRS_maleandfemale.txt")

cat("\nMDRS vs. Statin and All Variables\n")
MDRS_all_statin <- lm(MDRS~college+agri+statin+gender+ethnic+income+
                           smoke+symptom_years, data=q3_data)
summary(MDRS_all_statin)
confint(MDRS_all_statin)

cat("\nMDRS vs. Statin\n")
MDRS_statin <- lm(MDRS~statin+symptom_years, data=q3_data)
summary(MDRS_statin)
confint(MDRS_statin)

cat("\nMDRS vs. college\n")
MDRS_college <- lm(MDRS~college+symptom_years, data=q3_data)
summary(MDRS_college)
confint(MDRS_college)

cat("\nMDRS vs. agri\n")
MDRS_agri <- lm(MDRS~agri+symptom_years, data=q3_data)
summary(MDRS_statin)
confint(MDRS_statin)

cat("\nMDRS vs. gender\n")
MDRS_gender <- lm(MDRS~gender+symptom_years, data=q3_data)
summary(MDRS_gender)
confint(MDRS_gender)

cat("\nMDRS vs. ethnic\n")
MDRS_ethnic <- lm(MDRS~ethnic+symptom_years, data=q3_data)
summary(MDRS_ethnic)
confint(MDRS_ethnic)

cat("\nMDRS vs. income\n")
MDRS_income <- lm(MDRS~income+symptom_years, data=q3_data)
summary(MDRS_income)
confint(MDRS_income)

cat("\nMDRS vs. smoke\n")
MDRS_smoke <- lm(MDRS~smoke+symptom_years, data=q3_data)
summary(MDRS_smoke)
confint(MDRS_smoke)

sink()

##  Separates the males and females.  

male <- q3_data$gender=="Male"

q3_data_male <- q3_data[male,]
q3_data_female <- q3_data[!male,]

##  Repeats above for Males.

##  Output = UPDRS.  

sink(file="question3_results_UPDRS_male.txt")

cat("\nUPDRS vs. Statin and All Variables\n")
UPDRS_all_statin_male <- lm(UPDRS~college+agri+statin+ethnic+income+
                           smoke+symptom_years, data=q3_data_male)
summary(UPDRS_all_statin_male)
confint(UPDRS_all_statin_male)

cat("\nUPDRS vs. Statin\n")
UPDRS_statin_male <- lm(UPDRS~statin+symptom_years, data=q3_data_male)
summary(UPDRS_statin_male)
confint(UPDRS_statin_male)

cat("\nUPDRS vs. college\n")
UPDRS_college_male <- lm(UPDRS~college+symptom_years, data=q3_data_male)
summary(UPDRS_college_male)
confint(UPDRS_college_male)

cat("\nUPDRS vs. agri\n")
UPDRS_agri_male <- lm(UPDRS~agri+symptom_years, data=q3_data_male)
summary(UPDRS_statin_male)
confint(UPDRS_statin_male)

cat("\nUPDRS vs. ethnic\n")
UPDRS_ethnic_male <- lm(UPDRS~ethnic+symptom_years, data=q3_data_male)
summary(UPDRS_ethnic_male)
confint(UPDRS_ethnic_male)

cat("\nUPDRS vs. income\n")
UPDRS_income_male <- lm(UPDRS~income+symptom_years, data=q3_data_male)
summary(UPDRS_income_male)
confint(UPDRS_income_male)

cat("\nUPDRS vs. smoke\n")
UPDRS_smoke_male <- lm(UPDRS~smoke+symptom_years, data=q3_data_male)
summary(UPDRS_smoke_male)
confint(UPDRS_smoke_male)

sink()

##  Output = Golbe.  

sink(file="question3_results_Golbe_male.txt")

cat("\nGolbe vs. Statin and All Variables\n")
Golbe_all_statin_male <- lm(Golbe~college+agri+statin+ethnic+income+
                                smoke+symptom_years, data=q3_data_male)
summary(Golbe_all_statin_male)
confint(Golbe_all_statin_male)

cat("\nGolbe vs. Statin\n")
Golbe_statin_male <- lm(Golbe~statin+symptom_years, data=q3_data_male)
summary(Golbe_statin_male)
confint(Golbe_statin_male)

cat("\nGolbe vs. college\n")
Golbe_college_male <- lm(Golbe~college+symptom_years, data=q3_data_male)
summary(Golbe_college_male)
confint(Golbe_college_male)

cat("\nGolbe vs. agri\n")
Golbe_agri_male <- lm(Golbe~agri+symptom_years, data=q3_data_male)
summary(Golbe_statin_male)
confint(Golbe_statin_male)

cat("\nGolbe vs. ethnic\n")
Golbe_ethnic_male <- lm(Golbe~ethnic+symptom_years, data=q3_data_male)
summary(Golbe_ethnic_male)
confint(Golbe_ethnic_male)

cat("\nGolbe vs. income\n")
Golbe_income_male <- lm(Golbe~income+symptom_years, data=q3_data_male)
summary(Golbe_income_male)
confint(Golbe_income_male)

cat("\nGolbe vs. smoke\n")
Golbe_smoke_male <- lm(Golbe~smoke+symptom_years, data=q3_data_male)
summary(Golbe_smoke_male)
confint(Golbe_smoke_male)

sink()

##  Output = MDRS.  

sink(file="question3_results_MDRS_male.txt")

cat("\nMDRS vs. Statin and All Variables\n")
MDRS_all_statin_male <- lm(MDRS~college+agri+statin+ethnic+income+
                                smoke+symptom_years, data=q3_data_male)
summary(MDRS_all_statin_male)
confint(MDRS_all_statin_male)

cat("\nMDRS vs. Statin\n")
MDRS_statin_male <- lm(MDRS~statin+symptom_years, data=q3_data_male)
summary(MDRS_statin_male)
confint(MDRS_statin_male)

cat("\nMDRS vs. college\n")
MDRS_college_male <- lm(MDRS~college+symptom_years, data=q3_data_male)
summary(MDRS_college_male)
confint(MDRS_college_male)

cat("\nMDRS vs. agri\n")
MDRS_agri_male <- lm(MDRS~agri+symptom_years, data=q3_data_male)
summary(MDRS_statin_male)
confint(MDRS_statin_male)

cat("\nMDRS vs. ethnic\n")
MDRS_ethnic_male <- lm(MDRS~ethnic+symptom_years, data=q3_data_male)
summary(MDRS_ethnic_male)
confint(MDRS_ethnic_male)

cat("\nMDRS vs. income\n")
MDRS_income_male <- lm(MDRS~income+symptom_years, data=q3_data_male)
summary(MDRS_income_male)
confint(MDRS_income_male)

cat("\nMDRS vs. smoke\n")
MDRS_smoke_male <- lm(MDRS~smoke+symptom_years, data=q3_data_male)
summary(MDRS_smoke_male)
confint(MDRS_smoke_male)

sink()


##  Repeats above for Females.

##  Output = UPDRS.  

sink(file="question3_results_UPDRS_female.txt")

cat("\nUPDRS vs. Statin and All Variables\n")
UPDRS_all_statin_female <- lm(UPDRS~college+agri+statin+ethnic+income+
                                smoke+symptom_years, data=q3_data_female)
summary(UPDRS_all_statin_female)
confint(UPDRS_all_statin_female)

cat("\nUPDRS vs. Statin\n")
UPDRS_statin_female <- lm(UPDRS~statin+symptom_years, data=q3_data_female)
summary(UPDRS_statin_female)
confint(UPDRS_statin_female)

cat("\nUPDRS vs. college\n")
UPDRS_college_female <- lm(UPDRS~college+symptom_years, data=q3_data_female)
summary(UPDRS_college_female)
confint(UPDRS_college_female)

cat("\nUPDRS vs. agri\n")
UPDRS_agri_female <- lm(UPDRS~agri+symptom_years, data=q3_data_female)
summary(UPDRS_statin_female)
confint(UPDRS_statin_female)

cat("\nUPDRS vs. ethnic\n")
UPDRS_ethnic_female <- lm(UPDRS~ethnic+symptom_years, data=q3_data_female)
summary(UPDRS_ethnic_female)
confint(UPDRS_ethnic_female)

cat("\nUPDRS vs. income\n")
UPDRS_income_female <- lm(UPDRS~income+symptom_years, data=q3_data_female)
summary(UPDRS_income_female)
confint(UPDRS_income_female)

cat("\nUPDRS vs. smoke\n")
UPDRS_smoke_female <- lm(UPDRS~smoke+symptom_years, data=q3_data_female)
summary(UPDRS_smoke_female)
confint(UPDRS_smoke_female)

sink()

##  Output = Golbe.  

sink(file="question3_results_Golbe_female.txt")

cat("\nGolbe vs. Statin and All Variables\n")
Golbe_all_statin_female <- lm(Golbe~college+agri+statin+ethnic+income+
                                smoke+symptom_years, data=q3_data_female)
summary(Golbe_all_statin_female)
confint(Golbe_all_statin_female)

cat("\nGolbe vs. Statin\n")
Golbe_statin_female <- lm(Golbe~statin+symptom_years, data=q3_data_female)
summary(Golbe_statin_female)
confint(Golbe_statin_female)

cat("\nGolbe vs. college\n")
Golbe_college_female <- lm(Golbe~college+symptom_years, data=q3_data_female)
summary(Golbe_college_female)
confint(Golbe_college_female)

cat("\nGolbe vs. agri\n")
Golbe_agri_female <- lm(Golbe~agri+symptom_years, data=q3_data_female)
summary(Golbe_statin_female)
confint(Golbe_statin_female)

cat("\nGolbe vs. ethnic\n")
Golbe_ethnic_female <- lm(Golbe~ethnic+symptom_years, data=q3_data_female)
summary(Golbe_ethnic_female)
confint(Golbe_ethnic_female)

cat("\nGolbe vs. income\n")
Golbe_income_female <- lm(Golbe~income+symptom_years, data=q3_data_female)
summary(Golbe_income_female)
confint(Golbe_income_female)

cat("\nGolbe vs. smoke\n")
Golbe_smoke_female <- lm(Golbe~smoke+symptom_years, data=q3_data_female)
summary(Golbe_smoke_female)
confint(Golbe_smoke_female)

sink()

##  Output = MDRS.  

sink(file="question3_results_MDRS_female.txt")

cat("\nMDRS vs. Statin and All Variables\n")
MDRS_all_statin_female <- lm(MDRS~college+agri+statin+ethnic+income+
                               smoke+symptom_years, data=q3_data_female)
summary(MDRS_all_statin_female)
confint(MDRS_all_statin_female)

cat("\nMDRS vs. Statin\n")
MDRS_statin_female <- lm(MDRS~statin+symptom_years, data=q3_data_female)
summary(MDRS_statin_female)
confint(MDRS_statin_female)

cat("\nMDRS vs. college\n")
MDRS_college_female <- lm(MDRS~college+symptom_years, data=q3_data_female)
summary(MDRS_college_female)
confint(MDRS_college_female)

cat("\nMDRS vs. agri\n")
MDRS_agri_female <- lm(MDRS~agri+symptom_years, data=q3_data_female)
summary(MDRS_statin_female)
confint(MDRS_statin_female)

cat("\nMDRS vs. ethnic\n")
MDRS_ethnic_female <- lm(MDRS~ethnic+symptom_years, data=q3_data_female)
summary(MDRS_ethnic_female)
confint(MDRS_ethnic_female)

cat("\nMDRS vs. income\n")
MDRS_income_female <- lm(MDRS~income+symptom_years, data=q3_data_female)
summary(MDRS_income_female)
confint(MDRS_income_female)

cat("\nMDRS vs. smoke\n")
MDRS_smoke_female <- lm(MDRS~smoke+symptom_years, data=q3_data_female)
summary(MDRS_smoke_female)
confint(MDRS_smoke_female)

sink()









