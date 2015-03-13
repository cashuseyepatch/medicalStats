##  Performs conditional logistic regression using the matched pairs.  

library(survival)

q1_data <- read.csv("question1_data_91614.csv")

sink("question1_results_91614.txt")

cat("\nDisease vs. Statin, Agriculture, and College\n")
clogit_statin_agri_college <- clogit(disease~ statin + college 
                                            + agri + strata(id), data=q1_data)
summary(clogit_statin_agri_college)

cat("\nDisease vs. Statin, Agriculture, College, and Smoking\n")
clogit_statin_agri_college_smoke <- clogit(disease~ college + statin 
                            + agri + smoke_packyears + strata(id), data=q1_data)
summary(clogit_statin_agri_college_smoke)

