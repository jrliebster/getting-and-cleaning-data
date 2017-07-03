#load packages
library(pacman)
p_load(readr, dplyr, janitor, tidyr, ggplot2)

prescreen_ratings <- read_csv("prescreen data with research questions.csv")

prescreen_ratings[prescreen_ratings=="Strongly Agree"] <- 6
prescreen_ratings[prescreen_ratings=="Agree"] <- 5
prescreen_ratings[prescreen_ratings=="Somewhat Agree"] <- 4
prescreen_ratings[prescreen_ratings=="Somewhat Disagree"] <- 3
prescreen_ratings[prescreen_ratings=="Disagree"] <- 2
prescreen_ratings[prescreen_ratings=="Strongly Disagree"] <- 1

prescreen_ratings <- prescreen_ratings %>% 
  mutate_at(vars(starts_with("RRApplicantDemonstrate")),funs(as.numeric)) %>%
  filter(!is.na(RRApplicantDemonstratesTenacity250),
         !is.na(RRApplicantDemonstratedGoodFitForSubjectArea251),
         !is.na(RRApplicantDemonstratesPassionForSubjectArea252))

tenacity <- count(prescreen_ratings, RRApplicantDemonstratesTenacity250) 
subject_fit <- count(prescreen_ratings, RRApplicantDemonstratedGoodFitForSubjectArea251)
passion <- count(prescreen_ratings, RRApplicantDemonstratesPassionForSubjectArea252)

write_csv(prescreen_ratings, "prescreen ratings with research clean.csv")
