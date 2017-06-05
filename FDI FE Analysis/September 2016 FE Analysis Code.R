#load packages
library(pacman)
p_load(readxl, readr, dplyr, janitor, tidyr, stringr, ggplot2, data.table, knitr, scales, psych, Hmisc, corrplot, gtools, stats)


# Aggregating Steps:
#   1. Load all datasets and packages
# 2. Filter out Cohort 27 observations for "which cohort" column, only Cohort 28 observations left
# 3. Filter out unnecessary columns in all datasets
# a. FDI data--keep: response ID, status, FC ID, cohort, 5 digit ID 28, DBN, grades, CCT/ICT, SC, SETSS, D75, fellow assigned SA, obs date, 1a, 1c, 1e, comments, 2b, 2c, 2d, comments, 3b, 3c, 3d, comments, 4a, comments, effective
# i. Create a business rule for grades that pulls in the numbers to one column?--discuss if this is possible with Robert
# 1) Group by grade bands (lower elem/upper elem, middle, high) and pull 0/1 or y/n into a column with the grade band as a header (ask Jake for existing code)
# a) **look out for ENL teachers who teach across K-12
# i) This doesn't actually come up in analysis
# ii. Ensure ratings (developing, effective, etc.) are strings
# 1) Need to be numeric for LM
# a) Could have sep vector for numeric rating for descriptive statistics 
# 2) Can also get mode to show where distribution lies--MG never offers a specific average for FDI
# a) Think about it more as a grading curve 
# 3) Can also do ANOVA test for significance of relationship between a numeric and string variable 
# 4. Join NHF and Vacancy List
# a. keep only: NHF--jobID, effective date, employee ID, name, email, phone; Vacancy List--effective date, jobID, current location
# 5. Rename all 5 digit column codes to "fellow_code"
# a. Join all datasets by this variable
# 
# Business rules for cleaned data
# . All lowercase
# . mar_dom1a, mar_dom2a, mar_dom3b for domains for FDI
# TT2ID
# prin_column name for principal survey column names
# MG errs on the side of keeping variables in dataset, rather than removing 


#load datasets 
#FDI observation data for City, BC, Pace
#5 digit Fellow code crosswalk
#principal survey data
#EOT obseration ratings
#Fellow observation rating tracker (just the sheets "techniques detail" and "observation ratings detail")
#Vacancy list 11/30/16
#NHF 4/5/17

bc_fdi <- read_csv ("bc_fdi.csv")%>%
  clean_names()
pace_fdi <- read_csv ("pace_fdi.csv")%>%
  clean_names()
city_fdi <- read_csv ("city_fdi.csv")%>%
  clean_names()

hunter_sped_fdi <- read_csv ("hunterspedfdi.csv")%>%
  clean_names()
hunter_tesol_fdi <- read_csv ("huntertesolfdi.csv")%>%
  clean_names()
lehman_fdi <- read_csv ("lehmanfdi.csv")%>%
  clean_names()
liu_fdi <- read_csv ("liufdi.csv")%>%
  clean_names()
sju_fdi <- read_csv ("sjufdi.csv")%>%
  clean_names()

fellow_code_crosswalk <- read_csv ("fellow_code_crosswalk.csv")%>%
  clean_names()
principal_survey_data <- read_csv ("principal_survey_data.csv")%>%
  clean_names()
end_of_pst_ratings_detail <- read_csv ("end_of_pst_ratings_detail.csv")%>%
  clean_names()
final_pst_ratings <- read_csv("final_pst_ratings.csv")%>%
  clean_names()
technique_ratings <- read_csv ("technique_ratings.csv")%>%
  clean_names()
vacancy_list <- read_csv("vacancy_list_113016.csv")%>%
  clean_names()
nhf <- read_csv("nhf.csv")%>%
  clean_names()
june_2016_pst_ratings <-read_csv("june_2016_pst_final_ratings.csv")%>%
  clean_names()
fyi_report <- read_csv("fyireport.csv")%>%
  clean_names()
pip_report <- read_csv("pipreport.csv") %>%
  clean_names()
june_2016_crosswalk <- read_csv("cohort_27_crosswalk.csv") %>%
  clean_names()
all_fdi_2016 <- read_csv("cohort27_fdi.csv") %>%
  clean_names() 
coach_ratings <- read_csv("coachratings.csv") %>%
  clean_names()
lead_coach_survey <- read_csv("lead_coach_survey.csv") %>%
  clean_names()
selection <- read_csv("sept 2016 selection.csv") %>%
  clean_names()

selection_2016 <- read_csv("june 2016 selection.csv") %>%
  clean_names()

selection <- filter(selection, rrappstatus7 == "Enrolled")
selection <- selection[, (colnames(selection) %in% c("rrappuserid3","rrteachingsampleoverall132","rrcriticalthinkingoverall27", "rrteacherpresence23"))]
names(selection)[names(selection) == "rrappuserid3"] <- "tt2id"

selection_2016 <- filter(selection_2016, rrappstatus7 == "Enrolled")
selection_2016 <- selection_2016[, (colnames(selection_2016) %in% c("rrappuserid3","rrteachingsampleoverall132","rrcriticalthinkingoverall27", "rrteacherpresence23"))]
names(selection_2016)[names(selection_2016) == "rrappuserid3"] <- "tt2id"

lead_coach_survey <- lead_coach_survey[, !(colnames(lead_coach_survey) %in% c("respondentid","collectorid","startdate", "enddate", "ip_address", "custom_data"))]

coach_ratings <- coach_ratings %>%
  rename(coachname=name)
# Filter out Cohort 27 observations for "which cohort" column in all FDI datasets, add to sep dataset for comparison, then keep only Cohort 28 observations in all dataframe, complete cases as well
#checked against original excel files, all have correct number of cases
#before joining all fdi datasets together, need to create a column in each fdi dataset that signifies the university
bc_fdi_2016<-filter(bc_fdi, which_cohort_is_this_fellow_a_member_of ==  "Cohort 27") 
bc_fdi_2016<-filter(bc_fdi, status ==  "Complete") %>%
  mutate(university="BC")
bc_fdi_2016 <- bc_fdi_2016[, !(colnames(bc_fdi_2016) %in% c("please_select_the_cohort_28_fellow_id_that_corresponds_to_the_fellow_you_observed"))]

bc_fdi<-filter(bc_fdi, which_cohort_is_this_fellow_a_member_of ==  "Cohort 28") 
bc_fdi<-filter(bc_fdi, status ==  "Complete") %>%
  mutate(university="BC")

city_fdi_2016<-filter(city_fdi, which_cohort_is_this_fellow_a_member_of ==  "Cohort 27") 
city_fdi_2016<-filter(city_fdi, status ==  "Complete") %>%
  mutate(university="City")
city_fdi_2016 <- city_fdi_2016[, !(colnames(city_fdi_2016) %in% c("please_select_the_cohort_28_fellow_id_that_corresponds_to_the_fellow_you_observed"))]

city_fdi<-filter(city_fdi, which_cohort_is_this_fellow_a_member_of ==  "Cohort 28") 
city_fdi<-filter(city_fdi, status ==  "Complete") %>%
  mutate(university="City")

pace_fdi_2016<-filter(pace_fdi, which_cohort_is_this_fellow_a_member_of ==  "Cohort 27") 
pace_fdi_2016<-filter(pace_fdi, status ==  "Complete") %>%
  mutate(university="Pace")
pace_fdi_2016 <- pace_fdi_2016[, !(colnames(pace_fdi_2016) %in% c("please_select_the_cohort_28_fellow_id_that_corresponds_to_the_fellow_you_observed"))]

pace_fdi<-filter(pace_fdi, which_cohort_is_this_fellow_a_member_of ==  "Cohort 28") 
pace_fdi<-filter(pace_fdi, status ==  "Complete") %>%
  mutate(university="Pace")

# liu_fdi_2016<-filter(liu_fdi, which_cohort_is_this_fellow_a_member_of ==  "Cohort 27") 
# liu_fdi_2016<-filter(liu_fdi, status ==  "Complete") %>%
#   mutate(university="LIU")
# 
# sju_fdi_2016<-filter(sju_fdi, which_cohort_is_this_fellow_a_member_of ==  "Cohort 27") 
# sju_fdi_2016<-filter(sju_fdi, status ==  "Complete")
# sju_fdi_2016<-filter(sju_fdi, are_you_observing_a_teaching_fellow_or_a_partner_teacher ==  "Teaching Fellow") %>%
#   mutate(university="SJU")
# sju_fdi_2016 <- sju_fdi_2016[, !(colnames(sju_fdi_2016) %in% c("are_you_observing_a_teaching_fellow_or_a_partner_teacher", "which_cohort_is_this_partner_teacher_a_member_of", "please_select_the_cohort_5_partner_teacher_id_that_corresponds_to_the_fellow_you_observed"))]
# 
# lehman_fdi_2016<-filter(lehman_fdi, which_cohort_is_this_fellow_a_member_of ==  "Cohort 27") 
# lehman_fdi_2016<-filter(lehman_fdi, status ==  "Complete") %>%
#   mutate(university="Lehman")
# 
# hunter_tesol_fdi_2016<-filter(hunter_tesol_fdi, which_cohort_is_this_fellow_a_member_of ==  "Cohort 27") 
# hunter_tesol_fdi_2016<-filter(hunter_tesol_fdi, status ==  "Complete") %>%
#   mutate(university="Hunter")
# 
# hunter_sped_fdi_2016<-filter(hunter_sped_fdi, which_cohort_is_this_fellow_a_member_of ==  "Cohort 27") 
# hunter_sped_fdi_2016<-filter(hunter_sped_fdi, status ==  "Complete") %>%
#   mutate(university="Hunter")

all_fdi_2016 <- all_fdi_2016[all_fdi_2016$uni %in% c("Pace University" , "City College", "Brooklyn College"),] 

#join all fdi data together into one larger dataset using rbind (as all variables are same)
#then join with crosswalk of Fellow codes to we have their appid (change variables to have same name first)
all_fdi <- rbind(pace_fdi, city_fdi)
all_fdi <- rbind(all_fdi, bc_fdi)

names(all_fdi)[names(all_fdi) == "please_select_the_cohort_28_fellow_id_that_corresponds_to_the_fellow_you_observed"] <- "fellow_id"
names(fellow_code_crosswalk)[names(fellow_code_crosswalk) == "six_digit_id"] <- "fellow_id"
all_fdi <- left_join (all_fdi, fellow_code_crosswalk, by = "fellow_id")
names(all_fdi)[names(all_fdi) == "app_user_id"] <- "tt2id"
names(principal_survey_data)[names(principal_survey_data) == "please_enter_the_5_character_code_for_the_fellow_in_the_email_you_received_from_our_office"] <- "fellow_id"
principal_survey_data <- left_join(principal_survey_data, fellow_code_crosswalk, by="fellow_id")

#FE Team requested comparisons to March FDI data for C27, add in here and create variable that signifies which FDI data report each is
mar_fdi_2016 <- rbind(pace_fdi_2016, city_fdi_2016, bc_fdi_2016)

all_fdi_2016 <- all_fdi_2016 %>%  
  rename(fellow_id = c27id,
         university = uni)

june_2016_pst_ratings <- june_2016_pst_ratings %>%  
  rename(tt2id = tt2_id)
all_2016_fellow_data <- left_join(june_2016_pst_ratings, all_fdi_2016)
all_2016_fellow_data <-filter(all_2016_fellow_data, `current_status` ==  "Enrolled")
all_2016_fellow_data <- all_2016_fellow_data %>% drop_na(current_summative_score)
all_2016_fellow_data <-filter (all_2016_fellow_data, `current_summative_score` !=  "Not all ratings have been entered")
all_2016_fellow_data <- all_2016_fellow_data %>%
  filter(!is.na(fellow_id))
all_2016_fellow_data <- left_join(all_2016_fellow_data, selection_2016)

#client requested we only keep universities and subject areas we have in MY cohort as to have accurate comparison to June 2016 Fellows
all_2016_fellow_data <- all_2016_fellow_data[all_2016_fellow_data$oct_subject %in% c(NA , "ESL", "D75", "Special Education"), ] 
all_2016_fellow_data$oct_subject[is.na(all_2016_fellow_data$oct_subject)] <- "D75"

#all_fdi now includes school code and appid from fellow_code_crosswalk, no missing cases, all have appid

#remove columns that include survey access data (keep school DBN column to confirm it is the same school, but reference School Code, as this column follows the 00X000 format)
#remove comments columns!
all_fdi <- all_fdi[, !(colnames(all_fdi) %in% c("referer","extended_referer","language", "sessionid", "user_agent", "extended_user_agent", "tags", "ip_address", "contact_id", "legacy_comments", "longitude", "latitude", "country", "city", "state_region", "postal", "time_started", "comments", "url_redirect", "please_select_the_cohort_27_fellow_id_that_corresponds_to_the_fellow_you_observed"))]

#change fdi ratings column headings to match Megan's business rules (see line 28)

all_fdi <- all_fdi %>%  
  rename(mar_dom1a = x1a_please_record_your_observation_rating_for_component_1a_demonstrating_knowledge_of_content_and_pedagogy, 
  mar_dom1c = x1c_please_record_your_observation_rating_for_component_1c_setting_instructional_outcomes, 
  mar_dom1e = x1e_please_record_your_observation_rating_for_component_1e_designing_coherent_instruction, mar_dom2b = x2b_please_record_your_observation_rating_for_component_2b_establishing_a_culture_for_learning,
  mar_dom2c = x2c_please_record_your_observation_rating_for_component_2c_managing_classroom_procedures,
  mar_dom2d = x2d_please_record_your_observation_rating_for_component_2d_managing_student_behavior,
  mar_dom3b = x3b_please_record_your_observation_rating_for_component_3b_using_questioning_and_discussion_techniques,
  mar_dom3c = x3c_please_record_your_observation_rating_for_component_3c_engaging_students_in_learning, 
  mar_dom3d = x3d_please_record_your_observation_rating_for_component_3d_using_assessment_in_instruction,
  mar_dom4a = x4a_please_record_your_observation_rating_for_component_4a_reflecting_on_teaching,
  cohort = which_cohort_is_this_fellow_a_member_of, 
  effectiveness = how_effective_do_you_believe_this_fellow_is_compared_to_other_teachers_with_similar_experience_eg_first_year_fellows_compared_to_other_first_year_teachers,
  subject = what_is_the_fellow_s_assigned_university_subject_area_please_select_the_subject_they_are_training_in_at_their_university_not_the_subject_they_are_teaching_during_this_observation,
  other_sped_setting = n_a_if_applicable_please_identify_the_special_education_setting_of_the_observed_lesson_select_all_applicable_settings,
  d75 = d75_if_applicable_please_identify_the_special_education_setting_of_the_observed_lesson_select_all_applicable_settings,
  setts = setts_if_applicable_please_identify_the_special_education_setting_of_the_observed_lesson_select_all_applicable_settings,
  self_contained = self_contained_if_applicable_please_identify_the_special_education_setting_of_the_observed_lesson_select_all_applicable_settings,
  cct_ict = ctt_ict_if_applicable_please_identify_the_special_education_setting_of_the_observed_lesson_select_all_applicable_settings) 

mar_fdi_2016 <- mar_fdi_2016 %>%  
  rename(mar_dom1a = x1a_please_record_your_observation_rating_for_component_1a_demonstrating_knowledge_of_content_and_pedagogy, 
         mar_dom1c = x1c_please_record_your_observation_rating_for_component_1c_setting_instructional_outcomes, 
         mar_dom1e = x1e_please_record_your_observation_rating_for_component_1e_designing_coherent_instruction, mar_dom2b = x2b_please_record_your_observation_rating_for_component_2b_establishing_a_culture_for_learning,
         mar_dom2c = x2c_please_record_your_observation_rating_for_component_2c_managing_classroom_procedures,
         mar_dom2d = x2d_please_record_your_observation_rating_for_component_2d_managing_student_behavior,
         mar_dom3b = x3b_please_record_your_observation_rating_for_component_3b_using_questioning_and_discussion_techniques,
         mar_dom3c = x3c_please_record_your_observation_rating_for_component_3c_engaging_students_in_learning, 
         mar_dom3d = x3d_please_record_your_observation_rating_for_component_3d_using_assessment_in_instruction,
         mar_dom4a = x4a_please_record_your_observation_rating_for_component_4a_reflecting_on_teaching,
         cohort = which_cohort_is_this_fellow_a_member_of, 
         effectiveness = how_effective_do_you_believe_this_fellow_is_compared_to_other_teachers_with_similar_experience_eg_first_year_fellows_compared_to_other_first_year_teachers,
         subject = what_is_the_fellow_s_assigned_university_subject_area_please_select_the_subject_they_are_training_in_at_their_university_not_the_subject_they_are_teaching_during_this_observation,
         other_sped_setting = n_a_if_applicable_please_identify_the_special_education_setting_of_the_observed_lesson_select_all_applicable_settings,
         d75 = d75_if_applicable_please_identify_the_special_education_setting_of_the_observed_lesson_select_all_applicable_settings,
         setts = setts_if_applicable_please_identify_the_special_education_setting_of_the_observed_lesson_select_all_applicable_settings,
         self_contained = self_contained_if_applicable_please_identify_the_special_education_setting_of_the_observed_lesson_select_all_applicable_settings,
         cct_ict = ctt_ict_if_applicable_please_identify_the_special_education_setting_of_the_observed_lesson_select_all_applicable_settings) 

mar_fdi_2016 <- mar_fdi_2016[mar_fdi_2016$subject %in% c( "ESL", "D75", "Special Education", "Special Education - D75"), ] 


# #************Field consultant ID and subject areacolumns not changing--why? changed to clean version and it worked, so leaving this here as a reminder
## can also rename columns when selecting them [select(new_name = old_name)]
## read_csv(aasdfasdfhjaksdf, col_names =

# #			ii. Ensure ratings (developing, effective, etc.) are strings
# 1) Need to be numeric for LM
# a) Could have sep vector for numeric rating for descriptive statistics 
# 2) Can also get mode to show where distribution lies--MG never offers a specific average for FDI
# a) Think about it more as a grading curve 
# Can also do ANOVA test for significance of relationship between a numeric and string variable 

#join final_pst_ratings, end_of_pst_ratings_detail, and all_fdi (also added technique_ratings, PIP, FYI)
#use currentsummativescore, not finalsummativescore for more complete cases
end_of_pst_ratings_detail <- end_of_pst_ratings_detail %>%  
  rename(tt2id = tt2_id)
all_fellow_data <- left_join(final_pst_ratings, end_of_pst_ratings_detail, by = "tt2id")
all_fellow_data <- left_join(all_fellow_data, all_fdi)
all_fellow_data <- left_join(all_fellow_data, principal_survey_data)
all_fellow_data <-filter(all_fellow_data, `currentstatus` ==  "Enrolled")
all_fellow_data <- all_fellow_data %>% drop_na(currentsummativescore)
all_fellow_data <-filter (all_fellow_data, `currentsummativescore` !=  "Not all ratings have been entered")
all_fellow_data <- all_fellow_data %>% drop_na(effectiveness)
all_fellow_data <- left_join(all_fellow_data, selection, by = "tt2id") 

all_fellow_data <- all_fellow_data %>%
  filter(!is.na(rrteacherpresence23),
         !is.na(rrteachingsampleoverall132),
         !is.na(rrcriticalthinkingoverall27))
         
technique_ratings <- technique_ratings %>%
  rename(tt2id = tt2_id)
all_fellow_data <- left_join(all_fellow_data, technique_ratings, by = "tt2id")
all_fellow_data[all_fellow_data == "Missing"] <- "NA"

pip_report <- pip_report %>%
  rename(tt2id = rrappuserid3) %>%
  select(tt2id, rrpipsubmitted11, rrpipdate8)
fyi_report <- fyi_report %>%
  rename(tt2id = rrappuserid1) %>%
  group_by(tt2id) %>%
  summarise(count_fyi = n()) 

all_fellow_data <- left_join(all_fellow_data, fyi_report, by = "tt2id")
all_fellow_data <- left_join(all_fellow_data, pip_report, by = "tt2id")
all_fellow_data$rrpipsubmitted11[all_fellow_data$rrpipsubmitted11 == "PIP Submited"] <- 1 
all_fellow_data$rrpipsubmitted11[is.na(all_fellow_data$rrpipsubmitted11)] <- 0
all_fellow_data$count_fyi[is.na(all_fellow_data$count_fyi)] <- 0
all_fellow_data$rrpipdate8[is.na(all_fellow_data$rrpipdate8)] <- 0
all_fellow_data$everybody_writes[all_fellow_data$everybody_writes == "NA"] <- 0

all_fellow_data <- all_fellow_data %>%
  rename(principal_effectiveness = how_does_this_teaching_fellow_u_0092_s_performance_compare_to_other_first_year_teachers_you_u_0092_ve_worked_with)

#there are duplicates in the FYI report, as some Fellows received multiple FYIs
all_fellow_data <- all_fellow_data %>%
  group_by(tt2id) %>%
  arrange(currentsummativescore) %>%
  slice(1) 

tabyl(all_fdi$mar_dom1a)

#distribution of final summative PST scores
all_fellow_data$currentsummativescore <- as.numeric(as.character(all_fellow_data$currentsummativescore))
ggplot(all_fellow_data, aes(x = currentsummativescore)) +
  geom_histogram(binwidth=.05) +
  labs(title = "Distribution of Final PST Ratings") +
  theme_minimal()
#either transform data to fall into ranges of ratings, cutoff ranges (e.g. 2.26-2.3 is 2.3)

all_fellow_data$currentsummativescore <- as.numeric(as.character(all_fellow_data$currentsummativescore))

#for linear modeling, change back to string for descriptives or run descriptives first
#recode
all_fellow_data[all_fellow_data== "Highly Effective"] <- 6
all_fellow_data[all_fellow_data=="Effective+"] <- 5
all_fellow_data[all_fellow_data== "Effective"] <- 4
all_fellow_data[all_fellow_data== "Developing+"] <- 3
all_fellow_data[all_fellow_data== "Developing"] <- 2
all_fellow_data[all_fellow_data== "Ineffective"] <- 1

all_fellow_data[all_fellow_data== "Top 10%"] <- 4
all_fellow_data[all_fellow_data== "Above Average"] <- 3
all_fellow_data[all_fellow_data== "Average"] <- 2
all_fellow_data[all_fellow_data== "Below Average"] <- 1
all_fellow_data[all_fellow_data== "Bottom 10%"] <- 0

#high performers at selection coded as 1, lower as 0
all_fellow_data[all_fellow_data== "E"] <- 1
all_fellow_data[all_fellow_data== "FA+"] <- 1
all_fellow_data[all_fellow_data== "FA"] <- 0
all_fellow_data[all_fellow_data== "FA-"] <- 0
all_fellow_data[all_fellow_data== "NFA"] <- 0

all_2016_fellow_data[all_2016_fellow_data== "E"] <- 1
all_2016_fellow_data[all_2016_fellow_data== "FA+"] <- 1
all_2016_fellow_data[all_2016_fellow_data== "FA"] <- 0
all_2016_fellow_data[all_2016_fellow_data== "FA-"] <- 0
all_2016_fellow_data[all_2016_fellow_data== "NFA"] <- 0

all_2016_fellow_data[all_2016_fellow_data== "Highly Effective"] <- 6
all_2016_fellow_data[all_2016_fellow_data=="Effective+"] <- 5
all_2016_fellow_data[all_2016_fellow_data== "Effective"] <- 4
all_2016_fellow_data[all_2016_fellow_data== "Developing+"] <- 3
all_2016_fellow_data[all_2016_fellow_data== "Developing"] <- 2
all_2016_fellow_data[all_2016_fellow_data== "Ineffective"] <- 1

all_2016_fellow_data[all_2016_fellow_data== "Top 10%"] <- 4
all_2016_fellow_data[all_2016_fellow_data== "Above Average"] <- 3
all_2016_fellow_data[all_2016_fellow_data== "Average"] <- 2
all_2016_fellow_data[all_2016_fellow_data== "Below Average"] <- 1
all_fellow_data[all_fellow_data== "Bottom 10%"] <- 0

mar_fdi_2016[mar_fdi_2016== "Highly Effective"] <- 6
mar_fdi_2016[mar_fdi_2016=="Effective+"] <- 5
mar_fdi_2016[mar_fdi_2016== "Effective"] <- 4
mar_fdi_2016[mar_fdi_2016== "Developing+"] <- 3
mar_fdi_2016[mar_fdi_2016== "Developing"] <- 2
mar_fdi_2016[mar_fdi_2016== "Ineffective"] <- 1

mar_fdi_2016[mar_fdi_2016== "Top 10%"] <- 4
mar_fdi_2016[mar_fdi_2016== "Above Average"] <- 3
mar_fdi_2016[mar_fdi_2016== "Average"] <- 2
mar_fdi_2016[mar_fdi_2016== "Below Average"] <- 1
mar_fdi_2016[mar_fdi_2016== "Bottom 10%"] <- 0

#mutate_each or mutate_at or mutate_which to change them all at once (specify each or starts with)
all_fellow_data$effectiveness <- as.numeric(as.character(all_fellow_data$effectiveness))
all_fellow_data$round1 <- as.numeric(as.character(all_fellow_data$round1))
all_fellow_data$round2 <- as.numeric(as.character(all_fellow_data$round2))
all_fellow_data$round3 <- as.numeric(as.character(all_fellow_data$round3))
all_fellow_data$anchortechaveragehighestfor <- as.numeric(as.character(all_fellow_data$anchortechaveragehighestfor))
all_fellow_data$nonanchortechniqueaverage <- as.numeric(as.character(all_fellow_data$nonanchortechniqueaverage))
all_fellow_data$mar_dom1a <- as.numeric(as.character(all_fellow_data$mar_dom1a))
all_fellow_data$mar_dom1c <- as.numeric(as.character(all_fellow_data$mar_dom1c))
all_fellow_data$mar_dom1e <- as.numeric(as.character(all_fellow_data$mar_dom1e))
all_fellow_data$mar_dom2b <- as.numeric(as.character(all_fellow_data$mar_dom2b))
all_fellow_data$mar_dom2c <- as.numeric(as.character(all_fellow_data$mar_dom2c))
all_fellow_data$mar_dom2d <- as.numeric(as.character(all_fellow_data$mar_dom2d))
all_fellow_data$mar_dom3b <- as.numeric(as.character(all_fellow_data$mar_dom3b))
all_fellow_data$mar_dom3c <- as.numeric(as.character(all_fellow_data$mar_dom3c))
all_fellow_data$mar_dom3d <- as.numeric(as.character(all_fellow_data$mar_dom3d))
all_fellow_data$mar_dom4a <- as.numeric(as.character(all_fellow_data$mar_dom4a))
all_fellow_data$rrpipsubmitted11 <- as.numeric(as.character(all_fellow_data$rrpipsubmitted11))
all_fellow_data$count_fyi <- as.numeric(as.character(all_fellow_data$count_fyi))
all_fellow_data$x100percent <- as.numeric(as.character(all_fellow_data$x100percent))
all_fellow_data$what_to_do <- as.numeric(as.character(all_fellow_data$what_to_do))
all_fellow_data$strong_voice <- as.numeric(as.character(all_fellow_data$strong_voice))
all_fellow_data$positive_framing <- as.numeric(as.character(all_fellow_data$positive_framing))
all_fellow_data$anchor_highest_formal_average <- as.numeric(as.character(all_fellow_data$anchor_highest_formal_average))
all_fellow_data$engineer_efficiency <- as.numeric(as.character(all_fellow_data$engineer_efficiency))
all_fellow_data$strong_start <- as.numeric(as.character(all_fellow_data$strong_start))
all_fellow_data$cold_call <- as.numeric(as.character(all_fellow_data$cold_call))
all_fellow_data$stretch_it <- as.numeric(as.character(all_fellow_data$stretch_it))
all_fellow_data$control_the_game<- as.numeric(as.character(all_fellow_data$control_the_game))
all_fellow_data$everybody_writes <- as.numeric(as.character(all_fellow_data$everybody_writes))
all_fellow_data$non_anchor_highest_formal_average <- as.numeric(as.character(all_fellow_data$non_anchor_highest_formal_average))
all_fellow_data$principal_effectiveness <- as.numeric(as.character(all_fellow_data$principal_effectiveness))
all_fellow_data$observationaveragetodate <- as.numeric(all_fellow_data$observationaveragetodate)

all_fellow_data <- all_fellow_data %>%
  mutate(growth_total = round((currentsummativescore - round1), digits = 1), 
        growth_week3 = round((round3 - round2), digits = 1), 
        growth_week2 = round((round2 - round1), digits = 1))

all_2016_fellow_data$oct_rel_effect <- as.numeric(as.character(all_2016_fellow_data$oct_rel_effect))

#create PST quartile variable
# all_fellow_data <- all_fellow_data %>%
#   group_by(currentsummativescore) %>%
#   summarise(count = n()) %>%
#   mutate(percent = count / sum(count)) %>%
#   select(currentsummativescore, percent, count) # choose and reorder columns 

#pst_quartile 
#quantcut(all_fellow_data$currentsummativescore, q=seq(0,1,by=0.25))
#determine cut scores for each quartile, mutate variable to sort people into each quartile, run ANOVA to see if differences are stat sig; could also look at distribution of effectiveness by PST quartile
# 1. create cut scores
# 2. create variable for each teachers that uses cut scores to assign their PST score to a quartile 
# 3. group_by the quartile variable and run the mean of PST scores
# 4. test the differences in means by running an anova
pst_bottom <-quantile(all_fellow_data$currentsummativescore, .25, na.rm = TRUE)
pst_median <-quantile(all_fellow_data$currentsummativescore, .50, na.rm = TRUE)
pst_top <- quantile(all_fellow_data$currentsummativescore, .75, na.rm = TRUE)

all_fellow_data <- all_fellow_data %>%
  mutate(pst_quartile = ifelse(currentsummativescore > pst_top, 4,
                        ifelse(currentsummativescore > pst_median, 3,
                        ifelse(currentsummativescore > pst_bottom, 2, 1))))

pst_quartile_effectiveness <- all_fellow_data %>%
  group_by(pst_quartile) %>%
  summarise(mean_effectiveness = mean(dom2_dom3_avg, na.rm = TRUE))

pst_effectiveness <- all_fellow_data %>%
  group_by(effectiveness) %>%
  summarise(mean_pst_rating = mean(currentsummativescore, na.rm = TRUE))

pst_quartile_effectiveness_anova <- aov(all_fellow_data$effectiveness ~ all_fellow_data$pst_quartile)
summary(pst_quartile_effectiveness_anova)

#same for june 2016
pst_bottom_2016 <-quantile(all_2016_fellow_data$current_summative_score, .25, na.rm = TRUE)
pst_median_2016 <-quantile(all_2016_fellow_data$current_summative_score, .50, na.rm = TRUE)
pst_top_2016 <- quantile(all_2016_fellow_data$current_summative_score, .75, na.rm = TRUE)

all_2016_fellow_data <- all_2016_fellow_data %>%
  mutate(pst_quartile = ifelse(current_summative_score > pst_top_2016, 4,
                               ifelse(current_summative_score > pst_median_2016, 3,
                                      ifelse(current_summative_score > pst_bottom_2016, 2, 1))))

pst_quartile_2016_effectiveness <- all_2016_fellow_data %>%
  group_by(pst_quartile) %>%
  summarise(mean_effectiveness = mean(oct_rel_effect, na.rm = TRUE))

pst_effectiveness_2016 <- all_2016_fellow_data %>%
  group_by(oct_rel_effect) %>%
  drop_na(oct_rel_effect) %>%
  summarise(mean_pst_rating = mean(current_summative_score, na.rm = TRUE))

pst_quartile_effectiveness_anova_2016 <- aov(all_2016_fellow_data$oct_rel_effect ~ all_2016_fellow_data$pst_quartile)
summary(pst_quartile_effectiveness_anova_2016)

##group ratings by university
##group ratings by field consultant
##group ratings by training academy

#q2_retention_school_anova <- aov(q2_retention_school_anova$returning_employee_school ~ q2_retention_school_anova$school_perf_16) 
#cut_number in ggplot as well
#poverty_school_top_q <- quantile(poverty$tanf_snap_school, .25, na.rm = TRUE) 

#coretest_1 <- rcorr(as.matrix(all_fellow_data$effectiveness, all_fellow_data$currentsummativescore))


#correlation between relative effectiveness and average of all components in domain 1/2/3/4 (then just 2 and 3)
all_fellow_data <- all_fellow_data %>% 
  rowwise() %>% 
  mutate(mar_dom1 = mean(c(mar_dom1a,mar_dom1c, mar_dom1e)),
         mar_dom2 = mean(c(mar_dom2b, mar_dom2c, mar_dom2d)),
         mar_dom3 = mean(c(mar_dom3b, mar_dom3c, mar_dom3d)))

all_fellow_data <- all_fellow_data %>% 
  rowwise() %>% 
  mutate(all_dom_avg = mean(c(mar_dom1,mar_dom2, mar_dom3, mar_dom4a)),
         dom2_dom3_avg = mean(c(mar_dom2, mar_dom3)))



##all_dom_avg quartiles
all_dom_bottom <-quantile(all_fellow_data$all_dom_avg, .25, na.rm = TRUE)
all_dom_median <-quantile(all_fellow_data$all_dom_avg, .50, na.rm = TRUE)
all_dom_top <- quantile(all_fellow_data$all_dom_avg, .75, na.rm = TRUE)

all_fellow_data <- all_fellow_data %>%
  mutate(all_dom_quartile = ifelse(all_dom_avg > all_dom_top, 4,
                               ifelse(all_dom_avg > all_dom_median, 3,
                                      ifelse(all_dom_avg > all_dom_bottom, 2, 1))))

##dom2_dom3_avg quartiles
dom23_bottom <-quantile(all_fellow_data$dom2_dom3_avg, .25, na.rm = TRUE)
dom23_median <-quantile(all_fellow_data$dom2_dom3_avg, .50, na.rm = TRUE)
dom23_top <- quantile(all_fellow_data$dom2_dom3_avg, .75, na.rm = TRUE)

all_fellow_data <- all_fellow_data %>%
  mutate(dom23_quartile = ifelse(dom2_dom3_avg > dom23_top, 4,
                                   ifelse(dom2_dom3_avg > dom23_median, 3,
                                          ifelse(dom2_dom3_avg > dom23_bottom, 2, 1))))

# ggplot(all_fellow_data, aes(x = effectiveness, y = count)) +
#   geom_point() +
#   labs(title = "Distribution of FDI Effectiveness") +
#   theme_minimal()

mod1 = lm(all_fellow_data$mar_dom1a ~ all_fellow_data$dom2_dom3_avg) 
summary(mod1)
ggplot(all_fellow_data, aes(x = mar_dom1a, y = dom2_dom3_avg)) +
  geom_point() +
  geom_jitter(alpha = .3) +
  geom_smooth(method = "lm", formula = y~x) +
  labs(x = "Dom 1A", y = "Effectiveness") +
  theme_light()

#final PST summative scores as compared to FDI effectiveness ratings
mod2 = lm(all_fellow_data$currentsummativescore ~ all_fellow_data$dom2_dom3_avg)
summary(mod2)
ggplot(all_fellow_data, aes(x = dom2_dom3_avg, y = currentsummativescore)) +
  geom_point() +
  geom_jitter(alpha = .3) +
  geom_smooth(method = "lm", formula = y~x) +
  labs(x = "FDI Effectiveness", y = "Final PST Score") 


# fellow_hiring_data <- fellow_hiring_data %>% 
#   mutate(mid_year_vacancy=eff_date>2020-09-06)
# 
# mid_year_hires <- fellow_hiring_data %>%
#   mutate( == eff_date > "2020-09-06") 

vacancy_effective <- vacancy_list %>%
  select(eff_date, vacancy_create_date)

mod3 = lm(all_fellow_data$principal_effectiveness ~ all_fellow_data$effectiveness)
summary(mod3)
ggplot(all_fellow_data, aes(x = effectiveness, y = principal_effectiveness)) +
  geom_point() +
  geom_jitter(alpha = .3) +
  geom_smooth(method = "lm", formula = y~x) +
  labs(x = "FDI Effectiveness", y = "Principal Effectiveness") 

mod4 = lm(all_fellow_data$principal_effectiveness ~ all_fellow_data$currentsummativescore)
summary(mod4)
ggplot(all_fellow_data, aes(x = currentsummativescore, y = principal_effectiveness)) +
  geom_point() +
  geom_jitter(alpha = .3) +
  geom_smooth(method = "lm", formula = y~x) +
  labs(x = "PST Score", y = "Principal Effectiveness") 

all_fellow_data %>% 
  group_by(all_dom_quartile, pst_quartile) %>%
  drop_na(all_dom_quartile) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count)) %>%
  select(all_dom_quartile, pst_quartile, percent, count)

all_dom_quartile_dist <- all_fellow_data %>% 
  group_by(all_dom_quartile) %>%
  drop_na(all_dom_quartile) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count)) %>%
  select(all_dom_quartile, percent, count)

dom23_quartile_dist <- all_fellow_data %>% 
  group_by(dom23_quartile) %>%
  drop_na(dom23_quartile) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count)) %>%
  select(dom23_quartile, percent, count)

all_fellow_data %>% 
  group_by(dom23_quartile, pst_quartile) %>%
  drop_na(dom23_quartile) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count)) %>%
  select(dom23_quartile, pst_quartile, percent, count)

all_fdi_effectiveness_dist <- all_fellow_data %>% 
  group_by(dom23_quartile, all_dom_quartile, effectiveness) %>%
  drop_na(dom23_quartile) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count)) %>%
  select(dom23_quartile, all_dom_quartile, effectiveness, percent, count)

dom23_dist <- all_fellow_data %>% 
  group_by(dom2_dom3_avg) %>%
  drop_na(dom2_dom3_avg) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count)) %>%
  select(dom2_dom3_avg, percent, count)

dom23_2016_dist <- all_2016_fellow_data %>% 
  group_by(oct_dom2_dom3_avg) %>%
  drop_na(oct_dom2_dom3_avg) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count)) %>%
  select(oct_dom2_dom3_avg, percent, count)

mar_dom23_2016_dist <- mar_fdi_2016 %>% 
  group_by(mar_dom2_dom3_avg) %>%
  drop_na(mar_dom2_dom3_avg) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count)) %>%
  select(mar_dom2_dom3_avg, percent, count)

principal_ratings <- all_fellow_data %>% 
  group_by(principal_effectiveness) %>%
  drop_na(principal_effectiveness) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count)) %>%
  select(principal_effectiveness, percent, count)

dom1_dist <- all_fellow_data %>% 
  group_by(mar_dom1) %>%
  drop_na(mar_dom1) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count)) %>%
  select(mar_dom1, percent, count)

dom2_dist <- all_fellow_data %>% 
  group_by(mar_dom2) %>%
  drop_na(mar_dom2) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count)) %>%
  select(mar_dom2, percent, count)

dom3_dist <- all_fellow_data %>% 
  group_by(mar_dom3) %>%
  drop_na(mar_dom3) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count)) %>%
  select(mar_dom3, percent, count)

dom4_dist <- all_fellow_data %>% 
  group_by(mar_dom4a) %>%
  drop_na(mar_dom4a) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count)) %>%
  select(mar_dom4a, percent, count)

#fdi_effectiveness_and_final_pst_score <- lm(all_fdi$effectiveness ~ final_pst_ratings$finalsummativescore)
#I need to pull all numeric variables into a sep dataframe that I want to run correlations on. 
#Then, I run the cor, save it as an object, and use corrplot on that

cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}


#correlation between average score and fdi/pst/principal, descriptives, bring to team Friday and see what questions they have  
all_effectiveness <- all_fellow_data %>%
  ungroup() %>%
  select (currentsummativescore, effectiveness, all_dom_quartile, all_dom_avg, dom2_dom3_avg, dom23_quartile, pst_quartile, principal_effectiveness)
all_effectiveness <- all_effectiveness %>% drop_na(currentsummativescore, effectiveness, all_dom_avg, dom2_dom3_avg, all_dom_quartile, dom23_quartile, pst_quartile, principal_effectiveness)
all_effectiveness_corplot1 <- corrplot(cor(all_effectiveness),
                        p.mat = cor.mtest(all_effectiveness,0.95)[[1]],
                        sig.level=0.05,
                        method="number")


all_rounds_effectiveness <- all_fellow_data %>%
  ungroup() %>%
  select (currentsummativescore, round1, round2, round3, dom2_dom3_avg, dom23_quartile, pst_quartile, principal_effectiveness)
all_rounds_effectiveness <- all_rounds_effectiveness %>% drop_na(currentsummativescore, round1, round2, round3, dom2_dom3_avg, dom23_quartile, pst_quartile, principal_effectiveness)
all_rounds_effectiveness_corplot1 <- corrplot(cor(all_rounds_effectiveness),
                                       p.mat = cor.mtest(all_rounds_effectiveness,0.95)[[1]],
                                       sig.level=0.05,
                                       method="number")

#correlation between relative effectiveness and average of all components in domain 1/2/3/4 (then just 2 and 3)
all_2016_fellow_data$oct_dom1a <- as.numeric(as.character(all_2016_fellow_data$oct_dom1a))
all_2016_fellow_data$oct_dom1c <- as.numeric(as.character(all_2016_fellow_data$oct_dom1c))
all_2016_fellow_data$oct_dom1e <- as.numeric(as.character(all_2016_fellow_data$oct_dom1e))
all_2016_fellow_data$oct_dom2b <- as.numeric(as.character(all_2016_fellow_data$oct_dom2b))
all_2016_fellow_data$oct_dom2c <- as.numeric(as.character(all_2016_fellow_data$oct_dom2c))
all_2016_fellow_data$oct_dom2d <- as.numeric(as.character(all_2016_fellow_data$oct_dom2d))
all_2016_fellow_data$oct_dom3b <- as.numeric(as.character(all_2016_fellow_data$oct_dom3b))
all_2016_fellow_data$oct_dom3c <- as.numeric(as.character(all_2016_fellow_data$oct_dom3c))
all_2016_fellow_data$oct_dom3d <- as.numeric(as.character(all_2016_fellow_data$oct_dom3d))
all_2016_fellow_data$oct_dom4a <- as.numeric(as.character(all_2016_fellow_data$oct_dom4a))

mar_fdi_2016$mar_dom1a <- as.numeric(as.character(mar_fdi_2016$mar_dom1a))
mar_fdi_2016$mar_dom1c <- as.numeric(as.character(mar_fdi_2016$mar_dom1c))
mar_fdi_2016$mar_dom1e <- as.numeric(as.character(mar_fdi_2016$mar_dom1e))
mar_fdi_2016$mar_dom2b <- as.numeric(as.character(mar_fdi_2016$mar_dom2b))
mar_fdi_2016$mar_dom2c <- as.numeric(as.character(mar_fdi_2016$mar_dom2c))
mar_fdi_2016$mar_dom2d <- as.numeric(as.character(mar_fdi_2016$mar_dom2d))
mar_fdi_2016$mar_dom3b <- as.numeric(as.character(mar_fdi_2016$mar_dom3b))
mar_fdi_2016$mar_dom3c <- as.numeric(as.character(mar_fdi_2016$mar_dom3c))
mar_fdi_2016$mar_dom3d <- as.numeric(as.character(mar_fdi_2016$mar_dom3d))
mar_fdi_2016$mar_dom4a <- as.numeric(as.character(mar_fdi_2016$mar_dom4a))

all_2016_fellow_data <- all_2016_fellow_data %>% 
  rowwise() %>% 
  mutate(oct_dom1 = mean(c(oct_dom1a, oct_dom1c, oct_dom1e), na.rm=TRUE),
         oct_dom2 = mean(c(oct_dom2b, oct_dom2c, oct_dom2d), na.rm=TRUE),
         oct_dom3 = mean(c(oct_dom3b, oct_dom3c, oct_dom3d), na.rm=TRUE))

all_2016_fellow_data <- all_2016_fellow_data %>% 
  rowwise() %>% 
  mutate(oct_dom_avg = mean(c(oct_dom1, oct_dom2, oct_dom3, oct_dom4a)),
         oct_dom2_dom3_avg = mean(c(oct_dom2, oct_dom3)))

mar_fdi_2016 <- mar_fdi_2016 %>% 
  rowwise() %>% 
  mutate(mar_dom1 = mean(c(mar_dom1a, mar_dom1c, mar_dom1e), na.rm=TRUE),
         mar_dom2 = mean(c(mar_dom2b, mar_dom2c, mar_dom2d), na.rm=TRUE),
         mar_dom3 = mean(c(mar_dom3b, mar_dom3c, mar_dom3d), na.rm=TRUE))

mar_fdi_2016 <- mar_fdi_2016 %>% 
  rowwise() %>% 
  mutate(mar_dom_avg = mean(c(mar_dom1, mar_dom2, mar_dom3, mar_dom4a)),
         mar_dom2_dom3_avg = mean(c(mar_dom2, mar_dom3)))

##all_dom_avg quartiles
oct_dom_bottom <-quantile(all_2016_fellow_data$oct_dom_avg, .25, na.rm = TRUE)
oct_dom_median <-quantile(all_2016_fellow_data$oct_dom_avg, .50, na.rm = TRUE)
oct_dom_top <- quantile(all_2016_fellow_data$oct_dom_avg, .75, na.rm = TRUE)

all_2016_fellow_data <- all_2016_fellow_data %>%
  mutate(oct_dom_quartile = ifelse(oct_dom_avg > oct_dom_top, 4,
                                   ifelse(oct_dom_avg > oct_dom_median, 3,
                                          ifelse(oct_dom_avg > oct_dom_bottom, 2, 1))))

##dom2_dom3_avg quartiles
oct_dom23_bottom <-quantile(all_2016_fellow_data$oct_dom2_dom3_avg, .25, na.rm = TRUE)
oct_dom23_median <-quantile(all_2016_fellow_data$oct_dom2_dom3_avg, .50, na.rm = TRUE)
oct_dom23_top <- quantile(all_2016_fellow_data$oct_dom2_dom3_avg, .75, na.rm = TRUE)

all_2016_fellow_data <- all_2016_fellow_data %>%
  mutate(oct_dom23_quartile = ifelse(oct_dom2_dom3_avg > oct_dom23_top, 4,
                                 ifelse(oct_dom2_dom3_avg > oct_dom23_median, 3,
                                        ifelse(oct_dom2_dom3_avg > oct_dom23_bottom, 2, 1))))

mar_dom23_bottom <-quantile(mar_fdi_2016$mar_dom2_dom3_avg, .25, na.rm = TRUE)
mar_dom23_median <-quantile(mar_fdi_2016$mar_dom2_dom3_avg, .50, na.rm = TRUE)
mar_dom23_top <- quantile(mar_fdi_2016$mar_dom2_dom3_avg, .75, na.rm = TRUE)

mar_fdi_2016 <- mar_fdi_2016 %>%
  mutate(mar_dom23_quartile = ifelse(mar_dom2_dom3_avg > mar_dom23_top, 4,
                                     ifelse(mar_dom2_dom3_avg > mar_dom23_median, 3,
                                            ifelse(mar_dom2_dom3_avg > mar_dom23_bottom, 2, 1))))

all_2016_effectiveness <- all_2016_fellow_data %>%
  ungroup() %>%
  select (oct_rel_effect, oct_dom_quartile, oct_dom_avg, oct_dom2_dom3_avg, oct_dom23_quartile, pst_quartile) 
all_2016_effectiveness <- all_2016_effectiveness %>% drop_na(oct_rel_effect, oct_dom_quartile, oct_dom_avg, oct_dom2_dom3_avg, oct_dom23_quartile, pst_quartile)
all_2016_effectiveness_corplot1 <- corrplot(cor(all_2016_effectiveness),
                                       p.mat = cor.mtest(all_2016_effectiveness,0.95)[[1]],
                                       sig.level=0.05,
                                       method="number", na.rm=FALSE)

#look at mean FDIs
mean(all_fellow_data$dom2_dom3_avg, trim = 0, na.rm = TRUE)
mean(all_fellow_data$mar_dom1, trim = 0, na.rm = TRUE)
mean(all_fellow_data$mar_dom2, trim = 0, na.rm = TRUE)
mean(all_fellow_data$mar_dom3, trim = 0, na.rm = TRUE)
mean(all_fellow_data$mar_dom4a, trim = 0, na.rm = TRUE)

mean(all_2016_fellow_data$oct_dom2_dom3_avg, trim = 0, na.rm = TRUE)
mean(all_2016_fellow_data$oct_dom1, trim = 0, na.rm = TRUE)
mean(all_2016_fellow_data$oct_dom2, trim = 0, na.rm = TRUE)
mean(all_2016_fellow_data$oct_dom3, trim = 0, na.rm = TRUE)
mean(all_2016_fellow_data$oct_dom4a, trim = 0, na.rm = TRUE)

mean(mar_fdi_2016$mar_dom2_dom3_avg, trim = 0, na.rm = TRUE)
mean(mar_fdi_2016$mar_dom1, trim = 0, na.rm = TRUE)
mean(mar_fdi_2016$mar_dom2, trim = 0, na.rm = TRUE)
mean(mar_fdi_2016$mar_dom3, trim = 0, na.rm = TRUE)
mean(mar_fdi_2016$mar_dom4a, trim = 0, na.rm = TRUE)


#look at p.mat = res1[[1]]
#pull all variables I will need for corr, convert to numeric, add to one dataset for corrplot
overall_cor <- all_fellow_data %>%
  ungroup() %>%
  select (dom2_dom3_avg, observationaveragetodate, currentsummativescore, round1, round2, round3, anchortechaveragehighestfor, nonanchortechniqueaverage, mar_dom1a, mar_dom1c, mar_dom1e, mar_dom2b, mar_dom2c, mar_dom2d, mar_dom3b, mar_dom3c, mar_dom3d, mar_dom4a, growth_total, growth_week2, growth_week3, pst_quartile) 
overall_cor <- overall_cor %>% drop_na(dom2_dom3_avg, currentsummativescore, round1, round2, round3, anchortechaveragehighestfor, nonanchortechniqueaverage, mar_dom1a, mar_dom1c, mar_dom1e, mar_dom2b, mar_dom2c, mar_dom2d, mar_dom3b, mar_dom3c, mar_dom3d, mar_dom4a, growth_total, growth_week2, growth_week3)
overall_corplot1 <- corrplot(cor(overall_cor), 
                             p.mat = cor.mtest(overall_cor,0.95)[[1]], 
                             sig.level=0.05, 
                             method="number")
overall_corplot2 <- corrplot(cor(overall_cor), 
                             p.mat = cor.mtest(overall_cor,0.95)[[1]], 
                             insig="p-value", 
                             method="number")

##sig level shows black x over insignificant values
##insig="blank" leaves blank boxes for values that are not sig at .05 level
##insig="p-value" shows p value superimposed over correlation coefficient (if greater than .05)
##i like sig level, bc you can still see the corr coefficient 

domain_cor <- all_fellow_data %>%
  ungroup() %>%
  select (dom2_dom3_avg, currentsummativescore, mar_dom1a, mar_dom1c, mar_dom1e, mar_dom2b, mar_dom2c, mar_dom2d, mar_dom3b, mar_dom3c, mar_dom3d, mar_dom4a)
domain_cor <- domain_cor %>%drop_na(dom2_dom3_avg, currentsummativescore, mar_dom1a, mar_dom1c, mar_dom1e, mar_dom2b, mar_dom2c, mar_dom2d, mar_dom3b, mar_dom3c, mar_dom3d, mar_dom4a) 
domain_corplot1 <- corrplot(cor(domain_cor), 
                            p.mat = cor.mtest(domain_cor,0.95)[[1]], 
                            sig.level=0.05, 
                            method="number")
domain_corplot2 <- corrplot(cor(domain_cor), 
                            p.mat = cor.mtest(domain_cor,0.95)[[1]], 
                            insig="p-value", 
                            method="number")


fyi_pip_cor <- all_fellow_data %>%
  ungroup() %>%
  select (dom2_dom3_avg, currentsummativescore, count_fyi, rrpipsubmitted11, anchortechaveragehighestfor, nonanchortechniqueaverage) 
fyi_pip_cor <- fyi_pip_cor %>%drop_na(dom2_dom3_avg, currentsummativescore, count_fyi, rrpipsubmitted11, anchortechaveragehighestfor, nonanchortechniqueaverage) 
fyi_pip_corplot1 <- corrplot(cor(fyi_pip_cor), 
                             p.mat = cor.mtest(fyi_pip_cor,0.95)[[1]], 
                             sig.level=0.05, 
                             method="number")
fyi_pip_corplot2 <- corrplot(cor(fyi_pip_cor), 
                             p.mat = cor.mtest(fyi_pip_cor,0.95)[[1]], 
                             insig="p-value", 
                             method="number")
fyi_pip_corplot3 <- corrplot(cor(fyi_pip_cor), 
                             p.mat = cor.mtest(fyi_pip_cor,0.95)[[1]], 
                             insig="blank", 
                             method="number")

pst_indicators_cor <- all_fellow_data %>%
  ungroup() %>%
  select (x100percent, what_to_do, strong_voice, positive_framing, anchor_highest_formal_average, engineer_efficiency, strong_start, cold_call, stretch_it, control_the_game, everybody_writes, non_anchor_highest_formal_average, effectiveness, currentsummativescore) 
pst_indicators_cor <- pst_indicators_cor %>%drop_na(x100percent, what_to_do, strong_voice, positive_framing, anchor_highest_formal_average, engineer_efficiency, strong_start, cold_call, stretch_it, control_the_game, everybody_writes, non_anchor_highest_formal_average, effectiveness, currentsummativescore) 
pst_indicators_corplot1 <- corrplot(cor(pst_indicators_cor), 
                                    p.mat = cor.mtest(pst_indicators_cor,0.95)[[1]], 
                                    sig.level=0.05, 
                                    method="number")
pst_indicators_corplot2 <- corrplot(cor(pst_indicators_cor), 
                                    p.mat = cor.mtest(pst_indicators_cor,0.95)[[1]], 
                                    insig="p-value", 
                                    method="number")
pst_indicators_corplot3 <- corrplot(cor(pst_indicators_cor), 
                                    p.mat = cor.mtest(pst_indicators_cor,0.95)[[1]], 
                                    insig="blank", 
                                    method="number")

pst_2016_cor <- all_2016_fellow_data %>%
  ungroup() %>%
  select (anchor_technique_average, non_anchor_technique_average,current_summative_score, oct_rel_effect, pst_quartile) 
pst_2016_cor <- pst_2016_cor %>%drop_na(anchor_technique_average, non_anchor_technique_average,current_summative_score, oct_rel_effect, pst_quartile) 
pst_2016_corplot1 <- corrplot(cor(pst_2016_cor), 
                              p.mat = cor.mtest(pst_2016_cor,0.95)[[1]], 
                              sig.level=0.05, 
                              method="number")


rounds_cor <- all_fellow_data %>%
  ungroup() %>%
  select (anchor_highest_formal_average, non_anchor_highest_formal_average, round1, round2, round3, effectiveness, currentsummativescore) 
rounds_cor <- rounds_cor %>%drop_na(anchor_highest_formal_average, non_anchor_highest_formal_average, round1, round2, round3, effectiveness, currentsummativescore) 
rounds_corplot1 <- corrplot(cor(rounds_cor), 
                            p.mat = cor.mtest(rounds_cor,0.95)[[1]], 
                            sig.level=0.05, 
                            method="number")
rounds_corplot2 <- corrplot(cor(rounds_cor), 
                            p.mat = cor.mtest(rounds_cor,0.95)[[1]], 
                            insig="p-value", 
                            method="number")
rounds_corplot3 <- corrplot(cor(rounds_cor), 
                            p.mat = cor.mtest(rounds_cor,0.95)[[1]], 
                            insig="blank", 
                            method="number")

indicators_domains_cor <- all_fellow_data %>%
  ungroup() %>%
  select (x100percent, what_to_do, strong_voice, positive_framing, engineer_efficiency, strong_start, cold_call, stretch_it, control_the_game, everybody_writes,  mar_dom1a, mar_dom1c, mar_dom1e, mar_dom2b, mar_dom2c, mar_dom2d, mar_dom3b, mar_dom3c, mar_dom3d, mar_dom4a)
indicators_domains_cor <- indicators_domains_cor %>%drop_na(x100percent, what_to_do, strong_voice, positive_framing, engineer_efficiency, strong_start, cold_call, stretch_it, control_the_game, everybody_writes,  mar_dom1a, mar_dom1c, mar_dom1e, mar_dom2b, mar_dom2c, mar_dom2d, mar_dom3b, mar_dom3c, mar_dom3d, mar_dom4a)
indicators_domains_corplot1 <- corrplot(cor(indicators_domains_cor), 
                                        p.mat = cor.mtest(indicators_domains_cor,0.95)[[1]], 
                                        sig.level=0.05, 
                                        method="number")
indicators_domains_corplot3 <- corrplot(cor(indicators_domains_cor), 
                                        p.mat = cor.mtest(indicators_domains_cor,0.95)[[1]], 
                                        insig="blank", 
                                        method="number")

for_cor_non_anchor <- all_fellow_data %>%
  ungroup() %>%
  select (engineer_efficiency, strong_start, cold_call, stretch_it, control_the_game, everybody_writes,  mar_dom1a, mar_dom1c, mar_dom1e, mar_dom2b, mar_dom2c, mar_dom2d, mar_dom3b, mar_dom3c, mar_dom3d, mar_dom4a)
for_cor_non_anchor <- for_cor_non_anchor %>% drop_na(engineer_efficiency, strong_start, cold_call, stretch_it, control_the_game, everybody_writes,  mar_dom1a, mar_dom1c, mar_dom1e, mar_dom2b, mar_dom2c, mar_dom2d, mar_dom3b, mar_dom3c, mar_dom3d, mar_dom4a)

non_anchor_corplot1 <- corrplot(cor(for_cor_non_anchor), 
                                p.mat = cor.mtest(for_cor_non_anchor,0.95)[[1]], 
                                sig.level=0.05, 
                                method="number")

non_anchor_corplot3 <- corrplot(cor(for_cor_non_anchor), 
                                p.mat = cor.mtest(for_cor_non_anchor,0.95)[[1]], 
                                insig="blank", 
                                method="number")

for_cor_anchor <- all_fellow_data %>%
  ungroup() %>%
  select (x100percent, what_to_do, strong_voice, positive_framing, mar_dom1a, mar_dom1c, mar_dom1e, mar_dom2b, mar_dom2c, mar_dom2d, mar_dom3b, mar_dom3c, mar_dom3d, mar_dom4a)
for_cor_anchor <- for_cor_anchor %>% drop_na(x100percent, what_to_do, strong_voice, positive_framing, mar_dom1a, mar_dom1c, mar_dom1e, mar_dom2b, mar_dom2c, mar_dom2d, mar_dom3b, mar_dom3c, mar_dom3d, mar_dom4a)
anchor_corplot1 <- corrplot(cor(for_cor_anchor), 
                            p.mat = cor.mtest(for_cor_anchor,0.95)[[1]], 
                            sig.level=0.05, 
                            method="number")
anchor_corplot3 <- corrplot(cor(for_cor_anchor), 
                            p.mat = cor.mtest(for_cor_anchor,0.95)[[1]], 
                            insig="blank", 
                            method="number")


# for_cor_anchor_2 <- all_fellow_data %>%
#   ungroup() %>%
#   select (x100percent, what_to_do, strong_voice, positive_framing, mar_dom1a, mar_dom1c, mar_dom1e, mar_dom2b, mar_dom2c, mar_dom2d, mar_dom3b, mar_dom3c, mar_dom3d, mar_dom4a)
# for_cor_anchor_2 <- for_cor_anchor_2 %>% drop_na(x100percent, what_to_do, strong_voice, positive_framing, mar_dom1a, mar_dom1c, mar_dom1e, mar_dom2b, mar_dom2c, mar_dom2d, mar_dom3b, mar_dom3c, mar_dom3d, mar_dom4a)
# cortest_anchor_2 <- cor(for_cor_anchor_2)
# corrplot_anchor_2 <- corrplot(cortest_anchor, insig="blank", method="number")
# corrplot_anchor.1_2 <- corrplot(cortest_anchor_2, sig.level =0.05, method="number")
# corrplot_anchor.2_2 <- corrplot(cortest_anchor_2, insig="p-value", method="number")

coach_ratings <- coach_ratings %>%
  select(coachname, level, returner) %>%
  filter(!is.na(coachname))

#change last, first to first last name
coach_ratings$coachname <- sub("(\\w+),\\s(\\w+)","\\2 \\1", coach_ratings$coachname)

coach_ratings$coachname[coach_ratings$coachname=="Lori Lowe-Ann"] <- "Lori-Ann Lowe"
coach_ratings$coachname[coach_ratings$coachname=="Ravit-Lauren Franceskin"] <- "Lauren Ravit-Franceskin"

coach_ratings$level[coach_ratings$level=="Green"] <- 3
coach_ratings$level[coach_ratings$level=="Yellow"] <- 2
coach_ratings$level[coach_ratings$level=="Red"] <- 1

all_fellow_data <- left_join(all_fellow_data, coach_ratings)
all_fellow_data <- all_fellow_data[, !(colnames(all_fellow_data) %in% c("coach_name.x","curret_status.x","enrollee_name.x", "training_academy.x"))]

all_fellow_data$level <- as.numeric(as.character(all_fellow_data$level))


##need to group into PST ratings quartiles (do this after all datasets are joined)
pst_ratings_dist_d75 <- d75 %>%
  group_by(pst_quartile) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count)) %>%
  select(pst_quartile, percent, count)

pst_ratings_2016_dist_d75 <- d75_2016 %>%
  group_by(pst_quartile) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count)) %>%
  select(pst_quartile, percent, count)

#coretest_1 <- rcorr(as.matrix(all_fellow_data$effectiveness, all_fellow_data$currentsummativescore))

pst_ratings_dist_non_d75 <- non_d75 %>%
  group_by(pst_quartile) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count)) %>%
  select(pst_quartile, percent, count) # choose and reorder columns 
#coretest_1 <- rcorr(as.matrix(all_fellow_data$effectiveness, all_fellow_data$currentsummativescore))


#group by TA (need to first group PST scores by quartile)
training_academy_ratings_dist <- all_fellow_data %>%
  group_by(coachname, mean_effectiveness, pst_quartile) %>%
  summarise(count = n()) %>%
  select(coachname, mean_effectiveness, pst_quartile, count) %>%
  slice(1)

pst_quartile_effectiveness <- all_fellow_data %>%
  group_by(coachname, pst_quartile) %>%
  summarise(mean_effectiveness = mean(effectiveness, na.rm = TRUE))

coach_ratings_dist <- all_fellow_data %>%
  group_by(coachname, pst_quartile) %>%
  summarise(count = n()) %>%
  mutate(mean = mean(pst_quartile)) %>%
  select(coachname, pst_quartile, mean, count) %>%
  slice(1) 

coach_effectiveness_dist <- all_fellow_data %>%
  group_by(coachname, dom2_dom3_avg) %>%
  summarise(count = n()) %>%
  mutate(mean_effectiveness = mean(dom2_dom3_avg)) %>%
  select(coachname, mean_effectiveness, count) # choose and reorder columns 

coach_effectiveness_dist <- coach_effectiveness_dist %>%
  select(coachname, mean_effectiveness) %>%
  group_by(coachname) %>%
  arrange(mean_effectiveness) %>%
  slice(1) 

pst_growth <- all_fellow_data %>%
  group_by(growth_total, pst_quartile) %>%
  summarise(count = n()) %>%
  mutate(mean_growth = mean(growth_total)) %>%
  select(growth_total, pst_quartile, mean_growth, count) # choose and reorder columns 

coach_ta_scores <- all_fellow_data %>%
  group_by(coachname, effectiveness, pst_quartile, round1, round2, round3) %>%
  summarise(count = n()) %>%
  mutate(mean_pst = mean(pst_quartile)) %>%
  select(coachname, effectiveness, pst_quartile, round1, round2, round3, mean_pst, count) # choose and reorder columns 

coach_growth <- all_fellow_data %>%
  group_by(coachname, growth_total) %>%
  summarise(count = n()) %>%
  mutate(mean_growth = mean(growth_total)) %>%
  select(coachname, growth_total, mean_growth, count) # choose and reorder columns 

fdi_by_ta <- all_fellow_data %>%
  group_by(dom2_dom3_avg, trainingacademy) %>%
  summarise(count = n()) %>%
  mutate(mean_effectiveness = mean(dom2_dom3_avg)) %>%
  select(trainingacademy, mean_effectiveness, count) # choose and reorder columns 

fdi_by_ta_2 <- fdi_by_ta %>%
  select(trainingacademy, mean_effectiveness) %>%
  group_by(trainingacademy) %>%
  arrange(mean_effectiveness)%>%
  slice(1) 

for_cor_ta <- coach_ta_scores %>%
  ungroup() %>%
  select (effectiveness, pst_quartile, round1, round2, round3)
for_cor_ta <- for_cor_ta %>% drop_na(effectiveness, pst_quartile, round1, round2, round3)
ta_corplot1 <- corrplot(cor(for_cor_ta), 
                        p.mat = cor.mtest(for_cor_ta,0.95)[[1]], 
                        sig.level=0.05, 
                        method="number")
ta_corplot2 <- corrplot(cor(for_cor_ta), 
                        p.mat = cor.mtest(for_cor_ta,0.95)[[1]], 
                        insig="p-value", 
                        method="number")
ta_corplot3 <- corrplot(cor(for_cor_ta), 
                        p.mat = cor.mtest(for_cor_ta,0.95)[[1]], 
                        insig="blank", 
                        method="number")

#compared to June 2016 ratings (current_summative_score)
june_2016_pst_ratings %>%
  summarise(meanpstscore = mean(current_summative_score, na.rm = TRUE))

pst_2016_bottom <-quantile(june_2016_pst_ratings$current_summative_score, .25, na.rm = TRUE)
pst_2016_median <-quantile(june_2016_pst_ratings$current_summative_score, .50, na.rm = TRUE)
pst_2016_top <- quantile(june_2016_pst_ratings$current_summative_score, .75, na.rm = TRUE)

june_2016_pst_ratings <- june_2016_pst_ratings %>%
  mutate(pst_2016_quartile = ifelse(current_summative_score > pst_2016_top, 4,
                                    ifelse(current_summative_score > pst_2016_median, 3,
                                           ifelse(current_summative_score > pst_2016_bottom, 2, 1))))

pst_descriptives_2016 <- june_2016_pst_ratings %>%
  drop_na(pst_2016_quartile) %>%
  group_by(pst_2016_quartile) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count)) %>%
  select(pst_2016_quartile, percent, count)

all_fellow_data %>%
  ungroup() %>%
  summarise(meanpstscore = mean(currentsummativescore, na.rm = TRUE))

pst_descriptives <- all_fellow_data %>%
  drop_na(pst_quartile) %>%
  group_by(pst_quartile) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count)) %>%
  select(pst_quartile, percent, count)


all_fellow_data %>%
  mutate(fyi_range = ifelse(count_fyi >= 5, 3, ifelse(count_fyi >= 1, 2, 1))) %>%
  group_by(effectiveness, fyi_range) %>%
  summarise(count = n()) %>%
  select(effectiveness, fyi_range, count)

mean(all_fellow_data$anchortechaveragehighestfor)
mean(all_fellow_data$nonanchortechniqueaverage)
mean(all_2016_fellow_data$anchor_technique_average)
mean(all_2016_fellow_data$non_anchor_technique_average)

mean(all_fellow_data$round1)
mean(all_fellow_data$round2)
mean(all_fellow_data$round3)


all_2016_fellow_data <- all_2016_fellow_data %>% drop_na(week_2_fe_diagnostic, week_3_fe, week_4_fe, week_5_fe_obs_1, week_5_fe_obs_2) 

all_2016_fellow_data$week_2_fe_diagnostic <- as.numeric(as.character(all_2016_fellow_data$week_2_fe_diagnostic))
all_2016_fellow_data$week_5_fe_obs_2 <- as.numeric(as.character(all_2016_fellow_data$week_5_fe_obs_2))

mean(all_2016_fellow_data$week_2_fe_diagnostic) 
mean(all_2016_fellow_data$week_3_fe) 
mean(all_2016_fellow_data$week_4_fe) 
mean(all_2016_fellow_data$week_5_fe_obs_1) 
mean(all_2016_fellow_data$week_5_fe_obs_2) 


for_cor_principal_survey <- all_fellow_data %>%
  ungroup() %>%
  select (level, dom2_dom3_avg, pst_quartile, principal_effectiveness, currentsummativescore, observationaveragetodate, anchor_highest_formal_average, non_anchor_highest_formal_average)
for_cor_principal_survey <- for_cor_principal_survey %>% drop_na(level, dom2_dom3_avg, pst_quartile, principal_effectiveness)
principal_corplot1 <- corrplot(cor(for_cor_principal_survey), 
                               p.mat = cor.mtest(for_cor_principal_survey,0.95)[[1]], 
                               sig.level=0.05, 
                               method="number")
principal_corplot2 <- corrplot(cor(for_cor_principal_survey), 
                               p.mat = cor.mtest(for_cor_principal_survey,0.95)[[1]], 
                               insig="p-value", 
                               method="number")
principal_corplot3 <- corrplot(cor(for_cor_principal_survey), 
                               p.mat = cor.mtest(for_cor_principal_survey,0.95)[[1]], 
                               insig="blank", 
                               method="number")

principal_pst <- all_fellow_data %>%
  group_by(principal_effectiveness, pst_quartile) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count)) %>%
  select(principal_effectiveness, pst_quartile, percent) # choose and reorder columns 


#look at principal survey planning and prep question

coach_level_pst <- all_fellow_data %>%
  group_by(level, pst_quartile) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count)) %>%
  select(level, pst_quartile, percent)

coach_level_fdi <- all_fellow_data %>%
  group_by(level, effectiveness) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count)) %>%
  select(level, effectiveness, percent)

coach_level_principal <- all_fellow_data %>%
  group_by(level, principal_effectiveness) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count)) %>%
  select(level, principal_effectiveness, percent)

coach_level_d75 <- all_fellow_data %>%
  group_by(level, d75) %>%
  drop_na(level) %>%
  summarise(count = n()) %>%
  
  coach_returning_d75 <- all_fellow_data %>%
  group_by(returner, d75, pst_quartile) %>%
  summarise(count = n()) %>%
  select(returner, d75, pst_quartile, count)

mean(all_fellow_data$effectiveness)
table(all_fellow_data$effectiveness)
table(all_2016_fellow_data$oct_rel_effect)

#now compare vacancy date to performance
nhf <- nhf %>%
  rename(job_id=jobid)

hiring_data <- left_join(nhf, vacancy_list, by = "job_id")
hiring_data <- hiring_data[, (colnames(hiring_data) %in% c("job_id", "eff_date", "name", "file_no", "effective_date", "phone", "email", "nomination_date"))]
hiring_data$name <- sub("(\\w+),\\s(\\w+)","\\2 \\1", hiring_data$name)

hiring_data <- hiring_data %>%
  rename(enrolleename=name)

fellow_hiring_data <- left_join(hiring_data, all_fellow_data, by = "enrolleename")

fellow_hiring_data <- fellow_hiring_data %>%
  filter(!is.na(currentstatus)) 

fellow_hiring_data$eff_date <- as.Date(fellow_hiring_data$eff_date, format = "%m/%d/%Y")
fellow_hiring_data$nomination_date <- as.Date(fellow_hiring_data$nomination_date, format = "%m/%d/%Y")

fellow_hiring_data <- fellow_hiring_data %>%
  mutate(vacancy = ifelse(eff_date > "2016-09-06", "mid year", 
                          ifelse(eff_date <= "2016-09-06", "start of year", eff_date)))

table(fellow_hiring_data$vacancy)

#break out D75 as a group
d75 <-filter(all_fellow_data, d75 ==  "D75") 
non_d75 <- filter(all_fellow_data, is.na(d75))

d75_2016 <-filter(all_2016_fellow_data, oct_subject ==  "D75") 
non_d75_2016 <- filter(all_2016_fellow_data, oct_subject != "D75")

d75_summary<-summary(d75)
non_d75_summary <- summary (non_d75)

d75_item<-filter(all_fellow_data, subject == "Special Education - D75")
non_d75_item<-filter(all_fellow_data, subject != "Special Education - D75")

d75_2016_item<-filter(all_2016_fellow_data, oct_subject == "D75")
non_d75_2016_item<-filter(all_2016_fellow_data, oct_subject != "D75")

effectiveness_dist_d75 <- d75 %>%
  group_by(dom2_dom3_avg) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count)) %>%
  select(dom2_dom3_avg, percent, count) # choose and reorder columns 

effectiveness_dist_2016_d75 <- d75_2016 %>%
  group_by(oct_dom2_dom3_avg) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count)) %>%
  select(oct_dom2_dom3_avg, percent, count) 

effectiveness_dist_non_d75 <- non_d75 %>%
  group_by(dom2_dom3_avg) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count)) %>%
  select(dom2_dom3_avg, percent, count) 

effectiveness_dist_2016_non_d75 <- non_d75_2016 %>%
  group_by(oct_dom2_dom3_avg) %>%
  drop_na(oct_dom2_dom3_avg) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count)) %>%
  select(oct_dom2_dom3_avg, percent, count)  

d75_by_coach <- d75 %>%
  group_by(coachname, pst_quartile) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count)) %>%
  select(coachname, pst_quartile, percent, count) %>%
  slice(1)

subject_area_pst <- all_fellow_data %>%
  group_by(subject, currentsummativescore) %>%
  summarise(count = n()) %>%
  mutate(mean = mean(currentsummativescore)) %>%
  select(subject, currentsummativescore, mean, count) # choose and reorder columns 

subject_area_pst <- subject_area_pst %>%
  group_by(subject) %>%
  arrange(mean) %>%
  slice(1) 

subject_area_2016_pst <- all_2016_fellow_data %>%
  group_by(oct_subject, current_summative_score) %>%
  summarise(count = n()) %>%
  mutate(mean = mean(current_summative_score)) %>%
  select(oct_subject, mean) # choose and reorder columns 

subject_area_2016_pst <- subject_area_2016_pst %>%
  group_by(oct_subject) %>%
  arrange(mean) %>%
  slice(1) 

subject_area_effectiveness <- all_fellow_data %>%
  group_by(subject, dom2_dom3_avg) %>%
  summarise(count = n()) %>%
  mutate(mean = mean(dom2_dom3_avg)) %>%
  select(subject, dom2_dom3_avg, mean, count) 

subject_area_effectiveness <- subject_area_effectiveness %>%
  group_by(subject) %>%
  arrange(mean) %>%
  slice(1) 

effectiveness_sa <- all_fellow_data %>%
  group_by(dom2_dom3_avg, subject) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(subject) %>%
  mutate(percent_of_total = round((count/sum(count)*100), 2),
         cumulative = round(cumsum(percent_of_total))) %>%
  arrange(subject)

selection_and_fdi <- all_fellow_data %>%
  group_by(rrteacherpresence23,rrteachingsampleoverall132,rrcriticalthinkingoverall27, dom2_dom3_avg) %>%
  summarise(count = n()) %>%
  #mutate(mean = mean(dom2_dom3_avg)) %>%
  select(rrteacherpresence23,rrteachingsampleoverall132,rrcriticalthinkingoverall27, dom2_dom3_avg, count) %>%
  filter(!is.na(dom2_dom3_avg))

aggregate(selection_and_fdi[, 4], list(selection_and_fdi$rrteacherpresence23), mean)
aggregate(selection_and_fdi[, 4], list(selection_and_fdi$rrteachingsampleoverall132), mean)
aggregate(selection_and_fdi[, 4], list(selection_and_fdi$rrcriticalthinkingoverall27), mean)

selection_and_fdi_2016 <- all_2016_fellow_data %>%
  group_by(rrteacherpresence23,rrteachingsampleoverall132,rrcriticalthinkingoverall27, oct_dom2_dom3_avg) %>%
  summarise(count = n()) %>%
  select(rrteacherpresence23,rrteachingsampleoverall132,rrcriticalthinkingoverall27, oct_dom2_dom3_avg, count) %>%
  filter(!is.na(oct_dom2_dom3_avg))

aggregate(selection_and_fdi_2016[, 4], list(selection_and_fdi_2016$rrteacherpresence23), mean)
aggregate(selection_and_fdi_2016[, 4], list(selection_and_fdi_2016$rrteachingsampleoverall132), mean)
aggregate(selection_and_fdi_2016[, 4], list(selection_and_fdi_2016$rrcriticalthinkingoverall27), mean)

nrow(selection_and_fdi[selection_and_fdi$rrteacherpresence23 == 0,])
nrow(selection_and_fdi[selection_and_fdi$rrteachingsampleoverall132 == 0,])
nrow(selection_and_fdi[selection_and_fdi$rrcriticalthinkingoverall27 == 0,])

nrow(selection_and_fdi_2016[selection_and_fdi_2016$rrteacherpresence23 == 0,])
nrow(selection_and_fdi_2016[selection_and_fdi_2016$rrteachingsampleoverall132 == 0,])
nrow(selection_and_fdi_2016[selection_and_fdi_2016$rrcriticalthinkingoverall27 == 0,])

mean(selection_and_fdi$dom2_dom3_avg [selection_and_fdi$rrteacherpresence23==1 & selection_and_fdi$rrteachingsampleoverall132==1 & selection_and_fdi$rrcriticalthinkingoverall27==1] , na.rm = TRUE)
mean(selection_and_fdi_2016$oct_dom2_dom3_avg [selection_and_fdi_2016$rrteacherpresence23==1 & selection_and_fdi_2016$rrteachingsampleoverall132==1 & selection_and_fdi_2016$rrcriticalthinkingoverall27==1], na.rm = TRUE)

# selection_and_fdi$rrteacherpresence23 <- as.numeric(as.character(selection_and_fdi$rrteacherpresence23))
# selection_and_fdi$rrteachingsampleoverall132 <- as.numeric(as.character(selection_and_fdi$rrteachingsampleoverall132))
# selection_and_fdi$rrcriticalthinkingoverall27 <- as.numeric(as.character(selection_and_fdi$rrcriticalthinkingoverall27))
# 
# selection_and_fdi_2016$rrteacherpresence23 <- as.numeric(as.character(selection_and_fdi_2016$rrteacherpresence23))
# selection_and_fdi_2016$rrteachingsampleoverall132 <- as.numeric(as.character(selection_and_fdi_2016$rrteachingsampleoverall132))
# selection_and_fdi_2016$rrcriticalthinkingoverall27 <- as.numeric(as.character(selection_and_fdi_2016$rrcriticalthinkingoverall27))
# 
# selection_and_fdi <- selection_and_fdi %>%
#   select (rrteacherpresence23, rrteachingsampleoverall132, rrcriticalthinkingoverall27, dom2_dom3_avg)
# selection_and_fdi <- selection_and_fdi %>% drop_na(rrteacherpresence23, rrteachingsampleoverall132, rrcriticalthinkingoverall27, dom2_dom3_avg)
# selection_fdi_corrplot <- corrplot(cor(selection_and_fdi),
#                                             p.mat = cor.mtest(selection_and_fdi,0.95)[[1]],
#                                             sig.level=0.05,
#                                             method="number", na.rm=FALSE)
# 
# selection_and_fdi_2016 <- selection_and_fdi_2016 %>%
#   select (rrteacherpresence23, rrteachingsampleoverall132, rrcriticalthinkingoverall27, oct_dom2_dom3_avg)
# selection_fdi_2016_corrplot <- corrplot(cor(selection_and_fdi_2016),
#                                    p.mat = cor.mtest(selection_and_fdi_2016,0.95)[[1]],
#                                    sig.level=0.05,
#                                    method="number", na.rm=FALSE)

all_fellow_data %>%
  group_by(tt2id) %>%
  mutate(diff= currentsummativescore - observationaveragetodate) %>%
  ungroup(tt2id) %>%
  summarise(mean(diff))

  