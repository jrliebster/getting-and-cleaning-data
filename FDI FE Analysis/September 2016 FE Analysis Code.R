#load packages
library(pacman)
p_load(readxl, readr, dplyr, janitor, tidyr, stringr, ggplot2, data.table, corrplot)

# Aggregating Steps:
#   1. Load all datasets and packages
# 2. Filter out Cohort 27 observations for "which cohort" column, only Cohort 28 observations left
# 3. Filter out unnecessary columns in all datasets
# a. FDI data--keep: response ID, status, FC ID, cohort, 5 digit ID 28, DBN, grades, CCT/ICT, SC, SETSS, D75, fellow assigned SA, obs date, 1a, 1c, 1e, comments, 2b, 2c, 2d, comments, 3b, 3c, 3d, comments, 4a, comments, effective
# i. Ensure ratings (developing, effective, etc.) are strings
# 1) Need to be numeric for LM
# a) Could have sep vector for numeric rating for descriptive statistics 
# 3) Can also do ANOVA test for significance of relationship between a numeric and string variable 
# 4. Join NHF and Vacancy List
# a. keep only: NHF--jobID, effective date, employee ID, name, email, phone; Vacancy List--effective date, jobID, current location
# 5. Rename all 5 digit column codes to "fellow_code"
# a. Join all datasets by this variable
# 
# Business rules for cleaned data
# . All lowercase
# . mar_dom1a, mar_dom2a, mar_dom3b for domains for FDI
# TT2ID as unique id

# load datasets----------------------------------------------------------------------------
    # those without year are September 2016, those with years are June 2016 cohort

#FDI observation data for City, BC, Pace universities
bc_fdi <- read_csv ("bc_fdi.csv")%>%
  clean_names()

pace_fdi <- read_csv ("pace_fdi.csv")%>%
  clean_names()

city_fdi <- read_csv ("city_fdi.csv")%>%
  clean_names()

# 5 digit Fellow code crosswalk
fellow_code_crosswalk <- read_csv ("fellow_code_crosswalk.csv")%>%
  clean_names()

# principal survey data
principal_survey_data <- read_csv ("principal_survey_data.csv")%>%
  clean_names()

# End of Training observation ratings
end_of_pst_ratings_detail <- read_csv ("end_of_pst_ratings_detail.csv")%>%
  clean_names()

final_pst_ratings <- read_csv("final_pst_ratings.csv")%>%
  clean_names()

# Fellow observation rating tracker (just the sheets "techniques detail" and "observation ratings detail")
technique_ratings <- read_csv ("technique_ratings.csv")%>%
  clean_names()

# NYCDOE Vacancy list 11/30/16
vacancy_list <- read_csv("vacancy_list_113016.csv")%>%
  clean_names()

#  New Hire File from the NYCDOE 4/5/17
nhf <- read_csv("nhf.csv")%>%
  clean_names()

# FYI and PIP reports
fyi_report <- read_csv("fyireport.csv")%>%
    clean_names()

pip_report <- read_csv("pipreport.csv") %>%
    clean_names()

# coach ratings and lead coach survey data
coach_ratings <- read_csv("coachratings.csv") %>%
    clean_names()

lead_coach_survey <- read_csv("lead_coach_survey.csv") %>%
    clean_names()

# selection day ratings data
selection <- read_csv("sept 2016 selection.csv") %>%
    clean_names()

# june 2016 PST ratings
june_2016_pst_ratings <-read_csv("june_2016_pst_final_ratings.csv")%>%
  clean_names()

# june 2016 5 digit Fellow code crosswalk
june_2016_crosswalk <- read_csv("cohort_27_crosswalk.csv") %>%
  clean_names()

# june 2016 FDI
all_fdi_2016 <- read_csv("cohort27_fdi.csv") %>%
  clean_names() 

# june 2016 selection day ratings data
selection_2016 <- read_csv("june 2016 selection.csv") %>%
  clean_names()

# flter for enrolled Fellows and change column names to match for merge, select only columns needed, as there are dozens of unnecessary columns------------------------------------------------------------------
selection <- selection %>%
    filter (rrappstatus7 == "Enrolled") %>%
    select (c(rrappuserid3,rrteachingsampleoverall132,rrcriticalthinkingoverall27,rrteacherpresence23)) %>%
    rename (tt2id=rrappuserid3)

selection_2016 <- selection_2016 %>% 
    filter(rrappstatus7 == "Enrolled") %>%
    select (c(rrappuserid3,rrteachingsampleoverall132,rrcriticalthinkingoverall27,rrteacherpresence23)) %>%
    rename (tt2id=rrappuserid3)

lead_coach_survey <- lead_coach_survey %>%
    select(-c(respondentid,collectorid,startdate,enddate,ip_address,custom_data))

coach_ratings <- coach_ratings %>%
  rename(coachname=name)

# Filter out Cohort 27 observations for "which cohort" column in all FDI datasets--------------------------------------------------------------------------------------------------------
# add to sep dataset for comparison, then keep only Cohort 28 observations in all dataframe, complete cases as well
# checked against original excel files, all have correct number of cases
# before joining all fdi datasets together, need to create a column in each fdi dataset that signifies the university
bc_fdi_2016 <- filter (bc_fdi, which_cohort_is_this_fellow_a_member_of ==  "Cohort 27", 
                       status == "Complete") %>%
    mutate(university="BC") %>%
    select(-c(please_select_the_cohort_28_fellow_id_that_corresponds_to_the_fellow_you_observed))

bc_fdi <- filter(bc_fdi, which_cohort_is_this_fellow_a_member_of ==  "Cohort 28") %>%
    filter(status == "Complete") %>%
    mutate(university="BC")

city_fdi_2016<-filter(city_fdi, which_cohort_is_this_fellow_a_member_of ==  "Cohort 27",
                      status ==  "Complete") %>%
    mutate(university="City") %>%
    select(-c(please_select_the_cohort_28_fellow_id_that_corresponds_to_the_fellow_you_observed))

city_fdi<-filter(city_fdi, which_cohort_is_this_fellow_a_member_of ==  "Cohort 28",
                 status ==  "Complete") %>%
    mutate(university="City")

pace_fdi_2016<-filter(pace_fdi, which_cohort_is_this_fellow_a_member_of ==  "Cohort 27",
                      status ==  "Complete") %>%
    mutate(university="Pace") %>%
    select(-c(please_select_the_cohort_28_fellow_id_that_corresponds_to_the_fellow_you_observed))

pace_fdi<-filter(pace_fdi, which_cohort_is_this_fellow_a_member_of ==  "Cohort 28",
                 status ==  "Complete") %>%
    mutate(university="Pace")

all_fdi_2016 <- filter(all_fdi_2016, uni %in% c("Pace University" , "City College", "Brooklyn College"))

# join all fdi data together into one larger dataset using rbind (as all variables are same)----------------------------------------------------------------------------------------------
# then join with crosswalk of Fellow codes to we have their appid (change variables to have same name first)
all_fdi <- rbind(pace_fdi, city_fdi, bc_fdi)

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
all_2016_fellow_data <- filter(all_2016_fellow_data, `current_status` ==  "Enrolled")
all_2016_fellow_data <- all_2016_fellow_data %>% drop_na(current_summative_score)
all_2016_fellow_data <- filter (all_2016_fellow_data, `current_summative_score` !=  "Not all ratings have been entered")
all_2016_fellow_data <- all_2016_fellow_data %>%
  filter(!is.na(fellow_id))
all_2016_fellow_data <- left_join(all_2016_fellow_data, selection_2016)

#client requested we only keep universities and subject areas we have in MY cohort as to have accurate comparison to June 2016 Fellows
all_2016_fellow_data <- filter(all_2016_fellow_data, oct_subject %in% c(NA , "ESL", "D75", "Special Education"))
all_2016_fellow_data$oct_subject[is.na(all_2016_fellow_data$oct_subject)] <- "D75"

#remove columns that include survey access data (keep school DBN column to confirm it is the same school, but reference School Code, as this column follows the 00X000 format)
#remove comments columns!
all_fdi <- all_fdi[, !(colnames(all_fdi) %in% c("referer","extended_referer","language", "sessionid", "user_agent", "extended_user_agent", "tags", "ip_address", "contact_id", "legacy_comments", "longitude", "latitude", "country", "city", "state_region", "postal", "time_started", "comments", "url_redirect", "please_select_the_cohort_27_fellow_id_that_corresponds_to_the_fellow_you_observed"))]

#change fdi ratings column headings to match Megan's business rules---------------------------------------------------------------------
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

mar_fdi_2016 <- filter(mar_fdi_2016, subject %in% c("ESL", "D75", "Special Education", "Special Education - D75"))

## can also rename columns when selecting them [select(new_name = old_name)]---------------------------------------------------------------
## read_csv(aasdfasdfhjaksdf, col_names =

# #			ii. Ensure ratings (developing, effective, etc.) are strings
# 1) Need to be numeric for lm
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
all_fellow_data <- filter(all_fellow_data, `currentstatus` ==  "Enrolled")
all_fellow_data <- all_fellow_data %>% drop_na(currentsummativescore)
all_fellow_data <- filter (all_fellow_data, `currentsummativescore` !=  "Not all ratings have been entered")
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

#use effectiveness rating compared to other first year teachers as measure of effectiveness on principal survey------------------------------------------------
all_fellow_data <- all_fellow_data %>%
  rename(principal_effectiveness = how_does_this_teaching_fellow_u_0092_s_performance_compare_to_other_first_year_teachers_you_u_0092_ve_worked_with)

#there are duplicates in the FYI report, as some Fellows received multiple FYIs
all_fellow_data <- all_fellow_data %>%
  group_by(tt2id) %>%
  arrange(currentsummativescore) %>%
    distinct(currentsummativescore, .keep_all = TRUE) 

tabyl(all_fdi$mar_dom1a)

#distribution of final summative PST scores
all_fellow_data$currentsummativescore <- as.numeric(as.character(all_fellow_data$currentsummativescore))
ggplot(all_fellow_data, aes(x = currentsummativescore)) +
  geom_histogram(binwidth=.05) +
  labs(title = "Distribution of Final PST Ratings") +
  theme_minimal()

#for linear modeling, recode variables to numeric string--------------------------------------------------------
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

# mutate_each or mutate_at or mutate_which to change them all at once (specify each or starts with/contains)---------------------------------------------
all_fellow_data <- all_fellow_data %>% 
  mutate_at(vars(starts_with("mar")),funs(as.numeric)) %>%   
  mutate_at(vars(starts_with("round")),funs(as.numeric)) %>%
  mutate_at(vars(contains("anchor")),funs(as.numeric)) %>%
  mutate_at(vars(contains("effectiveness")),funs(as.numeric))
all_fellow_data$rrpipsubmitted11 <- as.numeric(as.character(all_fellow_data$rrpipsubmitted11))
all_fellow_data$count_fyi <- as.numeric(as.character(all_fellow_data$count_fyi))
all_fellow_data$x100percent <- as.numeric(as.character(all_fellow_data$x100percent))
all_fellow_data$what_to_do <- as.numeric(as.character(all_fellow_data$what_to_do))
all_fellow_data$strong_voice <- as.numeric(as.character(all_fellow_data$strong_voice))
all_fellow_data$positive_framing <- as.numeric(as.character(all_fellow_data$positive_framing))
all_fellow_data$engineer_efficiency <- as.numeric(as.character(all_fellow_data$engineer_efficiency))
all_fellow_data$strong_start <- as.numeric(as.character(all_fellow_data$strong_start))
all_fellow_data$cold_call <- as.numeric(as.character(all_fellow_data$cold_call))
all_fellow_data$stretch_it <- as.numeric(as.character(all_fellow_data$stretch_it))
all_fellow_data$control_the_game<- as.numeric(as.character(all_fellow_data$control_the_game))
all_fellow_data$everybody_writes <- as.numeric(as.character(all_fellow_data$everybody_writes))
all_fellow_data$observationaveragetodate <- as.numeric(all_fellow_data$observationaveragetodate)

all_fellow_data <- all_fellow_data %>%
  mutate(growth_total = round((currentsummativescore - round1), digits = 1), 
        growth_week3 = round((round3 - round2), digits = 1), 
        growth_week2 = round((round2 - round1), digits = 1))

all_2016_fellow_data$oct_rel_effect <- as.numeric(as.character(all_2016_fellow_data$oct_rel_effect))

#create PST quartile variable--------------------------------------------------------------------------------------------
#create variables that calculate the mean of sub-domains, then mean of all domains, and just domains 2 and 3
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
    group_by(dom2_dom3_avg) %>%
    summarise(mean_pst_rating = mean(currentsummativescore, na.rm = TRUE))

pst_quartile_effectiveness_anova <- aov(all_fellow_data$dom2_dom3_avg ~ all_fellow_data$pst_quartile)
summary(pst_quartile_effectiveness_anova)
confint(pst_quartile_effectiveness_anova) 

#now create pst score quartiles for june 2016 so I can compare performance between june 2016 and sept 2016 cohorts---------------------------------
pst_bottom_2016 <- quantile(all_2016_fellow_data$current_summative_score, .25, na.rm = TRUE)
pst_median_2016 <- quantile(all_2016_fellow_data$current_summative_score, .50, na.rm = TRUE)
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
pst_descriptives_2016 <- tabyl(all_2016_fellow_data, pst_quartile, show_na = FALSE)

#correlation between relative effectiveness and average of all components in domain 1/2/3/4 (then just 2 and 3)
#first need to calculate mean FDI domain and overall scores, then quartiles for scores
all_2016_fellow_data <- all_2016_fellow_data %>% 
    mutate_at(vars(starts_with("oct")),funs(as.numeric))

mar_fdi_2016 <- mar_fdi_2016 %>% 
    mutate_at(vars(starts_with("mar")),funs(as.numeric))

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

##dom2_dom3_avg quartiles--how the client defines FDI rating/effectiveness
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

#I need to pull all numeric variables into a sep dataframe that I want to run correlations on-------------------------------------------------
# Then, I run the cor, save it as an object, and use corrplot on that
# sig level shows black x over insignificant values
# insig="blank" leaves blank boxes for values that are not sig at .05 level
# insig="p-value" shows p value superimposed over correlation coefficient (if greater than .05)
# i like sig level, bc you can still see the corr coefficient 

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


all_2016_effectiveness <- all_2016_fellow_data %>%
    ungroup() %>%
    select (oct_rel_effect, oct_dom_quartile, oct_dom_avg, oct_dom2_dom3_avg, oct_dom23_quartile, pst_quartile) 
all_2016_effectiveness <- all_2016_effectiveness %>% drop_na(oct_rel_effect, oct_dom_quartile, oct_dom_avg, oct_dom2_dom3_avg, oct_dom23_quartile, pst_quartile)
all_2016_effectiveness_corplot1 <- corrplot(cor(all_2016_effectiveness),
                                            p.mat = cor.mtest(all_2016_effectiveness,0.95)[[1]],
                                            sig.level=0.05,
                                            method="number", na.rm=FALSE)

#plot relationships between FDI, PST, and principal effectiveness ratings-------------------------------------------------------
mod1 = lm(all_fellow_data$observationaveragetodate ~ all_fellow_data$dom2_dom3_avg) 
summary(mod1)
ggplot(all_fellow_data, aes(x = observationaveragetodate, y = dom2_dom3_avg)) +
    geom_point() +
    geom_jitter(alpha = .3) +
    geom_smooth(method = "lm", formula = y~x) +
    labs(x = "PST Observation Score", y = "FDI Effectiveness") +
    theme_light()

#final PST summative scores as compared to FDI effectiveness ratings
mod2 = lm(all_fellow_data$currentsummativescore ~ all_fellow_data$dom2_dom3_avg)
summary(mod2)
ggplot(all_fellow_data, aes(x = dom2_dom3_avg, y = currentsummativescore)) +
    geom_point() +
    geom_jitter(alpha = .3) +
    geom_smooth(method = "lm", formula = y~x) +
    labs(x = "FDI Effectiveness", y = "Final PST Score") 

mod3 = lm(all_fellow_data$principal_effectiveness ~ all_fellow_data$dom2_dom3_avg)
summary(mod3)
ggplot(all_fellow_data, aes(x = dom2_dom3_avg, y = principal_effectiveness)) +
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


#look at mean FDI scores-----------------------------------------------------------------------
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

t.test(all_fellow_data$dom2_dom3_avg, all_2016_fellow_data$oct_dom2_dom3_avg)

#no significant relationship between pst scores and FDI
#positive relationship between principal survey ratigns and PST scores

#internal client wants to see data cut a bunch of different ways, so need to save tables of descriptitives containing a variety of variables----------------------------------------
#comparisons/relationships between
#EOT observation ratings as well as summative PST score
#FDI ratings
#principal survey ratings
#subject areas
#TAs and coaches
#across cohorts (june 2016 and sept 2016)
all_dom_quartile_dist <- tabyl(all_fellow_data, all_dom_quartile)

all_fellow_data %>% 
  group_by(dom23_quartile, pst_quartile) %>%
  drop_na(dom23_quartile) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count)) %>%
  select(dom23_quartile, pst_quartile, percent, count)

dom23_quartile_dist <- tabyl(all_fellow_data, dom23_quartile)

dom23_dist <- tabyl(all_fellow_data, dom2_dom3_avg)

dom23_2016_dist <- tabyl(all_2016_fellow_data, oct_dom2_dom3_avg)

mar_dom23_2016_dist <- tabyl(mar_fdi_2016, mar_dom2_dom3_avg)

principal_ratings <- tabyl(all_fellow_data, principal_effectiveness, show_na = FALSE)

dom1_dist <- tabyl(all_fellow_data, mar_dom1)

dom2_dist <- tabyl(all_fellow_data, mar_dom2)

dom3_dist <- tabyl(all_fellow_data, mar_dom3)

dom4_dist <- tabyl(all_fellow_data, mar_dom4a)

#correlation between average score and fdi/pst/principal, descriptives, bring to team Friday and see what questions they have------------------------------------------------ 
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

#look at p.mat = res1[[1]] if sig levels seem off
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

domain_cor <- all_fellow_data %>%
  ungroup() %>%
  select (dom2_dom3_avg, currentsummativescore, mar_dom1a, mar_dom1c, mar_dom1e, mar_dom2b, mar_dom2c, mar_dom2d, mar_dom3b, mar_dom3c, mar_dom3d, mar_dom4a)
domain_cor <- domain_cor %>%drop_na(dom2_dom3_avg, currentsummativescore, mar_dom1a, mar_dom1c, mar_dom1e, mar_dom2b, mar_dom2c, mar_dom2d, mar_dom3b, mar_dom3c, mar_dom3d, mar_dom4a) 
domain_corplot1 <- corrplot(cor(domain_cor), 
                            p.mat = cor.mtest(domain_cor,0.95)[[1]], 
                            sig.level=0.05, 
                            method="number")

fyi_pip_cor <- all_fellow_data %>%
  ungroup() %>%
  select (dom2_dom3_avg, currentsummativescore, count_fyi, rrpipsubmitted11, anchortechaveragehighestfor, nonanchortechniqueaverage) 
fyi_pip_cor <- fyi_pip_cor %>%drop_na(dom2_dom3_avg, currentsummativescore, count_fyi, rrpipsubmitted11, anchortechaveragehighestfor, nonanchortechniqueaverage) 
fyi_pip_corplot1 <- corrplot(cor(fyi_pip_cor), 
                             p.mat = cor.mtest(fyi_pip_cor,0.95)[[1]], 
                             sig.level=0.05, 
                             method="number")

pst_indicators_cor <- all_fellow_data %>%
  ungroup() %>%
  select (x100percent, what_to_do, strong_voice, positive_framing, anchor_highest_formal_average, engineer_efficiency, strong_start, cold_call, stretch_it, control_the_game, everybody_writes, non_anchor_highest_formal_average, effectiveness, currentsummativescore) 
pst_indicators_cor <- pst_indicators_cor %>%drop_na(x100percent, what_to_do, strong_voice, positive_framing, anchor_highest_formal_average, engineer_efficiency, strong_start, cold_call, stretch_it, control_the_game, everybody_writes, non_anchor_highest_formal_average, effectiveness, currentsummativescore) 
pst_indicators_corplot1 <- corrplot(cor(pst_indicators_cor), 
                                    p.mat = cor.mtest(pst_indicators_cor,0.95)[[1]], 
                                    sig.level=0.05, 
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

indicators_domains_cor <- all_fellow_data %>%
  ungroup() %>%
  select (x100percent, what_to_do, strong_voice, positive_framing, engineer_efficiency, strong_start, cold_call, stretch_it, control_the_game, everybody_writes,  mar_dom1a, mar_dom1c, mar_dom1e, mar_dom2b, mar_dom2c, mar_dom2d, mar_dom3b, mar_dom3c, mar_dom3d, mar_dom4a)
indicators_domains_cor <- indicators_domains_cor %>%drop_na(x100percent, what_to_do, strong_voice, positive_framing, engineer_efficiency, strong_start, cold_call, stretch_it, control_the_game, everybody_writes,  mar_dom1a, mar_dom1c, mar_dom1e, mar_dom2b, mar_dom2c, mar_dom2d, mar_dom3b, mar_dom3c, mar_dom3d, mar_dom4a)
indicators_domains_corplot1 <- corrplot(cor(indicators_domains_cor), 
                                        p.mat = cor.mtest(indicators_domains_cor,0.95)[[1]], 
                                        sig.level=0.05, 
                                        method="number")

for_cor_non_anchor <- all_fellow_data %>%
  ungroup() %>%
  select (engineer_efficiency, strong_start, cold_call, stretch_it, control_the_game, everybody_writes,  mar_dom1a, mar_dom1c, mar_dom1e, mar_dom2b, mar_dom2c, mar_dom2d, mar_dom3b, mar_dom3c, mar_dom3d, mar_dom4a)
for_cor_non_anchor <- for_cor_non_anchor %>% drop_na(engineer_efficiency, strong_start, cold_call, stretch_it, control_the_game, everybody_writes,  mar_dom1a, mar_dom1c, mar_dom1e, mar_dom2b, mar_dom2c, mar_dom2d, mar_dom3b, mar_dom3c, mar_dom3d, mar_dom4a)

non_anchor_corplot1 <- corrplot(cor(for_cor_non_anchor), 
                                p.mat = cor.mtest(for_cor_non_anchor,0.95)[[1]], 
                                sig.level=0.05, 
                                method="number")

for_cor_anchor <- all_fellow_data %>%
  ungroup() %>%
  select (x100percent, what_to_do, strong_voice, positive_framing, mar_dom1a, mar_dom1c, mar_dom1e, mar_dom2b, mar_dom2c, mar_dom2d, mar_dom3b, mar_dom3c, mar_dom3d, mar_dom4a)
for_cor_anchor <- for_cor_anchor %>% drop_na(x100percent, what_to_do, strong_voice, positive_framing, mar_dom1a, mar_dom1c, mar_dom1e, mar_dom2b, mar_dom2c, mar_dom2d, mar_dom3b, mar_dom3c, mar_dom3d, mar_dom4a)
anchor_corplot1 <- corrplot(cor(for_cor_anchor), 
                            p.mat = cor.mtest(for_cor_anchor,0.95)[[1]], 
                            sig.level=0.05, 
                            method="number")

# look at relationship between coach ratings and Fellow performance--------------------------------------------
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

# add in specific research questions

# what were Fellow PST, principal, and FDI ratings by coach?
coach_ratings_dist <- all_fellow_data %>%
  group_by(coachname) %>%
  summarise(count = n(),
            mean_pst = mean(pst_quartile, na.rm = TRUE))

coach_effectiveness_dist <- all_fellow_data %>%
  group_by(coachname) %>% 
  summarise(count = n(),
            mean_avg = mean(dom2_dom3_avg, na.rm = TRUE))

all_fellow_data %>%
  group_by(growth_total, pst_quartile) %>%
  summarise(count = n(),
            mean_growth = mean(growth_total, na.rm = TRUE))

coach_ta_scores <- all_fellow_data %>%
  group_by(coachname, effectiveness, pst_quartile, round1, round2, round3) %>%
  summarise(count = n(),
            mean_pst = mean(pst_quartile))

# fellow growth by coach
all_fellow_data %>%
  group_by(coachname) %>%
  summarise(count = n(),
            mean_growth = mean(growth_total)) 

#look at performance by training academy--------------------------------------------------------------
for_cor_ta <- coach_ta_scores %>%
  ungroup() %>%
  select (effectiveness, pst_quartile, round1, round2, round3)
for_cor_ta <- for_cor_ta %>% drop_na(effectiveness, pst_quartile, round1, round2, round3)
ta_corplot1 <- corrplot(cor(for_cor_ta), 
                        p.mat = cor.mtest(for_cor_ta,0.95)[[1]], 
                        sig.level=0.05, 
                        method="number")
# mean pst scores, broken out by anchor and non-anchor, as well as by round------------------------------------
all_fellow_data %>%
  ungroup() %>%
  summarise(meanpstscore = mean(currentsummativescore, na.rm = TRUE))

pst_descriptives <- tabyl(all_fellow_data, pst_quartile)

all_fellow_data %>%
  mutate(fyi_range = ifelse(count_fyi >= 5, 3, ifelse(count_fyi >= 1, 2, 1))) %>%
  group_by(effectiveness, fyi_range) %>%
  summarise(count = n()) 

mean(all_fellow_data$anchortechaveragehighestfor)
mean(all_fellow_data$nonanchortechniqueaverage)
mean(all_2016_fellow_data$anchor_technique_average)
mean(all_2016_fellow_data$non_anchor_technique_average)

mean(all_fellow_data$round1)
mean(all_fellow_data$round2)
mean(all_fellow_data$round3)

all_2016_fellow_data <- all_2016_fellow_data %>% drop_na(week_2_fe_diagnostic, week_3_fe, week_4_fe, week_5_fe_obs_1, week_5_fe_obs_2) 

all_2016_fellow_data <- all_2016_fellow_data %>% 
  mutate_at(vars(contains("week")),funs(as.numeric))

mean(all_2016_fellow_data$week_2_fe_diagnostic) 
mean(all_2016_fellow_data$week_3_fe) 
mean(all_2016_fellow_data$week_4_fe) 
mean(all_2016_fellow_data$week_5_fe_obs_1) 
mean(all_2016_fellow_data$week_5_fe_obs_2) 

# relationship between principal survey data and pst scores, as well as FDI---------------------------
for_cor_principal_survey <- all_fellow_data %>%
  ungroup() %>%
  select (level, dom2_dom3_avg, pst_quartile, principal_effectiveness, currentsummativescore, observationaveragetodate, anchor_highest_formal_average, non_anchor_highest_formal_average)
for_cor_principal_survey <- for_cor_principal_survey %>% drop_na(level, dom2_dom3_avg, pst_quartile, principal_effectiveness)
principal_corplot1 <- corrplot(cor(for_cor_principal_survey), 
                               p.mat = cor.mtest(for_cor_principal_survey,0.95)[[1]], 
                               sig.level=0.05, 
                               method="number")

principal_pst <- all_fellow_data %>%
  group_by(principal_effectiveness, pst_quartile) %>%
  summarise(count = n())

coach_level_pst <- all_fellow_data %>%
    crosstab(level, pst_quartile, "row")

# crosstab (y axis, x axis, row/col percent (can leave third formula blank))
coach_level_pst <- all_fellow_data %>%
    crosstab(level, pst_quartile, "row")

coach_level_fdi <- all_fellow_data %>%
    crosstab(level, dom2_dom3_avg)


coach_level_principal <- all_fellow_data %>%
    crosstab(level, principal_effectiveness, "row")

coach_returning_d75 <- all_fellow_data %>%
  group_by(returner, d75, pst_quartile) %>%
  summarise(count = n()) %>%
  select(returner, d75, pst_quartile, count)

mean(all_fellow_data$effectiveness)

#create vector with just vacancy effective and create dates---------------------------------------------------
vacancy_effective <- vacancy_list %>%
    select(eff_date, vacancy_create_date)

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
#not a large enough sample size between MY and persistent vacancies to make conclusions about vacancy date and performance

#break out D75 as a group, look at subject area performance-----------------------------------------------------------------------------------------------------------
d75 <-filter(all_fellow_data, d75 ==  "D75") 
non_d75 <- filter(all_fellow_data, is.na(d75))

d75_2016 <-filter(all_2016_fellow_data, oct_subject ==  "D75") 
non_d75_2016 <- filter(all_2016_fellow_data, oct_subject != "D75")

d75_item<-filter(all_fellow_data, subject == "Special Education - D75")
non_d75_item<-filter(all_fellow_data, subject != "Special Education - D75")

d75_2016_item<-filter(all_2016_fellow_data, oct_subject == "D75")
non_d75_2016_item<-filter(all_2016_fellow_data, oct_subject != "D75")

effectiveness_dist_d75 <- d75 %>%
  group_by(dom2_dom3_avg) %>%
  summarise(count = n(),
            percent = count / sum(count)) 

effectiveness_dist_2016_d75 <- d75_2016 %>%
  group_by(oct_dom2_dom3_avg) %>%
  summarise(count = n(),
            percent = count / sum(count)) 

effectiveness_dist_non_d75 <- non_d75 %>%
  group_by(dom2_dom3_avg) %>%
  summarise(count = n(),
            percent = count / sum(count))

effectiveness_dist_2016_non_d75 <- non_d75_2016 %>%
  group_by(oct_dom2_dom3_avg) %>%
  drop_na(oct_dom2_dom3_avg) %>%
  summarise(count = n(),
            percent = count / sum(count))  

d75_by_coach <- d75 %>%
  group_by(coachname, pst_quartile) %>%
  summarise(count = n(),
            percent = count / sum(count))

subject_area_pst <- all_fellow_data %>%
    group_by(subject) %>%
    summarise(mean = mean(currentsummativescore)) %>%
    arrange(mean)

subject_area_effectiveness <- all_fellow_data %>%
  group_by(subject, dom2_dom3_avg) %>%
  summarise(count = n(),
            mean = mean(dom2_dom3_avg)) %>%
  arrange(mean) 

pst_ratings_dist_d75 <- tabyl(d75, pst_quartile) 

pst_ratings_2016_dist_d75 <- tabyl(d75_2016, pst_quartile)

pst_ratings_dist_non_d75 <- tabyl(non_d75, pst_quartile)

#look at relationship between selection performance and FDI ratings----------------------------------------
selection_and_fdi <- all_fellow_data %>%
  group_by(rrteacherpresence23,rrteachingsampleoverall132,rrcriticalthinkingoverall27, dom2_dom3_avg) %>%
  summarise(count = n()) %>%
  filter(!is.na(dom2_dom3_avg))

aggregate(selection_and_fdi[, 4], list(selection_and_fdi$rrteacherpresence23), mean)
aggregate(selection_and_fdi[, 4], list(selection_and_fdi$rrteachingsampleoverall132), mean)
aggregate(selection_and_fdi[, 4], list(selection_and_fdi$rrcriticalthinkingoverall27), mean)

selection_and_fdi_2016 <- all_2016_fellow_data %>%
  group_by(rrteacherpresence23,rrteachingsampleoverall132,rrcriticalthinkingoverall27, oct_dom2_dom3_avg) %>%
  summarise(count = n()) %>%
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

all_fellow_data %>%
  group_by(tt2id) %>%
  mutate(diff= currentsummativescore - observationaveragetodate) %>%
  ungroup(tt2id) %>%
  summarise(mean(diff))

  