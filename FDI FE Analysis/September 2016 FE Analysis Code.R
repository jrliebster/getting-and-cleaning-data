#load packages
library(pacman)
p_load(readxl, readr, dplyr, janitor, tidyr, stringr, ggplot2, data.table, knitr, scales, psych, Hmisc, corrplot)


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

# Filter out Cohort 27 observations for "which cohort" column in all FDI datasets, add to sep dataset for comparison, then keep only Cohort 28 observations in all dataframe, complete cases as well
#checked against original excel files, all have correct number of cases
#before joining all fdi datasets together, need to create a column in each fdi dataset that signifies the university
bc_fdi_2016<-filter(bc_fdi, which_cohort_is_this_fellow_a_member_of ==  "Cohort 27") 
bc_fdi_2016<-filter(bc_fdi_2016, status ==  "Complete") %>%
  mutate(university="BC")

city_fdi_2016<-filter(city_fdi, which_cohort_is_this_fellow_a_member_of ==  "Cohort 27") 
city_fdi_2016<-filter(city_fdi_2016, status ==  "Complete") %>%
  mutate(university="City")

pace_fdi_2016<-filter(pace_fdi, which_cohort_is_this_fellow_a_member_of ==  "Cohort 27") 
pace_fdi_2016<-filter(pace_fdi_2016, status ==  "Complete") %>%
  mutate(university="Pace")

all_fdi_2016 <- rbind(pace_fdi_2016, city_fdi_2016)
all_fdi_2016 <- rbind(all_fdi_2016, bc_fdi_2016)

bc_fdi<-filter(bc_fdi, which_cohort_is_this_fellow_a_member_of ==  "Cohort 28") 
bc_fdi<-filter(bc_fdi, status ==  "Complete") %>%
  mutate(university="BC")

city_fdi<-filter(city_fdi, which_cohort_is_this_fellow_a_member_of ==  "Cohort 28") 
city_fdi<-filter(city_fdi, status ==  "Complete") %>%
  mutate(university="City")

pace_fdi<-filter(pace_fdi, which_cohort_is_this_fellow_a_member_of ==  "Cohort 28") 
pace_fdi<-filter(pace_fdi, status ==  "Complete") %>%
  mutate(university="Pace")

#join all fdi data together into one larger dataset using rbind (as all variables are same)
#then join with crosswalk of Fellow codes to we have their appid (change variables to have same name first)
all_fdi <- rbind(pace_fdi, city_fdi)
all_fdi <- rbind(all_fdi, bc_fdi)

names(all_fdi)[names(all_fdi) == "please_select_the_cohort_28_fellow_id_that_corresponds_to_the_fellow_you_observed"] <- "fellow_id"
names(fellow_code_crosswalk)[names(fellow_code_crosswalk) == "six_digit_id"] <- "fellow_id"
all_fdi <- left_join (all_fdi, fellow_code_crosswalk, by = "fellow_id")
names(all_fdi)[names(all_fdi) == "app_user_id"] <- "tt2id"
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



# #************Field consultant ID and subject areacolumns not changing--why? changed to clean version and it worked, so leaving this here as a reminder
## can also rename columns when selecting them [select(new_name = old_name)]
## read_csv(aasdfasdfhjaksdf, col_names =

# names(all_fdi)[names(all_fdi) == "1a: :Please record your observation rating for component 1a: Demonstrating Knowledge of Content and Pedagogy."] <- "mar_dom1a"
# names(all_fdi)[names(all_fdi) == "1c::Please record your observation rating for component 1c: Setting Instructional Outcomes."] <- "mar_dom1c"
# names(all_fdi)[names(all_fdi) == "1e::Please record your observation rating for component 1e: Designing Coherent Instruction."] <- "mar_dom1e"
# names(all_fdi)[names(all_fdi) == "2b: :Please record your observation rating for component 2b: Establishing a Culture for Learning."] <- "mar_dom2b"
# names(all_fdi)[names(all_fdi) == "2c::Please record your observation rating for component 2c: Managing Classroom Procedures."] <- "mar_dom2c" 
# names(all_fdi)[names(all_fdi) == "2d::Please record your observation rating for component 2d: Managing Student Behavior."] <- "mar_dom2d"
# names(all_fdi)[names(all_fdi) == "3b::Please record your observation rating for component 3b: Using Questioning and Discussion Techniques."] <- "mar_dom3b"
# names(all_fdi)[names(all_fdi) == "3c::Please record your observation rating for component 3c: Engaging Students in Learning."] <- "mar_dom3c"
# names(all_fdi)[names(all_fdi) == "3d::Please record your observation rating for component 3d: Using Assessment in Instruction."] <- "mar_dom3d"
# names(all_fdi)[names(all_fdi) == "4a::Please record your observation rating for component 4a: Reflecting on Teaching."] <- "mar_dom4a"
# names(all_fdi)[names(all_fdi) == "How effective do you believe this Fellow is compared to other teachers with similar experience (eg- first year Fellows compared to other first year teachers)?"] <- "effectiveness_compared_to_other_teachers"
# names(all_fdi)[names(all_fdi) == "Comments for Domain 1"] <- "dom1_comments"
# names(all_fdi)[names(all_fdi) == "Comments for Domain 2"] <- "dom2_comments"
# names(all_fdi)[names(all_fdi) == "Comments for Domain 3"] <- "dom3_comments"
# names(all_fdi)[names(all_fdi) == "Comments for Domain 4"] <- "dom4_comments"
# names(all_fdi)[names(all_fdi) == "What is the Fellow's assigned university subject area? (Please select the subject they are training in at their university, NOT the subject they are teaching during this observation.)"] <- "fellow_subject_area" 
# names(all_fdi)[names(all_fdi) == "What was the date of this observation?"] <- "observation_date"
# names(all_fdi)[names(all_fdi) == "Please select your Field Consultant ID."] <- "field_consultant_id"
# names(all_fdi)[names(all_fdi) == "Which cohort is this Fellow a member of?"] <- "cohort"
# names(all_fdi)[names(all_fdi) == "Please enter the school DBN as a letter and 3-digit number (e.g. X232 or K021)"] <- "DBN" 
# names(all_fdi)[names(all_fdi) == "CTT/ICT:If applicable, please identify the Special Education setting of the observed lesson. Select all applicable settings."] <- "cct_ict" 
# names(all_fdi)[names(all_fdi) == "Self-contained:If applicable, please identify the Special Education setting of the observed lesson. Select all applicable settings."] <- "self_contained" 
# names(all_fdi)[names(all_fdi) == "SETTS:If applicable, please identify the Special Education setting of the observed lesson. Select all applicable settings."] <- "setts"
# names(all_fdi)[names(all_fdi) == "D75:If applicable, please identify the Special Education setting of the observed lesson. Select all applicable settings."] <- "d75"
# names(all_fdi)[names(all_fdi) == "N/A:If applicable, please identify the Special Education setting of the observed lesson. Select all applicable settings."] <- "other_sped_setting"

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
all_fellow_data <-filter(all_fellow_data, `currentstatus` ==  "Enrolled")
all_fellow_data <- all_fellow_data %>% drop_na(currentsummativescore)
all_fellow_data <-filter (all_fellow_data, `currentsummativescore` !=  "Not all ratings have been entered")
all_fellow_data <- all_fellow_data %>% drop_na(effectiveness)

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
all_fellow_data$d75[is.na(all_fellow_data$d75)] <- "NA"

#there are duplicates in the FYI report, as some Fellows received multiple FYIs
all_fellow_data <- all_fellow_data %>%
  group_by(tt2id) %>%
  arrange(currentsummativescore) %>%
  slice(1) 

# ratings_dist_dom1a <- all_fdi %>%
#   group_by(mar_dom1a) %>%
#   summarize(count = n()) %>%
#   mutate(percent = count / sum(count)*100) %>%
#   select(mar_dom1a, percent, count) # choose and reorder columns 
# 
# ratings_dist_dom1c <- all_fdi %>%
#   group_by(mar_dom1c) %>%
#   summarize(count = n()) %>%
#   mutate(percent = count / sum(count)*100) %>%
#   select(mar_dom1c, percent, count) # choose and reorder columns 
# 
# ratings_dist_dom1e <- all_fdi %>%
#   group_by(mar_dom1e) %>%
#   summarize(count = n()) %>%
#   mutate(percent = count / sum(count)*100) %>%
#   select(mar_dom1e, percent, count) # choose and reorder columns 
# 
# ratings_dist_dom2b <- all_fdi %>%
#   group_by(mar_dom2b) %>%
#   summarize(count = n()) %>%
#   mutate(percent = count / sum(count)*100) %>%
#   select(mar_dom2b, percent, count) # choose and reorder columns
# 
# ratings_dist_dom2c <- all_fdi %>%
#   group_by(mar_dom2c) %>%
#   summarize(count = n()) %>%
#   mutate(percent = count / sum(count)*100) %>%
#   select(mar_dom2c, percent, count) # choose and reorder columns
# 
# ratings_dist_dom2d <- all_fdi %>%
#   group_by(mar_dom2d) %>%
#   summarize(count = n()) %>%
#   mutate(percent = count / sum(count)*100) %>%
#   select(mar_dom2d, percent, count) # choose and reorder columns
# 
# ratings_dist_dom3b <- all_fdi %>%
#   group_by(mar_dom3b) %>%
#   summarize(count = n()) %>%
#   mutate(percent = count / sum(count)*100) %>%
#   select(mar_dom3b, percent, count) # choose and reorder columns
# 
# ratings_dist_dom3c <- all_fdi %>%
#   group_by(mar_dom3c) %>%
#   summarize(count = n()) %>%
#   mutate(percent = count / sum(count)*100) %>%
#   select(mar_dom3c, percent, count) # choose and reorder columns
# 
# ratings_dist_dom3d <- all_fdi %>%
#   group_by(mar_dom3d) %>%
#   summarize(count = n()) %>%
#   mutate(percent = count / sum(count)*100) %>%
#   select(mar_dom3d, percent, count) # choose and reorder columns
# 
# ratings_dist_dom4a <- all_fdi %>%
#   group_by(mar_dom4a) %>%
#   summarize(count = n()) %>%
#   mutate(percent = count / sum(count)*100) %>%
#   select(mar_dom4a, percent, count) # choose and reorder columns
# 
ratings_dist_effectiveness <- all_fellow_data %>%
  group_by(effectiveness) %>%
  summarize(count = n()) %>%
  mutate(percent = count / sum(count)*100) %>%
  select(effectiveness, percent, count) # choose and reorder columns


ratings_dist_effectiveness_2016 <- all_fdi_2016 %>%
  group_by(how_effective_do_you_believe_this_fellow_is_compared_to_other_teachers_with_similar_experience_eg_first_year_fellows_compared_to_other_first_year_teachers) %>%
  summarize(count = n()) %>%
  mutate(percent = count / sum(count)*100) %>%
  select(how_effective_do_you_believe_this_fellow_is_compared_to_other_teachers_with_similar_experience_eg_first_year_fellows_compared_to_other_first_year_teachers, percent, count) # choose and reorder columns

#distribution of final summative PST scores
all_fellow_data$currentsummativescore <- as.numeric(as.character(all_fellow_data$currentsummativescore))
ggplot(all_fellow_data, aes(x = currentsummativescore)) +
  geom_histogram(binwidth=.05) +
  labs(title = "Distribution of Final PST Ratings") +
  theme_minimal()
#either transform data to fall into ranges of ratings, cutoff ranges (e.g. 2.26-2.3 is 2.3)

all_fellow_data$currentsummativescore <- as.numeric(as.character(all_fellow_data$currentsummativescore))

#for linear modeling, change back to string for descriptives or run descriptives first
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

## RR Analysis 
all_fellow_data <- all_fellow_data %>%
  mutate(currentsummativescore = as.numeric(currentsummativescore))

bot_q <- quantile(all_fellow_data$currentsummativescore, .25)
top_q <- quantile(all_fellow_data$currentsummativescore, .75)

all_fellow_data <- all_fellow_data %>%
  mutate(pst_quartile = ifelse(currentsummativescore > top_q, "Top-Q",
                               ifelse(currentsummativescore < bot_q, "Bottom-Q", "Middle-Qs")),
         pst_quartile = factor(pst_quartile, levels = c("Bottom-Q", "Middle-Qs", "Top-Q")))

pst_quartile_performance <- all_fellow_data %>%
  group_by(pst_quartile) %>%
  summarise(mar_dom1a = mean(mar_dom1a, na.rm = TRUE),
            mar_dom1c = mean(mar_dom1c, na.rm = TRUE),
            mar_dom1e = mean(mar_dom1e, na.rm = TRUE),
            mar_dom2b = mean(mar_dom2b, na.rm = TRUE),
            mar_dom2c = mean(mar_dom2c, na.rm = TRUE),
            mar_dom2d = mean(mar_dom2d, na.rm = TRUE),
            mar_dom3b = mean(mar_dom3b, na.rm = TRUE),
            mar_dom3c = mean(mar_dom3c, na.rm = TRUE),
            mar_dom3d = mean(mar_dom3d, na.rm = TRUE),
            mar_dom4a = mean(mar_dom4a, na.rm = TRUE))
            
mar_dom1a_aov <- aov(pst_quartile_performance$mar_dom1a ~ pst_quartile_performance$pst_quartile)      
summary(mar_dom1a_aov)

# ggplot(all_fellow_data, aes(x = effectiveness, y = count)) +
#   geom_point() +
#   labs(title = "Distribution of FDI Effectiveness") +
#   theme_minimal()


mod1 = lm(all_fellow_data$mar_dom1a ~ all_fellow_data$effectiveness) 
summary(mod1)
ggplot(all_fellow_data, aes(x = mar_dom1a, y = effectiveness)) +
  geom_point() +
  geom_jitter(alpha = .3) +
  geom_smooth(method = "lm", formula = y~x) +
  labs(x = "Dom 1A", y = "Effectiveness") +
  theme_light()

#final PST summative scores as compared to FDI effectiveness ratings
mod2 = lm(all_fellow_data$currentsummativescore ~ all_fellow_data$effectiveness)
summary(mod2)
ggplot(all_fellow_data, aes(x = effectiveness, y = currentsummativescore)) +
  geom_point() +
  geom_jitter(alpha = .3) +
  geom_smooth(method = "lm", formula = y~x) +
  labs(x = "FDI Effectiveness", y = "Final PST Score") 



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



#pull all variables I will need for corr, convert to numeric, add to one dataset for corrplot
for_cor <- select (all_fellow_data, effectiveness, currentsummativescore, round1, round2, round3, anchortechaveragehighestfor, nonanchortechniqueaverage, mar_dom1a, mar_dom1c, mar_dom1e, mar_dom2b, mar_dom2c, mar_dom2d, mar_dom3b, mar_dom3c, mar_dom3d, mar_dom4a) %>%
  ungroup() 
for_cor <- for_cor[, !(colnames(for_cor) %in% c("tt2id"))] 
for_cor <- for_cor %>% drop_na(effectiveness, currentsummativescore, round1, round2, round3, anchortechaveragehighestfor, nonanchortechniqueaverage, mar_dom1a, mar_dom1c, mar_dom1e, mar_dom2b, mar_dom2c, mar_dom2d, mar_dom3b, mar_dom3c, mar_dom3d, mar_dom4a)
cortest <- cor(for_cor)
corrplot1 <- corrplot(cortest, method="number")
res1 <- cor.mtest(for_cor,0.95)
res2 <- cor.mtest(for_cor,0.99)
corrplot1 <- corrplot(cortest, p.mat = res1[[1]], sig.level=0.05, method="number")
corrplot1.1 <- corrplot(cortest, p.mat = res1[[1]], insig="p-value", method="number")
##sig level shows black x over insignificant values
##insig="blank" leaves blank boxes for values that are not sig at .05 level
##insig="p-value" shows p value superimposed over correlation coefficient (if greater than .05)
##i like sig level, bc you can still see the corr coefficient 

for_cor2 <- select (all_fellow_data, effectiveness, currentsummativescore, mar_dom1a, mar_dom1c, mar_dom1e, mar_dom2b, mar_dom2c, mar_dom2d, mar_dom3b, mar_dom3c, mar_dom3d, mar_dom4a) %>%
  ungroup() 
for_cor2 <- for_cor2[, !(colnames(for_cor2) %in% c("tt2id"))] 
for_cor2 <- for_cor2 %>%drop_na(effectiveness, currentsummativescore, mar_dom1a, mar_dom1c, mar_dom1e, mar_dom2b, mar_dom2c, mar_dom2d, mar_dom3b, mar_dom3c, mar_dom3d, mar_dom4a) 
cortest2 <- cor(for_cor2)
corrplot2 <- corrplot(cortest2, p.mat = res1[[1]], insig="p-value", method="number")

for_cor3 <- all_fellow_data %>%
  ungroup() %>%
  select (effectiveness, currentsummativescore, count_fyi, rrpipsubmitted11, anchortechaveragehighestfor, nonanchortechniqueaverage) 
for_cor3 <- for_cor3[, !(colnames(for_cor3) %in% c("tt2id"))] 
cortest3 <- cor(for_cor3)
corrplot3 <- corrplot(cortest3, p.mat = res1[[1]], insig="blank", method="number")
corrplot3.1 <- corrplot(cortest3, p.mat = res1[[1]], sig.level =0.05, method="number")
corrplot3.2 <- corrplot(cortest3, p.mat = res1[[1]], insig="p-value", method="number")

for_cor4 <- all_fellow_data %>%
  ungroup() %>%
  select (x100percent, what_to_do, strong_voice, positive_framing, anchor_highest_formal_average, engineer_efficiency, strong_start, cold_call, stretch_it, control_the_game, everybody_writes, non_anchor_highest_formal_average, effectiveness, currentsummativescore) 
for_cor4 <- for_cor4[, !(colnames(for_cor4) %in% c("tt2id"))] 
cortest4 <- cor(for_cor4)
corrplot4 <- corrplot(cortest4, p.mat = res1[[1]], insig="blank", method="number")
corrplot4.1 <- corrplot(cortest4, p.mat = res1[[1]], sig.level =0.05, method="number")
corrplot4.2 <- corrplot(cortest4, p.mat = res1[[1]], insig="p-value", method="number")

for_cor5 <- all_fellow_data %>%
  ungroup() %>%
  select (anchor_highest_formal_average, non_anchor_highest_formal_average, round1, round2, round3, effectiveness, currentsummativescore) 
for_cor5 <- for_cor5[, !(colnames(for_cor5) %in% c("tt2id"))] 
cortest5 <- cor(for_cor5)
corrplot5 <- corrplot(cortest5, p.mat = res1[[1]], insig="blank", method="number")
corrplot5.1 <- corrplot(cortest5, p.mat = res1[[1]], sig.level =0.05, method="number")
corrplot5.2 <- corrplot(cortest5, p.mat = res1[[1]], insig="p-value", method="number")


#break out D75 as a group
d75 <-filter(all_fellow_data, d75 ==  "D75") 
non_d75 <- filter(all_fellow_data, d75 == "NA")

d75_summary<-summary(d75)
non_d75_summary <- summary (non_d75)

#coretest_1 <- rcorr(as.matrix(all_fellow_data$effectiveness, all_fellow_data$currentsummativescore))


#compared to June 2016 ratings (current_summative_score)
