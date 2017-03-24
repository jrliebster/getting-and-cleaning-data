#I want to aggregate the DOE's new hire file (NHF) with NYCTF's hiring commitment form information on TT2 
#(any tab listed as "HCF data" includes only data from the HCF report in TT2)
# and pull the earliest date the Fellow stated they accepted an offer. 
#There are often 3 dates (date entered in DOE payroll on new hire file, date hiring commitment form was submitted, 
#and start date inputted by Fellow on hiring commitment form).


#load packages
library(pacman)
p_load(readxl, readr, dplyr, janitor, tidyr, stringr, ggplot2)

#upload datasets (Hiring information report from TT2 and NHF from DOE client)
fellow_hiring <- read_csv("hiringinformation.csv")
new_hire_file <- read_csv("fellowNHF.csv")

#rename phone number column in fellow_hiring and new_hire_file
names(fellow_hiring)[names(fellow_hiring) == "RRPhoneNumber18"] <- "phone_number"
names(new_hire_file)[names(new_hire_file) == "PhoneNumber"] <- "phone_number"


#filter for non-white race, add to POCHiring
POCHiring<-filter(fellow_hiring, Race != White) ##did not work, still includes white

#filter for non-white race, add to POC, worked!
POC <- filter(fellow_hiring, Race != "White", Race!="I prefer not to disclose", Race!="Not listed - Please add below", !is.na(Race))

#change blank cases in Race column to NA
fellow_hiring$Race[fellow_hiring$Race == ""] <- "No"

#change blank cases in RRPhoneInterviewRecommendation20 column to NA
fellow_hiring$RRPhoneInterviewRecommendation20[fellow_hiring$RRPhoneInterviewRecommendation20 == ""] <- NA 

#change blank cases in Cert.Description column to NA
new_hire_file$Cert.Description[new_hire_file$Cert.Description == ""] <- NA 

#change blank cases in Lic.Desc column to NA
new_hire_file$Lic.Desc[new_hire_file$Lic.Desc == ""] <- NA 

#join fellow_hiring and new_hire_file by phone number
all_fellow_hiring <- left_join(fellow_hiring, new_hire_file, by = "phone_number")

#if the reports cannot be joined by phone number, join by another variable or use fuzzy joins
#may need to rename multiple variables for joins

#add column that pulls earliest data from all_fellow_hiring, read_csv, and readr recognized column entries as dates
#Re: the NA values, use pmin, it takes an argument na.rm .  Specify na.rm = TRUE to not have it return NA whenever any argument is NA.

all_fellow_hiring$earliestdate <- mutate(all_fellow_hiring, earliest_date = pmin(RRHireDate11, RRFellowHiringCommittmentFormSubmittedDate16, NEEDNHFDATEVARIABLENAME, na.rm = TRUE))
all_fellow_hiring$earliest_date <- as.Date(all_fellow_hiring$earliest_date, format = "%m/%d/%y")

#need to create a crosswalk for Renewal Schools, then create a column in the clean dataset that signifies whether or not the school is a renewal
#import 2017RenewalCrosswalk
renewal_crosswalk <- read_csv("2017RenewalCrosswalk.csv")
#how can I join the crosswalk and all_fellow_hiring? do I need all DBNs of schools? or will it import as NA if the DBN is in the hire file but not the crosswalk?
all_fellow_hiring <- left_join(all_fellow_hiring, renewal_crosswalk, by = "DBN")

#change #N/A cases in Renewal column to No
fellow_hiring$Renewal.[fellow_hiring$Renewal == "NA"] <- No

#created crosswalk for schools participating in early hiring
early_hiring_crosswalk <- read_csv("earlyhiringcrosswalk.csv")

#create a variable to code which schools fellows were hired into that participated in early hiring
all_fellow_hiring <- left_join(all_fellow_hiring, early_hiring_crosswalk, by = DBN)
#change #N/A cases in early hiring column to No
all_fellow_hiring$EarlyHiring[all_fellow_hiring$EarlyHiring == "NA"] <- No

#extract borough code from DBN and add borough column
all_fellow_hiring <- all_fellow_hiring %>%
  mutate(borough = str_extract(DBN, "[A-Z]+" )) 
table(all_fellow_hiring$borough)

#-----------------------------------------------------------------------------------------------------
#descriptive statistics

#percent of all Hires working in X
hiring_by_borough <- count(all_fellow_hiring, borough, wt = NULL, sort = TRUE)
#SA number by week percent
hiring_by_borough$percent<- prop.table(hiring_by_borough$n)


#calculate percent of university hired by week
universitypercent <- all_fellow_hiring %>%
  group_by(`UniversityClean`, week_number.x) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(`UniversityClean`) %>%
  mutate(percent_of_total = round((count/sum(count)*100), 2),
         cumulative = round(cumsum(percent_of_total)))

#change week of hire to date format
all_fellow_hiring <- all_fellow_hiring %>%
  mutate(weekdate = parse_date_time(weekdate, orders = "%m/%d/%y"))

#cumulative percent of Fellow hiring by Borough
borough_percent <- all_fellow_hiring %>%
  group_by(borough, weekdate) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(borough) %>%
  mutate(percent_of_total = round((count/sum(count)*100), 2),
         cumulative = round(cumsum(percent_of_total)))

renewal_percent <- all_fellow_hiring %>%
  group_by(renewal, weekdate) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(renewal) %>%
  mutate(percent_of_total = round((count/sum(count)*100), 2),
         cumulative = round(cumsum(percent_of_total)))


#Percent of Fellows hired in Renewals by week
renewal_percent <- all_fellow_hiring %>%
  group_by(renewal, weekdate) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(renewal) %>%
  mutate(percent_of_total = round((count/sum(count)*100), 2),
         cumulative = round(cumsum(percent_of_total)))

#Percent of Bronx Cohort Fellows hired by week
Bronx_cohort_percent <- all_fellow_hiring %>%
  group_by(Bronx_cohort, weekdate) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(Bronx_cohort) %>%
  mutate(percent_of_total = round((count/sum(count)*100), 2),
         cumulative = round(cumsum(percent_of_total)))
