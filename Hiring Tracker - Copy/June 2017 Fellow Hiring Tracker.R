# I want to aggregate the DOE's new hire file (NHF) with NYCTF's hiring commitment form information on TT2 
# (any tab listed as "HCF data" includes only data from the HCF report in TT2)
# and pull the earliest date the Fellow stated they accepted an offer. 
# There are often 3 dates (date entered in DOE payroll on new hire file, date hiring commitment form was submitted, 
# and start date inputted by Fellow on hiring commitment form).

# load packages
library(pacman)
p_load(readxl, readr, dplyr, janitor, tidyr, stringr, ggplot2, lubridate, data.table)

# upload datasets (Hiring information report from TT2, comprehensive report, and NHF from DOE client)
fellow_hiring <- read_csv("hiringinformation.csv") %>%
    clean_names()
new_hire_file <- read_csv("fellowNHF.csv") %>%
    clean_names()
comprehensive <- read_csv("comprehensive.csv") %>%
    clean_names()

comprehensive <- comprehensive %>%
    distinct(rrappuserid2, .keep_all = TRUE)

# rename phone number column in fellow_hiring and new_hire_file
names(fellow_hiring)[names(fellow_hiring) == "rrphonenumber18"] <- "phone_number"
names(new_hire_file)[names(new_hire_file) == "phone"] <- "phone_number"
names(comprehensive)[names(comprehensive) == "rrprimaryphone5"] <- "phone_number"

# need to remove any characters that are not numbers from the phone number field, this is trailing (look for code that removes special characters from inside vector)
trim.trailing <- function (x) sub("\\-s+$", "", x) 
fellow_hiring$phone_number <- trim.trailing(fellow_hiring$phone_number)
fellow_hiring$phone_number <- gsub("[[:punct:]]", "", fellow_hiring$phone_number)

comprehensive$phone_number <- trim.trailing(comprehensive$phone_number)
comprehensive$phone_number <- gsub("[[:punct:]]", "", comprehensive$phone_number)

new_hire_file$phone_number <- trim.trailing(new_hire_file$phone_number)
new_hire_file$phone_number <- gsub("[[:punct:]]", "", new_hire_file$phone_number)

# change names of app user id field to match so I am able to join the comprehensive and fellow hiring datasets 
names(fellow_hiring)[names(fellow_hiring) == "rrappuserid1"] <- "appid"
names(comprehensive)[names(comprehensive) == "rrappuserid2"] <- "appid"

# remove NA school codes from fellow hiring and NHF
fellow_hiring <- fellow_hiring %>%
    filter(!is.na(rrschoolcode5))

new_hire_file <- new_hire_file %>%
    filter(!is.na(location))

# join comprehensive and fellow hiring
names(comprehensive)[names(comprehensive) == "rremail16"] <- "email"
comprehensive_nhf <- left_join(comprehensive, new_hire_file, by = "phone_number")
comprehensive_nhf_fh <- left_join(comprehensive_nhf, fellow_hiring, by = "phone_number")

table(is.na(comprehensive_nhf_fh$location), is.na(comprehensive_nhf_fh$rrschoolcode5))
# now, filter out all 800 Fellows who do not have hiring data from the C_nhf_fh
# must have data in location or rrschoolcode5
comprehensive_nhf_fh <- comprehensive_nhf_fh %>%
    filter(!is.na(location) | !is.na(rrschoolcode5))


test_df <- comprehensive_nhf_fh %>%
    filter(!is.na(location))

test_df_2 <- comprehensive_nhf_fh %>%
    filter(is.na(rrschoolcode5))

# next, need to remove unnecessary variables, like those related to scheudling interviews
# keep only useful columns df <- subset(df, select = c(a,c)) or select(-(c))
comprehensive_nhf_fh <- comprehensive_nhf_fh %>%
    select(-hire_type, -cluster, -network, -title, -terminated_from_job,-(problem_code_exists:dual_employment_issue), -(bq_submitted:background_investigation), -(certificate:appid.y))

#------------------------------------------------------------------------------------------
# Need to edit the rest of the code below
# from HD: need district and borough column

# change names of school code field to DBN so I am able to join the renewal crosswalk and fellow hiring datasets, remove DBN NA
names(fellow_hiring)[names(fellow_hiring) == "rrschoolcode5"] <- "DBN"
fellow_hiring <- filter(fellow_hiring, !is.na(DBN))

# change POCNonWhite column to name POC
names(fellow_hiring)[names(fellow_hiring) == "rrpocnonehite63"] <- "POC"

# change blank cases in Cert.Description column to NA
new_hire_file$cert_description[new_hire_file$cert_description == ""] <- NA 

# change blank cases in Lic.Desc column to NA
new_hire_file$lic_desc[new_hire_file$licdesc == ""] <- NA 

# join fellow_hiring and new_hire_file by phone number
# can't get these to properly merge and populate data by phone or email


# if the reports cannot be joined by phone number, join by another variable or use fuzzy joins
# may need to rename multiple variables for joins

# add column that pulls earliest data from all_fellow_hiring, read_csv, and readr recognized column entries as dates
# Re: the NA values, use pmin, it takes an argument na.rm .  Specify na.rm = TRUE to not have it return NA whenever any argument is NA.
all_fellow_hiring <- mutate(all_fellow_hiring, earliest_date = pmin(rrhiredate11, rrfellowhiringcommittmentformsubmitteddate16, na.rm = TRUE))
all_fellow_hiring$earliest_date <- as.Date(all_fellow_hiring$earliest_date, format = "%m/%d/%Y")

# created crosswalk for schools participating in early hiring
early_hiring_crosswalk <- read_csv("earlyhiringcrosswalk.csv") %>%
  remove_empty_cols()

# create a variable to code which schools fellows were hired into that participated in early hiring
all_fellow_hiring <- left_join(all_fellow_hiring, early_hiring_crosswalk, by = "DBN")
# change #N/A cases in early hiring column to No
all_fellow_hiring$EH[is.na(all_fellow_hiring$EH)] <- "No"

# replace DBNs that do not follow 00x000 format
all_fellow_hiring$DBN[all_fellow_hiring$DBN=="12*318"] <- "12x318"
all_fellow_hiring$DBN[all_fellow_hiring$DBN=="23 K 363"] <- "23k363"
all_fellow_hiring$DBN[all_fellow_hiring$DBN=="C9X241"] <- "09X241"
all_fellow_hiring$DBN[all_fellow_hiring$DBN=="D9X329"] <- "09X329" 

all_fellow_hiring$DBN <- tolower(all_fellow_hiring$DBN)

# extract borough code from DBN and add borough column
# tried ignore.case, but still pulls in only lower/uppercase letters (tried grep and str_extract)
all_fellow_hiring <- all_fellow_hiring %>%
  mutate(borough = str_extract(DBN, "[a-z]+" )) 

# change all letters in borough column to lowercase
table(all_fellow_hiring$borough)

# need to create a crosswalk for Renewal Schools, then create a column in the clean dataset that signifies whether or not the school is a renewal
# import 2017RenewalCrosswalk
renewal_crosswalk <- read_csv("2017RenewalCrosswalk.csv")
renewal_crosswalk$DBN <- tolower(renewal_crosswalk$DBN)
all_fellow_hiring <- left_join(all_fellow_hiring, renewal_crosswalk, by = "DBN")

# change #N/A cases in Renewal column to No
all_fellow_hiring$renewal[is.na(all_fellow_hiring$renewal)] <- "No"

#-----------------------------------------------------------------------------------------------------
# descriptive statistics
# Need to filter out any "incomplete" cases of RRStatusofChecklistItem14 before running descriptives


# percent of all Hires working in X
hiring_by_borough <- count(all_fellow_hiring, borough, wt = NULL, sort = TRUE)
hiring_by_subject <- count(all_fellow_hiring, rrprimarysubject15, wt = NULL, sort = TRUE)
# SA number by week percent
hiring_by_borough$percent<- prop.table(hiring_by_borough$n)


# calculate percent of university hired by week
# universitypercent <- all_fellow_hiring %>%
#   group_by(`UniversityClean`, week_number.x) %>%
#   summarise(count = n()) %>%
#   ungroup() %>%
#   group_by(`UniversityClean`) %>%
#   mutate(percent_of_total = round((count/sum(count)*100), 2),
#          cumulative = round(cumsum(percent_of_total)))

# change week of hire to date format
all_fellow_hiring <- all_fellow_hiring %>%
  mutate(week_number = week(earliest_date)) 

#------------------------
table(all_fellow_hiring, borough)

# cumulative percent of Fellow hiring by Borough
borough_percent <- all_fellow_hiring %>%
  group_by(borough, week_number) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(borough) %>%
  mutate(percent_of_total = round((count/sum(count)*100), 2),
         cumulative = round(cumsum(percent_of_total)))

# Percent of Fellows hired in Renewals
renewal_count <- all_fellow_hiring %>%
  group_by(renewal) %>%
  summarise(count = n())

# Percent of Bronx Cohort Fellows hired by week
Bronx_cohort_percent <- all_fellow_hiring %>%
  group_by(rrbronxcohort17, week_number) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(rrbronxcohort17) %>%
  mutate(percent_of_total = round((count/sum(count)*100), 2),
         cumulative = round(cumsum(percent_of_total)))

bronx_week <- all_fellow_hiring %>%
    filter(borough == "x") %>%
    group_by(borough, week_number) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    group_by(week_number) 
   
