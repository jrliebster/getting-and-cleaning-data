# What does attrition look like for those who apply in September?
# Date applied to notified from PS
# to date scheduled first interview and date of interview
# for those who were accepted in the first batch, when did they go to selection? (asked LC for list of those accepted in first batch)
# Also ask CT for collaborative data as a reference point

#load packages
library(pacman)
p_load(readxl, readr, dplyr, janitor, tidyr, stringr, ggplot)

# read comprehensive report (pulled from TT2)
comprehensive <- read_csv ("june 2017 comprehensive 071817.csv") %>%
    clean_names() %>%
    rename(appuserid = rrappuserid3)

# change date-based columns to date class 
comprehensive <- comprehensive %>% 
    mutate_at(vars(contains("date")),funs(as.Date), origin = "2015-01-01", format = "%m/%d/%Y")

# read in batch 1 acceptances list
batch1_acceptances <- read_csv ("batch one acceptances.csv") %>%
    clean_names() %>%
    mutate(batch_one = 1)

# join batch one acceptances with comprehensive data
comprehensive <- left_join(comprehensive, batch1_acceptances, by = "appuserid")

# when did candidates offered a spot in the program in batch 1 attend selection?
comprehensive$rrdateofinterview42 <- as.Date(comprehensive$rrdateofinterview42, format = "%m/%d/%Y")

batch_one_selection <- comprehensive %>%
    select(rrdateofinterview42, batch_one) %>%
    filter(batch_one == 1) %>%
    group_by(rrdateofinterview42, batch_one) %>%
    summarise(count=n())

# Date applied to notified from PS
# filter those out who have negative or zero days,
# as those represent dates candidates were notified from ehold 
# (so they appear to have been notified before actually submitting the app, 185 cases)
notified_from_ps <- comprehensive %>%
    filter(rreverappsubmitted25 == "Yes") %>%
    mutate(applied_to_notified = difftime(rrnotifiedafterprescreeningdate30, rrappsubmitteddate26, units = c("days"))) %>%
    filter(applied_to_notified > 0)

# calculate date between interview and offer 
# emove those with 0 days in between, as they are deferrals
offered <- comprehensive %>%
    filter(rreveroffered45 == "Yes") %>%
    mutate(selection_to_offer = difftime(rroffereddate46, rrdateofinterview42, units = c("days"))) %>%
    filter(selection_to_offer > 0)


