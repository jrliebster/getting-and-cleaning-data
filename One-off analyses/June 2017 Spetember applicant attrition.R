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

# change date-based columns to date class (waiting on answer on Teams as to how to change class to date for number of variables that all contain the word "date")
comprehensive <- comprehensive %>% 
    mutate_at(vars(contains("date")),funs(as.Date, format = "%m/%d/%Y"))
comprehensive$rrdateappstarted24 <- as.Date(comprehensive$rrdateappstarted24, format = "%m/%d/%Y")

comprehensive <- comprehensive %>% 
    mutate_at(vars(contains("date")), funs(as.Date(., origin="1900-01-01")))


comprehensive <- comprehensive %>% 
    mutate_at(vars(rrappsubmitteddate26, rrdateappstarted24, rrdateofbirth18, rrdatefirstinvitedtophoneinterview32, rrdatewhenscheduledphoneinterview34, rrdateofphoneinterview36, rrdateinvitedtoselectionday38, rrdatewhenscheduledinpersoninterview40, rrdateofinterview42, rrdateofenrollment48, rrdateofstarttraining50, rrdateoffinishtraining52),dmy) %>% 

comprehensive <- comprehensive %>% 
    mutate_at(vars(contains("date")),funs(as.Date), origin = "2015-01-01", format = "%m/%d/%Y")

convert_TT2_date_string <- function(x){ as.Date(comprehensive, format = "%m/%d/%Y", origin = "2015-01-01")}
comprehensive <- comprehensive %>%
    convert_TT2_date_string(rrappsubmitteddate26)

class(all_fellow_hiring$rrdateappstarted24)

# read in batch 1 acceptances list
batch1_acceptances <- read_csv ("batch one acceptances.csv") %>%
    clean_names() %>%
    mutate(batch_one = 1)

# join batch one acceptances with comprehensive data
comprehensive <- left_join(comprehensive, batch1_acceptances, by = "appuserid")

# when did batch 1 Fellows attend selection?
comprehensive$rrdateofinterview42 <- as.Date(comprehensive$rrdateofinterview42, format = "%m/%d/%Y")

batch_one_selection <- comprehensive %>%
    select(rrdateofinterview42, batch_one) %>%
    filter(batch_one == 1) %>%
    group_by(rrdateofinterview42, batch_one) %>%
    summarise(count=n())


