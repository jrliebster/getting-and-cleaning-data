library(pacman)
p_load(readxl, readr, dplyr, janitor, tidyr, stringr)
#pull reports from TT2 as .csv

last_four <- read_csv("PSLast4Weeks.csv") %>%
  remove_empty_rows() %>%
  mutate(time_period = "Last Four Weeks")


last_week <- read_csv("PSLastWeek.csv") %>%
  remove_empty_rows() %>%
  mutate(time_period = "Last Week")

overall <- read_csv ("PSOverall.csv") %>%
remove_empty_rows() %>%
  mutate(time_period = "Overall")

# First remove unneeded variables
names(overall)[names(overall) == "RRPR_Yes6"] <- "RRP_Yes6"
overall <- select(overall, RRPrescreener1, RRFiles2, RRCT_Yes3, RRHE_Yes5, RRP_Yes6, RRPS_IN7, RRPSD_ScreenOut16, RRPS_HR58, RRPS_RWC49, RRPS_R310, RRPS_RWR211, RRoverturns13, time_period)

# Rename variables to match other data frames, otherwise bind_rows won't work
overall <- rename(overall,RRoverturns8=RRoverturns13, RRHR513=RRPS_HR58, RRRWC412=RRPS_RWC49, RRRec311=RRPS_R310, RRRWR210=RRPS_RWR211, RRFiles3=RRFiles2, RRCT_Yes4=RRCT_Yes3, RRPSD_ScreenOut9=RRPSD_ScreenOut16)

# Create time period variable to designate metrics are from entire selection period and whether they are POC focused

# remove % and convert all numbers to numeric 
last_week$RRCT_Yes4 <-as.numeric(sub("%","",last_week$RRCT_Yes4)) 
last_four$RRCT_Yes4 <-as.numeric(sub("%","",last_four$RRCT_Yes4)) 
last_week$RRHE_Yes5 <-as.numeric(sub("%","",last_week$RRHE_Yes5)) 
last_four$RRHE_Yes5 <-as.numeric(sub("%","",last_four$RRHE_Yes5)) 
last_week$RRP_Yes6 <-as.numeric(sub("%","",last_week$RRP_Yes6)) 
last_four$RRP_Yes6 <-as.numeric(sub("%","",last_four$RRP_Yes6)) 
last_week$RRPS_IN7 <-as.numeric(sub("%","",last_week$RRPS_IN7)) 
last_four$RRPS_IN7 <-as.numeric(sub("%","",last_four$RRPS_IN7)) 
last_week$RRPSD_ScreenOut9 <-as.numeric(sub("%","",last_week$RRPSD_ScreenOut9)) 
last_four$RRPSD_ScreenOut9 <-as.numeric(sub("%","",last_four$RRPSD_ScreenOut9)) 
last_week$RRRWR210 <-as.numeric(sub("%","",last_week$RRRWR210)) 
last_four$RRRWR210 <-as.numeric(sub("%","",last_four$RRRWR210)) 
last_week$RRRec311 <-as.numeric(sub("%","",last_week$RRRec311)) 
last_four$RRRec311<-as.numeric(sub("%","",last_four$RRRec311))
last_week$RRRWC412 <-as.numeric(sub("%","",last_week$RRRWC412)) 
last_four$RRRWC412 <-as.numeric(sub("%","",last_four$RRRWC412))
last_week$RRHR513 <-as.numeric(sub("%","",last_week$RRHR513)) 
last_four$RRHR513 <-as.numeric(sub("%","",last_four$RRHR513)) 

#last_week <- last_week %>% 
  #mutate(CT = as.numeric(RRCT_Yes4))



# Add overall datafame to the bind
# Add new time periods to factor
# Arrange columns in whatever order makes sense for viewer
# Retitle variables if necessary

ps_rating_tracker <- bind_rows(last_four, last_week, overall) %>%
  arrange(RRPrescreener1, time_period) 

#split POC entries to remove - POC -
#strsplit(ps_rating_tracker$RRPrescreener1, "-")#
#ps_rating_tracker$POC <- strsplit(ps_rating_tracker$RRPrescreener1, "-")


ps_rating_tracker <- ps_rating_tracker %>%
  mutate(time_period = ifelse(grepl("- POC -", RRPrescreener1), "POC", time_period), 
  RRPrescreener1 = gsub("- POC - ","", RRPrescreener1),
  time_period = factor(time_period, ordered = is.ordered(c("Last Four Week", "Last Week", "Overall", "POC")))) %>%
  arrange(RRPrescreener1, time_period)


#create table that pulls in PS In ratings that are not within 6% of PS In total 
# psinoutsideavg <- subset[ps_rating_tracker$RRPS_IN7 > 97]
# Error in subset[ps_rating_tracker$RRPS_IN7 > 97] : 
#   object of type 'closure' is not subsettable
#PS In ratings above 97% and below 88%
psinoutside <- subset(ps_rating_tracker, RRPS_IN7 > 97 | RRPS_IN7 < 88)

#need to base this on total PSIN ratings for week
#create benchmark for PS In based on total PS in ratings overall
total_high_moving <- ps_rating_tracker$RRPS_IN7[ps_rating_tracker$RRPrescreener1 == "zz_TOTAL" & ps_rating_tracker$time_period == "Overall"] + 6
#create max when average plus 6 is too high
total_high_max <- 97
#create rule for when to use max and when to use average+6
total_high_use <- ifelse(total_high_moving > total_high_max, total_high_max, total_high_moving)

#create low for PS in rate
total_low_use <- ps_rating_tracker$RRPS_IN7[ps_rating_tracker$RRPrescreener1 == "zz_TOTAL" & ps_rating_tracker$time_period == "Overall"] - 6

#filter for PS In rates outside of bounds for just last week of data
psinlastweekoutside <- ps_rating_tracker %>%
  filter(time_period == "Last Week") %>%
  subset(RRPS_IN7 > total_high_use | RRPS_IN7 < total_low_use)
  
#filter for multiple variables using | psinoutside <- subset(ps_rating_tracker, RRPS_IN7 > 97 | RRPS_IN7 < 88 | RRHR513 > 25)
# ctlow <- subset(ps_rating_tracker, RRCT_Yes4 < 77)
# cthigh <- subset(ps_rating_tracker, RRCT_Yes4 > 89)
# hehigh <- subset(ps_rating_tracker, RRHE_Yes5 > 97)
# helow <- subset(ps_rating_tracker, RRHE_Yes5 < 88)
# prolow <-subset(ps_rating_tracker, RRP_Yes6 < 80)
# prohigh <-subset(ps_rating_tracker, RRP_Yes6 > 93)

#do the same for CT
ct_high_moving <- ps_rating_tracker$RRCT_Yes4[ps_rating_tracker$RRPrescreener1 == "zz_TOTAL" & ps_rating_tracker$time_period == "Overall"] + 6
#create max when average plus 6 is too high
ct_high_max <- 91
#create rule for when to use max and when to use average+6
ct_high_use <- ifelse(ct_high_moving > ct_high_max, ct_high_max, ct_high_moving)

#create low for PS in rate
ct_low_use <- ps_rating_tracker$RRCT_Yes4[ps_rating_tracker$RRPrescreener1 == "zz_TOTAL" & ps_rating_tracker$time_period == "Overall"] - 6
ct_low_max <- 77
ct_low_use <- ifelse(ct_low_use < ct_low_max, ct_low_max, ct_low_use)

#filter for PS In rates outside of bounds for just last week of data
ctlastweekoutside <- ps_rating_tracker %>%
  filter(time_period == "Last Week") %>%
  subset(RRCT_Yes4 > ct_high_use | RRCT_Yes4 < ct_low_use)


#do the same for Pro
pro_high_moving <- ps_rating_tracker$RRP_Yes6[ps_rating_tracker$RRPrescreener1 == "zz_TOTAL" & ps_rating_tracker$time_period == "Overall"] + 6
#create max when average plus 6 is too high
pro_high_max <- 93
#create rule for when to use max and when to use average+6
pro_high_use <- ifelse(pro_high_moving > pro_high_max, pro_high_max, pro_high_moving)

#create low for PS in rate
pro_low_use <- ps_rating_tracker$RRP_Yes6[ps_rating_tracker$RRPrescreener1 == "zz_TOTAL" & ps_rating_tracker$time_period == "Overall"] - 6
pro_low_max <- 80
pro_low_use <- ifelse(pro_low_use < pro_low_max, pro_low_max, pro_low_use)

#filter for PS In rates outside of bounds for just last week of data
prolastweekoutside <- ps_rating_tracker %>%
  filter(time_period == "Last Week") %>%
  subset(RRP_Yes6 > pro_high_use | RRP_Yes6 < pro_low_use)

#do the same for Highly Recommend
hr_high_moving <- ps_rating_tracker$RRHR513[ps_rating_tracker$RRPrescreener1 == "zz_TOTAL" & ps_rating_tracker$time_period == "Overall"] + 6
#create max when average plus 6 is too high
hr_high_max <- 22
#create rule for when to use max and when to use average+6
hr_high_use <- ifelse(hr_high_moving > hr_high_max, hr_high_max, hr_high_moving)

#create low for PS in rate
hr_low_use <- ps_rating_tracker$RRP_Yes6[ps_rating_tracker$RRPrescreener1 == "zz_TOTAL" & ps_rating_tracker$time_period == "Overall"] - 6
hr_low_max <- 8
hr_low_use <- ifelse(hr_low_use > hr_low_max, hr_low_max, hr_low_use)

#filter for PS In rates outside of bounds for just last week of data
hrlastweekoutside <- ps_rating_tracker %>%
  filter(time_period == "Last Week") %>%
  subset(RRHR513 > hr_high_use | RRHR513 < hr_low_use) 