#Schedule and Show Tracker: I want to be able to quickly determine the % of candidates who have been invited to interview that schedule interviews and attend them.
#Schedule and Show Tracker: I want to be able to quickly determine the % of candidates who have been invited to interview that schedule interviews and attend them. 
#I also want to know the average days it takes candidates to schedule and show at interviews.
# Pull comprehensive data set from TT2 (second tab has output)
# Determine the schedule and show rates for initial interview (phone) and interview (selection)
# look at 14 day (invited at least 14 days ago)
# look at 30 day (invited at least 30 days ago)
# look at schedule and show rates broken down by 30 day direct to selection (were not invited to initial interview) and 30 day PI to SD (went to initial interview first)
# ensure only interview dates in past are included in numerator/denominator for show rates (so those who have not had a chance to attend yet are not included in show rate)

# Considerations/concerns:
# need to filter out last week of events, to account for events not yet finalized
# pull this data weekly on Wednesdays, events are usually finalized on Mondays

###need to use datewhenscheduledinperson for SD date

library(pacman)
p_load(readxl, readr, dplyr, janitor, tidyr, stringr, ggplot2)

comprehensive <- read_csv("BitBucket/nyctf/Schedule and Show/comprehensive.csv")

#remove RRAppStatus60 that is anything before Invited to Initial Interview
comprehensive<-comprehensive[ ! comprehensive$RRAppStatus60 %in% c("Account Created", "Prescreened In","Application Started","Eligibility Hold",  "Application Incomplete", "Application Submitted", "Ineligible", "Automated Ineligible", "Prescreened Out"), ]

#keep only useful columns df <- subset(df, select = c(a,c))
comprehensive <- subset(comprehensive, select = c("RRAppUserId3", "RRPrimarySubject8", "RRLocationfromapplication17", "RREverinvitedtophoneinterview31", "RRDateFirstInvitedtoPhoneInterview32", "RRScheduledaphoneinterview33", "RRDatecandidatescheduledfirstphoneinterview34", "RRPhoneinterviewed35", "RRDateofphoneinterview36", "RREverinvitedtoSelectionDay37","RRDateInvitedtoSelectionDay38", "RRScheduledaninpersoninterview39", "RRDatewhenscheduledinpersoninterview40", "RREverInterviewed41", "RRDateofinterview42"))

#change all date variables to date class and consistent format (could this be done more efficiently?)
#lapply?
#comprehensive[,"RRDateofinterview42", "RRDateFirstInvitedtoPhoneInterview32", "RRDatecandidatescheduledfirstphoneinterview34", "RRDateofphoneinterview36", "RRDateInvitedtoSelectionDay38", "RRDatewhenscheduledinpersoninterview40"] <- as.Date(comprehensive[,"RRDateofinterview42", "RRDateFirstInvitedtoPhoneInterview32", "RRDatecandidatescheduledfirstphoneinterview34", "RRDateofphoneinterview36", "RRDateInvitedtoSelectionDay38", "RRDatewhenscheduledinpersoninterview40"], "%m/%d/%Y")
#Marie--this could change to become more efficient/streamlined. I'll send you an updated version if it does (Marie runs the code each week and reports on the analysis)
comprehensive$RRDateofinterview42 <- as.Date(comprehensive$RRDateofinterview42, "%m/%d/%Y")
comprehensive$RRDateFirstInvitedtoPhoneInterview32 <- as.Date(comprehensive$RRDateFirstInvitedtoPhoneInterview32, "%m/%d/%Y")
comprehensive$RRDatecandidatescheduledfirstphoneinterview34 <- as.Date(comprehensive$RRDatecandidatescheduledfirstphoneinterview34, "%m/%d/%Y")
comprehensive$RRDateofphoneinterview36 <- as.Date(comprehensive$RRDateofphoneinterview36, "%m/%d/%Y")
comprehensive$RRDateInvitedtoSelectionDay38 <- as.Date(comprehensive$RRDateInvitedtoSelectionDay38, "%m/%d/%Y")
comprehensive$RRDatewhenscheduledinpersoninterview40 <- as.Date(comprehensive$RRDatewhenscheduledinpersoninterview40, "%m/%d/%Y")

#calculate schedule and show rates for initial interview--need to remove all observations that are "No" for RREverinvitedtophoneinterview31
initialschedulecount <- count(comprehensive, RREverinvitedtophoneinterview31, RRScheduledaphoneinterview33, wt = NULL, sort = TRUE)
initialshowcount <- count(comprehensive, RRPhoneinterviewed35, RRScheduledaphoneinterview33, wt = NULL, sort = TRUE)
initialschedulecount<-initialschedulecount[!(initialschedulecount$RREverinvitedtophoneinterview31=="No"),]
initialshowcount<-initialshowcount[!(initialshowcount$RRScheduledaphoneinterview33=="No"),]

#next, for both initial and interview, need to filter schedule and show rates for dates that have not occurred yet (remove from show rate)
#then, make 14 and 30 day restrictions (filter for those who were invited 14+ or 30+ days ago)

#Sys.Date() gives current date, filter out anything less than or equal to 14 days before sys.date (same for 30)
#need to remove all future events (including today's date to the past Monday). Event attendance data is finalized on Mondays, so we would not want to include any events that occurred after the most recent Monday, as it can skew show rates (it will show that all canidates did not attend an event if it has not yet been finalized)
#CleanHiredFellows$daysBetweenHCF <- as.Date(CleanHiredFellows$HCFDate.x, format = "%m/%d/%y") - as.Date(CleanHiredFellows$NominationDate.x, format = "%m/%d/%y")
current_date <- Sys.Date()

#create variables with outputs 1 and 0 to determine if candidates were invited to initial and/or interview at least 14 or at least 30 days prior to current date
comprehensive <- comprehensive %>%
  mutate (invitedtophoneinterview14day = ifelse(current_date - RRDateFirstInvitedtoPhoneInterview32  >= 14, 1, 0)) %>%
  mutate (invitedtophoneinterview30day = ifelse(current_date - RRDateFirstInvitedtoPhoneInterview32  >= 30, 1, 0)) %>%
  mutate (invitedtointerview14day = ifelse(current_date - RRDateInvitedtoSelectionDay38  >= 14, 1, 0)) %>%
  mutate (invitedtointerview30day = ifelse(current_date - RRDateInvitedtoSelectionDay38  >= 30, 1, 0)) 


#count sched and show for PI and SD that have been filtered for current date - 2 (issue here is that PI_sched_and_show and SD_sched_and_show only includes those who scheduled)
#filters for phone and selection dates in future/prior to this past monday
initialscheduleshowcount30 <- comprehensive %>%
  filter(invitedtophoneinterview30day==1,
         RRDatecandidatescheduledfirstphoneinterview34 < (current_date - 2) | is.na (RRDatecandidatescheduledfirstphoneinterview34), 
         RRDateofphoneinterview36 < (current_date - 2) | is.na (RRDateofphoneinterview36)) %>%
  #count(invitedtophoneinterview30day, RRScheduledaphoneinterview33, RRPhoneinterviewed35, wt = NULL, sort = TRUE) (not longer needed, as I'll be summarising below)
  summarise(invited_initial = sum(RREverinvitedtophoneinterview31 == "Yes", na.rm = TRUE),
          show_initial = sum(RRScheduledaphoneinterview33 == "Yes" & RRPhoneinterviewed35 == "Yes", na.rm = TRUE),
          scheduled_initial = sum(RRScheduledaphoneinterview33 == "Yes", na.rm = TRUE),
          no_show_initial = sum(RRScheduledaphoneinterview33 == "Yes" & RRPhoneinterviewed35 =="No", na.rm =TRUE)) %>%
  mutate(show_rate_initial30 = show_initial/(show_initial + no_show_initial)) %>%
  mutate(schedule_rate_initial30 = scheduled_initial/invited_initial)

  
initialscheduleshowcount14 <- comprehensive %>%
  filter(invitedtophoneinterview14day==1,
         RRDatecandidatescheduledfirstphoneinterview34 < (current_date - 2) | is.na (RRDatecandidatescheduledfirstphoneinterview34), 
         RRDateofphoneinterview36 < (current_date - 2) | is.na (RRDateofphoneinterview36)) %>%
  #count(invitedtophoneinterview14day, RRScheduledaphoneinterview33, RRPhoneinterviewed35, wt = NULL, sort = TRUE)
  summarise(invited_initial = sum(RREverinvitedtophoneinterview31 == "Yes", na.rm = TRUE),
          show_initial = sum(RRScheduledaphoneinterview33 == "Yes" & RRPhoneinterviewed35 == "Yes", na.rm = TRUE),
          scheduled_initial = sum(RRScheduledaphoneinterview33 == "Yes", na.rm = TRUE),
          no_show_initial = sum(RRScheduledaphoneinterview33 == "Yes" & RRPhoneinterviewed35 =="No", na.rm =TRUE)) %>%
  mutate(show_rate_initial14 = show_initial/(show_initial + no_show_initial))%>%
  mutate(schedule_rate_initial14 = scheduled_initial/invited_initial)

interviewscheduleshowcount14 <- comprehensive %>%
  filter(invitedtointerview14day==1) %>%
  summarise(show_rate_interview14 = sum(RREverInterviewed41 == "Yes", na.rm = TRUE)/
              sum(RRDatewhenscheduledinpersoninterview40 < (current_date - 2), na.rm = TRUE),
            schedule_rate_interview14 = sum(RRScheduledaninpersoninterview39 == "Yes", na.rm = TRUE)/
              sum(RREverinvitedtoSelectionDay37 == "Yes", na.rm = TRUE))


interviewscheduleshowcount14 <- comprehensive %>%
  filter(invitedtointerview14day==1) %>%
  #count(invitedtointerview14day, RRScheduledaninpersoninterview39, RREverInterviewed41, wt = NULL, sort = TRUE)
  summarise(invited_interview = sum(RREverinvitedtoSelectionDay37 == "Yes", na.rm = TRUE),
            show_interview = sum(RRScheduledaninpersoninterview39 == "Yes" & RREverInterviewed41 == "Yes", na.rm = TRUE),
            scheduled_interview = sum(RRScheduledaninpersoninterview39 == "Yes", na.rm = TRUE),
            no_show_interview = sum(RRScheduledaninpersoninterview39 == "Yes" & RREverInterviewed41 =="No", na.rm =TRUE)) %>%
  mutate(show_rate_interview14 = show_interview/(show_interview + no_show_interview)) %>%
  mutate(schedule_rate_interview14 = scheduled_interview/invited_interview)

interviewscheduleshowcount30 <- comprehensive %>%
  filter(invitedtointerview30day==1) %>%
  #count(invitedtointerview30day, RRScheduledaninpersoninterview39, RREverInterviewed41, wt = NULL, sort = TRUE)
  summarise(invited_interview = sum(RREverinvitedtoSelectionDay37 == "Yes", na.rm = TRUE),
          show_interview = sum(RRScheduledaninpersoninterview39 == "Yes" & RREverInterviewed41 == "Yes", na.rm = TRUE),
          scheduled_interview = sum(RRScheduledaninpersoninterview39 == "Yes", na.rm = TRUE),
          no_show_interview = sum(RRScheduledaninpersoninterview39 == "Yes" & RREverInterviewed41 =="No", na.rm =TRUE)) %>%
  mutate(show_rate_interview30 = show_interview/(show_interview + no_show_interview))%>%
  mutate(schedule_rate_interview30 = scheduled_interview/invited_interview)

#add all invited, schedule, show count to one dataframe
scheduleandshow <-data.frame(interviewscheduleshowcount30, interviewscheduleshowcount14, initialscheduleshowcount14, initialscheduleshowcount30)

#finally, select only schedule and show rates, and use gather to convert data from wide format to long, round to 2 decimal places
#muliply by 100 to get percent, add to scheduleandshow dataframe
output <- scheduleandshow %>%
  select(schedule_rate_interview14, show_rate_interview14, schedule_rate_initial14, show_rate_initial14, schedule_rate_interview30, show_rate_interview30, schedule_rate_initial30, show_rate_initial30) %>%
  gather(scheduleandshow, value) %>% # what you want the column names to be
  mutate(value = round(value*100, digits = 2)) 
  

#calculate days between invite and schedule, schedule and show
comprehensive$phonedaystoschedule <- as.Date(comprehensive$RRDatecandidatescheduledfirstphoneinterview34, format = "%m/%d/%y") - as.Date(comprehensive$RRDateFirstInvitedtoPhoneInterview32, format = "%m/%d/%y")
comprehensive$phonedaystointerview <- as.Date(comprehensive$RRDateofphoneinterview36, format = "%m/%d/%y") - as.Date(comprehensive$RRDatecandidatescheduledfirstphoneinterview34, format = "%m/%d/%y")
comprehensive$interviewdaystoschedule <- as.Date(comprehensive$RRDatewhenscheduledinpersoninterview40, format = "%m/%d/%y") - as.Date(comprehensive$RRDateInvitedtoSelectionDay38, format = "%m/%d/%y")
comprehensive$interviewdaystointerview <- as.Date(comprehensive$RRDateofinterview42, format = "%m/%d/%y") - as.Date(comprehensive$RRDatewhenscheduledinpersoninterview40, format = "%m/%d/%y")

#mean days between invited, scheduled, show for initial and interview
averagedays <- comprehensive %>%
  mutate (phone_avg_days_schedule = round(mean(comprehensive$phonedaystoschedule, na.rm=TRUE),digits=1),
          phone_avg_days_show = round(mean(comprehensive$phonedaystointerview, na.rm=TRUE),digits=1),
          interview_avg_days_schedule = round(mean(comprehensive$interviewdaystoschedule, na.rm=TRUE),digits=1),
          interview_avg_days_show = round(mean(comprehensive$interviewdaystointerview, na.rm=TRUE),digits=1))


averagedays <- comprehensive %>%
    summarise (phone_avg_days_schedule = round(mean(phonedaystoschedule, na.rm=TRUE),digits=1),
               phone_avg_days_show = round(mean(phonedaystointerview, na.rm=TRUE),digits=1),
               interview_avg_days_schedule = round(mean(interviewdaystoschedule, na.rm=TRUE), digits=1),
               interview_avg_days_show = round(mean(interviewdaystointerview, na.rm=TRUE),digits=1))
  


