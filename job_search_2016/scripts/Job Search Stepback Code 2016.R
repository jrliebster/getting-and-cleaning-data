#load packages

p_load(readxl, dplyr, ggplot2, scales, knitr, tidyr, readr, magrittr, stringr, stringi, lubridate, dplyr, janitor, Hmisc)


#load dataframes
hiring <- read.csv("hiringuse.csv")
comprehensivedata <- read.csv("ComprehensiveJune2016FellowData.csv", stringsAsFactors = FALSE)
universitydata <-read.csv("UniversityAssignment2016.csv", strip.white=TRUE)
PS <-read.csv("PS2016.csv")
PI <-read.csv("PI2016.csv")
SD <- read.csv("SD2016.csv")


#import 2015 data (still need university, conprehensive, selection data)
Hiring2015 <- read.csv("HiredFellows2015Use.csv")

#rename app id column
names(comprehensivedata)[names(comprehensivedata) == "RRAppUserId3"] <- "APPID"

#join hiring and comprehensive data by App ID
ComprehensiveHiring2016 <- left_join(hiring, comprehensivedata, by="APPID")


#rename App ID column in PS, PI, SD sets
names(PS)[names(PS) == "RRAppUserId48"] <- "APPID"
names(PI)[names(PI) == "AppUserId"] <- "APPID"
names(SD)[names(SD) == "AppUserId"] <- "APPID"

#join selection data
ComprehensiveSelection2016 <- left_join(PS, SD, by = "APPID")
ComprehensiveSelection2016 <- left_join(ComprehensiveSelection2016, PI, by = "APPID")

#Cleam ComprehensiveSelection2016
ComprehensiveSelection2016 <- select(ComprehensiveSelection2016, APPID, FinalDecision.y,Final.Decision, SelectorRecommendation, RRPhoneInterviewRecommendation20, RRPrescreenDecision14, RRPrimarySubject6)

#Clean ComprehensiveHiring2016
ComprehensiveHiring2016Clean <- select(ComprehensiveHiring2016, APPID, District, Location, Subject, JobID, Subject.Area, Cert.Description, Lic.Desc, Renewal., DBN, RRFirstName4, RRLastName5, RREmail6, RRGender9, RREthnicity10, RRAmericanIndianOrAlaskaNative11, RRAsian12, RRBlackOrAfricanAmerican13, RRLatinoHispanic14, RRNativeHawaiianOrOtherPacificIslander15, RRWhite16, RRLocationfromapplication17, RRUndergradGPA19, RRDateofEnrollment48, RRAppSubmittedDate26)

#join comprehensive selection clean and comprehensive hiring data
HiredFellows <- left_join(ComprehensiveHiring2016Clean, ComprehensiveSelection2016, by = "APPID") 


#rename race columns in HiredFellows
names(HiredFellows)[names(HiredFellows) == "RRAmericanIndianOrAlaskaNative11"] <- "AIAN"
names(HiredFellows)[names(HiredFellows) == "RRAsian12"] <- "Asian"
names(HiredFellows)[names(HiredFellows) == "RRBlackOrAfricanAmerican13"] <- "Black"
names(HiredFellows)[names(HiredFellows) == "RRLatinoHispanic14"] <- "Latino"
names(HiredFellows)[names(HiredFellows) == "RRNativeHawaiianOrOtherPacificIslander15"] <- "NHPI"
names(HiredFellows)[names(HiredFellows) == "RRWhite16"] <- "White"
names(HiredFellows)[names(HiredFellows) == "RREthnicity10"] <- "Race"

#filter for non-white race, add to POCHiring
POCHiring<-filter(HiredFellows, Race != White) ##did not work, still includes white

#filter for non-white race, add to POC, worked!
POC <- filter(HiredFellows, Race != "White", Race!="I prefer not to disclose", Race!="Not listed - Please add below", !is.na(Race))

#change blank cases in Race column to NA
HiredFellows$Race[HiredFellows$Race == ""] <- "No"

#change #N/A cases in Renewal. column to No
HiredFellows$Renewal.[HiredFellows$Renewal. == "#N/A"] <- NA 

#change blank cases in RRPhoneInterviewRecommendation20 column to NA
HiredFellows$RRPhoneInterviewRecommendation20[HiredFellows$RRPhoneInterviewRecommendation20 == ""] <- NA 

#change blank cases in Cert.Description column to NA
HiredFellows$Cert.Description[HiredFellows$Cert.Description == ""] <- NA 

#change blank cases in Lic.Desc column to NA
HiredFellows$Lic.Desc[HiredFellows$Lic.Desc == ""] <- NA 

# percentage by category library(data.table)
#  d = data.table(data)
#   a = d[, list(lag=unique(lag), percentage=as.numeric(table(lag)/length(lag))), by="user_type"]

#Remove site code, file review, ever app submitted, ever app started, fingerprints, date start teaching, summative PST outcome (all NA), terminate, background, dual employment column
#ComprehensiveHiring2016$RRSiteCode1.x <- NULL

#make table of Renewal schools count (worked!!)
Renewals = count(HiredFellows, Renewal. == "Yes")  


#count by district (worked)
Districts <- count(HiredFellows, District, wt = NULL, sort = TRUE)

#count by district and SA
DistrictsSA<- count(HiredFellows, District, Subject.Area, wt = NULL, sort = TRUE)

#count by district and location
DistrictsLoc<- count(HiredFellows, District, RRLocationfromapplication17, wt = NULL, sort = TRUE)

#Spread DistrictSA table by subject area (long view to wide view)
DistrictsSA_Wide <- HiredFellows %>%
  group_by(District, Subject.Area) %>%
  summarise(count = n()) %>%
  spread(Subject.Area, count, fill = 0)

#can also use summarise instead of count to get same result as count 

#hist for one variable, plot for 2 variables
# hist(HiredFellows$District)
# plot(HiredFellows$District, HiredFellows$RRPrimarySubject6)


#import HCFDate and NHFDate to compare submission dates of hiring forms
NHFDate <- read_csv("NHFDate.csv")
HCFDate <- read_csv("HCFDate.csv")                             

#join HCF and NHF data by APPID
HiringDate <- left_join(NHFDate, HCFDate, by = "APPID")

#import Fellow date for stated hiring date
FellowDate <- read_csv("FellowDate.csv")

#join hiring date and fellow date for all 3 dates (fellow reported on HCF, HCF submission, and HCF nomination)
HiringDates <- left_join(HiringDate, FellowDate, by = "APPID")
HiringDates$FellowStatedDate <- as.Date(HiringDates$FellowStatedDate, format = "%m/%d/%y")
HiringDates$NominationDate <- as.Date(HiringDates$NominationDate, format = "%m/%d/%y")
HiringDates$HCFDate <- as.Date(HiringDates$HCFDate, format = "%m/%d/%y")


#add column that pulls earliest data from HiringDate, formula below from Sam Firke, pulls left most column date, not earliest, changed to read_csv, and readr recognized column entries as dates
#Re: the NA values, type ?pmin to get the manual page for that function.  You'll see it takes an argument na.rm .  Specify na.rm = TRUE to not have it return NA whenever any argument is NA.
?pmin
#na.rm worked, no longer pulls NA values
earliestdate <- mutate(HiringDates, earliest_date = pmin(NominationDate, HCFDate, FellowStatedDate, na.rm = TRUE))
earliestdate$earliest_date <- as.Date(earliestdate$earliest_date, format = "%m/%d/%y")

#replace 1/1/00 with 8/11
earliestdate$earliest_date[earliestdate$earliest_date=="2000-01-01"] <- "2016-08-11"
earliestdate$earliest_date[earliestdate$earliest_date=="2002-08-17"] <- "2016-08-17"
earliestdate$earliest_date[earliestdate$earliest_date=="2015-08-10"] <- "2016-08-10"

#convert date to week of year
earliestdate <- earliestdate %>%
  # First convert "dates" to R date format; see the help file for
  # parse_date_time to understand the codes I supplied to "orders"
  mutate(date = parse_date_time(earliest_date, orders = "ymd")) %>%
  # Create vector of numeric weeks from the new date variable
  mutate(week_number = week(date)) 

#merge cleanhiredfellows and earliest date
CleanHiredFellows <- left_join(earliestdate, ComprehensiveHiring2016Clean, by ="APPID")


#count of hires by week
hiresbyweek <- earliestdate %>%
  filter(earliest_date > "2015-12-31") %>%
  group_by(week_number) %>%
  summarise(count = n()) %>%
  spread(week_number, count, fill = 0)

#merge cleanhiredfellows and earliest date
CleanHiredFellows <- left_join(earliestdate, CleanHiredFellows, by ="APPID")

#plot hist of earliest hiring date, Error in hist.default(hiring$Districts, earliestdate$earliest_date) : 'x' must be numeric
#hist(hiring$Districts, earliestdate$week_number)

#export earliestdate as CSV
write_csv(earliestdate, "earliestdate.csv")
                               
#importweekcrosswalk
weekcrosswalk <- read_csv("weekofhirecrosswalk.csv")

#join weekcrosswalk and cleanhiredfellows (column names equated)
CleanHiredFellows <- left_join(CleanHiredFellows, weekcrosswalk, by = c("week_number.x" = "week_number"))

#next, calculate percentage of subject area hired each week and in each district

#table of count of SAs hired by week
subjectareanumber<- count(CleanHiredFellows, week_number.x, Subject.Area, wt = NULL, sort = TRUE)
#SA number by week percent
subjectareanumber$percent<- prop.table(subjectareanumber$n)
#SA number by district
subjectareanumberloction <- count(CleanHiredFellows, Subject.Area, District, wt = NULL, sort = TRUE)

#remove duplicates so there are 1187 obs for each hiring data set; HiringDates <- unique( HiringDates[ , ] )--only including column 1 drops all other columns, but does remove correct number of duplicate obs)
#group APPID to see how many duplicates there are and which IDs have duplicates--counts how many times each ID appears
DuplicateHiringDates <- HiringDates %>%
  group_by(APPID) %>%
  tally() %>%
  arrange(desc(n)) %>%
  filter(n > 1)

#grouped by APPID and earliest_date, then removed duplicates that were not earliest date
CleanHiredFellows <- CleanHiredFellows %>%
  group_by(APPID) %>%
  arrange(earliest_date.x) %>%
  slice(1) 

#Separate project, unrelated to job search stepback----------------------------
#load PS indicator ratings
# PSindicatorratings <- read.csv("PSindicatorratings.csv")
# HEYes = count(PSindicatorratings, PS.High.Expectations == "TRUE", PS.Believes.All.Students.Can.Achieve == "FA-")
# 
# #Filter for PS HE = TRUE, then sum of all 5 variables, then filter for 2 or more
# HEYes <- PSindicatorratings %>%
#   filter(PS.High.Expectations == "TRUE") %>%
#   sum(PSindicatorratings$PS.High.Expectations == "TRUE" | PSindicatorratings$PS.Believes.All.Students.Can.Achieve == "FA-" | PSindicatorratings$PS.Takes.Responsibility.for.Student.Achievement == "FA-" | PSindicatorratings$PS.Demonstrates.Passion.and.Commitment.to.Educational.Excellence == "FA-" | PSindicatorratings$ShowsEmpathyAndRespectForAllStudents2 == "FA-", na.rm = TRUE)
# 
# # from Robert, 10/19, to calculate how many candidates have more than 2 FA- ratings
# PSindicatorratings %>%  
#   filter(PSindicatorratings$PS.High.Expectations == TRUE) %>%
#   group_by(AppUserId) %>%
#   summarise(TwoFAMinus =  sum(PSindicatorratings$PS.Believes.All.Students.Can.Achieve == "FA-", PSindicatorratings$PS.Takes.Responsibility.for.Student.Achievement == "FA-",PSindicatorratings$PS.Demonstrates.Passion.and.Commitment.to.Educational.Excellence == "FA-", PSindicatorratings$ShowsEmpathyAndRespectForAllStudents2 == "FA-")) %>%
#   filter(TwoFAMinus == 2) %>%
#   count()
#end of separate project-----------------------------------------------------

#export clean hired fellows
write_csv(CleanHiredFellows, "CleanHiredFellows.csv")

#replace I with district aligned borough code
CleanHiredFellows$DBN <- as.character(CleanHiredFellows$DBN)
CleanHiredFellows$DBN[CleanHiredFellows$DBN=="17I017"] <- "17K017"
CleanHiredFellows$DBN[CleanHiredFellows$DBN=="04I004"] <- "04M004"
CleanHiredFellows$DBN[CleanHiredFellows$DBN=="03I003"] <- "03M003"

#add borough column to Hired Fellows
CleanHiredFellows <- CleanHiredFellows %>%
  mutate(borough = str_extract(DBN, "[A-Z]+" )) 
table(CleanHiredFellows$borough)
# K   M   Q   R   X 
# 335 255 170   2 425 

#hiring by borough by week
HiringBoroughWeek <-  CleanHiredFellows %>%
  group_by(week_number.x, borough) %>%
  summarise(count = n()) %>%
  spread(., week_number.x, count)

#remove duplicates from universitydata--order by APPID, select and rename columns
universitydata <- universitydata %>%
  group_by(RRAppUserId3) %>%
  arrange(RRAppUserId3) %>%
  slice(1) %>%
  select(APPID = `RRAppUserId3`, University = `RRUniversityAssignment22`)

#count universitydata appids to determine where duplicates are
# universitydata %>%
#   group_by(APPID) %>%
#   tally() %>%
#   arrange(desc(n))

#merge universitydata with cleanhiredfellows
CleanHiredFellows <- left_join(CleanHiredFellows, universitydata, by = "APPID")

#hiring by week by university
universityhiring <- count(CleanHiredFellows, University, Subject.Area, week_number.x, District, wt = NULL, sort = TRUE)

#export subjectareapercent
write.csv(subjectareapercent, "subjectareabyweek.csv")

#import PST scores data
PSTRatings <- read.csv("PSTRating.csv")

#merge PSTRatings and CleanHiredFellows
CleanHiredFellows <- left_join(CleanHiredFellows, PSTRatings, by = "APPID")

#drop nomination date nas in cleanhiredfellows
CleanHiredFellows %>% drop_na(NominationDate.x)


#need to create subject area groups
#create crosswalk in excel-one column with all original subject areas, one column with groups, read back into R
subjectareagroups <- read_csv("subjectareagroups.csv")
#rename subject area column to match
names(subjectareagroups)[names(subjectareagroups) == "Subject Area"] <- "Subject.Area"
#merge clean hired fellows and subject area groups by subject area colunm
CleanHiredFellows <-left_join(CleanHiredFellows, subjectareagroups, by = "Subject.Area")

#compunding % of SAs hired by week, adding to 100% in last week
#change subjectareapercent to subject groups
subjectareapercent <- CleanHiredFellows %>%
  group_by(week_number.x, `Subject Group`) %>%
  summarise(count = n()) %>%
  mutate(percentsubject = round((value/sum(value)*100), 2)) %>%
  spread(week_number.x, count, fill = 0)

#calculate compounding % of each subject group, by week, totalling to 100% by week 41
#subject group first, as we are most interested in aggregating groups by this variable
#ungroup undoes the group_by function--otherwise it would have given percentage of total
subjectareapercent <- CleanHiredFellows %>%
  group_by(`Subject Group`, week_number.x) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(`Subject Group`) %>%
  mutate(percent_of_total = round((count/sum(count)*100), 2),
         cumulative = round(cumsum(percent_of_total)))
#can remove round command if want to keep the decimals following whole percent
#line chart would be best way to visualize (y cum percent, x axis week, lines are subjects)

write.csv(subjectareapercent, "subjectareapercent.csv")

#create university groups (removes subject area, just leaves university) in excel crosswalk, import
UniversityCrosswalk <- read_csv("UniversityCrosswalk.csv")

#clean trailing spaces off of Lehman Math and ESL, use read.csv?or?read.table?you can set the parameterstrip.white=TRUE. 
trim.trailing <- function (x) sub("\\s+$", "", x) 
CleanHiredFellows$University <- trim.trailing(CleanHiredFellows$University)

#merge clean hired fellows and university crosswalk by subject
CleanHiredFellows <-left_join(CleanHiredFellows, UniversityCrosswalk, by = "University")

#calculate percent of university hired by week
universitypercent <- CleanHiredFellows %>%
  group_by(`UniversityClean`, week_number.x) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(`UniversityClean`) %>%
  mutate(percent_of_total = round((count/sum(count)*100), 2),
         cumulative = round(cumsum(percent_of_total)))

#change week of hire to date format
CleanHiredFellows <- CleanHiredFellows %>%
  mutate(weekdate = parse_date_time(weekdate, orders = "%m/%d/%y"))

#cumulative percent of Fellow hiring by Borough
boroughpercent <- CleanHiredFellows %>%
  group_by(borough, weekdate) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(borough) %>%
  mutate(percent_of_total = round((count/sum(count)*100), 2),
         cumulative = round(cumsum(percent_of_total)))

renewalpercent <- CleanHiredFellows %>%
  group_by(Renewal., weekdate) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(Renewal.) %>%
  mutate(percent_of_total = round((count/sum(count)*100), 2),
         cumulative = round(cumsum(percent_of_total)))
         

#Percent of Fellows hired in Renewals by week
renewalpercent <- CleanHiredFellows %>%
  group_by(Renewal., weekdate) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(Renewal.) %>%
  mutate(percent_of_total = round((count/sum(count)*100), 2),
         cumulative = round(cumsum(percent_of_total)))

#export renewalpercent
write_csv(renewalpercent, "renewalbyweek.csv")


#look up Caroline Cornell to determine why she is not on the universitydate set--what is her university? was it an error? can i input her university manually?

#table of PST summative scores by quartile
#create new variable that groups PST scores into 1.9-2.2, 2.2-2.6, 2.6 and above
CleanHiredFellows <- CleanHiredFellows %>%
  mutate(PSTGroup = ifelse(Final.Summative.Score >= 2.6, 3, ifelse(Final.Summative.Score >= 2.2, 2, 1)))  

test_count <-  CleanHiredFellows %>%
  group_by(PSTGroup, Final.Summative.Score) %>%
  tally()
test_count2 <-  CleanHiredFellows %>%
  group_by(PSTGroup) %>%
  tally()


#graph final summative score by hire week to see if there is a connection between perf at PST and hire date
hist(CleanHiredFellows$Final.Summative.Score)
plot(CleanHiredFellows$Final.Summative.Score, CleanHiredFellows$week_number.x)
ggplot(CleanHiredFellows, aes(x = Final.Summative.Score, y = week_number.x)) +
  geom_point()


#load NHF for all Hires in NYCDOE 2016
NHF <- read_csv("NHF2016.csv")

#remove empty rows from NHF
remove_empty_rows(NHF)

#add borough column to NHF
NHF <- NHF %>%
  mutate(borough = str_extract(Location, "[A-Z]+" )) 
table(NHF$borough)

#percent of all Hires working in X
NHFBorough<- count(NHF, borough, wt = NULL, sort = TRUE)
#SA number by week percent
NHFBorough$percent<- prop.table(NHFBorough$n)

#export NHF Borough percent
write_csv(NHFBorough, "NHFBorough.csv")

#import all 2016 Hires
allhires <- read_csv("2016AllHiresFV.csv")
#remove random rows that have all NAs
allhires <- allhires[complete.cases(allhires[,2]),] 

allhires <- allhires %>%
  mutate(borough = str_extract(Location, "[A-Z]+" )) 
table(allhires$borough)


#compunding % of SAs hired by week, adding to 100% in last week
#change subjectareapercent to subject groups
subjectareapercent <- CleanHiredFellows %>%
  group_by(week_number.x, `Subject Group`) %>%
  summarise(count = n()) %>%
  mutate(percentsubject = round((value/sum(value)*100), 2)) %>%
  spread(week_number.x, count, fill = 0)

#run a correlation between week number and final summative rating 

#rcorr(x, type="pearson") # type can be pearson or spearman
#mtcars is a data frame 
#rcorr(as.matrix(mtcars))

# PSTWeekCorrelation <- CleanHiredFellows %>%
#   rcorr(PSTGroup, week_number, type="pearson") DID NOT WORK


#clean 2015 for comparison

#convert date to week of year for 2015
Hiring2015 <- Hiring2015 %>%
  # First convert "dates" to R date format; see the help file for
  # parse_date_time to understand the codes I supplied to "orders"
  mutate(date = parse_date_time(Nomination.Date, orders = "%m/%d/%Y")) %>%
  # Create vector of numeric weeks from the new date variable
  mutate(week_number = week(date)) 
#change weeknumber to date format
#join weekcrosswalk and cleanhiredfellows
Hiring2015 <- left_join(Hiring2015, weekcrosswalk, by ="week_number")

#replace I/XP/KLC with district aligned borough code 
Hiring2015$Location <- as.character(Hiring2015$Location)
Hiring2015$Location[Hiring2015$Location=="23I023"] <- "23K023"
Hiring2015$Location[Hiring2015$Location=="07XP07"] <- "07X007"
Hiring2015$Location[Hiring2015$Location=="88KLC1"] <- "88K607"

#add borough column to Hiring2015
Hiring2015 <- Hiring2015 %>%
  mutate(borough = str_extract(Location, "[A-Z]+" )) 
table(Hiring2015$borough)

#calculate hiring by borough by week 2015
Hiring2015BoroughWeek <-  Hiring2015 %>%
  group_by(week_number, borough) %>%
  summarise(count = n()) %>%
  spread(., week_number, count)


#calculate difference in time between earliest date and nomination date for 2016
CleanHiredFellows$daysBetween <- as.Date(CleanHiredFellows$earliest_date.x, format = "%m/%d/%y") - as.Date(CleanHiredFellows$NominationDate.x, format = "%m/%d/%y")
CleanHiredFellows$daysBetweenHCF <- as.Date(CleanHiredFellows$HCFDate.x, format = "%m/%d/%y") - as.Date(CleanHiredFellows$NominationDate.x, format = "%m/%d/%y")

