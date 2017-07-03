#load packages
library(pacman)
p_load(readxl, readr, dplyr, janitor, tidyr, stringr, ggplot2, data.table)

bronx_preferences <- read_csv("bronx_preferences.csv")
early_hiring_engagement <- read_csv("earlyhiring_eventregistration.csv")

names(bronx_preferences)[names(bronx_preferences) == "RRAppUserId3"] <- "TT2ID"
names(early_hiring_engagement)[names(early_hiring_engagement) == "RRAppUserId5"] <- "TT2ID"

early_hiring_engagement <- early_hiring_engagement[!duplicated(early_hiring_engagement), ]
bronx_preferences <- bronx_preferences[!duplicated(bronx_preferences), ]


early_hiring_engagement <- left_join(early_hiring_engagement, bronx_preferences, by = "TT2ID")
early_hiring_engagement <- early_hiring_engagement[!duplicated(early_hiring_engagement), ]

length(unique(early_hiring_engagement$TT2ID))

participants <- table(early_hiring_engagement$RRLastName4)
  

length(early_hiring_engagement$TT2ID)
attendance_freq <- as.data.frame(table(early_hiring_engagement$TT2ID))
summary(attendance_freq)

table(participants)
