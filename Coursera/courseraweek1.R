#load packages
library(pacman)
p_load(readxl, readr, dplyr, janitor, tidyr, stringr, ggplot2, corrplot)

week1_data <- read_csv("courseraweek1data.csv")

sum(is.na(week1_data$Ozone))

mean(!is.na(week1_data$Ozone))
ozone_mean <- na.omit(week1_data$Ozone)

mean(ozone_mean)

extract <- filter(week1_data, Ozone > 31, Temp > 90)
mean(extract$Solar.R)

datasub19<-subset(week1_data, week1_data$Month==6,select=Temp)
apply(datasub19,2,mean)

datasub20<-subset(week1_data,!is.na(Ozone)&week1_data$Month==5,select=Ozone)
apply(datasub20,2,max)

install.packages("swirl")
packageVersion("swirl")
library(swirl)
install_from_swirl("R Programming")
swirl()


