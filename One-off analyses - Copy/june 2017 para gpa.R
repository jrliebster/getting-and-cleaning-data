library(pacman)
p_load(readr, dplyr, janitor, tidyr, stringr, ggplot2, data.table, corrplot)

doe <- read_csv("june 2017 doe staff.csv")

demo <- read_csv("june 2017 demographics.csv")

all_data <- left_join(demo, doe, by = "RRAppUserId1")

para <- all_data %>%
    filter(RRYesWorkatNYCDOE9 == "Paraprofessional") 

mean(para$RRUndergradGPA21)
