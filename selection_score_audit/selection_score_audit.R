


if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(tidyr, readxl, readr, dplyr, janitor, tidyr, stringr, zoo, magrittr)


dat <- read_excel("NYCTF File Review Week 1 for Robert.xlsx")

dat[dat== "E"] <- 5
dat[dat== "FA+"] <- 4
dat[dat== "FA"] <- 3
dat[dat== "FA-"] <- 2
dat[dat== "NFA"] <- 1


dat$critical_thinking_recalc <- case_when(
  (dat$TeachingSamplePlanningFormCT == 1 |dat$GroupActivityCT == 1 |dat$TeachingSampleReteachCT == 1) ~ 1,
   ~ 2,
  TRUE ~ 1000
)

# confirm that all values calculated correctly
sum(dat$critical_thinking_recalc != dat$CriticalThinkingOverall)

