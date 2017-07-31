#load packages
library(pacman)
p_load(readxl, readr, dplyr, janitor, tidyr, data.table)

# read housing data 
housing <- read_csv ("week1quizdata.csv") %>%
    clean_names()

housing_over_million <- housing %>%
    filter(val == 24) %>%
    count(val == 24)

# read natural gas data
dat <- read.table("naturalgas.csv", nrows = 18:23, ncol(7:15))
