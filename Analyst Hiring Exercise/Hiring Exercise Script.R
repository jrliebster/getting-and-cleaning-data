# Business Rules:
# Values of "<10" may be omitted (i.e., treated as NA)
# Ensure that every school on the Student Scores list has a match on the Educator Effectiveness Snapshot list

# load packages
library(pacman)
p_load(readr, dplyr, janitor, tidyr, stringr, ggplot2, data.table, corrplot)

# read data with clean names
student_scores <- read_csv ("StudentScores.csv") %>%
  clean_names()

# revert file names back to original!!
educator_effectiveness <- read_csv ("EducatorEffectivenessSnapshot.csv", col_names = TRUE, skip = 7) %>%
  clean_names()

# delete rows with missing data/values of less than 10
student_scores <-filter (student_scores, number_tested !=  "< 10")
educator_effectiveness <- educator_effectiveness %>%
  filter(!is.na(total))

# ensure all numbers are read as numeric variables
# Look at RR code review to shorten this
student_scores <- student_scores %>% 
  mutate_at(vars(number_tested:average_scaled_score), funs(as.numeric(.)))

# Part 1: Summarize the Student Scores data to show the percent of students who are proficient or higher in math at each school.   
  # a.	Populate a table showing the top ten schools in math proficiency along with their proficiency rates (percent of students scoring at proficient or higher). There should be one percentage per school that accounts for all students at the school.
math_proficiency <- student_scores %>% 
  filter(subject_name == "Mathematics", subgroup == "All Students") %>%
  select(building_name, subject_name, number_tested, percent_proficient) %>%
  mutate(number_proficient = number_tested*percent_proficient) %>%
  group_by(building_name) %>% 
  summarise(school_math_proficiency_percent = weighted.mean(percent_proficient, number_proficient))


# Douglass Academy showing as NaN, as they had 0 students at levels 1/2
math_proficiency$school_math_proficiency_percent[math_proficiency$school_math_proficiency_percent=="NaN"] <- 0

# keep full list of schools in sep vector to complete part two, create new table for top 10  
top_math_proficiency <- math_proficiency %>%
  arrange(desc(school_math_proficiency_percent)) %>%
  slice(1:10)

bottom_math_proficiency <- math_proficiency %>%
  arrange((school_math_proficiency_percent)) %>%
  slice(1:10)

write_csv(top_math_proficiency, "mathproficiency.csv")


# Part 2:  Explore the relationship between math proficiency and educator effectiveness at the school level.
# a.	Calculate the correlation between the percent of students at a school who are proficient in math (the result of Part 1) and the percent of educators at a school who are rated Effective or Highly Effective.  All schools in the Student Scores file should be used in this correlation.
# b.	The client for this work is the district's Director of School Improvement.  Provide the Director with a brief (no more than a paragraph or two) interpretation of the result of this correlation. What did you find? What do you think the district can learn from this result?   


# Ensure that every school on the Student Scores list has a match on the Educator Effectiveness Snapshot list
# same number of observations when joined, with no missing data
student_scores$building_name %in% educator_effectiveness$location
# not all return "TRUE", so not every student score location has a match on educator effectiveness snapshot

math_proficiency <- math_proficiency %>%
  rename(location = building_name)

# Change names of schools on math_proficiency to match educator_effectiveness
math_proficiency$location[math_proficiency$location=="Pulaski Elementary-Middle School"] <- "Pulaski Elem-Middle School"
math_proficiency$location[math_proficiency$location=="Carstens Elementary-Middle School"] <- "Carstens Elem-Middle School"
math_proficiency$location[math_proficiency$location=="All Buildings"] <- "Detroit City School District"

math_and_effectiveness <- merge(math_proficiency, educator_effectiveness, by = "location") 
math_and_effectiveness$school_math_proficiency_percent <- as.numeric(as.character(math_and_effectiveness$school_math_proficiency_percent))

cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

math_and_effectiveness_corr <- math_and_effectiveness %>%
  ungroup() %>%
  select (effective_or_more_percent, school_math_proficiency_percent)
math_and_effectiveness_corrplot <- corrplot(cor(math_and_effectiveness_corr), 
                                p.mat = cor.mtest(math_and_effectiveness_corr,0.95)[[1]], 
                                sig.level=0.05, 
                                method="number")

# alternatively, if just need corr coefficient and p-value, can use cor.test(math_and_effectiveness$school_math_proficiency_percent, math_and_effectiveness$effective_or_more_percent)

# visualize the relatinship between effectiveness and proficiency in a more straightforward way for client
mod1 = lm(math_and_effectiveness$effective_or_more_percent ~ math_and_effectiveness$school_math_proficiency_percent) 
summary(mod1)
ggplot(math_and_effectiveness, aes(x = school_math_proficiency_percent, y = effective_or_more_percent)) +
  geom_point() +
  geom_jitter(alpha = .3) +
  geom_smooth(method = "lm", formula = y~x) +
  labs(x = "Math Proficiency", y = "Teacher Effectiveness") +
  theme_light()

# check the mean, as well as number of teachers rated effective/highly effective to see the spread
mean(math_and_effectiveness$effective_or_more_percent)


# Part 3:  Examine the spread of educator ratings in the district. The district is interested in how the evaluation rating scale has been used in practice.
# a.	Create a chart illustrating a main message that you see in the ratings data.  The audience for this chart is the Director of School Improvement. 
# b.	Provide a short accompanying description for your chart.  What trend(s) do you notice and how might they be actionable for the district?

# subset educator effectiveness ratings for just Detroit City School District (overall dist ratings)
# convert to long format
# plot

district_ratings <- subset(educator_effectiveness, subset = location=="Detroit City School District")
district_ratings <- district_ratings %>%
  select(highly_effective, effective, minimally_effective, ineffective)
district_ratings <- gather(district_ratings, rating, count, highly_effective:ineffective, factor_key=TRUE)

district_ratings_percent <- subset(educator_effectiveness, subset = location=="Detroit City School District") %>%
  select(highly_effective_percent, effective_percent, minimally_effective_percent, ineffective_percent)
district_ratings_percent <- gather(district_ratings_percent, rating, percent, highly_effective_percent:ineffective_percent, factor_key=TRUE)

rating_by_school <- educator_effectiveness %>%
  select(location, highly_effective_percent, effective_percent, minimally_effective_percent, ineffective_percent)

write_csv(district_ratings, "districtratings.csv")
write_csv(district_ratings_percent, "districtratingspercent.csv")
write_csv(rating_by_school, "schoolratings.csv")
