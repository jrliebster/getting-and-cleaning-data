# Programming assignment steps:
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# data source: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

library(pacman)
p_load(readr, dplyr, utils, stringr, janitor, tidyr, data.table, knitr)

# 6 total datasets to load; xtrain, xtest, subjecttest, subjecttrain, ytrain, ytest
# all are .txt files
# col names for: 
   # subject datasets are "sub_id"
   # y datasets are "activity_id"
   # x datasets are colnames from "features"
   # activity label is "activity_type"

features = read.table('./features.txt',header=FALSE)
subject_train = read.table('./train/subject_train.txt',header=FALSE, col.names = ("sub_id"))
x_train = read.table('./train/x_train.txt',header=FALSE)
colnames(x_train) <- features[,2]
y_train = read.table('./train/y_train.txt',header=FALSE, col.names = ("activity_id"))
activity_label <- read.table('./activity_labels.txt', header=FALSE, col.names = c("activity_id", "activity_type"))
subject_test = read.table('./test/subject_test.txt',header=FALSE, col.names = ("sub_id"))
x_test = read.table('./test/x_test.txt',header=FALSE)
colnames(x_test) <- features[,2]
y_test = read.table('./test/y_test.txt',header=FALSE, col.names = ("activity_id"))


#merge all train datasets, then merge all test datasets
train_data <- cbind(y_train, subject_train, x_train)
test_data <- cbind(y_test, subject_test, x_test)

# merge the train and test data sets together
all_data <- rbind(train_data, test_data) %>%
  clean_names()


# Extract the measurements on the mean and standard deviation for each measurement.
mean_std_data <- all_data %>%
  select(contains("mean"),
         contains("std"),
         "activity_id",
         "sub_id")

# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.
mean_std_data <- left_join(mean_std_data, activity_label, by = "activity_id")
names(mean_std_data) <- gsub("acc", "acceleration", names(mean_std_data))
names(mean_std_data) <- gsub("^t", "time", names(mean_std_data))
names(mean_std_data) <- gsub("^f", "frequency", names(mean_std_data))
names(mean_std_data) <- gsub("bodybody", "body", names(mean_std_data))
names(mean_std_data) <- gsub("mag", "magnitude", names(mean_std_data))

# From the data set in step 4, 
# creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.
tidy_data <- mean_std_data %>%
  group_by(sub_id, activity_type) %>%
  summarise_all(funs(mean))

