# Programming assignment steps:
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# data source: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

library(pacman)
p_load(readr, dplyr, utils, stringr, janitor, tidyr, data.table)

# merge the train and test data sets 
# 6 total to load; xtrain, xtest, subjecttest, subjecttrain, ytrain, ytest
# all are .txt files

subject_train = read.table('./train/subject_train.txt',header=FALSE)
x_train = read.table('./train/x_train.txt',header=FALSE)
y_train = read.table('./train/y_train.txt',header=FALSE)

subject_test = read.table('./test/subject_test.txt',header=FALSE)
x_test = read.table('./test/x_test.txt',header=FALSE)
y_test = read.table('./test/y_test.txt',header=FALSE)
features = read.table('./features.txt',header=FALSE)

x_data <- rbind(x_train, x_test)
y_data <- rbind(y_train, y_test)
subject_data <- rbind(subject_train, subject_test)

test = readLines("./features.txt")
colnames(x_data)= features
