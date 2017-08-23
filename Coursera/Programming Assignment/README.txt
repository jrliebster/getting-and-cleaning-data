==================================================================
Human Activity Recognition Using Smartphones Dataset
Version 1.0
==================================================================
Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
Smartlab - Non Linear Complex Systems Laboratory
DITEN - Universit? degli Studi di Genova.
Via Opera Pia 11A, I-16145, Genoa, Italy.
activityrecognition@smartlab.ws
www.smartlab.ws
==================================================================

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 


The dataset includes the following files:
=========================================

- 'README.txt'

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

- 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 

- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 

- 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 

Analysis: 

-Merge the training and the test sets to create one data set.
-Extract only the measurements on the mean and standard deviation for each measurement.
-Use descriptive activity names to name the activities in the data set
-Appropriately label the data set with descriptive variable names.
-From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.

The code executes the following steps:
1.	Uses cbind to merge the “train” and “test” datasets, adding column headers and subject and activity identifiers to the larger dataset from the “features” and “activity labels” files.
2.	Extract only the observations on the mean and standard deviation for each variable by selecting only variables with names that contain “std” or “mean”.
3.	Name the six activities identified in the dataset, matching the numeric identifier in the larger dataset to the activity names in the “activity labels” files.
4.	Labels the data with descriptive names by replacing any non-descriptive variable name components with descriptive ones.
a.	Replaces "acc" with "acceleration”
b.	Replaces "^t" with "time"
c.	Replaces “^f" with "frequency"
d.	Replaces "bodybody" with "body"
e.	Replaces "mag" with "magnitude”
5.	Creates an independent tidy dataset with the average of each variable for each activity and subject.  
a.	The tidy dataset groups subject identifier and activity, resulting in 180 observations of 89 variables (so there is one mean for each of the 30 subjects and their 6 activities each). 


License:
========
Use of this dataset in publications must be acknowledged by referencing the following publication [1] 

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.
