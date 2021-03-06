# Codebook
This document describes the data, the variables and code run to clean up the raw data and then run the data analyses (code contained in the run_analysis.R script).

## Dataset
The Human Activity Recognition Using Smartphones Dataset (Version 1.0) has data collected from 30 subjects (19-48 years) performing activities of daily living (ADL). Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a Samsung Galaxy S smartphone that has embedded sensor signals (accelerometer and gyroscope) to capture the activities of the subjects. The accelerometer captures the 3-axial linear acceleration while the gyroscope captures 3-axial angular velocity. The dataset is randomly divided into 1) training data (70% of subjects) 2) test data (30% of subjects)
Each record in the dataset contains: 
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

## Variables
We considered the following files for our data analyses:
- 'train/X_train.txt': Training set.
- 'train/y_train.txt': Training labels.
- 'train/subject_train.txt': Each row identifies the subject who performed the activity for the  training sample. Its range is from 1 to 30. 

- 'test/X_test.txt': Test set.
- 'test/y_test.txt': Test labels.
- 'test/subject_test.txt': Each row identifies the subject who performed the activity for the test sample. Its range is from 1 to 30.
- 'activity_labels.txt': Links the class labels with their activity name. Labelled as follows: 
1 WALKING
2 WALKING_UPSTAIRS
3 WALKING_DOWNSTAIRS
4 SITTING
5 STANDING
6 LAYING
 
## Describing the code in run_analysis.R script
First we read in above txt files using ‘read.table’ command.

### STEP 1: To merge training and test datasets
Used ‘cbind’ to bind columns of training datasets together: subject_train, y_train, X_train; to bind columns of test datasets together: subject_test, y_test, X_test
Used ‘rbind’ to bind rows of training & test datasets together to result in final_dataset

### STEP 2: To extract only the measurements on the mean and standard deviation for each measurement
Subsetted the columns with variables that calculate mean and standard deviation to create subdataset using below command:
subdataset <- final_dataset[ ,c(1:8, 43:48, 83:88, 123:128, 163:168, 203:204, 216:217, 229:230, 242:243, 255:256, 268:273, 347:352, 426:431, 505:506, 518:519, 531:532, 544:545)]

### STEP 3: To use descriptive activity names to name the activities in the data set
Used ‘read.table’ to read in activity_labels file and stored in activity object.
Merged activity dataframe with subdataset data frame by using common class activity labels in V1 and V1.1 columns in respective data frames to create mergeddataset that has 69 columns (includes activity).
Installed the packages ‘ply’ and ‘dplyr’. Used library command to get the packages in current R session.
Removed the column containing activity_labels (V1.1) from mergeddataset by selecting only the columns that we need from the mergeddataset (used ‘select’ command) and stored in mergeddataset1 object (68 columns). 

### STEP 4: To label the data set appropriately with descriptive variable names
Used ‘colnames’ function to set the column names of mergeddataset1

### STEP 5: To create a second, independent tidy data set with the average of each variable for each activity and each subject from the dataset in STEP 4
Used ‘arrange’ command to order the mergeddataset1 by subject and stored in mergeddataset2.
Used ‘dim’ function on mergeddataset2; mergeddataset2 has 10,299 rows & 68 columns.
Used ‘group_by’ function on mergeddataset2 to sort first by ‘subject’ column and then by ‘activity’ column and then passed (using chain operator %>%) the resulting data frame to ‘summarise_at’ function to calculate mean on each set of values of each variable for each activity & each subject (subject-activity pair) and stored this tidy dataset in mergeddatasetfinal
Used ‘dim’ function on mergeddatasetfinal; mergeddatasetfinal has 180 rows & 68 columns with one observation per row (only one observation for each subject-activity pair) and one variable per column and hence a tidy dataset.

### STEP 6: To export the final tidy dataset from step 5
Used ‘write.table’ function to write the mergeddatasetfinal data frame (tidy dataset) into a tab delimited text file named finalfile.txt in my current working directory.



