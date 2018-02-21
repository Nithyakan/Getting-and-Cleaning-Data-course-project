#PEER REVIEWED ASSIGNMENT
#Feb 17, 2018
getwd()
setwd("C:/Users/nithyak/Desktop/COURSERA/Getting_and_cleaning_data/WK_4/PEER REVIEWED ASSIGNMENT/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset")
getwd()

#READ IN DATA
subject_train <- read.table("./train/subject_train.txt")
X_train <- read.table("./train/X_train.txt")
y_train <- read.table("./train/y_train.txt")


subject_test <- read.table("./test/subject_test.txt")
X_test <- read.table("./test/X_test.txt")
y_test <- read.table("./test/y_test.txt")

#STEP 1: 
#Merge training and test datasets
#BIND columns of training datasets together
train_dataset <- cbind(subject_train, y_train, X_train)

#BIND columns of test datasets together
test_dataset <- cbind(subject_test, y_test, X_test)

#BIND rows of training & test datasets together
final_dataset <- rbind(test_dataset, train_dataset)

#STEP 2:
#Extract only the measurements on the mean and standard deviation for each measurement
subdataset <- final_dataset[ ,c(1:8, 43:48, 83:88, 123:128, 163:168, 203:204, 216:217, 229:230, 242:243, 255:256, 268:273, 347:352, 426:431, 505:506,
                                518:519, 531:532, 544:545)]

#STEP 3:
#Use descriptive activity names to name the activities in the data set
activity <- read.table("./activity_labels.txt")
mergeddataset <- merge(subdataset, activity, by.x = "V1.1", by.y = "V1")

#Remove column containing activity_labels
mergeddataset1 <- select(mergeddataset,V1,V2.y, V1.2, V2.x,V3:V6,V41:V46, V81:V86,V121:V126, V161:V166, V201:V202, V214:V215, V227:V228, V240:V241, V253:V254, 
                         V266:V271, V345:V350, V424:V429, V503:V504, V516:V517, V529:V530,V542:V543)

#STEP 4:
#Appropriately label the data set with descriptive variable names.
colnames(mergeddataset1) <- c("subject", "activity", "tBodyAccMeanX", "tBodyAccMeanY", "tBodyAccMeanZ", "tBodyAccSDX", "tBodyAccSDY", "tBodyAccSDZ","tGravityAccMeanX",
                              "tGravityAccMeanY", "tGravityAccMeanZ","tGravityAccSDX", "tGravityAccSDY", "tGravityAccSDZ", "tBodyAccJerkMeanX", "tBodyAccJerkMeanY",
                              "tBodyAccJerkMeanZ", "tBodyAccJerkSDX", "tBodyAccJerkSDY", "tBodyAccJerkSDZ", "tBodyGyroMeanX", "tBodyGyroMeanY", "tBodyGyroMeanZ",
                              "tBodyGyroSDX", "tBodyGyroSDY", "tBodyGyroSDZ", "tBodyGyroJerkMeanX", "tBodyGyroJerkMeanY", "tBodyGyroJerkMeanZ", "tBodyGyroJerkSDX", 
                              "tBodyGyroJerkSDY", "tBodyGyroJerkSDZ", "tBodyAccMagMean", "tBodyAccMagSD", "tGravityAccMagMean", "tGravityAccMagSD", "tBodyAccJerkMagMean",
                              "tBodyAccJerkMagSD", "tBodyGyroMagMean","tBodyGyroMagSD","tBodyGyroJerkMagMean","tBodyGyroJerkMagSD","fBodyAccMeanX","fBodyAccMeanY",
                              "fBodyAccMeanZ", "fBodyAccSDX","fBodyAccSDY","fBodyAccSDZ","fBodyAccJerkMeanX","fBodyAccJerkMeanY","fBodyAccJerkMeanZ","fBodyAccJerkSDX",
                              "fBodyAccJerkSDY","fBodyAccJerkSDZ","fBodyGyroMeanX","fBodyGyroMeanY","fBodyGyroMeanZ","fBodyGyroSDX","fBodyGyroSDY",
                              "fBodyGyroSDZ","fBodyAccMagMean","fBodyAccMagSD","fBodyBodyAccJerkMagMean","fBodyBodyAccJerkMagSD","fBodyBodyGyroMagMean",
                              "fBodyBodyGyroMagSD","fBodyBodyGyroJerkMagMean","fBodyBodyGyroJerkMagSD")

#STEP 5:
#From the data set in step 4, creates a second, independent tidy data set with the average 
#of each variable for each activity and each subject.

mergeddataset2 <- arrange(mergeddataset1, subject)

mergeddatasetfinal <- mergeddataset2 %>%
         group_by(subject, activity) %>%
         summarise_at(c("tBodyAccMeanX", "tBodyAccMeanY", "tBodyAccMeanZ", "tBodyAccSDX", "tBodyAccSDY", "tBodyAccSDZ","tGravityAccMeanX", "tGravityAccMeanY", 
                        "tGravityAccMeanZ","tGravityAccSDX", "tGravityAccSDY", "tGravityAccSDZ", "tBodyAccJerkMeanX", "tBodyAccJerkMeanY", "tBodyAccJerkMeanZ", 
                        "tBodyAccJerkSDX", "tBodyAccJerkSDY", "tBodyAccJerkSDZ", "tBodyGyroMeanX", "tBodyGyroMeanY", "tBodyGyroMeanZ", "tBodyGyroSDX", 
                        "tBodyGyroSDY", "tBodyGyroSDZ", "tBodyGyroJerkMeanX", "tBodyGyroJerkMeanY", "tBodyGyroJerkMeanZ", "tBodyGyroJerkSDX",
                        "tBodyGyroJerkSDY", "tBodyGyroJerkSDZ", "tBodyAccMagMean", "tBodyAccMagSD", "tGravityAccMagMean", "tGravityAccMagSD", 
                        "tBodyAccJerkMagMean", "tBodyAccJerkMagSD", "tBodyGyroMagMean","tBodyGyroMagSD","tBodyGyroJerkMagMean","tBodyGyroJerkMagSD",
                        "fBodyAccMeanX","fBodyAccMeanY","fBodyAccMeanZ", "fBodyAccSDX","fBodyAccSDY","fBodyAccSDZ","fBodyAccJerkMeanX","fBodyAccJerkMeanY",
                        "fBodyAccJerkMeanZ","fBodyAccJerkSDX", "fBodyAccJerkSDY","fBodyAccJerkSDZ","fBodyGyroMeanX","fBodyGyroMeanY","fBodyGyroMeanZ",
                        "fBodyGyroSDX","fBodyGyroSDY", "fBodyGyroSDZ","fBodyAccMagMean","fBodyAccMagSD","fBodyBodyAccJerkMagMean","fBodyBodyAccJerkMagSD",
                        "fBodyBodyGyroMagMean", "fBodyBodyGyroMagSD","fBodyBodyGyroJerkMagMean","fBodyBodyGyroJerkMagSD"), mean, na.rm = TRUE )


write.table(mergeddatasetfinal, "./finalfile.txt", sep="\t")
















