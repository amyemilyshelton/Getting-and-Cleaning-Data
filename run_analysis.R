#Final Project Week 4, Getting and Cleaning Data
library(dplyr)

#Step before step 1
#Unzip the files, creates a Dataset directory
unzip("UCI_HARDataset.zip") 

############################################
#Step 1, merge the training and test data set to create one data set for all 30 subjects
############################################
#My working directory is .../UCI HAR Dataset
#Read the test data files into R
#The X-test file contains the test measurement summaries
#The y-test file contains the activity performed for the test measurement
#The subject_test file identities the subject number 1-21

test_values = read.table(file.path("test", "X_test.txt"))
test_activity = read.table(file.path("test", "y_test.txt"))
test_subject = read.table(file.path("test", "subject_test.txt"))

#Read the train data files into R, same contents as test subjects, 
#x- measurements, y- activity performed 1-6, subject- identifies which subject it was 22-30

train_values = read.table(file.path("train", "X_train.txt"))
train_activity = read.table(file.path("train", "y_train.txt"))
train_subject = read.table(file.path("train", "subject_train.txt"))


#Read the features file into R, features identifies what the columns are, column names
features = read.table("features.txt", as.is = TRUE)

#Read the activity labels into R and add column names
labels = read.table("activity_labels.txt")
colnames(labels) <- c("activityid","activity")

#Create a single data file from the 8 files
test <- cbind(test_subject, test_activity, test_values)
train <- cbind(train_subject, train_activity, train_values)

#combine test and train subject data
activity <- rbind(train , test)

#Name the columns in the combined file
colnames(activity) <- c("subject", "activity", features[, 2])

#####################################
#Step 2 extract only the measurements on the mean and standard deviation for each measurement
#####################################
#Create the new file with the average and standard deviation measurements
#Use grep1 on the column names to find the mean and standard deviation columns

msd_dataset <- grepl("subject|activity|mean|std", colnames(activity))

#keep data in these columns only, save to a new file
Activity <- activity[, msd_dataset]

######################################
#Step 3, use descriptive activity names to name the activities in the data set
######################################
# replace activity values 1-6 with named factor labels
Activity$activity <- factor(Activity$activity,levels = labels[, 1], labels = labels[, 2])

#####################################
#Step 4, appropriately label the data set with descriptive variable names
#####################################

# get column names
ActivityCols <- colnames(Activity)

# remove special characters, the - and () and replace it with nothing
ActivityCols <- gsub("[\\(\\)-]", "", ActivityCols)

# make the names more readable (descriptive)
ActivityCols <- gsub("^f", "frequencyDomain", ActivityCols)
ActivityCols <- gsub("^t", "time", ActivityCols)
ActivityCols <- gsub("Acc", "Accelerometer", ActivityCols)
ActivityCols <- gsub("Gyro", "Gyroscope", ActivityCols)
ActivityCols <- gsub("Mag", "Magnitude", ActivityCols)
ActivityCols <- gsub("Freq", "Frequency", ActivityCols)
ActivityCols <- gsub("mean", "Mean", ActivityCols)
ActivityCols <- gsub("std", "StandardDeviation", ActivityCols)
#Fix an error
ActivityCols <- gsub("BodyBody", "Body", ActivityCols)

# use new labels as column names
colnames(Activity) <- ActivityCols

##############################
#Step 5, create a second independent tidy data set with the average
#of each variable for each activity and each subject, using #4's dataset
##############################

# group by subject and activity and summarise using mean
ActivityMeans <- Activity %>% 
  group_by(subject, activity) %/%
  summarise(funs(mean))

# output to file "tidy_data.txt"
write.table(ActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)


