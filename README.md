## Getting-and-cleaning-data-hw
this is for the assignment

## import packages needed and set the workpath
library("plyr")

library("data.table")

library("reshape2")

file.path <- "/Users/emmasun/Desktop/data science/cleaning data"

setwd(file.path)

## this below is the requirement of the assignment:

Create one R script called run_analysis.R that does the following:

1. Merges the training and the test sets to create one data set.

2. Extracts only the measurements on the mean and standard deviation for each measurement.

3. Uses descriptive activity names to name the activities in the data set

4. Appropriately labels the data set with descriptive activity names.

5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.




## Load activity labels and data colnames

activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]

featuresname <- read.table("./UCI HAR Dataset/features.txt")[,2]

## Extract only the mean and std

extract_features <- grepl("mean|std", featuresname)

# Load X_test and y_test data

X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")

y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")

subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

names(X_test) = featuresname

# Extract only the mean and std

X_test = X_test[,extract_features]

# Load activity labels

y_test[,2] = activity_labels[y_test[,1]]

names(y_test) = c("activity_num", "activity_Label")

names(subject_test) = "subject"

# Bind test data

test_data <- cbind(as.data.table(subject_test), y_test, X_test)

# Load X_train and y_train data.

X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")

y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")

subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

names(X_train) = featuresname

# Extract only the mean and std

X_train = X_train[,extract_features]

# Load activity data

y_train[,2] = activity_labels[y_train[,1]]

names(y_train) = c("activity_num", "activity_Label")

names(subject_train) = "subject"

# Bind train data

train_data <- cbind(as.data.table(subject_train), y_train, X_train)

# Merge test and train data

mergeddata = rbind(test_data, train_data)

id_labels   = c("subject", "activity_num", "activity_Label")

data_labels = setdiff(colnames(mergeddata), id_labels)

melt_data   = melt(mergeddata, id = id_labels, measure.vars = data_labels)


# Apply mean function to dataset using dcast function

tidy_data   = dcast(melt_data, subject + activity_Label ~ variable, mean)

# Uses descriptive activity names to name the activities in the data set

names(tidy_data)<-gsub("Acc", "Accelerometer", names(tidy_data))

names(tidy_data)<-gsub("Gyro", "Gyroscope", names(tidy_data))

names(tidy_data)<-gsub("BodyBody", "Body", names(tidy_data))

names(tidy_data)<-gsub("Mag", "Magnitude", names(tidy_data))

names(tidy_data)<-gsub("^t", "Time", names(tidy_data))

names(tidy_data)<-gsub("^f", "Frequency", names(tidy_data))

names(tidy_data)<-gsub("tBody", "TimeBody", names(tidy_data))

names(tidy_data)<-gsub("-mean()", "Mean", names(tidy_data), ignore.case = TRUE)

names(tidy_data)<-gsub("-std()", "STD", names(tidy_data), ignore.case = TRUE)

names(tidy_data)<-gsub("-freq()", "Frequency", names(tidy_data), ignore.case = TRUE)

#export the tidy data

write.table(tidy_data, file = "./tidy_data.txt")
