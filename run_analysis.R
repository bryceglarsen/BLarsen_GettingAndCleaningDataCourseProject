#Step 1: Merge training and test sets to create one data set
#Read in all test data and create test data set
test_x <- read.table("./test/X_test.txt")
test_y <- read.table("./test/y_test.txt", col.names=c("activityid"))
test_subject <- read.table("./test/subject_test.txt", col.names=c("subjectid"))

test_data <- cbind(test_subject, test_y, test_x)

#Read in all training data and create training data set
train_x <- read.table("./train/X_train.txt")
train_y <- read.table("./train/y_train.txt", col.names=c("activityid"))
train_subject <- read.table("./train/subject_train.txt", col.names=c("subjectid"))

train_data <- cbind(train_subject, train_y, train_x)

#Merge data sets into one
data <- rbind(test_data, train_data)

#Step 2: Extract only the measurements on the mean and
#the standard deviation for each measurement

#Read in all features
features <- read.table("features.txt", col.names=c("featureid", "featurelabel"))
#Extract features of interest (mean/std)
keyfeatures <- features[grepl("mean\\(\\)", features$featurelabel) | grepl("std\\(\\)", features$featurelabel), ]

datafiltered <- data[, c(c(1,2), keyfeatures$featureid + 2)]

#Rename feature columns in dataset with meaningful text


#Step 3: Name activities with descriptive activity text.
#Read in activities
activities <- read.table("activity_labels.txt", col.names=c("activityid", "activitylabel"))
#Create new column and match activity ID in existing data table
#on activity ID and return activity label in new column "activity label"
data2 <- datafiltered
data2$activitylabel <- activities[match(datafiltered$activityid, activities$activityid), 2]

#Move activity label to front of data set
col_idx <- grep("activitylabel", names(data2))
data2 <- data2[, c(col_idx, (1:ncol(data2))[-col_idx])]
#Reorder as subjectid, activityid, activitylabel, features
data2 <- data2[, c(2, 3, 1, (4:ncol(data2)))]


#Step 4: Label the data set with descriptive variable names. 
data3 <- data2

#Clean up feature labels
keyfeatures$featurelabel = gsub("\\(\\)", "", keyfeatures$featurelabel)
keyfeatures$featurelabel = gsub("Acc", "Acceleration", keyfeatures$featurelabel)
keyfeatures$featurelabel = gsub("Mag", "Magnitude", keyfeatures$featurelabel)
keyfeatures$featurelabel = gsub("BodyBody", "Body", keyfeatures$featurelabel)

#Reassign data column names with cleaned feature labels
for (i in 1:length(keyfeatures$featurelabel)) {
  colnames(data3)[i + 3] <- keyfeatures$featurelabel[i]
}

#Step 5: Create a second, independent tidy data set with the average of each variable for each activity and each subject.

#Calculate mean of each feature for each subject
tidydataset <- aggregate(. ~subjectid + activityid + activitylabel, data3, mean)

#Write to file
write.table(tidydataset, "tidydataset.csv", row.names = FALSE)