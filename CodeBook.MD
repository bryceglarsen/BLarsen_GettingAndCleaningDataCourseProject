Step 1: Merges the training and the test sets to create one data set.
  - "/(test|train)/X_test.txt
  - "/(test|train)/y_test.txt"
  - "/(test|train)/subject_test.txt"

Step 2: Extracts only the measurements on the mean and standard deviation for each measurement.
  - Opens "features.txt" and grepl on mean/std
  - Subsets merged data on key features/measurements

Step 3: Uses descriptive activity names to name the activities in the data set.
  - Opens "activity_labels.txt"
  - Match activity ID of subset on ID of new activity labels table and add to table
  
Step 4: Appropriately labels the data set with descriptive variable names.
  - Clean up the key features using gsub
  - Loop through key features (add 3 to account for first 3 columns) and rename columns
  
Step 5: From the data set in step 4, creates a second, independent tidy data set with the 
average of each variable for each activity and each subject.
  - Open data set and use aggregate function on subject and activity ID to perform mean
  function on all other columns.
  - Write to "tidydataset.csv"
