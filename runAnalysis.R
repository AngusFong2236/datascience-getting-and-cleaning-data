
runAnalysis <- function() {
  

  filePath <- function(...) { paste(..., sep = "/") }

  downloadData <- function() {
  # download datafile if it not already exist
  
  
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    downloadDir <- "c:/data"

    zipFile <- filePath(downloadDir, "dataset.zip")
    if(!file.exists(zipFile)) { download.file(url, zipFile, method = "curl") }
     

    dataDir <- "c:/data"
    if(!file.exists(dataDir)) { unzip(zipFile, exdir = ".") }

    dataDir
  }

  dataDir <- downloadData()

  # Merge 2 datasets (training, test) to merged dataset.


  XTrain <<- read.table("train/X_train.txt")
  XTest  <<- read.table("test/X_test.txt")
  merged <- rbind(XTrain, XTest)


  featureNames <- read.table("features.txt")[, 2]
  names(merged) <- featureNames

  # Extract only the measurements on the mean and standard deviation for each measurement.
  # Limit to columns with feature names matching mean() or std():
  matches <- grep("(mean|std)\\(\\)", names(merged))
  limited <- merged[, matches]

  # Use descriptive activity names to name the activities in the data set.
  # Get the activity data and map to nicer names:

  yTrain <- read.table("train/y_train.txt")
  yTest  <- read.table("test/y_test.txt")
  yMerged <- rbind(yTrain, yTest)[, 1]

  activityNames <-
    c("Walking", "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying")
  activities <- activityNames[yMerged]

  # Appropriately label the data set with descriptive variable names.
  # Change t to Time, f to Frequency, mean() to Mean and std() to StdDev
  # Remove extra dashes and BodyBody naming error from original feature names
  names(limited) <- gsub("^t", "Time", names(limited))
  names(limited) <- gsub("^f", "Frequency", names(limited))
  names(limited) <- gsub("-mean\\(\\)", "Mean", names(limited))
  names(limited) <- gsub("-std\\(\\)", "StdDev", names(limited))
  names(limited) <- gsub("-", "", names(limited))
  names(limited) <- gsub("BodyBody", "Body", names(limited))

  # Add activities and subject with nice names
  
  subjectTrain <- read.table("train/subject_train.txt")
  subjectTest  <- read.table("test/subject_test.txt")
  
  subjects <- rbind(subjectTrain, subjectTest)[, 1]

  tidy <- cbind(Subject = subjects, Activity = activities, limited)

  # Create a second, independent tidy data set with the average of each variable for each activity and each subject.
 # library(plyr)
  # Column means for all but the subject and activity columns
  limitedColMeans <- function(data) { colMeans(data[,-c(1,2)]) }
  tidyMeans <- ddply(tidy, .(Subject, Activity), limitedColMeans)
  names(tidyMeans)[-c(1,2)] <- paste0("Mean", names(tidyMeans)[-c(1,2)])

  # Write file
  write.table(tidyMeans, "tidy_projWork.txt", row.names = FALSE)

}


