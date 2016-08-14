# Training Data Input
training = read.csv("UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
training[,562] = read.csv("UCI HAR Dataset/train/Y_train.txt", sep="", header=FALSE)
training[,563] = read.csv("UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE)

# Test Data Input
testing = read.csv("UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
testing[,562] = read.csv("UCI HAR Dataset/test/Y_test.txt", sep="", header=FALSE)
testing[,563] = read.csv("UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE)

# Activity Labels
activityLabels = read.csv("UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE)

# Calculations
features = read.csv("UCI HAR Dataset/features.txt", sep="", header=FALSE)
features[,2] = gsub('-mean', 'Mean', features[,2])
features[,2] = gsub('-std', 'Std', features[,2])
features[,2] = gsub('[-()]', '', features[,2])

# Combine Data
allData = rbind(training, testing)

# Mean and SD for each observation
cols <- grep(".*Mean.*|.*Std.*", features[,2])

# Descriptive activity = activities

features <- features[cols,]
cols <- c(cols, 562, 563)
allData <- allData[,cols]

# Add the column names to allData
colnames(allData) <- c(features$V2, "Activity", "Subject")
colnames(allData) <- tolower(colnames(allData))

currentActivity = 1
for (currentActivityLabel in activityLabels$V2) {
        allData$activity <- gsub(currentActivity, currentActivityLabel, allData$activity)
        currentActivity <- currentActivity + 1
}

allData$activity <- as.factor(allData$activity)
allData$subject <- as.factor(allData$subject)

tidy_data = aggregate(allData, by=list(activity = allData$activity, subject=allData$subject), mean)

# Delete subject and activity column.
tidy_data[,90] = NULL
tidy_data[,89] = NULL

# Write tidy_data
write.table(tidy_data, "tidy_data.txt", sep="\t", row.names=FALSE)

tidy_data
