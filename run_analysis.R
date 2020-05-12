#Getting and cleaning data
#This program attempts to achieve the following goals
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# NOTE- In order to save time i have unpacked the "getdata_projectfiles_UCI HAR Dataset.zip" directly into my working directory .
#packeges needed to be installed are
#"data.table", "dplyr"
library(data.table)
library(dplyr)

# Load activity labels + features
activityLabels<-fread("UCI HAR Dataset/activity_labels.txt",col.names = c("classLabels","activityName"))
#since i unziped the given "getdata_projectfiles_UCI HAR Dataset.zip" zipfile directly into my working directory
#i can just use 'fread' to read data into desired dataset without using file.path

features<-fread("UCI HAR Dataset/features.txt",col.names = c("index","featureNames"))

#we only need mean and std variable among all the different types of variables stored in dataset "features"
featuresWanted<-grep("(mean|std)\\(\\)", features$featureNames)
measurements <- features[featuresWanted, featureNames] #"measurements only contains mean and std variables of features
measurements<-gsub("[()]","",measurements)

# Load train datasets
train <- fread( "UCI HAR Dataset/train/X_train.txt")[, featuresWanted, with = FALSE]
setnames(train,colnames(train),measurements) #setnames sets the values in train dataset along the variables in measurement dataset
#now to read train activities and subject numbers and bind them in train dataset
trainActivities <- fread( "UCI HAR Dataset/train/Y_train.txt", col.names = c("Activity"))
trainSubjects <- fread( "UCI HAR Dataset/train/subject_train.txt", col.names = c("SubjectNum"))
train <- cbind(trainSubjects, trainActivities, train)

#same procedure for test dataset
test <- fread("UCI HAR Dataset/test/X_test.txt")[, featuresWanted, with = FALSE]
data.table::setnames(test, colnames(test), measurements)
testActivities <- fread("UCI HAR Dataset/test/Y_test.txt", col.names = c("Activity"))
testSubjects <- fread("UCI HAR Dataset/test/subject_test.txt", col.names = c("SubjectNum"))
test <- cbind(testSubjects, testActivities, test)

#now that we have both train and test dataset with only mean and std variables we can bind them
merged<-rbind(train,test)
merged$Activity<-factor(merged$Activity,levels = activityLabels$classLabels,labels = activityLabels$activityName)

#The output of merging the two dataset in accordance to the activity as levels and activity names as labels should look like this 
#SubjectNum Activity tBodyAcc-mean-X tBodyAcc-mean-Y tBodyAcc-mean-Z tBodyAcc-std-X tBodyAcc-std-Y tBodyAcc-std-Z
#1:          1 STANDING       0.2885845     -0.02029417      -0.1329051     -0.9952786     -0.9831106     -0.9135264
 
#now that we have succesfully merged the dataset we need to create a second independent tidy dataset with avg 
#of each variable
merged2<-merged%>%group_by(Activity,SubjectNum)%>%mutate_at(vars(3:66),list(~mean(.)))
#merged2 conatins the required the dataset so we will store it in a text file called "tidyData.txt"
file.create("tidyData.txt")
fwrite(merged2,file = "tidyData.txt",quote = FALSE)



