
##Code Book for Course Project
Overview
Source of the original data:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
Full Description at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
Process
The script run_analysis.R performs the following process to clean up the data and create tiny data sets:

Merge the training and test sets to create one data set.

Reads features.txt and uses only the measurements on the mean and standard deviation for each measurement.

Reads activity_labels.txt and applies human readable activity names to name the activities in the data set.

Labels the data set with descriptive names. (Names are converted to lower case; underscores and brackets are removed.)

Merges the features with activity labels and subject IDs. The result is saved as tidyData.txt.

The average of each measurement for each activity and each subject is merged to a second data set. The result is saved as tidyData2.txt.

Variables
testData - table contents of test/X_test.txt
trainData - table contents of train/X_train.txt
X - Measurement data. Combined data set of the two above variables
testSub - table contents of test/subject_test.txt
trainSub - table contents of test/subject_train.txt
S - Subjects. Combined data set of the two above variables
testLabel - table contents of test/y_test.txt
trainLabel - table contents of train/y_train.txt
Y - Data Labels. Combined data set of the two above variables.
featuresList - table contents of features.txt
features - Names of for data columns derived from featuresList
keepColumns - logical vector of which features to use in tidy data set
activities - table contents of activity_labels.txt. Human readable
tidyData - subsetted, human-readable data ready for output according to project description.
uS - unique subjects from S
nS - number of unique subjects
nA - number of activities
nC - number of columns in tidyData
td - second tiny data set with average of each variable for each activity and subject
Output
tidyData.txt
tidyData.txt is a 10299x68 data frame.

The first column contains subject IDs.
The second column contains activity names.
The last 66 columns are measurements.
Subject IDs are integers between 1 and 30.





