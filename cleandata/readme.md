
## Getting and Cleaning Data, Samsung Dataset

# This R Script prepares the Samsung Dataset for further analysis

It includes the following steps:

Set up the enviroment

Install the necessary packages plyr and reshape2

Dowload the data file in fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

Unzip the folder which is after downloading already in your work directory

Load test, training and name data

Filtering columns with mean and standard deviation in name

Replaces the activity codes with descriptive names

Creates a tidy data set with the average of each variable for each activity and each subject 


