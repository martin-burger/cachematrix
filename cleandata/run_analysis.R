

##install.packages("download.file")

library(downloader)
library(plyr)
library(reshape2)


##fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"


## download.file(fileURL, destfile ="Dataset.zip")


# Load test, training and name data

test_table_x <- read.table("Dataset/UCI HAR Dataset/test/X_test.txt")
test_table_y <- read.table("Dataset/UCI HAR Dataset/test/y_test.txt")
test_subject <- read.table("Dataset/UCI HAR Dataset/test/subject_test.txt")

train_table_X <- read.table("Dataset/UCI HAR Dataset/train/X_train.txt")
train_table_Y <- read.table("Dataset/UCI HAR Dataset/train/y_train.txt")
train_subject <- read.table("Dataset/UCI HAR Dataset/train/subject_train.txt")

names_X <- read.table("Dataset/UCI HAR Dataset/features.txt")
names_activity <- read.table("Dataset/UCI HAR Dataset/activity_labels.txt")


# some data type transformation

names_activity$V1 <- as.factor(names_activity$V1)
names_activity$V2 <- as.character(names_activity$V2)

names(test_table_x) <- names_X$V2
names(test_table_y) <- "Activity_Code"
names(test_subject) <- "Subject"

names(train_table_X) <- names_X$V2
names(train_table_Y) <- "Activity_Code"
names(train_subject) <- "Subject"

test_table <- cbind(test_subject,test_table_y,test_table_x)
train_table <- cbind(train_subject,train_table_Y,train_table_X)

table_all <- rbind(test_table,train_table)

# get onley columns with mean or standard deviation in name

ptn <- "mean\\(\\)|std\\(\\)|Subject|Activity_Code"

table_Sample <- table_all[1:2,]
table_Sample_2 <- table_Sample[grepl(ptn,names(table_Sample))]

table_filtered <- table_all[grepl(ptn,names(table_Sample))]

# add descriptive activitiy names

table_names_activity <- table_filtered
x <- cbind(activity_Names$V1, activity_Names$V2)
table_names_activity$Activity_Code <- x[table_names_activity$Activity_Code,2]
colnames(table_names_activity)[2] <- "Activity_Name"

#create a tidy data set

table_Melt <- melt(table_names_activity,id=c("Subject","Activity_Name"))
Tidy_table <- dcast(table_Melt,Subject + Activity_Name ~ variable,mean)
write.table(Tidy_table, "tidy_dataset.txt")





