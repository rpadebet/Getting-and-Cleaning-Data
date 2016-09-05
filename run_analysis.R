# Load libraries
library(dplyr)
library(tidyr)

# Download the necessary files
file_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
dfile_name <-"Dataset.zip"

# download.file(file_url,dfile_name)   #Used to download the file to local directory
unzip(dfile_name)

# Load the Train Files
X_train <- tbl_df(read.table(file = "./UCI HAR Dataset/train/X_train.txt", header = FALSE))
y_train<-tbl_df(read.table(file = "./UCI HAR Dataset/train/y_train.txt", header = FALSE))
subject_train <- tbl_df(read.table(file = "./UCI HAR Dataset/train/subject_train.txt", header = FALSE))

# Load the Test Files
X_test <- tbl_df(read.table(file = "./UCI HAR Dataset/test/X_test.txt", header = FALSE))
y_test<-tbl_df(read.table(file = "./UCI HAR Dataset/test/y_test.txt", header = FALSE))
subject_test <- tbl_df(read.table(file = "./UCI HAR Dataset/test/subject_test.txt", header = FALSE))

# Load the feature names and activity names files
features <- tbl_df(read.table(file = "./UCI HAR Dataset/features.txt", header = FALSE))
activity_labels <- tbl_df(read.table(file = "./UCI HAR Dataset/activity_labels.txt", header = FALSE))

# Merging the Train and Test Datasets
X <- bind_rows(X_train,X_test)
y <- bind_rows(y_train,y_test)
subject <- bind_rows(subject_train,subject_test)

# Combining the Feature Values dataset (X) with Activity(y) and Subject(subject) datasets
DataSetMerged <- bind_cols(subject,y,X)

# Finding only columns with mean() or std() in features.txt
meanIdx <- grep(pattern = "mean()",as.character(features$V2),value = FALSE,fixed = TRUE)
stdIdx <- grep(pattern = "std()",as.character(features$V2),value = FALSE,fixed = TRUE)
Idx <- sort(c(meanIdx,stdIdx),decreasing = FALSE)

# Subsetting the Feature Values dataset(X)    
features_filtered <- features[Idx,]
X_filt <- select(X,Idx)

# Naming Activities with appropriate names using Activities.txt
y_desc <- left_join(y,activity_labels,"V1")

# Naming the datasets with descriptive variable names
names(X_filt)<-as.character(features_filtered$V2)
names(y_desc)<-c("ActivityId","Activity")
names(subject) <- c("Subject")

# Combining the datasets and creating a Data set with descriptive variable names
DataSetMergedNamed <- bind_cols(subject,y_desc,X_filt)

# Removing Activity Id Column
# Reshaping the dataset to variable(Feature) vs variable Value - long form
# Grouping the data set by Subject, Activity and Feature
# Calculating the Average of each such group
DataTidy<-DataSetMergedNamed%>%
    select(-ActivityId)%>%
    gather(key = Feature ,value = VariableValue, - Subject, -Activity)%>%
    mutate(Feature = as.factor(Feature))%>%
    group_by(Subject,Activity,Feature)%>%
    summarize(Average = mean(VariableValue))

# Summarizing the data
print(summary(DataTidy))

# Saving the tidy data to text and csv file
write.table(DataTidy,file = "./TidyData.txt",col.names = TRUE)
write.csv(DataTidy,file = "./TidyData.csv")

# Cleaning up by removing all environment variables
# rm(list = ls())
