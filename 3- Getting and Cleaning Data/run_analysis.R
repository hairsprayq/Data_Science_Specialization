#Merges the training and the test sets to create one data set.

#Extracts only the measurements on the mean and standard deviation for each measurement. 

#Uses descriptive activity names to name the activities in the data set

#Appropriately labels the data set with descriptive variable names. 

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Load libraries.

library(dplyr)
library(tidyr)

# Download data set.

filename <- "dataFiles.zip"
path <- getwd()

# Checking if file already exists.
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}  

# Checking if folder exists
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

# Read data set.

x_test <- read.delim(text = gsub("  ", " ", readLines("UCI HAR Dataset/test/X_test.txt")), 
                     header= FALSE, sep = " ", dec = ".")
x_train <- read.delim(text = gsub("  ", " ", readLines("UCI HAR Dataset/train/X_train.txt")), 
                      header= FALSE, sep = " ", dec = ".")
y_test <- read.delim("UCI HAR Dataset/test/y_test.txt", 
                     header= FALSE)
y_train <- read.delim("UCI HAR Dataset/train/y_train.txt", 
                      header= FALSE)
subject_test <- read.delim("UCI HAR Dataset/test/subject_test.txt", 
                           header= FALSE)
subject_train <- read.delim("UCI HAR Dataset/train/subject_train.txt", 
                            header= FALSE)
col_names_df <- read.delim("UCI HAR Dataset/features.txt", 
                           header = FALSE, sep = " ")

# Merge label data and name column.

label <- bind_rows(y_test,y_train)
colnames(label) <- ("Activity")

# Merge subject data and name column.
subject <- bind_rows(subject_test,subject_train)
colnames(subject) <- "Subject"

# Merge data set, name columns & select measurements on mean and standard deviation.
df <- bind_rows(x_test,x_train)
df[1] <- NULL
col_names <- unlist(col_names_df[,2])
colnames(df) <- col_names
df <- df %>% select(contains("mean()") | contains("std()"))

# Merge all data.

df <- bind_cols(subject,label,df)

# Use descriptive activity names

df$Activity <- recode(as.character(df$Activity), "1" = "WALKING", "2" = "WALKING UPSTAIRS",
                      "3" = "WALKING DOWNSTAIRS", "4" = "SITTING",
                      "5" = "STANDING", "6"= "LAYING")

# Appropriately label the data set with descriptive variable names

names(df)<-gsub("Acc", "Accelerometer", 
           gsub("tBody", "TimeBody",
           gsub("mean()", "Mean",
           gsub("std()", "Standard Deviation",
           gsub("Gyro", "Gyroscope",
           gsub("Mag", "Magnitude",
           gsub("^t", "Time",
           gsub("^f", "Frequency",
           gsub("BodyBody", "Body",
             names(df))))))))))

# Create independent tidy data set with the average of each variable for each activity and each subject. 
tidy_data<- df %>%
  group_by(Subject, Activity) %>%
  summarise_all(mean)
write.table(tidy_data, "TidyData.txt", row.name=FALSE)