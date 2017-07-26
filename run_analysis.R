###  1.Merges the training and the test sets to create one data set

setwd("C:/Users/xrma/Desktop/UCI HAR Dataset")
obs_test <- read.table(".//test/X_test.txt")
obs_train <- read.table(".//train/X_train.txt")

library(tidyr)
library(dplyr)

obs_tot <- bind_rows(obs_test,obs_train)   ###resulting dataframe "obs_tot"



###  2.Extracts only the measurements on the mean and standard deviation for each measurement. 

features <- read.table("features.txt")
mean_ind <- grep("mean",features$V2)
std_ind <- grep("std",features$V2)

mean_std <- select(obs_tot,mean_ind,std_ind)   ###resulting selected columns of mean & std



###  3.Uses descriptive activity names to name the activities in the data set

activity_labels <- read.table("activity_labels.txt")

label_test <- read.table(".//test/y_test.txt")
label_train <- read.table(".//train/y_train.txt")

label_tot <- bind_rows(label_test,label_train)
names(label_tot) <- "activities"

for(i in 1:length(label_tot[,1])){
  if(label_tot[i,1]==1){
    label_tot[i,1] <- "WALKING"
  }else if(label_tot[i,1]==2){
    label_tot[i,1] <- "WALKING_UPSTAIRS"
  }else if(label_tot[i,1]==3){
    label_tot[i,1] <- "WALKING_DOWNSTAIRS"
  }else if(label_tot[i,1]==4){
    label_tot[i,1] <- "SITTING"
  }else if(label_tot[i,1]==5){
    label_tot[i,1] <- "STANDING"
  }else{
    label_tot[i,1] <- "LAYING"
  }
}

names(obs_tot) <- features$V2
obs_tot.name <- bind_cols(label_tot,obs_tot)   ###resulting dataset with activities' names

###   4.Appropriately labels the data set with descriptive variable names
# We can use names() <- sub/gsub() to transfer every abbreviation to more specific names
# Just skip this process, hope you can understand :)


###   5.From the data set in step 4, creates a second, independent tidy data set with 
###   the average of each variable for each activity and each subject

subject_test <- read.table(".//test/subject_test.txt")
subject_train <- read.table(".//train/subject_train.txt")
subject_tot <- bind_rows(subject_test,subject_train)
names(subject_tot) <- "subject"
data <- bind_cols(subject_tot,obs_tot.name)
tidydata <- data %>% group_by(subject,activities) %>% summarise_all(mean)
write.table(tidydata, "TidyData.txt", row.name=FALSE)
