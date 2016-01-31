#This is final project submission material
library (dplyr)
library (sqldf)

setwd("C:/Users/<NAME>/Desktop/Coursera")

##########################################
#Read in the feature labels
##########################################
{
  #Read in the features labels and strip out "variable number" attached to each variable
  features_text<-read.csv("C:/Users/<NAME>/Desktop/Coursera/UCI HAR Dataset/features.txt", sep = "\t", header = FALSE, strip.white = TRUE)
  features_text_clean<-as.data.frame(gsub("^[0-9]{1,}","",features_text$V1))
  colnames(features_text_clean)<-"feature_names"
  rm(features_text)
}

##########################################
#Read in the training set
##########################################
{
  #Read in the data using read.table, and use the carriage return as separator
  #I tried multiple functions (e.g. read.csv, read.fwf), but always had issues with bad reads or not reading the inputs into correct rows and columns
  x_train<-read.table("C:/Users/<NAME>/Desktop/Coursera/UCI HAR Dataset/train/X_train.txt", header = FALSE, sep="\r")
  x_train$V1<-as.character(x_train$V1)
  
  #Create reference variables
  train_records<-length(x_train$V1)
  train_vars<-length(features_text_clean$feature_names) 
  train_char_count<-nchar(x_train$V1[1])
  fixed_widths<-16
  
  #Create clean data frame. Im sure there is a more elegant way of doing this. However, in the interest of time, I used nested for loops to "place" each data point into its correct cell
  x_train_clean<-data.frame()
  for (i in 1:train_records){
    storage_vector<-substring(x_train$V1[i], seq(1,train_char_count,fixed_widths), seq(16,train_char_count, fixed_widths))
    storage_vector<-as.numeric(storage_vector)
    for (j in 1:train_vars){
      x_train_clean[i,j]<-storage_vector[j]
    }
  }
  colnames(x_train_clean)<-features_text_clean$feature_names
  
  #Read in the training set y values
  y_train_clean<-read.csv("C:/Users/<NAME>/Desktop/Coursera/UCI HAR Dataset/train/y_train.txt", sep = "\t", header = FALSE, strip.white = TRUE)
  colnames(y_train_clean)<-"activity"
  
  #Read in the training set subjects
  subjects_train<-read.csv("C:/Users/<NAME>/Desktop/Coursera/UCI HAR Dataset/train/subject_train.txt", sep = "\t", header = FALSE, strip.white = TRUE)
  colnames(subjects_train)<-"user"
  
  #create the tidy training set
  training<-cbind(subjects_train, y_train_clean, x_train_clean)
  
  #Clean up the workspace
  rm(x_train_clean)
  rm(y_train_clean)
  rm(subjects_train)
  rm(x_train)
  rm(y_train)
  rm(storage_vector)
  rm(i)
  rm(j)
}

##########################################
#Read in the testing set
##########################################
{
  #Read in the data using read.table, and use the carriage return as separator
  #I tried multiple functions (e.g. read.csv, read.fwf), but always had issues with bad reads or not reading the inputs into correct rows and columns
  x_test<-read.table("C:/Users/<NAME>/Desktop/Coursera/UCI HAR Dataset/test/X_test.txt", header = FALSE, sep="\r")
  x_test$V1<-as.character(x_test$V1)
  
  #Create reference variables
  test_records<-length(x_test$V1)
  test_vars<-length(features_text_clean$feature_names) 
  test_char_count<-nchar(x_test$V1[1])
  fixed_widths<-16
  
  #Create clean data frame. Im sure there is a more elegant way of doing this. However, in the interest of time, I used nested for loops to "place" each data point into its correct cell
  x_test_clean<-data.frame()
  for (i in 1:test_records){
    storage_vector<-substring(x_test$V1[i], seq(1,test_char_count,fixed_widths), seq(16,test_char_count, fixed_widths))
    storage_vector<-as.numeric(storage_vector)
    for (j in 1:test_vars){
      x_test_clean[i,j]<-storage_vector[j]
    }
  }
  colnames(x_test_clean)<-features_text_clean$feature_names
  
  #Read in the training set y values
  y_test_clean<-read.csv("C:/Users/<NAME>/Desktop/Coursera/UCI HAR Dataset/test/y_test.txt", sep = "\t", header = FALSE, strip.white = TRUE)
  colnames(y_test_clean)<-"activity"
  
  #Read in the training set subjects
  subjects_test<-read.csv("C:/Users/<NAME>/Desktop/Coursera/UCI HAR Dataset/test/subject_test.txt", sep = "\t", header = FALSE, strip.white = TRUE)
  colnames(subjects_test)<-"user"
  
  #create the tidy training set
  testing<-cbind(subjects_test, y_test_clean, x_test_clean)
  
  #Clean up the workspace
  rm(x_test_clean)
  rm(y_test_clean)
  rm(subjects_test)
  rm(x_test)
  rm(y_test)
  rm(storage_vector)
  rm(i)
  rm(j)
}

##########################################
#Bind the training and testing set
##########################################
{
  #Cleanup the workspace
  rm(fixed_widths)
  rm(test_char_count)
  rm(test_records)
  rm(test_vars)
  rm(train_char_count)
  rm(train_records)
  rm(train_vars)
  rm(features_text_clean)
  
  #create full smartphone data set. This contains ALL of the variables
  smartphone_data_full<-rbind(training,testing)
  rm(testing)
  rm(training)
}

##########################################
#Create the full tidy data set 
##########################################
{
  #Scope this down to just means and stddev variables by using regex to index the column names and then only select those columns
  index<-as.data.frame(grep("user|activity|mean|std",colnames(smartphone_data_full)))
  colnames(index)<-"index_mean_std"
  smartphone_mean_std<-smartphone_data_full[,c(index$index_mean_std)]
  rm(index)
  rm(smartphone_data_full)
  
  #Read in the activity descriptions. 
  activity_lookup<-read.csv("C:/Users/<NAME>/Desktop/Coursera/UCI HAR Dataset/activity_labels.txt",sep="\t",header=FALSE)
  activity_lookup_clean<-as.data.frame(gsub("^[0-9]{1,}","",activity_lookup$V1))
  activity_lookup_clean$activity<-c(1:6)
  colnames(activity_lookup_clean)<-c("Activity_desc", "activity")
  rm(activity_lookup)
  activity_lookup_clean #I view this and then use for loop to insert each label
  rm(activity_lookup_clean)
  
  #Add activity descriptions to the dataframe 
  for (k in 1:length(smartphone_mean_std$activity)){
    if(smartphone_mean_std$activity[k]==1){
      smartphone_mean_std$activity[k]<-"WALKING"
    }
    if(smartphone_mean_std$activity[k]==2){
      smartphone_mean_std$activity[k]<-"WALKING_UPSTAIRS"
    }
    if(smartphone_mean_std$activity[k]==3){
      smartphone_mean_std$activity[k]<-"WALKING_DOWNSTAIRS"
    }
    if(smartphone_mean_std$activity[k]==4){
      smartphone_mean_std$activity[k]<-"SITTING"
    }
    if(smartphone_mean_std$activity[k]==5){
      smartphone_mean_std$activity[k]<-"STANDING"
    }
    if(smartphone_mean_std$activity[k]==6){
      smartphone_mean_std$activity[k]<-"LAYING"
    }
  }
  rm(k)
  
  #Cleanup names in the tidy dataframe and rename as "tidy"
  name_cleanup<-colnames(smartphone_mean_std)
  name_cleanup<-gsub("[[:punct:]]","",name_cleanup)
  name_cleanup<-gsub("[[:space:]]","",name_cleanup)
  colnames(smartphone_mean_std)<-name_cleanup
  smartphone_tidy_mean_std<-smartphone_mean_std
  rm(smartphone_mean_std)
  rm(name_cleanup)
}

##########################################
#Create the summary tidy data set (means)
##########################################
{
  #Pivot on user and activity and determine means of each variable. Here I assume the instructions meant we do a basic arithmetic average on all variables (regardless of mean or std dev)
  tidy_summary = group_by(smartphone_tidy_mean_std, user, activity)
  tidy_summary = summarise(tidy_summary, 
                           Mean_tBodyAccmeanX = mean(tBodyAccmeanX),
                           Mean_tBodyAccmeanY = mean(tBodyAccmeanY),
                           Mean_tBodyAccmeanZ = mean(tBodyAccmeanZ),
                           Mean_tBodyAccstdX = mean(tBodyAccstdX),
                           Mean_tBodyAccstdY = mean(tBodyAccstdY),
                           Mean_tBodyAccstdZ = mean(tBodyAccstdZ),
                           Mean_tGravityAccmeanX = mean(tGravityAccmeanX),
                           Mean_tGravityAccmeanY = mean(tGravityAccmeanY),
                           Mean_tGravityAccmeanZ = mean(tGravityAccmeanZ),
                           Mean_tGravityAccstdX = mean(tGravityAccstdX),
                           Mean_tGravityAccstdY = mean(tGravityAccstdY),
                           Mean_tGravityAccstdZ = mean(tGravityAccstdZ),
                           Mean_tBodyAccJerkmeanX = mean(tBodyAccJerkmeanX),
                           Mean_tBodyAccJerkmeanY = mean(tBodyAccJerkmeanY),
                           Mean_tBodyAccJerkmeanZ = mean(tBodyAccJerkmeanZ),
                           Mean_tBodyAccJerkstdX = mean(tBodyAccJerkstdX),
                           Mean_tBodyAccJerkstdY = mean(tBodyAccJerkstdY),
                           Mean_tBodyAccJerkstdZ = mean(tBodyAccJerkstdZ),
                           Mean_tBodyGyromeanX = mean(tBodyGyromeanX),
                           Mean_tBodyGyromeanY = mean(tBodyGyromeanY),
                           Mean_tBodyGyromeanZ = mean(tBodyGyromeanZ),
                           Mean_tBodyGyrostdX = mean(tBodyGyrostdX),
                           Mean_tBodyGyrostdY = mean(tBodyGyrostdY),
                           Mean_tBodyGyrostdZ = mean(tBodyGyrostdZ),
                           Mean_tBodyGyroJerkmeanX = mean(tBodyGyroJerkmeanX),
                           Mean_tBodyGyroJerkmeanY = mean(tBodyGyroJerkmeanY),
                           Mean_tBodyGyroJerkmeanZ = mean(tBodyGyroJerkmeanZ),
                           Mean_tBodyGyroJerkstdX = mean(tBodyGyroJerkstdX),
                           Mean_tBodyGyroJerkstdY = mean(tBodyGyroJerkstdY),
                           Mean_tBodyGyroJerkstdZ = mean(tBodyGyroJerkstdZ),
                           Mean_tBodyAccMagmean = mean(tBodyAccMagmean),
                           Mean_tBodyAccMagstd = mean(tBodyAccMagstd),
                           Mean_tGravityAccMagmean = mean(tGravityAccMagmean),
                           Mean_tGravityAccMagstd = mean(tGravityAccMagstd),
                           Mean_tBodyAccJerkMagmean = mean(tBodyAccJerkMagmean),
                           Mean_tBodyAccJerkMagstd = mean(tBodyAccJerkMagstd),
                           Mean_tBodyGyroMagmean = mean(tBodyGyroMagmean),
                           Mean_tBodyGyroMagstd = mean(tBodyGyroMagstd),
                           Mean_tBodyGyroJerkMagmean = mean(tBodyGyroJerkMagmean),
                           Mean_tBodyGyroJerkMagstd = mean(tBodyGyroJerkMagstd),
                           Mean_fBodyAccmeanX = mean(fBodyAccmeanX),
                           Mean_fBodyAccmeanY = mean(fBodyAccmeanY),
                           Mean_fBodyAccmeanZ = mean(fBodyAccmeanZ),
                           Mean_fBodyAccstdX = mean(fBodyAccstdX),
                           Mean_fBodyAccstdY = mean(fBodyAccstdY),
                           Mean_fBodyAccstdZ = mean(fBodyAccstdZ),
                           Mean_fBodyAccmeanFreqX = mean(fBodyAccmeanFreqX),
                           Mean_fBodyAccmeanFreqY = mean(fBodyAccmeanFreqY),
                           Mean_fBodyAccmeanFreqZ = mean(fBodyAccmeanFreqZ),
                           Mean_fBodyAccJerkmeanX = mean(fBodyAccJerkmeanX),
                           Mean_fBodyAccJerkmeanY = mean(fBodyAccJerkmeanY),
                           Mean_fBodyAccJerkmeanZ = mean(fBodyAccJerkmeanZ),
                           Mean_fBodyAccJerkstdX = mean(fBodyAccJerkstdX),
                           Mean_fBodyAccJerkstdY = mean(fBodyAccJerkstdY),
                           Mean_fBodyAccJerkstdZ = mean(fBodyAccJerkstdZ),
                           Mean_fBodyAccJerkmeanFreqX = mean(fBodyAccJerkmeanFreqX),
                           Mean_fBodyAccJerkmeanFreqY = mean(fBodyAccJerkmeanFreqY),
                           Mean_fBodyAccJerkmeanFreqZ = mean(fBodyAccJerkmeanFreqZ),
                           Mean_fBodyGyromeanX = mean(fBodyGyromeanX),
                           Mean_fBodyGyromeanY = mean(fBodyGyromeanY),
                           Mean_fBodyGyromeanZ = mean(fBodyGyromeanZ),
                           Mean_fBodyGyrostdX = mean(fBodyGyrostdX),
                           Mean_fBodyGyrostdY = mean(fBodyGyrostdY),
                           Mean_fBodyGyrostdZ = mean(fBodyGyrostdZ),
                           Mean_fBodyGyromeanFreqX = mean(fBodyGyromeanFreqX),
                           Mean_fBodyGyromeanFreqY = mean(fBodyGyromeanFreqY),
                           Mean_fBodyGyromeanFreqZ = mean(fBodyGyromeanFreqZ),
                           Mean_fBodyAccMagmean = mean(fBodyAccMagmean),
                           Mean_fBodyAccMagstd = mean(fBodyAccMagstd),
                           Mean_fBodyAccMagmeanFreq = mean(fBodyAccMagmeanFreq),
                           Mean_fBodyBodyAccJerkMagmean = mean(fBodyBodyAccJerkMagmean),
                           Mean_fBodyBodyAccJerkMagstd = mean(fBodyBodyAccJerkMagstd),
                           Mean_fBodyBodyAccJerkMagmeanFreq = mean(fBodyBodyAccJerkMagmeanFreq),
                           Mean_fBodyAccMagstd = mean(fBodyAccMagstd),
                           Mean_fBodyAccMagmeanFreq = mean(fBodyAccMagmeanFreq),
                           Mean_fBodyBodyAccJerkMagmean = mean(fBodyBodyAccJerkMagmean),
                           Mean_fBodyBodyAccJerkMagstd = mean(fBodyBodyAccJerkMagstd),
                           Mean_fBodyBodyAccJerkMagmeanFreq = mean(fBodyBodyAccJerkMagmeanFreq),
                           Mean_fBodyBodyGyroJerkMagmeanFreq = mean(fBodyBodyGyroJerkMagmeanFreq))
}

##########################################
#Final output
##########################################
{
  write.table(tidy_summary,file="C:/Users/<NAME>/Desktop/Coursera/UCI HAR Dataset/tidy_summary.txt", row.names = FALSE)
  write.table(smartphone_tidy_mean_std,file="C:/Users/<NAME>/Desktop/Coursera/UCI HAR Dataset/smartphone_tidy_mean_std.txt", row.names = FALSE)
}
