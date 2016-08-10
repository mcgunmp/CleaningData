#set working directory
setwd("C:/Users/Mike/Desktop/R_programming_2/Cleaning/UCI HAR Dataset")

require(dplyr)

#bring data in
features<-read.table("features.txt",stringsAsFactors = FALSE)
head(features)
activity<-read.table("activity_labels.txt", stringsAsFactors = FALSE)
head(activity)
subject_train<-read.table("./train/subject_train.txt", stringsAsFactors = FALSE)
head(subject_train)
x_train<-read.table("./train/x_train.txt", stringsAsFactors = FALSE)
head(x_train)
y_train<-read.table("./train/y_train.txt", stringsAsFactors = FALSE)
head(y_train)
subject_test<-read.table("./test/subject_test.txt", stringsAsFactors = FALSE)
head(subject_test)
x_test<-read.table("./test/x_test.txt", stringsAsFactors = FALSE)
head(x_test)
y_test<-read.table("./test/y_test.txt", stringsAsFactors = FALSE)
head(y_test)

#give each column a name
colnames(activity)<-c('activity_ID', 'activity_type')
colnames(subject_train)<-('subject_ID')
colnames(x_train)<-features[,2]
colnames(y_train)<-('activity_ID')
colnames(subject_test)<-('subject_ID')
colnames(x_test)<-features[,2]
colnames(y_test)<-('activity_ID')

# 1 Merges the training and the test sets to create one data set.

#Merge training data set into one data frame
Train_Data<-cbind(y_train,x_train,subject_train)

#Merge test data set into one data frame
Test_Data<-cbind(y_test,x_test,subject_test)


#Merge test frame with train data frame
db<-rbind(Train_Data,Test_Data)

#Create a list of column names
colNames  = colnames(db);


#2 Extracts only the measurements on the mean and standard deviation for each measurement.
# Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | 
                   grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) 
                 & !grepl("mean..-",colNames) | grepl("-std..",colNames) 
                 & !grepl("-std()..-",colNames));


# Keep only desired columns
db= db[logicalVector==TRUE];


# 3 Uses descriptive activity names to name the activities in the data set
db = merge(db,activity,by='activity_ID',all.x=TRUE);
colNames  = colnames(db); 

#4 Appropriately labels the data set with descriptive variable names.


for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};


# Reassigning the new descriptive column names to the finalData set
colnames(db) = colNames;


# 5 From the data set in step 4, create a second, independent tidy data set
#  with the average of each variable for each activity and each subject.

# Create a new table, finalDataNoActivityType without the activityType column
db_No_activity  = db[,names(db) != 'activityType'];

# Summarizing the table to include just the mean of each variable for each activity and each subject
tidyData    = aggregate(db_No_activity[,names(db_No_activity) != c('activity_ID','subject_ID')],
                        by=list(activity_ID=db_No_activity$activity_ID,subject_ID = 
                                  db_No_activity$subject_ID),mean);

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,activity,by='activity_ID',all.x=TRUE);

# Export the tidyData set 
write.csv(tidyData, './tidyData.csv',row.names=TRUE);
