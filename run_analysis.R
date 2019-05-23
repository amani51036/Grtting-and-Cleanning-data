library(plyr)

features <- read.table("./features.txt", col.names = c("f_id","functions"))
activities <- read.table("./activity_labels.txt", col.names = c("code", "activity"))

x_train <- read.table('./train/X_train.txt',col.names = features$functions)
x_test <- read.table('./test/X_test.txt',col.names = features$functions)

y_train <- read.table('./train/y_train.txt', col.names = "code")
y_test <- read.table('./test/y_test.txt', col.names = "code")

subject_train <- read.table('./train/subject_train.txt', col.names = "subject")
subject_test <- read.table('./test/subject_test.txt', col.names = "subject")

#assigning column names
colnames(x_train) <- features[,2] 
colnames(y_train) <-"activityId"
colnames(subject_train) <- "subjectId"
      
colnames(x_test) <- features[,2] 
colnames(y_test) <- "activityId"
colnames(subject_test) <- "subjectId"
      
colnames(activities ) <- c('activityId','activityType')

# 1. Merges the training and the test sets to create one data set.

merge_train <- cbind(y_train, subject_train, x_train)
merge_test <- cbind(y_test, subject_test, x_test)

dataset1 <- rbind(merge_train, merge_test)

# 2.Extracts only the measurements on the mean and standard deviation for each measurement.

col_names <- colnames(dataset1)
      
mean_std <- (grepl("activityId" , col_names) | 
                       grepl("subjectId" , col_names) | 
                       grepl("mean.." , col_names) | 
                       grepl("std.." , col_names) 
                      )
      
dataset2 <- dataset1[ , mean_std == TRUE]

# 3.Uses descriptive activity names to name the activities in the data set
dataset3 <- merge(dataset2, activities,
                           by='activityId',
                           all.x=TRUE)


#4. Appropriately labels the data set with descriptive variable names.

	#this is done in previous steps

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidy_data <- aggregate(. ~subjectId + activityId, dataset3, mean)
tidy_data<- tidy_data[order(tidy_data$subjectId, tidy_data$activityId),]
      
write.table(tidy_data, "tidy_data.txt", row.name=FALSE)

