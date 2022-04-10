#load and merge test data
xtest <- read.table("UCI HAR Dataset/test/X_test.txt", header=FALSE)
ytest <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = c("Activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = c("Subject"))
all_test_together <- cbind(ytest, subject_test, xtest)

#load and merge train data
xtrain <- read.table("UCI HAR Dataset/train/X_train.txt", header=FALSE)
ytrain <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = c("Activity"))
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = c("Subject"))
all_train_together <- cbind(ytrain, subject_train, xtrain)

#merge train and test data
all_together <- rbind(all_test_together, all_train_together)

#filter mean and standard dev 
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("ID", "Feature"))
f <- c(as.vector(features[, "Feature"]), "Subject", "Activity")
filt_ids <- grepl("mean|std",f)
filt_data = all_together[, filt_ids]

#add activity names
activity_label <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("ID","Activities"))
final <- filt_data %>% full_join (activity_label, by = "ID") %>%
  
#tiding the names
normalized_names <- f[filt_ids]
normalized_names <- gsub("Mag", "-Magnitude", normalized_names)
normalized_names <- gsub("Gyro", "-Gyroscope", normalized_names)
normalized_names <- gsub("Acc", "-Accelerometer", normalized_names)
names(filt_data) <- normalized_names

#creating the subset
creat_subset <- tbl_df(filt_data)%>%
  summarise_each(funs(mean)) %>% 
  gather(measure, mean)

#save to file
write.table(creat_subset, file="tidy_data.txt", row.name=FALSE)
