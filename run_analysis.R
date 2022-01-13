install.packages("dplyr")
require("dplyr")

directory <- "./specdata/UCI HAR Dataset"
setwd(directory)

#uploading the files into the environment

#labels for activity and featres
features <- read.table('features.txt', col.names = c('Index_Feature','Feature'))

activity_labels <- read.table('activity_labels.txt', 
                              col.names= c('Index_Activity','Activity'))

#test data
subject_test <- read.table('./test/subject_test.txt',
                           col.names = 'Subject')

X_test <- read.table('./test/X_test.txt', 
                     col.names = features$Feature)

y_test <- read.table('./test/y_test.txt', col.names ="ID")

#train data
subject_train <- read.table('./train/subject_train.txt', 
                            col.names = 'Subject')
X_train <- read.table('./train/X_train.txt',
                      col.names = features$Feature,)
y_train <- read.table('./train/y_train.txt', col.names = "ID")


#binding of data
X_train_p = cbind(y_train, subject_train,X_train)
X_test_p = cbind(y_test, subject_test,X_test)
names(X_train_p)[1] <- 'Activity'
names(X_test_p)[1]<- 'Activity'

# 1. Merges the training and the test sets to create one data set
#Merging train and test sets

X_data <- rbind(X_train,X_test)
y_data <- rbind(y_train,y_test)
subject_data <- rbind(subject_train,subject_test)

# 2. Extracting only the measurements on the mean and standard deviation
data_merged <- cbind(subject_data, y_data,X_data )

extracted <- data_merged %>% select(Subject, ID, contains("mean"),contains("std"))

# 3. Uses descriptive activity names to name the activities in the dataset
extracted$ID <- activity_labels[extracted$ID,2]

# 4. Appropriately labels the data set with descriptive variable names
names(extracted)[2]="Activity"

names(extracted) <- sub(x = extracted,pattern = '^t',replacement = 'Time domain signal: ')
names(extracted) <- sub(x = extracted,pattern = '^f',replacement = 'Frequency domain signal: ')
names(extracted) <- sub(x = extracted,pattern = '-',replacement = ', ')
names(extracted) <- sub(x = extracted,pattern = 'mean\\(\\)',replacement = ' mean value ')
names(extracted) <- sub(x = extracted,pattern = 'std\\(\\)',replacement = ' standart deviation ')
names(extracted) <- sub(x = extracted,pattern = '-X',replacement = 'in X direction')
names(extracted) <- sub(x = extracted,pattern = '-Y',replacement = 'in Y direction')
names(extracted) <- sub(x = extracted,pattern = '-Z',replacement = 'in Z direction')
names(extracted) <- sub(x = extracted,pattern = 'AccJerk',replacement = ' acceleration jerk')
names(extracted) <- sub(x = extracted,pattern = 'Acc',replacement = ' acceleration')
names(extracted) <- sub(x = extracted,pattern = 'GyroJerk',replacement = ' angular velocity jerk')
names(extracted) <- sub(x = extracted,pattern = 'Gyro',replacement = ' angular velocity')
names(extracted) <- sub(x = extracted,pattern = 'Mag',replacement = ' magnitude')

colnames(X_clean) <- extracted