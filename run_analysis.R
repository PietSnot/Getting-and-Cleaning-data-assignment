# course assignment
# course is "Getting and Cleaning data"
#

#####################################################################
#
#   required packages
#
#####################################################################

require(plyr)
require(dplyr)
require(data.table)

#####################################################################
#
# information extracted with excel: these are the columns to keep
#
#####################################################################

cols_to_keep = c(1,2,3,4,5,6,41,42,43,44,45,46,81,82,83,84,85,86,
                 121,122,123,124,125,126,161,162,163,164,165,166,
                 201,202,214,215,227,228,240,241,253,254,266,267,
                 268,269,270,271,294,295,296,345,346,347,348,349,
                 350,373,374,375,424,425,426,427,428,429,452,453,
                 454,503,504,513,516,517,526,529,530,539,542,543,
                 552
                )

# message(length(cols_to_keep))

#####################################################################
#
#   the names of the headers of the big files
#
#####################################################################

headers <- c("tBodyAcc_mean_X","tBodyAcc_mean_Y","tBodyAcc_mean_Z",
             "tBodyAcc_std_X","tBodyAcc_std_Y","tBodyAcc_std_Z",
             "tGravityAcc_mean_X","tGravityAcc_mean_Y","tGravityAcc_mean_Z",
             "tGravityAcc_std_X","tGravityAcc_std_Y","tGravityAcc_std_Z",
             "tBodyAccJerk_mean_X","tBodyAccJerk_mean_Y","tBodyAccJerk_mean_Z",
             "tBodyAccJerk_std_X","tBodyAccJerk_std_Y","tBodyAccJerk_std_Z",
             "tBodyGyro_mean_X","tBodyGyro_mean_Y","tBodyGyro_mean_Z",
             "tBodyGyro_std_X","tBodyGyro_std_Y","tBodyGyro_std_Z",
             "tBodyGyroJerk_mean_X","tBodyGyroJerk_mean_Y","tBodyGyroJerk_mean_Z",
             "tBodyGyroJerk_std_X","tBodyGyroJerk_std_Y","tBodyGyroJerk_std_Z",
             "tBodyAccMag_mean","tBodyAccMag_std","tGravityAccMag_mean",
             "tGravityAccMag_std","tBodyAccJerkMag_mean","tBodyAccJerkMag_std",
             "tBodyGyroMag_mean","tBodyGyroMag_std","tBodyGyroJerkMag_mean",
             "tBodyGyroJerkMag_std","fBodyAcc_mean_X","fBodyAcc_mean_Y",
             "fBodyAcc_mean_Z","fBodyAcc_std_X","fBodyAcc_std_Y","fBodyAcc_std_Z",
             "fBodyAcc_meanFreq_X","fBodyAcc_meanFreq_Y","fBodyAcc_meanFreq_Z",
             "fBodyAccJerk_mean_X","fBodyAccJerk_mean_Y","fBodyAccJerk_mean_Z",
             "fBodyAccJerk_std_X","fBodyAccJerk_std_Y","fBodyAccJerk_std_Z",
             "fBodyAccJerk_meanFreq_X","fBodyAccJerk_meanFreq_Y",
             "fBodyAccJerk_meanFreq_Z","fBodyGyro_mean_X","fBodyGyro_mean_Y",
             "fBodyGyro_mean_Z","fBodyGyro_std_X","fBodyGyro_std_Y","fBodyGyro_std_Z",
             "fBodyGyro_meanFreq_X","fBodyGyro_meanFreq_Y","fBodyGyro_meanFreq_Z",
             "fBodyAccMag_mean","fBodyAccMag_std","fBodyAccMag_meanFreq",
             "fBodyBodyAccJerkMag_mean","fBodyBodyAccJerkMag_std",
             "fBodyBodyAccJerkMag_meanFreq","fBodyBodyGyroMag_mean",
             "fBodyBodyGyroMag_std","fBodyBodyGyroMag_meanFreq",
             "fBodyBodyGyroJerkMag_mean","fBodyBodyGyroJerkMag_std",
             "fBodyBodyGyroJerkMag_meanFreq"
            )

# message(length(headers))

#####################################################################
#
# reading in the training data
#
#####################################################################

# loading raw training data
training_data_raw <- read.csv("train/X_train.csv", header = F, sep = ";")

#keeping the columns with mean or std
td <- training_data_raw[, cols_to_keep]

# naming the columns
names(td) <- headers

# toevoegen persons
persons <- read.csv("train/subject_train.txt", header = F, sep = " ", 
                    colClasses = c("integer")
                   )

# adding a name to persons
names(persons) <- "person"

# and adding persons to td
td <- cbind(td, persons)

# and loading activity
activities <- read.csv("train/y_train.txt", header = F, sep = " ", 
                       colClasses = c("integer")
                      )
names(activities) <- "activity"

# and adding activities to td
data_training <- cbind(td, activities)

##########################################################################
#
# we're done, now doing the exact same for test
#
##########################################################################

# loading raw training data
test_data_raw <- read.csv("test/X_test.csv", header = F, sep = ";")

#keeping the columns with mean or std
td <- test_data_raw[, cols_to_keep]

# naming the columns
names(td) <- headers

# toevoegen persons
persons <- read.csv("test/subject_test.txt", header = F, sep = " ", 
                    colClasses = c("integer")
)

# adding a name to persons
names(persons) <- "person"

# and adding persons to td
td <- cbind(td, persons)

# and loading activity
activities <- read.csv("test/y_test.txt", header = F, sep = " ", 
                       colClasses = c("integer")
                      )
names(activities) <- "activity"

# and adding activities to td
td <- cbind(td, activities)

#########################################################################
#
#    and here comes the nearly final big file
#
#########################################################################

big_data <- rbind(data_training, td)

# o yeah, giving meaningfull names 

activity_names <- data.frame("activity" = 1:6,
                             "activity_name" = c("Walking","Walking Upstairs",
                                                 "Walking Downstairs", "Sitting",
                                                 "Standing","Laying"
                                                )
                            )

# and merging

big_data_final = merge(big_data, activity_names, by = "activity", all.y = T)

#########################################################################
#
#  creating the second required table, full of means
#
#########################################################################

grouped_big_data <- group_by(big_data_final, activity, person)

grouped_means <- summarize( grouped_big_data,
                            tBodyAcc_mean_X  =  mean(tBodyAcc_mean_X , rm.na = TRUE) ,
                            tBodyAcc_mean_Y  =  mean(tBodyAcc_mean_Y , rm.na = TRUE) ,
                            tBodyAcc_mean_Z  =  mean(tBodyAcc_mean_Z , rm.na = TRUE) ,
                            tBodyAcc_std_X  =  mean(tBodyAcc_std_X , rm.na = TRUE) ,
                            tBodyAcc_std_Y  =  mean(tBodyAcc_std_Y , rm.na = TRUE) ,
                            tBodyAcc_std_Z  =  mean(tBodyAcc_std_Z , rm.na = TRUE) ,
                            tGravityAcc_mean_X  =  mean(tGravityAcc_mean_X , rm.na = TRUE) ,
                            tGravityAcc_mean_Y  =  mean(tGravityAcc_mean_Y , rm.na = TRUE) ,
                            tGravityAcc_mean_Z  =  mean(tGravityAcc_mean_Z , rm.na = TRUE) ,
                            tGravityAcc_std_X  =  mean(tGravityAcc_std_X , rm.na = TRUE) ,
                            tGravityAcc_std_Y  =  mean(tGravityAcc_std_Y , rm.na = TRUE) ,
                            tGravityAcc_std_Z  =  mean(tGravityAcc_std_Z , rm.na = TRUE) ,
                            tBodyAccJerk_mean_X  =  mean(tBodyAccJerk_mean_X , rm.na = TRUE) ,
                            tBodyAccJerk_mean_Y  =  mean(tBodyAccJerk_mean_Y , rm.na = TRUE) ,
                            tBodyAccJerk_mean_Z  =  mean(tBodyAccJerk_mean_Z , rm.na = TRUE) ,
                            tBodyAccJerk_std_X  =  mean(tBodyAccJerk_std_X , rm.na = TRUE) ,
                            tBodyAccJerk_std_Y  =  mean(tBodyAccJerk_std_Y , rm.na = TRUE) ,
                            tBodyAccJerk_std_Z  =  mean(tBodyAccJerk_std_Z , rm.na = TRUE) ,
                            tBodyGyro_mean_X  =  mean(tBodyGyro_mean_X , rm.na = TRUE) ,
                            tBodyGyro_mean_Y  =  mean(tBodyGyro_mean_Y , rm.na = TRUE) ,
                            tBodyGyro_mean_Z  =  mean(tBodyGyro_mean_Z , rm.na = TRUE) ,
                            tBodyGyro_std_X  =  mean(tBodyGyro_std_X , rm.na = TRUE) ,
                            tBodyGyro_std_Y  =  mean(tBodyGyro_std_Y , rm.na = TRUE) ,
                            tBodyGyro_std_Z  =  mean(tBodyGyro_std_Z , rm.na = TRUE) ,
                            tBodyGyroJerk_mean_X  =  mean(tBodyGyroJerk_mean_X , rm.na = TRUE) ,
                            tBodyGyroJerk_mean_Y  =  mean(tBodyGyroJerk_mean_Y , rm.na = TRUE) ,
                            tBodyGyroJerk_mean_Z  =  mean(tBodyGyroJerk_mean_Z , rm.na = TRUE) ,
                            tBodyGyroJerk_std_X  =  mean(tBodyGyroJerk_std_X , rm.na = TRUE) ,
                            tBodyGyroJerk_std_Y  =  mean(tBodyGyroJerk_std_Y , rm.na = TRUE) ,
                            tBodyGyroJerk_std_Z  =  mean(tBodyGyroJerk_std_Z , rm.na = TRUE) ,
                            tBodyAccMag_mean  =  mean(tBodyAccMag_mean , rm.na = TRUE) ,
                            tBodyAccMag_std  =  mean(tBodyAccMag_std , rm.na = TRUE) ,
                            tGravityAccMag_mean  =  mean(tGravityAccMag_mean , rm.na = TRUE) ,
                            tGravityAccMag_std  =  mean(tGravityAccMag_std , rm.na = TRUE) ,
                            tBodyAccJerkMag_mean  =  mean(tBodyAccJerkMag_mean , rm.na = TRUE) ,
                            tBodyAccJerkMag_std  =  mean(tBodyAccJerkMag_std , rm.na = TRUE) ,
                            tBodyGyroMag_mean  =  mean(tBodyGyroMag_mean , rm.na = TRUE) ,
                            tBodyGyroMag_std  =  mean(tBodyGyroMag_std , rm.na = TRUE) ,
                            tBodyGyroJerkMag_mean  =  mean(tBodyGyroJerkMag_mean , rm.na = TRUE) ,
                            tBodyGyroJerkMag_std  =  mean(tBodyGyroJerkMag_std , rm.na = TRUE) ,
                            fBodyAcc_mean_X  =  mean(fBodyAcc_mean_X , rm.na = TRUE) ,
                            fBodyAcc_mean_Y  =  mean(fBodyAcc_mean_Y , rm.na = TRUE) ,
                            fBodyAcc_mean_Z  =  mean(fBodyAcc_mean_Z , rm.na = TRUE) ,
                            fBodyAcc_std_X  =  mean(fBodyAcc_std_X , rm.na = TRUE) ,
                            fBodyAcc_std_Y  =  mean(fBodyAcc_std_Y , rm.na = TRUE) ,
                            fBodyAcc_std_Z  =  mean(fBodyAcc_std_Z , rm.na = TRUE) ,
                            fBodyAcc_meanFreq_X  =  mean(fBodyAcc_meanFreq_X , rm.na = TRUE) ,
                            fBodyAcc_meanFreq_Y  =  mean(fBodyAcc_meanFreq_Y , rm.na = TRUE) ,
                            fBodyAcc_meanFreq_Z  =  mean(fBodyAcc_meanFreq_Z , rm.na = TRUE) ,
                            fBodyAccJerk_mean_X  =  mean(fBodyAccJerk_mean_X , rm.na = TRUE) ,
                            fBodyAccJerk_mean_Y  =  mean(fBodyAccJerk_mean_Y , rm.na = TRUE) ,
                            fBodyAccJerk_mean_Z  =  mean(fBodyAccJerk_mean_Z , rm.na = TRUE) ,
                            fBodyAccJerk_std_X  =  mean(fBodyAccJerk_std_X , rm.na = TRUE) ,
                            fBodyAccJerk_std_Y  =  mean(fBodyAccJerk_std_Y , rm.na = TRUE) ,
                            fBodyAccJerk_std_Z  =  mean(fBodyAccJerk_std_Z , rm.na = TRUE) ,
                            fBodyAccJerk_meanFreq_X  =  mean(fBodyAccJerk_meanFreq_X , rm.na = TRUE) ,
                            fBodyAccJerk_meanFreq_Y  =  mean(fBodyAccJerk_meanFreq_Y , rm.na = TRUE) ,
                            fBodyAccJerk_meanFreq_Z  =  mean(fBodyAccJerk_meanFreq_Z , rm.na = TRUE) ,
                            fBodyGyro_mean_X  =  mean(fBodyGyro_mean_X , rm.na = TRUE) ,
                            fBodyGyro_mean_Y  =  mean(fBodyGyro_mean_Y , rm.na = TRUE) ,
                            fBodyGyro_mean_Z  =  mean(fBodyGyro_mean_Z , rm.na = TRUE) ,
                            fBodyGyro_std_X  =  mean(fBodyGyro_std_X , rm.na = TRUE) ,
                            fBodyGyro_std_Y  =  mean(fBodyGyro_std_Y , rm.na = TRUE) ,
                            fBodyGyro_std_Z  =  mean(fBodyGyro_std_Z , rm.na = TRUE) ,
                            fBodyGyro_meanFreq_X  =  mean(fBodyGyro_meanFreq_X , rm.na = TRUE) ,
                            fBodyGyro_meanFreq_Y  =  mean(fBodyGyro_meanFreq_Y , rm.na = TRUE) ,
                            fBodyGyro_meanFreq_Z  =  mean(fBodyGyro_meanFreq_Z , rm.na = TRUE) ,
                            fBodyAccMag_mean  =  mean(fBodyAccMag_mean , rm.na = TRUE) ,
                            fBodyAccMag_std  =  mean(fBodyAccMag_std , rm.na = TRUE) ,
                            fBodyAccMag_meanFreq  =  mean(fBodyAccMag_meanFreq , rm.na = TRUE) ,
                            fBodyBodyAccJerkMag_mean  =  mean(fBodyBodyAccJerkMag_mean , rm.na = TRUE) ,
                            fBodyBodyAccJerkMag_std  =  mean(fBodyBodyAccJerkMag_std , rm.na = TRUE) ,
                            fBodyBodyAccJerkMag_meanFreq  =  mean(fBodyBodyAccJerkMag_meanFreq , rm.na = TRUE) ,
                            fBodyBodyGyroMag_mean  =  mean(fBodyBodyGyroMag_mean , rm.na = TRUE) ,
                            fBodyBodyGyroMag_std  =  mean(fBodyBodyGyroMag_std , rm.na = TRUE) ,
                            fBodyBodyGyroMag_meanFreq  =  mean(fBodyBodyGyroMag_meanFreq , rm.na = TRUE) ,
                            fBodyBodyGyroJerkMag_mean  =  mean(fBodyBodyGyroJerkMag_mean , rm.na = TRUE) ,
                            fBodyBodyGyroJerkMag_std  =  mean(fBodyBodyGyroJerkMag_std , rm.na = TRUE) ,
                            fBodyBodyGyroJerkMag_meanFreq  =  mean(fBodyBodyGyroJerkMag_meanFreq , rm.na = TRUE)
                         )

write.table(grouped_means, "grouped_means.txt", sep = "\t", row.names = F)