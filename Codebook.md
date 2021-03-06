#Codebook

this is the codebook for the assaignment, as it will help you understand how my code works.

#About the data itself

the source:

1. download the data: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

2. description of the data: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

below is the info about the data from the readme document with the data files
==================================================================
Human Activity Recognition Using Smartphones Dataset
Version 1.0
==================================================================
Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
Smartlab - Non Linear Complex Systems Laboratory
DITEN - Universit‡ degli Studi di Genova.
Via Opera Pia 11A, I-16145, Genoa, Italy.
activityrecognition@smartlab.ws
www.smartlab.ws
==================================================================

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

For each record it is provided:
======================================

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

The dataset includes the following files:
=========================================

- 'README.txt'

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

- 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 

- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 

- 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 

Notes: 
======
- Features are normalized and bounded within [-1,1].
- Each feature vector is a row on the text file.

For more information about this dataset contact: activityrecognition@smartlab.ws

License:
========
Use of this dataset in publications must be acknowledged by referencing the following publication [1] 

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.
Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.

# what this code is for:

1. Merges the training and the test sets to create one data set.

2. Extracts only the measurements on the mean and standard deviation for each measurement.

3. Uses descriptive activity names to name the activities in the data set

4. Appropriately labels the data set with descriptive activity names.

5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#how the code works:
Basically I imported the test dataset, the training dataset and the features. 

I extracted the mean and std (step 2 up there)、merge the dataset with the features seperately with test data and training data. 

I then merge the two together (step 1), get the average of each varible for each activity and each subject(step 5)

Before the last step I renamed the activities and label the dataset (step 3 and 4). Finally, I export the tidy data ( step 5).

#variables(features)

 [1] "subject"                                           "activity_Label"                                    "TimeBodyAccelerometerMean()-X"                    
 [4] "TimeBodyAccelerometerMean()-Y"                     "TimeBodyAccelerometerMean()-Z"                     "TimeBodyAccelerometerSTD()-X"                     
 [7] "TimeBodyAccelerometerSTD()-Y"                      "TimeBodyAccelerometerSTD()-Z"                      "TimeGravityAccelerometerMean()-X"                 
[10] "TimeGravityAccelerometerMean()-Y"                  "TimeGravityAccelerometerMean()-Z"                  "TimeGravityAccelerometerSTD()-X"                  
[13] "TimeGravityAccelerometerSTD()-Y"                   "TimeGravityAccelerometerSTD()-Z"                   "TimeBodyAccelerometerJerkMean()-X"                
[16] "TimeBodyAccelerometerJerkMean()-Y"                 "TimeBodyAccelerometerJerkMean()-Z"                 "TimeBodyAccelerometerJerkSTD()-X"                 
[19] "TimeBodyAccelerometerJerkSTD()-Y"                  "TimeBodyAccelerometerJerkSTD()-Z"                  "TimeBodyGyroscopeMean()-X"                        
[22] "TimeBodyGyroscopeMean()-Y"                         "TimeBodyGyroscopeMean()-Z"                         "TimeBodyGyroscopeSTD()-X"                         
[25] "TimeBodyGyroscopeSTD()-Y"                          "TimeBodyGyroscopeSTD()-Z"                          "TimeBodyGyroscopeJerkMean()-X"                    
[28] "TimeBodyGyroscopeJerkMean()-Y"                     "TimeBodyGyroscopeJerkMean()-Z"                     "TimeBodyGyroscopeJerkSTD()-X"                     
[31] "TimeBodyGyroscopeJerkSTD()-Y"                      "TimeBodyGyroscopeJerkSTD()-Z"                      "TimeBodyAccelerometerMagnitudeMean()"             
[34] "TimeBodyAccelerometerMagnitudeSTD()"               "TimeGravityAccelerometerMagnitudeMean()"           "TimeGravityAccelerometerMagnitudeSTD()"           
[37] "TimeBodyAccelerometerJerkMagnitudeMean()"          "TimeBodyAccelerometerJerkMagnitudeSTD()"           "TimeBodyGyroscopeMagnitudeMean()"                 
[40] "TimeBodyGyroscopeMagnitudeSTD()"                   "TimeBodyGyroscopeJerkMagnitudeMean()"              "TimeBodyGyroscopeJerkMagnitudeSTD()"              
[43] "FrequencyBodyAccelerometerMean()-X"                "FrequencyBodyAccelerometerMean()-Y"                "FrequencyBodyAccelerometerMean()-Z"               
[46] "FrequencyBodyAccelerometerSTD()-X"                 "FrequencyBodyAccelerometerSTD()-Y"                 "FrequencyBodyAccelerometerSTD()-Z"                
[49] "FrequencyBodyAccelerometerMeanFreq()-X"            "FrequencyBodyAccelerometerMeanFreq()-Y"            "FrequencyBodyAccelerometerMeanFreq()-Z"           
[52] "FrequencyBodyAccelerometerJerkMean()-X"            "FrequencyBodyAccelerometerJerkMean()-Y"            "FrequencyBodyAccelerometerJerkMean()-Z"           
[55] "FrequencyBodyAccelerometerJerkSTD()-X"             "FrequencyBodyAccelerometerJerkSTD()-Y"             "FrequencyBodyAccelerometerJerkSTD()-Z"            
[58] "FrequencyBodyAccelerometerJerkMeanFreq()-X"        "FrequencyBodyAccelerometerJerkMeanFreq()-Y"        "FrequencyBodyAccelerometerJerkMeanFreq()-Z"       
[61] "FrequencyBodyGyroscopeMean()-X"                    "FrequencyBodyGyroscopeMean()-Y"                    "FrequencyBodyGyroscopeMean()-Z"                   
[64] "FrequencyBodyGyroscopeSTD()-X"                     "FrequencyBodyGyroscopeSTD()-Y"                     "FrequencyBodyGyroscopeSTD()-Z"                    
[67] "FrequencyBodyGyroscopeMeanFreq()-X"                "FrequencyBodyGyroscopeMeanFreq()-Y"                "FrequencyBodyGyroscopeMeanFreq()-Z"               
[70] "FrequencyBodyAccelerometerMagnitudeMean()"         "FrequencyBodyAccelerometerMagnitudeSTD()"          "FrequencyBodyAccelerometerMagnitudeMeanFreq()"    
[73] "FrequencyBodyAccelerometerJerkMagnitudeMean()"     "FrequencyBodyAccelerometerJerkMagnitudeSTD()"      "FrequencyBodyAccelerometerJerkMagnitudeMeanFreq()"
[76] "FrequencyBodyGyroscopeMagnitudeMean()"             "FrequencyBodyGyroscopeMagnitudeSTD()"              "FrequencyBodyGyroscopeMagnitudeMeanFreq()"        
[79] "FrequencyBodyGyroscopeJerkMagnitudeMean()"         "FrequencyBodyGyroscopeJerkMagnitudeSTD()"          "FrequencyBodyGyroscopeJerkMagnitudeMeanFreq()"   


Thank you!
