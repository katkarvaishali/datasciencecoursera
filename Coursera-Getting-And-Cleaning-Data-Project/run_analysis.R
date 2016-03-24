#-----------------------------------------------------------------------------------------------------
# 	Coursera Getting and Cleaning Data Course Project
#	Owner : Vaishali Katkar 
#	Date Created : 24-03-2016
#	runAnalysis.r File Description :
# 		This script will perform the following steps on the UCI HAR Dataset downloaded from  
# 		https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip  
# 			1. Merge the training and the test sets to create one data set. 
# 			2. Extract only the measurements on the mean and standard deviation for each measurement.  
# 			3. Use descriptive activity names to name the activities in the data set 
# 			4. Appropriately label the data set with descriptive activity names.  
# 			5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
#-----------------------------------------------------------------------------------------------------
	
	# Clean up workspace 
	rm(list=ls())
	
	## Set working directory
	setwd("G:/RWrokingDirectory")

	## Create directory if not exists
	if(!file.exists("./data")){
		dir.create("./data")
	}

	##Get the data
	fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
	download.file(fileUrl,destfile="./data/Dataset.zip")

	##Unzip the file
	unzip(zipfile="./data/Dataset.zip",exdir="./data")

	##unzipped files are in the folder UCI HAR Dataset. Get the list of the files
	path_rf <- file.path("./data" , "UCI HAR Dataset")
	files <- list.files(path_rf, recursive = TRUE)
	files
	
	##Load required packages
	library(dplyr)
	library(data.table)
	library(tidyr)
	
	##The files that will be used to load data are listed as follows
	##•test/subject_test.txt
	##•test/X_test.txt
	##•test/y_test.txt
	##•train/subject_train.txt
	##•train/X_train.txt
	##•train/y_train.txt
	
	##	1.Values of Varible Activity consist of data from "Y_train.txt" and "Y_test.txt"
	##	2.values of Varible Subject consist of data from "subject_train.txt" and "subject_test.txt"
	##	3.Values of Varibles Features consist of data from "X_train.txt" and "X_test.txt"
	##	4.Names of Varibles Features come from "features.txt"
	##	5.levels of Varible Activity come from "activity_labels.txt"
	##	So use Activity, Subject and Features as part of descriptive variable names for data in data frame.
	
	##Read data from the files into the variables	
		#Read the Activity files
			dataActvtTest  <- read.table(file.path(path_rf, "test" , "Y_test.txt" ), header = FALSE)
			dataActvtTrain <- read.table(file.path(path_rf, "train", "Y_train.txt"), header = FALSE)
		
		#Read the Subject files
			dataSubTest  <- read.table(file.path(path_rf, "test" , "subject_test.txt"), header = FALSE)
			dataSubTrain <- read.table(file.path(path_rf, "train", "subject_train.txt"), header = FALSE)
			
		#Read Fearures files
			dataFeaturTest  <- read.table(file.path(path_rf, "test" , "X_test.txt" ), header = FALSE)
			dataFeaturTrain <- read.table(file.path(path_rf, "train", "X_train.txt"), header = FALSE)
		
		#Look at the properties of the above varibles
			str(dataActvtTest)
			str(dataActvtTrain)		
			str(dataSubTest)		
			str(dataSubTrain)		
			str(dataFeaturTest)		
			str(dataFeaturTrain)
	
	##Merge the training and the test sets to create one data set	
		#Concatenate the data tables by rows
		dataSub <- rbind(dataSubTrain, dataSubTest)
		dataActvt <- rbind(dataActvtTrain, dataActvtTest)
		dataFeatur <- rbind(dataFeaturTrain, dataFeaturTest)
		
		#set names to variables
			names(dataSub) <- c("subject")
			names(dataActvt) <- c("activity")
			dataFeaturNames <- read.table(file.path(path_rf, "features.txt"), head = FALSE)
			names(dataFeatur) <- dataFeaturNames$V2
			
		#Merge columns to get the data frame Data for all data
			dataCombine <- cbind(dataSub, dataActvt)
			Data <- cbind(dataFeatur, dataCombine)
			
	##Extract only the measurements on the mean and standard deviation for each measurement
		#Subset Name of Features by measurements on the mean and standard deviation
		subdataFeaturNames <- dataFeaturNames$V2[grep("mean\\(\\)|std\\(\\)", dataFeaturNames$V2)]
		
		#Subset the data frame Data by seleted names of Features
		selectedNames <- c(as.character(subdataFeaturNames), "subject", "activity" )
		Data <- subset(Data, select = selectedNames)
		
		#Check the structures of the data frame Data
		str(Data)
		
	## Use descriptive activity names to name the activities in the data set
		#Read descriptive activity names from "activity_labels.txt:
		activityLabel <- read.table(file.path(path_rf, "activity_labels.txt"), header = FALSE)
		
		# Factorize Variale activity in the data frame Data using descriptive activity names
		Data$activity<-factor(Data$activity); 
		Data$activity<- factor(Data$activity,labels=as.character(activityLabel$V2))
		
		# check the headers
		head(Data$activity,30)
		
	## Appropriately label the data set with descriptive variable names
		# In the former part, variables activity and subject and names of the activities have been labelled using descriptive names.
		# In this part, Names of Feteatures will labelled using descriptive variable names.
			#•prefix t is replaced by time
			#•Acc is replaced by Accelerometer
			#•Gyro is replaced by Gyroscope
			#•prefix f is replaced by frequency
			#•Mag is replaced by Magnitude
			#•BodyBody is replaced by Body
			
		names(Data) <-gsub ("^t", "time", names(Data))
		names(Data) <-gsub ("^f", "frequency", names(Data))
		names(Data) <-gsub ("Acc", "Accelerometer", names(Data))
		names(Data) <-gsub ("Gyro", "Gyroscope", names(Data))
		names(Data) <-gsub ("Mag", "Magnitude", names(Data))
		names(Data) <-gsub ("BodyBody", "Body", names(Data))

		# check the labels
		names(Data)
		
	## Create a second, independent tidy data set and ouput it
		# In this part,a second, independent tidy data set will be created with the average of each variable 
		# for each activity and each subject based on the data set in above step.
		
		library(plyr);
		Data2<-aggregate(. ~subject + activity, Data, mean)
		Data2<-Data2[order(Data2$subject,Data2$activity),]
		
		# Check Dataset structure
		str(Data2)
		
		# Check Dataset summary
		summary(Data2)
		
		write.table(Data2, file = "tidydata.txt", row.name=FALSE)
		
		# Check the tidydata.txt
		DataTidy -> read.table("tidydata.txt", sep=" ", head=TRUE) 
		DataTidy
#------------------ End of Code -----------------------------------------------------------------------------		
		
		
		
		
		