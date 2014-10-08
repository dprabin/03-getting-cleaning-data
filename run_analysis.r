
##Step 1: Read data and merge data
###Set Working Directory 
setwd("/users/MacBookAir/Movies/coursera/03 Getting and cleaning data/Course project/UCI HAR Dataset/")

###Read x and y variables for Training and Testing 
#The data sets are available in two different sets - training set and testing set. We will now read all the data sets to appropriate variables.
xtrain<-read.table("train/X_train.txt")
xtest<-read.table("test/X_test.txt")
#variable Names
features<-read.table("features.txt")
#activity 
ytrain<-read.table("train/y_train.txt")
ytest<-read.table("test/y_test.txt")
#activity Labels for the data above
activityLabels<-read.table("activity_labels.txt")
#Read suubject from files as numbers
subjectTrain<-read.table("./train/subject_train.txt")
subjectTest<-read.table("./test/subject_test.txt")

###Merge training and testing data to x and y variable
#We will now merge xtrain and xtest datasets to x variable and ytraiing and ytest datasets to y variable and subjectTrain and SubjectTest to subject variable.
x<-rbind(xtrain,xtest)
y<-rbind(ytrain,ytest)
subject<-rbind(subjectTrain,subjectTest)

#With this, training and test variables for x and y are merged to x and y variables.
#The features table shows two columns. we don't need the first column. We only need the second column as variable names. Also, the variable contains uppercase and lowercase characters. Change all to lowercase to avoid confusion.
features<-tolower(features[,2])

###Apply the feature name labels as names of x variable
names(x)<-features
names(y)<-"activity"
names(subject)<-"subject"


###Merge the three variables x y and subject to one
data<-cbind(y,subject,x)



##Step 2: Extract only Mean and Standard Deviation values

###Now find out only variables that are mean values and standard deviation values.
meanStdColumns<-grep("[m|M]ean\\(.*?\\)|[s|S]td\\(.*?\\)",names(data))
# also the regex "mean\\(\\)|std\\(\\)" works as it is already lowercase

#With this code, the meanStdColumns has missed activity and subject, so include these too in our final result. In our case, they are column 1 and 2 of data variable.
newColIndex<-c(1,2,meanStdColumns)

###Extract those columns to new variable
reduced<-data[,newColIndex]


##Step 3: Use descriptive activity names to name the activities in the data set
#Now we will replace numbers in activity columns
###Replace activities in variable y with labels
#There are six activity labels so loop 6 times 
#and replace y with  labels
for(a in 1:6){
    aInData = which(reduced$activity==a)
    reduced[aInData,1] <- as.character(activityLabels[a,2])
}


##Step 4: Appropriately label the data set with descriptive variable names. 
#Now change the number to labels like Subject1, Subject2...
reduced[,2]<-paste("Subject-",as.character(reduced[,2]),sep="")

###Clean and apply variable Names
varName<-colnames(reduced)
#remove () from varName
varName<-gsub("\\()","",varName)
#replace t with Time and f with Frequency
varName<-gsub("^(t)","Time",varName)
varName<-gsub("^(f)","Frequency",varName)
#Capitalize initial letters of body, gravity, accleration, jerk,mag
varName<-gsub("[bB]ody","Body",varName)
varName<-gsub("[gG]ravity","Gravity",varName)
varName<-gsub("[aA]cc","Accleration",varName)
varName<-gsub("[jJ]erk","Jerk",varName)
varName<-gsub("[mM]ag","Magnitude",varName)
#Now Apply the variable names to colnames
colnames(reduced)<-varName


##Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#Change the subjectname to factor variable so that we can use tapply function to find mean of factors
data$subject<-as.factor(data$subject)

#Finding Mean
#with aggregate function (this gives some warnings)
#by subject and activity
newdata1<-aggregate(reduced,by=list(reduced$subject,reduced$activity),mean)[,-c(3,4)]

#by using dplyr package (the mean are same)
library(dplyr)
#Again, find mean by subjectname and activity
#for that, combine values of these 
newcol<-paste(reduced$subject,reduced$activity,sep="-")
#now merge the new column with old column
data<-cbind(newcol,reduced)
#find mean by newcol
newdata<-data %>% group_by(newcol) %>% summarise_each(funs(mean))
#now write this data to a file
write.table(newdata,file="tidydata_mean_subject-activities.txt",row.name=FALSE)