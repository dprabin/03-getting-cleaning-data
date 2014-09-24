##Step 1: Read data and merge them
###Set Working Directory 
setwd("/users/MacBookAir/Movies/coursera/03 Getting and cleaning data/Course project/UCI HAR Dataset/")

###Read x and y variables for Training and Testing 
xtrain<-read.table("./train/X_train.txt")
ytrain<-read.table("./train/y_train.txt")

xtest<-read.table("./test/X_test.txt")
ytest<-read.table("./test/y_test.txt")

###Merge training and testing data to x and y variable
x<-rbind(xtrain,xtest)
y<-rbind(ytrain,ytest)


##Step 2: Extract only Mean and Standard Deviation values
###load variable names
features<-read.table("features.txt")
head(features)
features<-tolower(features[,2])

###Apply the feature name labels as names of x variable
names(x)<-features

###Now find out only variables that are mean values and standard deviation values.
meanStdColumns<-grep("[m|M]ean\\(.*?\\)|[s|S]td\\(.*?\\)",names(x))
# also the regex "mean\\(\\)|std\\(\\)" works as it is already lowercase
length(meanStdColumns)

###Extract those columns to new variable
reducedx<-x[,meanStdColumns]

###Write the reduced data to new file
#write.csv(reducedx,file="reducedx.csv")
write.table(reducedx,file="reducedx.txt",row.name=FALSE)


##Step 3: Use descriptive activity names to name the activities in the data set
###Read Activity labels from the file
activityLabels<-read.table("activity_labels.txt")

###Replace activities in variable y with labels
#First create an empty yLabels
yLabels<-0
#There are six activity labels so loop 6 times and replace y with  labels
for(a in 1:6){
    #find a in y
    ainy = which(y==a)
    yLabels[ainy] <- as.character(activityLabels[a,2])
}


##Step 4: Appropriately labels the data set with descriptive variable names. 
subjectTrain<-read.table("./train/subject_train.txt")
subjectTest<-read.table("./test/subject_test.txt")

#Merge these to single variable
subject<-rbind(subjectTrain,subjectTest)

#Now change the number to labels like Subject1, Subject2...
subject[,1]<-paste("Subject-",as.character(subject[,1]),sep="")
#rename column name to subjectname
names(subject)<-"subjectname"

#Merge all these data to one variable named data
data<-cbind(subject,yLabels,reducedx)
#Rename names of column 1 and 2
names(data[1:2])<-c("subjectname","activity")


##Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
data$subjectname<-as.factor(data$subjectname)

library(dplyr)
#Find mean by subjectname
newdata<-data %>% group_by(subjectname) %>% summarise_each(funs(mean))
#now write this data to a file
write.table(newdata,file="meandata_subject.txt",row.name=FALSE)

#Again, find mean by activity
newdata<-data %>% group_by(yLabels) %>% summarise_each(funs(mean))
#now write this data to a file
write.table(newdata,file="meandata_activities.txt",row.name=FALSE)

#Again, find mean by subjectname and activity
#for that, combine values of these 
newcol<-paste(subject,yLabels,sep="-")
#now merge the new column with old column
data<-cbind(newcol,reducedx)
#find mean by newcol
newdata<-data %>% group_by(newcol) %>% summarise_each(funs(mean))
#now write this data to a file
write.table(newdata,file="meandata_subject_activities.txt",row.name=FALSE)