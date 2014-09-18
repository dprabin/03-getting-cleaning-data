#Create one R script called run_analysis.R that does the following. 
#1. Merges the training and the test sets to create one data set.
#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#3. Uses descriptive activity names to name the activities in the data set
#4. Appropriately labels the data set with descriptive variable names. 
#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

##Step 1: Read data and merge them

###Set Working Directory 
#```{r setworkingdirectory}
setwd("/users/MacBookAir/Movies/coursera/03 Getting and cleaning data/Course project/UCI HAR Dataset/")
#```

###Read x and y variables for Training and Testing 
#The data sets are available in two different sets - training set and testing set. We will now read all the data sets to appropriate variables.
#```{r readtraintestxy}
xtrain<-read.table("./train/X_train.txt")
ytrain<-read.table("./train/y_train.txt")

xtest<-read.table("./test/X_test.txt")
ytest<-read.table("./test/y_test.txt")
#```

###Merge training and testing data to x and y variable
#We will now merge xtrain and xtest datasets to x variable and ytraiing and ytest datasets to y variable.
#```{r combinetesttraintoxy}
x<-rbind(xtrain,xtest)
y<-rbind(ytrain,ytest)
#```
#With this, training and test variables for x and y are merged to x and y variables.

##Step 2: Extract only Mean and Standard Deviation values

###load variable names
#The variable names are stored in features.txt file.
#```{r readVariableNames}
features<-read.table("features.txt")
head(features)
#```
#The features table shows two columns. we don't need the first column. We only need the second column as variable names. Also, the variable contains uppercase and lowercase characters. Change all to lowercase to avoid confusion.
#```{r convertfeaturesvectortolower}
features<-tolower(features[,2])
#```
  

###Apply the feature name labels as names of x variable
#```{r giveVariablenamesTox}
names(x)<-features
#```


###Now find out only variables that are mean values and standard deviation values.
#We want only the means or std outside the braces eg
# - mean(abcsum,efg) will count
# - sum(abcmean,abcstd) does not count as mean or std
#```{r meanVariableFind}
meanStdColumns<-grep("[m|M]ean\\(.*?\\)|[s|S]td\\(.*?\\)",names(x))
# also the regex "mean\\(\\)|std\\(\\)" works as it is already lowercase
length(meanStdColumns)
#```

###Extract those columns to new variable
#```{r extractMeanStdColumns}
reducedx<-x[,meanStdColumns]
#```

###Write the reduced data to new file
#```{r writeDataToFile}
#write.csv(reducedx,file="reducedx.csv")
write.table(reducedx,file="reducedx.txt",row.name=FALSE)
#```


##Step 3: Use descriptive activity names to name the activities in the data set

###Read Activity labels from the file
#The activity labels are stored in the activity_labels.txt file.
#```{r readActivityLabelsFromFile}
activityLabels<-read.table("activity_labels.txt")
#```


###Replace activities in variable y with labels
#```{r replaceActivityLabessIny}
#First create an empty yLabels
yLabels<-0
#There are six activity labels so loop 6 times 
#and replace y with  labels
for(a in 1:6){
  #find a in y
  ainy = which(y==a)
  yLabels[ainy] <- as.character(activityLabels[a,2])
}
#```

##Step 4: Appropriately labels the data set with descriptive variable names. 

#Read subject numbers from training and testing dataset
#```{r}
subjectTrain<-read.table("./train/subject_train.txt")
subjectTest<-read.table("./test/subject_test.txt")
#```

#Merge these to single variable
#```{r}
subject<-rbind(subjectTrain,subjectTest)
#```

#Now change the number to labels like Subject1, Subject2...
#```{r}
subject[,1]<-paste("Subject-",as.character(subject[,1]),sep="")
#rename column name to subjectname
names(subject)<-"subjectname"
#```

#Merge all these data to one variable named data
#```{r}
data<-cbind(subject,yLabels,reducedx)
#Rename names of column 1 and 2
names(data[1:2])<-c("subjectname","activity")
#```


##Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#Change the subjectname to factor variable so that we can use tapply function to find mean of factors
#```{r changeSubjectNameToFactor}
data$subjectname<-as.factor(data$subjectname)
#```

#Finding Mean
#```{r findMeanByFactor}
#tapply(data,data$subjectname,mean)
#tapply seems to work with only one column
#newdata<-0
#for(name in unique(data$name)){
#    rowIndex<-which(data$subjectname == name)
#    result<-colMeans(data[,rowIndex])
#    newdata[name,]<-result
#    newdata
#}
library(dplyr)
newdata<-data %>% group_by(subjectname) %>% summarise_each(funs(mean))

#now write this data to a file
write.table(newdata,file="meandata_subject.txt",row.name=FALSE)
#```