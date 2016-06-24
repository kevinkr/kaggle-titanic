#install.packages("RCurl")
#install.packages("foreign")
#install.packages("MKLE")
install.packages("reshape")
install.packages("caret")
library(RCurl)
library(foreign)
library(reshape)
library(caret)
train <- read.csv(text=getURL("https://raw.githubusercontent.com/kevinkr/kaggle-titanic/master/Data/train.csv"),header = TRUE, stringsAsFactors = FALSE)
test <- read.csv(text=getURL("https://raw.githubusercontent.com/kevinkr/kaggle-titanic/master/Data/test.csv"),header = TRUE, stringsAsFactors = FALSE)

#rename to match code example
trainData <- train
testData <- test

# save the number of  rows in the train dataset
trainData.nrow<-seq(1, nrow(trainData)) 

# Merge data to simplify preprocessing
trainData<-rbind(trainData, cbind(testData, Survived=rep(NA, nrow(testData)))) 

#Examine some data to understand it
head(trainData)

#Examine some basic data
plot(density(trainData$Age, na.rm = TRUE))
plot(density(trainData$Pclass, na.rm = TRUE))

#plot proportions for gender
counts <- table(trainData$Survived, trainData$Sex)
barplot(counts, xlab = "Gender", ylab = "Number of People", main = "survived and deceased between male and female")

counts
counts[2] / (counts[1] + counts[2])
counts[4] / (counts[3] + counts[4])

#Examine cabin class
Pclass_survival <- table(trainData$Survived, trainData$Pclass)
barplot(Pclass_survival, xlab = "Cabin Class", ylab = "Number of People", 
        main = "survived and deceased between male and female")

#proportions
Pclass_survival
Pclass_survival[2] / (Pclass_survival[1] + Pclass_survival[2])
Pclass_survival[4] / (Pclass_survival[3] + Pclass_survival[4])
Pclass_survival[6] / (Pclass_survival[5] + Pclass_survival[6])


#Examine distribution of class fare
#split into buckets
trainData$Fare2 <- '30+'
trainData$Fare2[trainData$Fare < 30 & trainData$Fare >= 20] <- '20-30'
trainData$Fare2[trainData$Fare < 20 & trainData$Fare >= 10] <- '10-20'
trainData$Fare2[trainData$Fare < 10] <- '<10'

Fare2_survival <- table(trainData$Survived, trainData$Fare2)

Fare2_survival
barplot(Fare2_survival, xlab="Fare Group", ylab="Number of People", main="survived and deceased per fare group")

#proportions
Fare2_survival[2]/(Fare2_survival[1]+Fare2_survival[2])
Fare2_survival[4]/(Fare2_survival[3]+Fare2_survival[4])
Fare2_survival[6]/(Fare2_survival[5]+Fare2_survival[6])
Fare2_survival[8]/(Fare2_survival[7]+Fare2_survival[8])

#Examine Age Class
#	Split into buckets

trainData$AgeClass <- '60+'
trainData$AgeClass[trainData$Age < 60 & trainData$Age >= 50] <- '50-60'
trainData$AgeClass[trainData$Age < 50 & trainData$Age >= 40] <- '40-50'
trainData$AgeClass[trainData$Age < 40 & trainData$Age >= 30] <- '30-40'
trainData$AgeClass[trainData$Age < 30 & trainData$Age >= 20] <- '20-30'
trainData$AgeClass[trainData$Age < 20 & trainData$Age >= 10] <- '10-20'
trainData$AgeClass[trainData$Age < 10] <- '0-9'



AgeClass_survival <- table(trainData$Survived, trainData$AgeClass)

barplot(AgeClass_survival, xlab="Age Group", ylab="Number of People", main="survived and deceased per age group")

#Examine Title
#function to extract title

getTitle <- function(data) {
  title.dot.start <- regexpr("\\,[A-Z ]{1,20}\\.", data$Name, TRUE)
  title.comma.end <- title.dot.start
  + attr(title.dot.start, "match.length")-1
  data$Title <- substr(data$Name, title.comma.end+2,title.comma.end+attr(title.dot.start, "match.length")-2)
  return (data$Title)
}

#fetch titles
trainData$Title <- getTitle(trainData)
#view unique
unique(trainData$Title)

Title_survival <- table(trainData$Survived, trainData$Title)
barplot(Title_survival, xlab="Title", ylab="Number of People", main="survived and deceased per Title")

Title_survival



#remove some data 
trainData[c("PassengerId", "Ticket", "Fare", "Cabin")] <- list(NULL)
str(trainData)

#Examine Embarked 
Embarked_survival <- table(trainData$Survived, trainData$Embarked)
barplot(Embarked_survival, xlab="Port of Embarkment", ylab="Number of People", main="survived and deceased per Port of Embark")

Embarked_survival
prop.table(Embarked_survival,2)

#factorize 
trainData$Sex <- as.factor(trainData$Sex)
trainData$AdultChild <- as.factor(trainData$AdultChild)
trainData$Embarked <- as.factor(trainData$Embarked)
trainData$Fare2 <- as.factor(trainData$Fare2)
trainData$AgeClass <- as.factor(trainData$AgeClass)
trainData$Title <- as.factor(trainData$Title)

#inferences on Missing ages
master_vector = grep("Master.",trainData$Name, fixed=TRUE)
miss_vector = grep("Miss.", trainData$Name, fixed=TRUE)
mrs_vector = grep("Mrs.", trainData$Name, fixed=TRUE)
mr_vector = grep("Mr.", trainData$Name, fixed=TRUE)
dr_vector = grep("Dr.", trainData$Name, fixed=TRUE)

#rename in the vectors to standardize naming
for(i in master_vector) {
  trainData$Name[i] = "Master"
}
for(i in miss_vector) {
  trainData$Name[i] = "Miss"
}
for(i in mrs_vector) {
  trainData$Name[i] = "Mrs"
}
for(i in mr_vector) {
  trainData$Name[i] = "Mr"
}
for(i in dr_vector) {
  trainData$Name[i] = "Dr"
}

#replace the missing ages with their respective title-group average. 
#This means that if we have a missing age entry for a man named Mr. Bond, 
#we substitute his age for the average age for all passenger with the title 
#Mr. Similarly for Master, Miss, Mrs, and Dr. We then write a for loop that 
#goes through the entire Train data set and checks if the age value is missing
master_age = round(mean(trainData$Age[trainData$Name == "Master"], na.rm = TRUE), digits = 2)
miss_age = round(mean(trainData$Age[trainData$Name == "Miss"], na.rm = TRUE), digits =2)
mrs_age = round(mean(trainData$Age[trainData$Name == "Mrs"], na.rm = TRUE), digits = 2)
mr_age = round(mean(trainData$Age[trainData$Name == "Mr"], na.rm = TRUE), digits = 2)
dr_age = round(mean(trainData$Age[trainData$Name == "Dr"], na.rm = TRUE), digits = 2)

for (i in 1:nrow(trainData)) {
  if (is.na(trainData[i,5])) {
    if (trainData$Name[i] == "Master") {
      trainData$Age[i] = master_age
    } else if (trainData$Name[i] == "Miss") {
      trainData$Age[i] = miss_age
    } else if (trainData$Name[i] == "Mrs") {
      trainData$Age[i] = mrs_age
    } else if (trainData$Name[i] == "Mr") {
      trainData$Age[i] = mr_age
    } else if (trainData$Name[i] == "Dr") {
      trainData$Age[i] = dr_age
    } else {
      print("Uncaught Title")
    }
  }
}

#remove Name
trainData[c("Name")] <- list(NULL)

#Update missing Embarked value with the most common value:
summary(trainData$Embarked)

trainData$Embarked[which(is.na(trainData$Embarked) | trainData$Embarked=="")] <- 'S'



model <- glm(Survived ~.,family=binomial(link='logit'),data=trainData)
