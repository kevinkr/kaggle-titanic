install.packages("RCurl")
install.packages("foreign")
install.packages("MKLE")
library(RCurl)
library(foreign)
train <- read.csv(text=getURL("https://raw.githubusercontent.com/kevinkr/kaggle-titanic/master/Data/train.csv"),header = TRUE, stringsAsFactors = FALSE)
test <- read.csv(text=getURL("https://raw.githubusercontent.com/kevinkr/kaggle-titanic/master/Data/test.csv"),header = TRUE, stringsAsFactors = FALSE)

#rename to match code example
trainData <- train
testData <- test

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
  title.dot.start <- regexpr("\\,[A-Z]{1-20}\\.", data$Name, TRUE)
  title.comma.end <- title.dot.start + attr(title.dot.start, "match.length")-1
  data$Title <- substr(data$Name, title.dot.start+2, title.comma.end-1)
  return (data$Title)
}

#fetch titles
trainData$Title <- getTitle(trainData)
unique(trainData$Title)



