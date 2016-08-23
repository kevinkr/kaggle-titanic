#Saad Lamouri SVM model
#https://www.youtube.com/watch?v=tDsiiUYRphM
#https://github.com/slamouri/Kaggle/blob/master/models/sklearn/skLearnSVM.R

library(e1071)
library(caret)
library(RCurl)

# load data

# read data into memory
train <- read.csv(text=getURL("https://raw.githubusercontent.com/kevinkr/kaggle-titanic/master/Data/train.csv"),header =  TRUE, stringsAsFactors = FALSE)
test <- read.csv(text=getURL("https://raw.githubusercontent.com/kevinkr/kaggle-titanic/master/Data/test.csv"),header =  TRUE, stringsAsFactors = FALSE)

# save the number of  rows in the train dataset
train.nrow<-seq(1, nrow(train)) 

#drop survived column in train
train.nosurvive <- train[-2]
train.survive.value <- train[2]

predictors<-rbind(train.nosurvive, test) 



#idenitfy NA values, fill in
apply(predictors, 2, FUN = function(x) sum(is.na(x))) # count number of NAs
#Address Age and Fare

#Addressing Age via Title
#Extract title of each persons name to a new variable “Title”
predictors$Title<-regmatches(as.character(predictors$Name),regexpr("\\,[A-z ]{1,20}\\.", as.character(predictors$Name)))
predictors$Title<-unlist(lapply(predictors$Title,FUN=function(x) substr(x, 3, nchar(x)-1)))
table(predictors$Title)

#Merge 17 different title groups to the most common 4 groups.
predictors$Title[which(predictors$Title %in% c("Mme", "Mlle"))] <- "Miss"
predictors$Title[which(predictors$Title %in% c("Lady", "Ms", "the Countess", "Dona"))] <- "Mrs"
predictors$Title[which(predictors$Title=="Dr" & predictors$Sex=="female")] <- "Mrs"
predictors$Title[which(predictors$Title=="Dr" & predictors$Sex=="male")] <- "Mr"
predictors$Title[which(predictors$Title %in% c("Capt", "Col", "Don", "Jonkheer", "Major", "Rev", "Sir"))] <- "Mr"
predictors$Title<-as.factor(predictors$Title) #convert to factor variable 

#Update unknown age with median age for each group of title
title.age<-aggregate(predictors$Age,by = list(predictors$Title), FUN = function(x) median(x, na.rm = T))
predictors[is.na(predictors$Age), "Age"] <- apply(predictors[is.na(predictors$Age), ] , 1, function(x) title.age[title.age[, 1]==x["Title"], 2])



#Now we want to clean up some of those NAs. You may notice that we have a lot of 
#missing ages. I’ll touch on those later in this post. For the one missing fare 
#value, I use the median value of all fares within the passenger class that the 
#passenger with the missing fare is in.
aggregate(Fare ~ Pclass, data = predictors, FUN = "median")

predictors$Pclass[which(is.na(predictors$Fare))]
#So, the passenger is in class 3. I therefore assign a value of $8.05 to that passenger’s fare.
predictors$Fare[which(is.na(predictors$Fare))] <- 8.05

#We also have passengers who have a fare of $0. We impute these passengers’ 
#fares using the median fare of the Pclass as well.
length(which(predictors$Fare == 0)) # How many passengers have a fare of $0?

# assign median fare of respective Pclass to passengers who have a $0 fare.
predictors$Fare[which(predictors$Fare == 0)] <- 
  ifelse(predictors$Pclass[which(predictors$Fare == 0)] == 1, 60.0, 
         ifelse(predictors$Pclass[which(predictors$Fare == 0)] == 2, 15.0458, 
                8.05)) 

#identify missing values
#And this code counts the number of blanks using the same method.
apply(predictors, 2, FUN = function(x) sum(x == ""))  # count number of blanks
#Address Cabin and Embarked

#Embarked - replace missing values with most common port
predictors$Embarked[which(is.na(predictors$Embarked) | predictors$Embarked=="")] <- 'S'

#Return first character of Cabin or add "Missing"
#I wrote this...;) 8-23-16
predictors$Cabin <- ifelse (predictors$Cabin=="", "Missing", substring(predictors$Cabin, 1, 1))

###
#All missing data elminated. Time to convert to numerical values.
###
str(predictors)
#predictors$Embarked <- as.factor(predictors$Embarked)
#predictors$Sex <- as.factor(predictors$Sex)
#predictors$Cabin <- as.factor(predictors$Cabin)
#convert int and factor to numeric
#predictors[, c(1,2,4,6,7,10,11,12)] <- sapply(predictors[, c(1,2,4,6,7,10,11,12)], as.numeric)

#drop name
predictors <- predictors[-3]

library(dummies)

new_predictors <- dummy.data.frame(predictors, names = c("PassengerID", "Pclass", "Sex", "Age", "SibSp", "Parch", "Ticket", "Cabin", "Embarked", "Title"))





# Explore Data Relationships
library(corrgram)
corrgram(new_predictors,order=NULL,lower.panel=panel.shade,
         upper.panel=NULL,text.panel = panel.txt)

# Apply Principal Component Analysis
pca <- prcomp(new_predictors)
summary(pca)

# Construct Training Data
components <- 12
allData <- pca$x[,1:components]

trainData <- as.data.frame(allData[1:891,])
trainData <- data.frame(trainData, train.survive.value)

# Partition the Data Set
inTrain <- createDataPartition(y = trainData$Survived, p = .75, list = F)

training <- trainData[inTrain,]
testing <- trainData[-inTrain,]

# Fine-tune the Model Parameters
set.seed(102)
tuned <- tune.svm(Survived~., data = training, gamma = seq(.1, .5, by = .1),
                  cost = seq(1,60, by = 10))
tuned$best.parameters

# Train and Test the Model
model  <- svm(Survived~., data = training, 
              gamma=tuned$best.parameters$gamma, 
              cost=tuned$best.parameters$cost, 
              type="eps-regression")
summary(model)

fit <- fitted(model)
print (paste("training accuracy = ", 
             sum(fit == training[,components+1])/length(fit)))

pred <- predict(model, testing[,-(components+1)])
print (paste("testing accuracy = ", 
             sum(pred == testing[,components+1])/length(pred)))
tab <- table(pred, testing[,components+1])
tab
classAgreement(tab)

# Predict with New Data Set
finalData <- as.data.frame(allData[892:1309,])
#finalData <- as.data.frame(allData[1001:10000,])
final <- predict(model, newdata = finalData)

fd <- data.frame(PassengerId = 892:1309, Survived = final)

# write results
write.csv(fd, file = "saad-lamouri-svm-pass4-predictions.csv",row.names=F, quote = F)

#0.7752