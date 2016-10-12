# bootcamp kaggle titanic

#decision tree

# load libraries
library(ggplot2)
library(RCurl)
library(mice)
library(randomForest)
library(rpart)
library(mlbench)
library(caret)
library(dplyr)
library(Hmisc)

# load datasets
trainData <- read.csv(text=getURL("https://raw.githubusercontent.com/kevinkr/kaggle-titanic/master/Data/train.csv"),header = TRUE, stringsAsFactors = FALSE)
testData <- read.csv(text=getURL("https://raw.githubusercontent.com/kevinkr/kaggle-titanic/master/Data/test.csv"),header = TRUE, stringsAsFactors = FALSE)

# save the number of  rows in the train dataset
trainData.nrow<-seq(1, nrow(trainData))

# Merge data to simplify preprocessing
allData<-rbind(trainData, cbind(testData, Survived=rep(NA, nrow(testData)))) 

# Assign factors
allData$Survived <- as.factor(allData$Survived)
allData$Sex <- as.factor(allData$Sex)
allData$Embarked <- as.factor(allData$Embarked)
allData$Pclass <- as.factor(allData$Pclass)

#Assign levels
#levels(allData$Survived) <- c("Dead", "Survived")  #0 1
#levels(allData$Embarked) <- c("Unknown", "Cherbourg", "Queenstown", "Southampton") # UNK C Q S

# Assess Sex
# summary(allData$Sex)
# counts <- table(allData$Survived, allData$Sex)
# barplot(counts, xlab = "Gender", ylab = "Number of People", main = "survived and deceased between male and female")
# 
# counts <- table(allData$Survived, allData$Embarked)
# barplot(counts, xlab = "Embarked", ylab = "Number of People", main = "survived and deceased for embarkment")
# 
# counts <- table(allData$Survived, allData$Pclass)
# barplot(counts, xlab = "Pclass", ylab = "Number of People", main = "survived and deceased for class")


###############
# Cleaning the data
###############

# fill in missing Ebarked value
allData$Embarked[which(is.na(allData$Embarked) | allData$Embarked=="")] <- 'S'

# drop unknown level now
allData$Embarked <- factor(allData$Embarked)
levels(allData$Embarked)

#####
# Resolve NA's in Age
#####

# Fix missing Fare
allData$Fare[1044] <- mean(allData[allData$Pclass == 3 & allData$Embarked == 'S' & allData$Age > 60, 'Fare'], na.rm = TRUE)

table(allData$Fare == 0)
# 17 missing fares
# 
#...then pass that list to the following custom function I created for imputing the missing values:
imputeMedian <- function(impute.var, filter.var, var.levels) {
  for (v in var.levels) {
    impute.var[ which( filter.var == v)] <- impute(impute.var[ 
      which( filter.var == v)])
  }
  return (impute.var)
}

allData$Fare[ which( allData$Fare == 0 )] <- NA

allData$Fare <- imputeMedian(allData$Fare, allData$Pclass, 
                              as.numeric(levels(allData$Pclass)))


# Plot age distributions
 par(mfrow=c(1,2)) 
 hist(allData$Fare, freq=F, main='Age: Original Data', 
      col='darkgreen', ylim=c(0,0.04))


 par(mfrow=c(1,1)) 


#################
# Assign age per average fare in Pclass
##################

library(GGally)
ggpairs(allData[!names(allData) %in% c('PassengerId','Name','Ticket','Cabin')], ggplot2::aes(color=Survived))

g <- ggplot(data = allData, aes(x=Age))
g + geom_histogram(aes(color=Pclass, fill=Pclass))

#Examine missing Age
#split data set for comparison

# Trying to find patterns in data that could be potentially used to impute Age

allData<- allData %>% 
  mutate (missAge = is.na(Age) )

#Pclass
g <- ggplot(data = subset(allData, !is.na(Survived)), aes(x=FareBin))
g + geom_bar(aes(color=Survived, fill=Survived)) + facet_wrap(~ missAge)

#Fare
g <- ggplot(data = subset(allData, !is.na(Survived)), aes(x=Fare))
g + geom_bar(aes(color=Survived, fill=Survived)) + facet_wrap(FareBin ~ missAge) + xlim(0,100) 


g <- ggplot(data = subset(allData, !is.na(Survived)), aes(x=Fare))
g + geom_bar(aes(color=Survived, fill=Survived)) + facet_wrap(newTicket ~ missAge) + xlim(0,100) 


g <- ggplot(data = subset(allData, !is.na(Survived)), aes(x=NotAlone))
g + geom_bar(aes(color=Survived, fill=Survived)) + facet_wrap(~ missAge)

g <- ggplot(data = subset(allData, !is.na(Survived)), aes(x=Pclass))
g + geom_bar(aes(color=Survived, fill=Survived)) + facet_wrap(~ missAge)

#Pclass
g <- ggplot(data = subset(allData, !is.na(Survived)), aes(x=FareBin))
g + geom_bar(aes(color=Survived, fill=Survived)) + facet_wrap(missAge ~ Pclass)

#Title
g <- ggplot(data = subset(allData, !is.na(Survived)), aes(x=Title))
g + geom_bar(aes(color=Survived, fill=Survived)) + facet_wrap(missAge ~ Pclass)

# Compare mean fares by ticket groups to see if it can be used to impute Age
missAgeMeanFare <- allData%>%
  group_by(newTicket)%>%
  filter(missAge==1)%>%
  summarise(mean(Fare))

hasAgeMeanFare <- allData%>%
  group_by(newTicket)%>%
  filter(missAge==0)%>%
  summarise(mean(Fare))
colnames(hasAgeMeanFare) <- c("newTicket", "hasAgeMeanFare(mean(Fare))")

ageMeanCompare <- left_join(missAgeMeanFare, hasAgeMeanFare, by="newTicket")


# Check if there is NewTicket - Pclass association
missAgePclassComp <- allData%>%
  group_by(newTicket)%>%
  filter(missAge==1)%>%
  count(Pclass)

hasAgePclassComp <- allData%>%
  group_by(newTicket)%>%
  filter(missAge==0)%>%
  count(Pclass)

agePclassCompare <- left_join(missAgePclassComp, hasAgePclassComp, by="newTicket")

# # Perform mice imputation, excluding certain less-than-useful variables:
# mice_mod1 <- mice(allData[, !names(allData) %in% c('PassengerId','Name','Ticket','Cabin','Survived')], method='rf') 
# 
# 
# 
# # Save the complete output 
# mice_output <- complete(mice_mod)

#Let’s compare the results we get with the original distribution of passenger ages 
#to ensure that nothing has gone completely awry.

# Plot age distributions
# par(mfrow=c(1,2))
# hist(allData$Age, freq=F, main='Age: Original Data', 
#      col='darkgreen', ylim=c(0,0.04))
# hist(mice_output$Age, freq=F, main='Age: MICE Output', 
#      col='lightgreen', ylim=c(0,0.04))
# 
# par(mfrow=c(1,1))
# Replace Age variable from the mice model.
#allData$Age <- mice_output$Age

###
# Fare
###
par(mfrow=c(1,2))
g <- ggplot(allData[allData$Pclass==3 & allData$Title=="Mr" & allData$missAge==0,], aes(x=Fare))
g + geom_density(aes(x=Fare), fill="gray50")

g <- ggplot(allData[allData$Pclass==3 & allData$Title=="Mr" & allData$missAge==1,], aes(x=Fare))
g + geom_density(aes(x=Fare), fill="gray50")

# g <- ggplot(allData, aes(x=Fare))
# g + geom_area(stat="bin")

#g <- ggplot(allData, aes(x=log(Fare)))
#g + geom_area(stat="bin")

#check for NA's
# table(allData$Embarked)
# table(allData$Fare)
# table(allData$Age)

# 3 people on one ticket...revising down
#allData$RevisedFare[which(allData$Fare > 200)] <- max(allData$Fare)/3

# scale the Fare
allData$ScaledFare <- (allData$Fare - min(allData$Fare))/(max(allData$Fare) - min(allData$Fare))
summary(allData$ScaledFare)

####
# Bin the fare
allData$FareBin <- 'over200'
allData$FareBin[allData$Fare < 200 & allData$Fare >= 50] <- '50to200'
allData$FareBin[allData$Fare < 50 & allData$Fare >= 30] <- '30to50'
allData$FareBin[allData$Fare < 30 & allData$Fare >= 20] <- '20to30'
allData$FareBin[allData$Fare < 20 & allData$Fare >= 10] <- '10to20'
allData$FareBin[allData$Fare < 10] <- 'lessThan10'
allData$FareBin <- factor(allData$FareBin)


###
# Assign adult or child
####
# Create the column child, and indicate whether child or adult
allData$Child[allData$Age < 13] <- 'True'
allData$Child[allData$Age >= 13] <- 'False'
allData$Child <- factor(allData$Child)

#plot(density(allData$Age, na.rm = TRUE))
# Show counts
#table(allData$Child, allData$Survived)

#####
# Assign Mother
#####
# Adding Mother variable
allData$Mother <- 'False'
allData$Mother[allData$Sex == 'female' & allData$Parch > 0 & allData$Age > 16] <- 'True'
allData$Mother <- factor(allData$Mother)

# Not alone
allData$NotAlone <- 'False'
allData$NotAlone[(allData$SibSp + allData$Parch) >= 1] <- 'True'
allData$NotAlone <- factor(allData$NotAlone)

#####
# 5. Give kids and women preference
#####
allData$WomenChildrenFirst <- 'False'
allData$WomenChildrenFirst[which(allData$Sex=="female" | allData$Age < 14)] <- 'True'
allData$WomenChildrenFirst <- factor(allData$WomenChildrenFirst)
#g <-  ggplot(allData, aes(x=Age,y=ScaledFare))
#g + geom_point(aes(color=!is.na(Survived))) + facet_wrap(~WomenChildrenFirst)

# Show counts
#table(allData$WomenChildrenFirst, allData$Survived)

######
# 6. Keep SibSp <= 2 and Parch <= 2 . . . small groups survive better
#####
allData$SmallGroups <- 'False'
allData$SmallGroups[which(allData$SibSp <= 2 & allData$Parch <= 2)] <- 'True'
allData$SmallGroups <- factor(allData$SmallGroups)
#table(allData$Survived, allData$SmallGroups)

# g <-  ggplot(allData, aes(x=Age,y=ScaledFare))
# g + geom_point(aes(color=!is.na(Survived))) + facet_grid(SmallGroups ~ Sex)
# 
# #######
# Associate surviving children with parents
######
# TBD


##################
# Work on names
##################

# Grab title from passenger names
allData$Title <- gsub('(.*, )|(\\..*)', '', allData$Name)
unique(allData$Title)
# Reassign mlle, ms, and mme accordingly
allData$Title[allData$Title == 'Mlle'] <- 'Miss' 
allData$Title[allData$Title == 'Ms'] <- 'Miss'
allData$Title[allData$Title == 'Mme'] <- 'Mrs' 
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
allData$Title[allData$Title %in% rare_title]  <- 'RareTitle'
allData$Title <- factor(allData$Title)

########################
# Find children who survived
########################
#
# get last Name
temp <- strsplit(allData$Name, ", ")
temp.rows <- do.call('rbind', temp)
allData$LastName <- temp.rows[,1]
 
# child.survivors.index <- subset(allData, subset = allData$Survived == 1)
# child.survivors.index.2 <- subset(child.survivors.index, subset=duplicated(child.survivors.index$LastName)| duplicated(child.survivors.index$LastName, fromLast = TRUE))
# child.survivors.index.2$survivorGroup <- 'Yes'
# 
# allData$survivorGroup <- 'No'
###????? How to merge ????


####################
# HasCabin feature
####################
allData$hasCabin <- 'False'
for(i in 1:length(allData$Survived)){
  if(allData$Cabin[i]!="") allData$hasCabin[i] <- 'True'
}
#table(allData$Survived, allData$hasCabin)
allData$hasCabin <- factor(allData$hasCabin)


###############
# Trim ticket number to two characters
###############
#remove spaces and special characters
library(stringr)
allData$newTicket <- str_replace_all(allData$Ticket, "[^[:alnum:]]", "")
# trim to first three characters
allData$newTicket <- substr(allData$newTicket, 1, 2)
allData$newTicket <- factor(allData$newTicket)


####
# Add title + embarkment combo
allData$TitleEmbarked <- paste(allData$Title,allData$Embarked, sep="")
allData$TitleEmbarked <- factor(allData$TitleEmbarked)

#####
# Pclass + Embarked
allData$PclassEmbarked <- paste(allData$Pclass,allData$Embarked, sep="")
allData$PclassEmbarked <- factor(allData$TitleEmbarked)

#######
# Women CHildren First + Embarked
allData$WCFEmbarked <- paste(allData$WomenChildrenFirst,allData$Embarked, sep="")
allData$WCFEmbarked <- factor(allData$WCFEmbarked)

######
# One hot encoding here...Pclass
#####
#library(dummies)

#allData <- dummy.data.frame(allData, names=c("Pclass"), sep="_")
#allData <- dummy.data.frame(allData, names=c("SibSp"), sep="_")
#allData <- dummy.data.frame(allData, names=c("Parch"), sep="_")

#allData <- dummy.data.frame(allData, names=c("newTicket"), sep="")

###################
# split into test and train
####################
# drop columns Name, Ticket, Cabin
#allData <- allData[ -c(6,11,13) ]

allData$Name <- NULL
allData$Ticket <- NULL
allData$Cabin <- NULL
allData$Fare <- NULL
allData$LastName <- NULL
allData$newTicket <- NULL
allData$Age <- NULL
allData$NotAlone <- NULL
allData$Child <- NULL
allData$Mother <- NULL


###
# Done just glmnet
###
titanicDummy <- dummyVars("~.",data=allData, fullRank=F)
allData <- as.data.frame(predict(titanicDummy,allData))
print(names(allData))
allData$Survived <- as.factor(allData$Survived)

##########################################
# Create train set
trainSet <- allData[1:891,]
# drop passenger id from trainSet
trainSet <- trainSet[-c(1)]

#return test Set
testSet <- allData[892:nrow(allData),]



# Test Options
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 27


metric <- "Accuracy"

preProcess=c("center", "scale")

# Linear Discriminant Analysis
set.seed(seed)
fit.lda <- train(Survived~., data=trainSet, method="lda", metric=metric, preProc=c("center", "scale"), trControl=control)
# Logistic Regression
set.seed(seed)
fit.glm <- train(Survived~., data=trainSet, method="glm", metric=metric, trControl=control)
# GLMNET
set.seed(seed)
fit.glmnet <- train(Survived~., data=trainSet, method="glmnet", metric=metric, preProc=c("center", "scale"), trControl=control)
# SVM Radial
set.seed(seed)
fit.svmRadial <- train(Survived~., data=trainSet, method="svmRadial", metric=metric, preProc=c("center", "scale"), trControl=control, fit=FALSE)
# kNN
set.seed(seed)
fit.knn <- train(Survived~., data=trainSet, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)
# Naive Bayes
set.seed(seed)
fit.nb <- train(Survived~., data=trainSet, method="nb", metric=metric, trControl=control)
# CART
set.seed(seed)
fit.cart <- train(Survived~., data=trainSet, method="rpart", metric=metric, trControl=control)
# C5.0
set.seed(seed)
fit.c50 <- train(Survived~., data=trainSet, method="C5.0", metric=metric, trControl=control)
# Bagged CART
set.seed(seed)
fit.treebag <- train(Survived~., data=trainSet, method="treebag", metric=metric, trControl=control)
# Random Forest
set.seed(seed)
fit.rf <- train(Survived~., data=trainSet, method="rf", metric=metric, trControl=control)
# Stochastic Gradient Boosting (Generalized Boosted Modeling)
set.seed(seed)
fit.gbm <- train(Survived~., data=trainSet, method="gbm", metric=metric, trControl=control, verbose=FALSE)


############
# Model selection
############

results <- resamples(list(logistic=fit.glm, glmnet=fit.glmnet,
                          svm=fit.svmRadial, knn=fit.knn, nb=fit.nb, cart=fit.cart, c50=fit.c50,
                          bagging=fit.treebag, rf=fit.rf, gbm=fit.gbm))
# Table comparison
summary(results)

# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)

###########################
# Choosing glmnet
#######
MyTrainControl=trainControl(
  method = "cv",
  number=10,
  repeats=5
)

model <- train(Survived~.,data=trainSet,method='glmnet',
               tuneGrid = expand.grid(.alpha=(1:10) * 0.05,.lambda = (1:10) * 0.05),
               trControl=MyTrainControl,
               metric = "Accuracy")
model
plot(model, metric='Accuracy')

importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)


testSet$Survived <- predict(model, newdata = testSet)

submission <- testSet[,c("PassengerId", "Survived")]
write.table(submission, file = "ds-dojo-10-6-2016-v5-glmnet.csv", col.names = TRUE, row.names = FALSE, sep = ",")

