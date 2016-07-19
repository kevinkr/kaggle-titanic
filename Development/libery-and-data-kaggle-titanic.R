#Liberty and Data Kaggle titanic
#http://www.libertyanddata.com/predicting-titanic-survivors-using-r/

library(caret)
library(rpart)
library(pROC)
library(RCurl)
library(foreign)
library(reshape)

#Load data
titanic_train <- read.csv(text=getURL("https://raw.githubusercontent.com/kevinkr/kaggle-titanic/master/Data/train.csv"),header = TRUE, stringsAsFactors = TRUE)
titanic_test <- read.csv(text=getURL("https://raw.githubusercontent.com/kevinkr/kaggle-titanic/master/Data/test.csv"),header = TRUE, stringsAsFactors = TRUE)
titanic_train$Set <- "Train" # Add column to identify train set in combined set
titanic_test$Set <- "Test"   # Add column to identify test set in combined set 
str(titanic_train)

par(mfrow = c(2,3))

Pclass.prop <- prop.table(table(titanic_train$Survived, titanic_train$Pclass),2)
barplot(Pclass.prop, main = "Survival Rates by Pclass", xlab = "Pclass", ylab = "Percent Survived or Perished", col = c("red", "black"))

Sex.prop <- prop.table(table(titanic_train$Survived, titanic_train$Sex),2)
barplot(Sex.prop, main = "Survival Rates by Sex", xlab = "Sex", ylab = "Percent Survived or Perished", col = c("red", "black"))

SibSp.prop <- prop.table(table(titanic_train$Survived, titanic_train$SibSp),2)
barplot(SibSp.prop, main = "Survival Rates by SibSp", xlab = "SibSp", ylab = "Percent Survived or Perished", col = c("red", "black"))

Parch.prop <- prop.table(table(titanic_train$Survived, titanic_train$Parch),2)
barplot(Parch.prop, main = "Survival Rates by Parch", xlab = "Parch", ylab = "Percent Survived or Perished", col = c("red", "black"))

Embarked.prop <- prop.table(table(titanic_train$Survived, titanic_train$Embarked),2)
barplot(Embarked.prop, main = "Survival Rates by Embarked", xlab = "Embarked", ylab = "Percent Survived or Perished", col = c("red", "black"))

Age.bins <- cut(titanic_train$Age, c(0, 10,20,30,40,50,60,70,80,90,100))
Age.prop <- prop.table(table(titanic_train$Survived, Age.bins),2)
barplot(Age.prop, main = "Survival Rates by Age", xlab = "Age", ylab = "Percent Survived or Perished", col = c("red", "black"), legend = row.names(Age.prop))

#Data Prep
titanic_test$Survived <- NA  # Add Survived column to test set so we can combine train and test sets
full_data <- rbind(titanic_train, titanic_test)
str(full_data)

#Since I imported the data with the option StringsAsFactors = TRUE, I’ll need to clean up the classes a bit.
full_data$Name <- as.character(full_data$Name)
full_data$Ticket <- as.character(full_data$Ticket)
full_data$Cabin <- as.character(full_data$Cabin)
full_data$Survived <- as.factor(full_data$Survived)

#This code counts the number of occurences of NA by column. True values are coded as 1, so summing all the values gives us the number of occurences of NA or or a blank
apply(full_data, 2, FUN = function(x) sum(is.na(x))) # count number of NAs

#And this code counts the number of blanks using the same method.
apply(full_data, 2, FUN = function(x) sum(x == ""))  # count number of blanks

#Now we want to clean up some of those NAs. You may notice that we have a lot of missing ages. I’ll touch on those later in this post. For the one missing fare value, I use the median value of all fares within the passenger class that the passenger with the missing fare is in.
aggregate(Fare ~ Pclass, data = full_data, FUN = "median")

full_data$Pclass[which(is.na(full_data$Fare))]
#So, the passenger is in class 3. I therefore assign a value of $8.05 to that passenger’s fare.
full_data$Fare[which(is.na(full_data$Fare))] <- 8.05

#We also have passengers who have a fare of $0. We impute these passengers’ fares using the median fare of the Pclass as well.
length(which(full_data$Fare == 0)) # How many passengers have a fare of $0?

# assign median fare of respective Pclass to passengers who have a $0 fare.
full_data$Fare[which(full_data$Fare == 0)] <- 
    ifelse(full_data$Pclass[which(full_data$Fare == 0)] == 1, 60.0, 
    ifelse(full_data$Pclass[which(full_data$Fare == 0)] == 2, 15.0458, 
    8.05)) 

#Finally, we have two records with missing information for the port of embarkation. I filter the data to find out which records these are, then I determine if one port is more common than the rest.
full_data[which(full_data$Embarked == ""), ]

prop.table(table(full_data$Embarked))

#Since the vast majority of Embarked are “S”, I impute the values for rows 62 and 830 to be “S”
full_data$Embarked[c(62, 830)] <- "S"


###########
#Feature Engineering
############

#Wehrley gettitle function
getTitle <- function(data) {
  title.dot.start <- regexpr("\\,[A-Z ]{1,20}\\.", data$Name, TRUE)
  title.comma.end <- title.dot.start + attr(title.dot.start, "match.length")-1
  data$Title <- substr(data$Name, title.dot.start+2, title.comma.end-1)
  return (data$Title)
}  
full_data$Title <- getTitle(full_data)

changeTitles <- function(data, old.titles, new.title) {
  for (honorific in old.titles) {
    data$Title[ which( data$Title == honorific)] <- new.title
  }
  return (data$Title)
}

#We can use this function to assign the titles to broader categories and update our feature. The only change I made from Wehrley’s was to add the title of “Dona” when assigning the titles to categories.

full_data$Title <- changeTitles(full_data, c("Capt", "Col", "Don", "Dona", "Dr", "Jonkheer", "Lady", "Major",
                                             "Rev", "Sir"), "Noble")
full_data$Title <- changeTitles(full_data, c("the Countess", "Ms"), "Mrs")
full_data$Title <- changeTitles(full_data, c("Mlle", "Mme"), "Miss")
# We want to change Title from character to factor
full_data$Title <- as.factor(full_data$Title) 

#Now we return to those missing ages. The reason we didn’t worry about them earlier is because I wanted to use the new title feature to help us impute the ages. Many of the titles suggest a certain age group. For example, “master” was used to refer to a young male. We’ll use a decision tree (rpart package) to estimate the missing ages and then update the Age column with the new ages.

estimated_age <- rpart(Age ~ Pclass  + Sex + SibSp + Parch + Fare + Embarked + Title, 
                       data = full_data[!is.na(full_data$Age), ], method = "anova")
# only update rows where Age is NA
full_data$Age[is.na(full_data$Age)] <- predict(estimated_age, full_data[is.na(full_data$Age), ]) 

#Another step we can take is to consider the onboard family size for each passenger. It seems logical that larger families would be more likely to stick together and could therefore find it more difficult to escape the ship. This step is very simple; I add the number of parents/children, the number of siblings/spouses, and 1 for the passenger.
full_data$Family <- full_data$SibSp + full_data$Parch + 1

#Plotting the proportion of survivors by family size seems to lend support to the idea that larger families were less likely to survive.
Family.prop <- prop.table(table(full_data[full_data$Set == "Train", ]$Survived, full_data[full_data$Set == "Train", ]$Family),2) # Get proportion of survivors by family size (for training rows only)

barplot(Family.prop, main = "Survival Rates by Family Size", 
        xlab = "Family Size", ylab = "Percent Survived or Perished", 
        xlim = c(0,12), , col = c("red", "black"), legend = rownames(Family.prop))

#Of course, the larger families were less common, which we can see by counting the number of passengers that belonged to each family size grouping.
barplot(table(full_data$Family), xlab = "Family Size", ylab = "Count of Passengers", main = "Count of Passengers by Family Size", ylim = c(0,800))

#Now we’re ready to start building some models. We’ll first want to split our full_data back into the train and test sets.
titanic_train <- full_data[which(full_data$Set == "Train"), ]
titanic_test <- full_data[which(full_data$Set == "Test"), ]

set.seed(421)

#Logistic Regression
#The first model is the traditional classification go-to of logistic regression.
logistic.model <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Family + Fare + Embarked + Title, 
                      data = titanic_train, family = binomial("logit"))

#After training the model, we look at the ROC curve and area under the ROC curve (AUC). 
logistic.roc <- roc(titanic_train$Survived, predict(logistic.model, newdata = titanic_train, type = "response"))
plot.roc(logistic.roc, print.auc = TRUE)

#Our AUC on our train set is 0.876, which is not bad at all. Let’s generate the predictions for the test data and create a CSV to submit to Kaggle for scoring. Notice that I use a cutoff probability value of 0.5 to classify whether a passenger survives or not. This value is a parameter that we can adjust to improve our fit, but I won’t get into that in this post. A cutoff of 0.5 is oftentimes the default value used.
logistic.test <- predict(logistic.model, newdata = titanic_test, type = "response")
logistic_submission <- data.frame(PassengerId = titanic_test$PassengerId, Survived = ifelse(logistic.test >= 0.5, 1, 0))
write.csv(logistic_submission, file = "liberty-and-data-logistic_submission.csv", row.names = FALSE)
# Kaggle score 0.77990


#################
#Generalized Additive Model (GAM)
##################
#The second model choice, and the model for which I originally decided to write this post, is the generalized additive model (GAM). GAMs are both interpretable and flexible, which makes them very useful. I will build a fairly simple boosted GAM using the train function in the caret package.
gam.model <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch + Family + Fare + Embarked + Title, 
                   data = titanic_train, 
                   method = "gamboost", 
                   preProc = c("center", "scale"))

#To get a sense of the accuracy, we can look at our model as well as the confusion matrix and statistics.
gam.model

