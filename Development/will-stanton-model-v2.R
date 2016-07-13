#model based on Will Stanton
library(caret)
library(randomForest)


readData <- function(path.name, file.name, column.types, missing.types) {
  read.csv( url( paste(path.name, file.name, sep="") ), 
            colClasses=column.types,
            na.strings=missing.types )
}

Titanic.path <- "https://raw.githubusercontent.com/kevinkr/kaggle-titanic/master/Data/"
train.data.file <- "train.csv"
test.data.file <- "test.csv"
missing.types <- c("NA", "")
train.column.types <- c('integer',   # PassengerId
                        # 'factor',    # Survived
                        'integer',    # Survived
                        'factor',    # Pclass
                        'character', # Name
                        'factor',    # Sex
                        'numeric',   # Age
                        'integer',   # SibSp
                        'integer',   # Parch
                        'character', # Ticket
                        'numeric',   # Fare
                        'character', # Cabin
                        'factor'     # Embarked
)
test.column.types <- train.column.types[-2]     # # no Survived column in test.csv

train.raw <- readData(Titanic.path, train.data.file, 
                      train.column.types, missing.types)
trainSet <- train.raw

test.raw <- readData(Titanic.path, test.data.file, 
                     test.column.types, missing.types)
testSet <- test.raw

head(trainSet)

head(testSet)

#We want to know which variables are the best predictors for “Survived,” so we will look at the crosstabs between “Survived” and each other variable
table(trainSet[,c("Survived", "Pclass")])

#Looking at this crosstab, we can see that “Pclass” could be a useful predictor of “Survived.” Why? The first column of the crosstab shows that of the passengers in Class 1, 136 survived and 80 died (ie. 63% of first class passengers survived). On the other hand, in Class 2, 87 survived and 97 died (ie. only 47% of second class passengers survived). Finally, in Class 3, 119 survived and 372 died (ie. only 24% of third class passengers survived). 
#We definitely want to use Pclass in our model, because it definitely has strong predictive value of whether someone survived or not.

table(trainSet[,c("Survived", "Sex")])
#Sex looks useful

table(trainSet[,c("Survived", "Embarked")])
#Embarked

#Plots for continuous variables

#Plots are often a better way to identify useful continuous variables than crosstabs are (this is mostly because crosstabs aren't so natural for numerical variables). We will use “conditional” box plots to compare the distribution of each continuous variable, conditioned on whether the passengers survived or not ('Survived' = 1 or 'Survived' = 0). You may need to install the *fields* package first, just like you installed *caret* and *randomForest*.

# Comparing Age and Survived.
library(fields)
bplot.xy(trainSet$Survived, trainSet$Age)
#The box plot of age for those who survived and and those who died are nearly the same. That means that Age probably did not have a large effect on whether one survived or not. The y-axis is Age and the x-axis is Survived (Survived = 1 if the person survived, 0 if not).

#make temp variable and change 0 levels to 1 to plot on log y scale
trainSet$Fare2 <- trainSet$Fare
trainSet$Fare2[(trainSet$Fare2 == 0)] <-1 

bplot.xy(trainSet$Survived, trainSet$Fare2, main="Titanic Pasenger Fate and Fare Paid", 
         xlab="Fate", ylab="Fair Paid", log="y")

#Training a model

#Training the model uses a pretty simple command in caret, but it's important to understand each piece of the syntax. First, we have to convert Survived to a Factor data type, so that caret builds a classification instead of a regression model. Then, we use the train command to train the model (go figure!). You may be asking what a random forest algorithm is. You can think of it as training a bunch of different decision trees and having them vote
#(remember, this is an irresponsibly fast tutorial). Random forests work pretty well in *lots* of different situations, so I often try them first. 

# Convert Survived to Factor
trainSet$Survived <- factor(trainSet$Survived)

# Set a random seed (so you will get the same results as me)
set.seed(42)

# Train the model using a "random forest" algorithm
model <- train(Survived ~ Pclass + Sex + SibSp +   
                 Embarked + Parch + Fare, # Survived is a function of the variables we decided to include
               data = trainSet, # Use the trainSet dataframe as the training data
               method = "rf",# Use the "random forest" algorithm
               trControl = trainControl(method = "cv", # Use cross-validation
                                        number = 5) # Use 5 folds for cross-validation
)

#Evaluating the model

#For the purposes of this tutorial, we will use cross-validation scores to evaluate our model. Note: in real life (ie. not Kaggle), most data scientists also split the training set further into a training set and a validation set, but that is for another post. 

#Cross-validation is a way to evaluate the performance of a model without needing any other data than the training data. It sounds complicated, but it's actually a pretty simple trick. Typically, you randomly split the training data into 5 equally sized pieces called “folds” (so each piece of the data contains 20% of the training data). Then, you train the model on 4/5 of the data, and check its accuracy on the 1/5 of the data you left out. You then repeat this process with each split of the data. At the end, you average the percentage accuracy across the five different splits of the data to get an average accuracy. Caret does this for you, and you can see the scores by looking at the model output:

model

#Making predictions on the test set

#Using caret, it is easy to make predictions on the test set to upload to Kaggle. You just have to call the predict method on the model object you trained. Let's make the predictions on the test set and add them as a new column.
testSet$Survived <- predict(model, newdata = testSet)

#Uh, oh! There is an error here! When you get this type of error in R, it means that you are trying to assign a vector of one length to a vector of a different length, so the two vectors don't line up. So how do we fix this problem? 
#One annoying thing about caret and randomForest is that if there is missing data in the variables you are using to predict, it will just not return a prediction at all (and it won't throw an error!). So we have to find the missing data ourselves. 
summary(testSet)

#As you can see, the variable “Fare” has one NA value. Let's fill (“impute”“) that value in with the mean of the "Fare” column (there are better and fancier ways to do this, but that is for another post). We do this with an ifelse statement. Read it as follows: if an entry in the column “Fare” is NA, then replace it with the mean of the column (also removing the NA's when you take the mean). Otherwise, leave it the same.
testSet$Fare <- ifelse(is.na(testSet$Fare), mean(testSet$Fare, na.rm = TRUE), testSet$Fare)

#Okay, now that we fixed that missing value, we can try again to run the predict method
testSet$Survived <- predict(model, newdata = testSet)

#Let's remove the unnecessary columns that Kaggle doesn't want, and then write the testSet to a csv file.
submission <- testSet[,c("PassengerId", "Survived")]
write.table(submission, file = "will-stanton-submission.csv", col.names = TRUE, row.names = FALSE, sep = ",")
