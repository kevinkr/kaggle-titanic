# Analyze munged Titanic Data
# Mark Peterson

setwd("~/GitHub/kaggle-titanic/Development")

# Load packages
library(caret)
library(MASS)
library(pROC)

fullData <- read.csv("imputedData_.csv")
head(fullData)

trainData <- fullData[!is.na(fullData$Survived),]
testData  <- fullData[ is.na(fullData$Survived),]

##KJK added
trainData$Survived <- revalue(trainData$Survived
                              , c("yes" = "1" , "no" = "0"))

# Trim to usable rows
trimTrain <- trainData[,-c(grep("PassengerId|Name|Cabin|SibSp|Parch|Ticket|TicketOut"
                                ,names(trainData)))]
head(trimTrain)
names(trimTrain)[apply(trimTrain,2,function(x){any(is.na(x))})]

## split training data into train batch and test batch
set.seed(16652)
rowsTrain <- createDataPartition(trainData$Survived
                                 , p = 0.8
                                 , list = FALSE)


batchTrain <- trimTrain[ rowsTrain, ]
validTrain <- trimTrain[!rowsTrain, ]
names(trimTrain)

# Play with logit
fullLogit <- glm(factor(Survived) ~ .
                 , batchTrain[,]
                 , family = binomial("logit"))
summary(fullLogit)
anova(fullLogit)

steppedLogit <- stepAIC(fullLogit
                        , direction = "both")
anova(steppedLogit)
summary(steppedLogit)

# Directions seem a bit odd -- double check the success/failures
testOut <- predict(steppedLogit
                   , type = "resp" # Needed to get 0 to 1 range
)
hist(testOut)
plot(testOut ~ batchTrain$Survived)
plot(as.numeric(batchTrain$Survived) ~ testOut)

valsToCheck <- seq(0.01,.99,.01)
goodCalls <- unlist(lapply(valsToCheck,function(x){
  chisq.test( table(testOut > x, batchTrain$Survived))$statistic
}))

hist(goodCalls)
plot(goodCalls ~ valsToCheck)  
plot(goodCalls ~ valsToCheck
     , xlim = c(.4,.6))  
valsToCheck[which.max(goodCalls)]

# Try cross validation:
cvMethod <- trainControl(method = "repeatedcv",
                         repeats = 3,
                         summaryFunction = twoClassSummary,
                         classProbs = TRUE)

# Basing on good model above
trained <- train(Survived ~ Pclass + Sex + Age + Family + Title + groupSurvival
                 , data = batchTrain
                 , method = "glm"
                 , metric = "ROC"
                 , trControl = cvMethod
)

summary(trained)



