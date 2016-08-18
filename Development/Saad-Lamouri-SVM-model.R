#Saad Lamouri SVM model
#https://www.youtube.com/watch?v=tDsiiUYRphM
#https://github.com/slamouri/Kaggle/blob/master/models/sklearn/skLearnSVM.R

library(e1071)
library(caret)
library(RCurl)

# load data

# read data into memory
train <- read.csv(text=getURL("https://raw.githubusercontent.com/kevinkr/kaggle-titanic/master/Data/train.csv"),header =  TRUE)
test <- read.csv(text=getURL("https://raw.githubusercontent.com/kevinkr/kaggle-titanic/master/Data/test.csv"),header =  TRUE)

# save the number of  rows in the train dataset
train.nrow<-seq(1, nrow(train)) 

#drop survived column in train
train.nosurvive <- train[-2]

predictors<-rbind(train.nosurvive, test) 


# Explore Data Relationships
library(corrgram)
corrgram(predictors,order=NULL,lower.panel=panel.shade,
         upper.panel=NULL,text.panel = panel.txt)

# Apply Principal Component Analysis
pca <- prcomp(predictors)
summary(pca)
