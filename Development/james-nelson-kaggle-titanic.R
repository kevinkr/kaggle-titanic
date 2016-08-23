#Jim Nelson Comparison of ML models for Titanic competition
#
#https://rpubs.com/JamesENelson/153826
#
###################
#LOAD PACKAGES AND DATASETS
#######################
#load packages
library(dplyr)
library(ggplot2)
library(RCurl)

# read data into memory
train <- read.csv(text=getURL("https://raw.githubusercontent.com/kevinkr/kaggle-titanic/master/Data/train.csv"),header =  TRUE, stringsAsFactors = TRUE)
test <- read.csv(text=getURL("https://raw.githubusercontent.com/kevinkr/kaggle-titanic/master/Data/test.csv"),header =  TRUE, stringsAsFactors = TRUE)
str(train)
str(test)


#########################
###Data Transformation and Verification
#########################
#
#Change survived to factor
train$Survived<- factor(train$Survived)

#Create dummy variable in test
test<- mutate(test, Survived = "none")

#Create sorting variable dataset before combining
test <- mutate(test, dataset = "testset")
train <- mutate(train, dataset = "trainset")

#Combine training and test datasets for feature engineering
titanic.combined <- rbind(test, train)
str(titanic.combined)

#Rename and create local data frame for simplicity
data<- tbl_df (titanic.combined)

#Factorize Pclass, dataset and Survived variables
data$Pclass <- factor(data$Pclass)
data$dataset <- factor(data$dataset)
data$Survived<- factor(data$Survived)

#Check for duplicates
IDdups <- distinct(data, PassengerId)
dim(IDdups)

Namedups <- distinct(data, Name)
dim(Namedups)

filter(data, duplicated(Name)) 
filter(data, grepl('Kelly|Connolly', Name, Age ))

##############################
#Data Exploration
###############################
#Descriptive stats
summary(tbl_df(data))
head(data)

#1. overall Age and Cabin variables are missing ~20% of values
#2. Fare is missing 1 value
#3. Embarked is missing 2 values

#Visualize some potentially important features as a function of survival
#Age
trainset<-data%>% arrange(dataset)%>%slice(419:1309)
head(trainset)
glimpse(trainset)

hist_Age <- ggplot(trainset, aes(Age, fill=Survived))
  hist_Age +geom_bar(binwidth = 5)

  hist_Age + geom_bar(position="fill", binwidth=4) #proportions 

#Sex
hist_Sex <- ggplot(trainset, aes(x=Sex, fill=Survived))  
  hist_Sex + geom_bar(position= "fill") # defaults to stacking
  hist_Sex + geom_bar(position= "fill") #proportions
  
#Pclass (cabin type)
hist_Pclass <- ggplot(trainset, aes(x=Pclass, fill=Survived))
  hist_Pclass + geom_bar() # defaults to stacking 
  hist_Pclass + geom_bar(position= "fill") #proportions
  
#SibSp (no. siblings/spouse)
hist_SibSp <- ggplot(trainset, aes(x=SibSp, fill=Survived, binwidth = .0005))
  hist_SibSp + geom_bar() # defaults to stacking
  hist_SibSp + geom_bar(position= "fill") #proportions  
  
#Parch (no. parents/children)
hist_Parch <- ggplot(trainset, aes(x=Parch, fill=Survived))
  hist_Parch + geom_bar() # defaults to stacking  
  hist_Parch + geom_bar(position= "fill") #proportions 
  
########################
#Feature Engineering
###########################
#Hypothesis 1: data visualization suggests being a child and/or a female increased your odds of survival
#Create feature Child from feature Age <= 16 yrs
data <- data %>%
  mutate(Child = Age <=16) 
data$Child <- factor(data$Child)
  glimpse (data)

#Visualize survival as a function of Child
  trainset<-data%>% arrange(dataset)%>%slice(419:1309)
  
  hist_Child <- ggplot(trainset, aes(x=Child, fill=Survived))
  hist_Child + geom_bar() # defaults to stacking
  
  hist_Child + geom_bar(position= "fill") #proportions 
  