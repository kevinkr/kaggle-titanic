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

#Hypothesis 2: Did a persons Title effect survivability?
  
  #Create new feature called Title based on the Name feature
  
  Mr<-filter(data, grepl('Mr.' ,Name, fixed=TRUE ))
  Mr<-mutate(Mr, title = 'Mr')
  
  Mrs<-filter(data, grepl('Mrs.', Name, fixed=TRUE ))
  Mrs<-mutate(Mrs, title = 'Mrs')
  
  Miss<-filter(data, grepl('Miss.', Name, fixed=TRUE ))
  Miss<-mutate(Miss, title = 'Miss')
  
  Master<-filter(data, grepl('Master.', Name, fixed=TRUE  ))
  Master<-mutate(Master, title = 'Master')
  
  Dr <-filter(data, grepl('Dr.', Name, fixed=TRUE  ))
  Dr<-mutate(Dr, title = 'UCMale')
  
  Rev<-filter(data, grepl('Rev.', Name, fixed=TRUE  ))
  Rev<-mutate(Rev, title = 'UCMale')
  
  Ms<-filter(data, grepl('Ms.', Name, fixed=TRUE  ))
  Ms<-mutate(Ms, title = 'Mrs')
  
  Major<-filter(data, grepl('Major.', Name, fixed=TRUE  ))
  Major<-mutate(Major, title = 'UCMale')
  
  Col<-filter(data, grepl('Col.', Name, fixed=TRUE  ))
  Col<-mutate(Col, title = 'UCMale')
  
  Dona<-filter(data, grepl('Dona.', Name, fixed=TRUE  ))
  Dona<-mutate(Dona, title = 'UCFemale')
  
  Don<-filter(data, grepl('Don.', Name, fixed=TRUE  ))
  Don<-mutate(Don, title = 'UCMale')
  
  Capt<-filter(data, grepl('Capt.', Name, fixed=TRUE  ))
  Capt<-mutate(Capt, title = 'UCMale')
  
  Sir<-filter(data, grepl('Sir.', Name, fixed=TRUE  ))
  Sir<-mutate(Sir, title = 'UCMale')
  
  Lady<-filter(data, grepl('Lady.', Name, fixed=TRUE  ))
  Lady<-mutate(Lady, title = 'UCFemale')
  
  Mlle<-filter(data, grepl('Mlle.', Name, fixed=TRUE  ))
  Mlle<-mutate(Mlle, title = 'Miss')
  
  Mme<-filter(data, grepl('Mme.', Name, fixed=TRUE  ))
  Mme<-mutate(Mme, title = 'Miss')
  
  Ctss<-filter(data, grepl('Countess.', Name, fixed=TRUE  ))
  Ctss<-mutate(Ctss, title = 'UCFemale')
  
  Jonk<-filter(data, grepl('Jonkheer.', Name, fixed=TRUE  ))
  Jonk<-mutate(Jonk, title = 'UCMale')
  
  Dr<-Dr[-8, ] # remove the female Dr from 'Dr' df
  
  FDr<-filter(data, grepl('Leader', Name, fixed=TRUE  ))
  FDr<-mutate(FDr, title = 'UCFemale')
  
  # Create seperate title class, by sex, for people with titles indicative of the upper class
  UCMale<- rbind(Dr, Rev, Sir, Major, Col, Capt, Don, Jonk)
  UCFemale<- rbind(Lady, Dona, Ctss, FDr)
  
  # combine "Ms" with "Mrs" and "Mme"/"Mlle" with Miss
  Mrs<- rbind(Mrs, Ms)
  Miss<- rbind(Miss, Mme, Mlle)  
  
  # combine all title into one variable "title"
  tbl_df(alltitles<-rbind(Mr, Mrs, Miss, Master, UCMale, UCFemale))
  glimpse (alltitles) 
  tail(alltitles)
  
  # create dummy variable for data df
  data<-mutate(data, title = "none")
  glimpse(data)
  
  data<-arrange(data, PassengerId)
  head(data)
  
  alltitles<- arrange(alltitles, PassengerId)
  head(alltitles)
  
  # add new feature "title" to data df
  data$title<-alltitles$title
  summary(data)
  
  data$title <- factor(data$title)#factorize 'title'
  
  #Survival as a function of title
  trainset<-data%>% arrange(dataset)%>%slice(419:1309)
  head (trainset)
  glimpse(trainset)
  
  hist_title <- ggplot(trainset, aes(x=title, fill=Survived))
  hist_title + geom_bar() # defaults to stacking
  hist_title + geom_bar(position= "fill") #proportions 
  
  #Verify Age range for each title group
  data%>%
    group_by(title)%>%
    filter(!is.na(Age))%>%
    summarise(min(Age))
  
  data%>%
    group_by(title)%>%
    filter(!is.na(Age))%>%
    summarise(max(Age))
  
  #How many people with titles of “Mr” and “Mrs” are <=16
  under16<-filter(data, Age<=16)
  under16%>%group_by(title)%>% summarise(n())
  
  data%>%group_by(title)%>% summarise(n())
  
  #Update Child feature based on above data;assume Miss is not a Child
  is.na(data$Child[data$title=="Master"]<-TRUE)
  is.na(data$Child[data$title=="Mr" ]<-FALSE)
  is.na(data$Child[data$title=="Mrs" ]<-FALSE)
  is.na(data$Child[data$title=="UCMale" ]<-FALSE)
  is.na(data$Child[data$title=="UCFemale" ]<-FALSE)
  is.na(data$Child[data$title=="Miss" ]<-FALSE)
  
  #Hypothesis 3: Data visualization suggests traveling alone 
  #decreased your odds of survival but also suggests 
  #families >=4 had decreased survival odds
  
  #Create 2 new categorical features notalone and familysize
  
  data<- data %>% 
    mutate (familysize = SibSp  + Parch +1 ) %>%
    mutate(notalone = familysize >1) 
  
  data$notalone<- factor(data$notalone)
  glimpse (data)
  
  #Visualize survival as a function of notalone and familysize
  trainset<-data%>% arrange(dataset)%>%slice(419:1309)
  head (trainset)
  glimpse(trainset)
  
  hist_notalone <- ggplot(trainset, aes(x=notalone, fill=Survived))
  hist_notalone + geom_bar() # defaults to stacking
  hist_notalone + geom_bar(position= "fill") #proportions
  
  hist_familysize <- ggplot(trainset, aes(x=familysize, fill=Survived))
  hist_familysize + geom_bar() # defaults to stacking
  hist_familysize + geom_bar(position= "fill") #proportions
  
  #Hypothesis 4: data visualization suggests that small families 
  #had increased odds of survival
  
  #Create new categorical feature smallfamily from 
  #familysize >1 but <4 (ie between 2-4 people total)
  data$smallfamily[data$familysize >1 & data$familysize<=4] <-1
  data$smallfamily[data$familysize == 1 | data$familysize>4 ] <-0
  data$smallfamily <- factor(data$smallfamily)
  
  #Create feature for just 3rd Class to test as a surrogate for Pclass
  data$thirdClass[data$Pclass ==3 ] <-1
  data$thirdClass[data$Pclass ==1 | data$Pclass==2 ] <-0
  data$thirdClass <- factor(data$thirdClass)  
  
  #Visualize survival as a function of having a smallfamily or 3rd class cabin
  trainset<-data%>% arrange(dataset)%>%slice(419:1309)
  head (trainset)
  glimpse(trainset)
  
  hist_smallfamily <- ggplot(trainset, aes(x=smallfamily, fill=Survived))
  hist_smallfamily + geom_bar() # defaults to stacking
  hist_smallfamily + geom_bar(position= "fill") #proportions
  
  #Visualize thirdClass
  hist_thirdclass <- ggplot(trainset, aes(x=thirdClass, fill=Survived))
  hist_thirdclass + geom_bar() # defaults to stacking
  hist_thirdclass+ geom_bar(position= "fill") #proportions
  
  #Impute value for Age based on logit model
  ageimp <- lm(Age~ Pclass+smallfamily+SibSp+title,  data= data)
  summary(ageimp)
  
  # assign imputed Age values for NAs in combined.df
  for(i in 1:nrow(data)) {
    if(is.na(data[i, "Age"])) {
      data[i, "Age"] <- predict(ageimp, newdata = data[i, ])  
    }
  }
  
  #Impute missing fare value for passenger 1044 based on median cost of thirdclass single ticket
  data<-arrange(data, desc(thirdClass))
  data<-arrange(data, SibSp)
  data<-arrange(data, Parch)
  
  threemeanfare<-data[1:472, "Fare"]
  summary(threemeanfare)
  
  arrange(data, PassengerId)
  data[59,"Fare"]<-7.854
  summary(data$Fare)
  
  #Split data df into train and test datasets
  data<-arrange(data, dataset)
  test<- data[1:418, ]
  class(test)
  
  train<-data[419:1309, ]
  train$Survived <- droplevels(train$Survived) 
  test$Survived <- droplevels(test$Survived) 
  str(test)
  str(train)
  
  #################################
  #MACHINE LEARNING PREDICTIVE MODELING
  ##################################
  #
  #Logistic Regression
  library(glm2)
  
  #First perform univariate logistic regression for each important feature
  #Age
  agemodel <- glm(Survived ~ Age, family="binomial", data= train)
  summary(agemodel)
  exp(cbind(OR = coef(agemodel), confint(agemodel))) # odds ratios and 95% CI
  
  #Sex
  sexmodel <- glm(Survived ~ Sex, family="binomial", data= train)
  summary(sexmodel)
  exp(cbind(OR = coef(sexmodel), confint(sexmodel))) # odds ratios and 95% CI
  
  #Cabin class
  Pclassmodel <- glm(Survived ~ Pclass, family="binomial", data= train)
  summary(Pclassmodel)
  exp(cbind(OR = coef(Pclassmodel), confint(Pclassmodel))) # odds ratios and 95% CI
  
  #3rdclass
  thirdclassmodel <- glm(Survived ~ thirdClass, family="binomial", data= train)
  summary(thirdclassmodel)
  exp(cbind(OR = coef(thirdclassmodel), confint(thirdclassmodel))) # odds ratios and 95% CI
  
  #Sibs/spouse
  sibsmodel <- glm(Survived ~ SibSp, family="binomial", data= train)
  summary(sibsmodel)
  exp(cbind(OR = coef(sibsmodel), confint(sibsmodel))) # odds ratios and 95% CI
  
  #Parents/children
  Parchmodel <- glm(Survived ~ Parch, family="binomial", data= train)
  summary(Parchmodel)
  exp(cbind(OR = coef(Parchmodel), confint(Parchmodel))) # odds ratios and 95% CI
  
  #Fare
  faremodel <- glm(Survived ~ Fare, family="binomial", data= train)
  summary(faremodel)
  exp(cbind(OR = coef(faremodel), confint(faremodel))) # odds ratios and 95% CI
  
  #Embarked
  embmodel <- glm(Survived ~ Embarked, family="binomial", data= train)
  summary(embmodel)
  exp(cbind(OR = coef(embmodel), confint(embmodel))) # odds ratios and 95% CI
  
  