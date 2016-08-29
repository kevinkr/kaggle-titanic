#Jim Nelson Comparison of ML models for Titanic competition
#
#https://rpubs.com/JamesENelson/153826
#
#Modifed for use with Azure ML
#
library("dplyr")

# read data into memory
# Map 1-based optional input ports to variables
train <- maml.mapInputPort(1) # class: data.frame
test <- maml.mapInputPort(2) # class: data.frame


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
#str(titanic.combined)

#Rename and create local data frame for simplicity
data<- tbl_df (titanic.combined)

#Factorize Pclass, dataset and Survived variables
data$Pclass <- factor(data$Pclass)
data$dataset <- factor(data$dataset)
data$Survived<- factor(data$Survived)

########################
#Feature Engineering
###########################
#Hypothesis 1: data visualization suggests being a child and/or a female increased your odds of survival
#Create feature Child from feature Age <= 16 yrs
data <- data %>%
  mutate(Child = Age <=16) 
data$Child <- factor(data$Child)

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
  
  # create dummy variable for data df
  data<-mutate(data, title = "none")
  
  data<-arrange(data, PassengerId)
  
  alltitles<- arrange(alltitles, PassengerId)
  
  # add new feature "title" to data df
  data$title<-alltitles$title
  
  data$title <- factor(data$title)#factorize 'title'
  
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
  
  #Impute value for Age based on logit model
  ageimp <- lm(Age~ Pclass+smallfamily+SibSp+title,  data= data)

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
  

  arrange(data, PassengerId)
  data[59,"Fare"]<-7.854

  #Split data df into train and test datasets
  data<-arrange(data, dataset)
  #test<- data[1:418, ]

  #train<-data[419:1309, ]
  
  # Select data.frame to be sent to the output Dataset port
  maml.mapOutputPort("data");
