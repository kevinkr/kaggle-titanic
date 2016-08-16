#amazonaws example
#http://rstudio-pubs-static.s3.amazonaws.com/24969_894d890964fd4308ab537bfde1f784d2.html

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
                        'factor',    # Survived 
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
d <- train.raw

test.raw <- readData(Titanic.path, test.data.file, 
                     test.column.types, missing.types)
t <- test.raw

# save the number of  rows in the train dataset
d.nrow<-seq(1, nrow(d)) 

# Merge data to simplify preprocessing
d<-rbind(d, cbind(t, Survived=rep(NA, nrow(t)))) 

library(reshape)
library(caret)

#Check missing values (empty or NA)
d.miss <- melt(apply(d[, -2], 2, function(x) sum(is.na(x) | x=="")))
cbind(row.names(d.miss)[d.miss$value>0], d.miss[d.miss$value>0,])

#cabin has ~80% missing values, don't use
table(d$Embarked)

#replace missing values with most common port
d$Embarked[which(is.na(d$Embarked) | d$Embarked=="")] <- 'S'

#Some Fare values contains sum for tickets were purchased in groups. Introduce a new variable “Price” that will be Fare per person.

# Update missing Fare value with 0.
d$Fare[which(is.na(d$Fare))] <- 0 

# calculate Ticket Price (Fare per person)
ticket.count <- aggregate(d$Ticket, by=list(d$Ticket), function(x) sum( !is.na(x) ))
d$Price<-apply(d, 1, function(x) as.numeric(x["Fare"]) / ticket.count[which(ticket.count[, 1] == x["Ticket"]), 2])

#Price related to passenger class. Missig price values (price=0) we can update with median price per passenger class:
pclass.price<-aggregate(d$Price, by = list(d$Pclass), FUN = function(x) median(x, na.rm = T))
d[which(d$Price==0), "Price"] <- apply(d[which(d$Price==0), ] , 1, function(x) pclass.price[pclass.price[, 1]==x["Pclass"], 2])

#Introduce a new variable “TicketCount” which is the number of passengers that have the same ticket number.
d$TicketCount<-apply(d, 1, function(x) ticket.count[which(ticket.count[, 1] == x["Ticket"]), 2])

#Extract title of each persons name to a new variable “Title”
d$Title<-regmatches(as.character(d$Name),regexpr("\\,[A-z ]{1,20}\\.", as.character(d$Name)))
d$Title<-unlist(lapply(d$Title,FUN=function(x) substr(x, 3, nchar(x)-1)))
table(d$Title)

#Merge 17 different title groups to the most common 4 groups.
d$Title[which(d$Title %in% c("Mme", "Mlle"))] <- "Miss"
d$Title[which(d$Title %in% c("Lady", "Ms", "the Countess", "Dona"))] <- "Mrs"
d$Title[which(d$Title=="Dr" & d$Sex=="female")] <- "Mrs"
d$Title[which(d$Title=="Dr" & d$Sex=="male")] <- "Mr"
d$Title[which(d$Title %in% c("Capt", "Col", "Don", "Jonkheer", "Major", "Rev", "Sir"))] <- "Mr"
d$Title<-as.factor(d$Title) #convert to factor variable 

#Update unknown age with median age for each group of title
title.age<-aggregate(d$Age,by = list(d$Title), FUN = function(x) median(x, na.rm = T))
d[is.na(d$Age), "Age"] <- apply(d[is.na(d$Age), ] , 1, function(x) title.age[title.age[, 1]==x["Title"], 2])

#Split train and test data
t <- d[-d.nrow, ] # test data. It has no "Survival" values. 
d <- d[d.nrow, ] #Train data
set.seed(1234)
inTrain<-createDataPartition(d$Survived, p = 0.8)[[1]]

#Fitting a linear model that includes all variables
fit.8 <- glm(Survived ~ Pclass+Sex+Age+SibSp+Parch+Embarked+Title+Price+TicketCount, data=d[inTrain,], family=binomial(("logit")))
summary(fit.8)

confusionMatrix(d[-inTrain,"Survived"], as.numeric(as.numeric(predict(fit.8, d[-inTrain,])>0.5)))$overall[1]

# Code to prepeare test data for sumbiting on Kaggle. This code commented and for refernce only. 
fit.8 <- glm(Survived ~ Pclass+Sex+Age+SibSp+Parch+Embarked+Title+Price, data=d, family=binomial(("logit")))
predict.8<-predict(fit.8,  newdata=t, type="response")
t$Survived <- as.numeric(as.numeric(predict.8)>0.5)
write.csv(t[,c("PassengerId", "Survived")],"amazonaws-v1.csv", row.names=F)

#Fitting a linear model that includes only 5 statistically significant variable.
fit.5 <- glm(Survived ~ Pclass+Age+SibSp+Parch+Title, data=d[inTrain,], family=binomial("logit"))
summary(fit.5)  

confusionMatrix(d[-inTrain,"Survived"], as.numeric(as.numeric(predict(fit.5, d[-inTrain,])>0.5)))$overall[1]

# train model using all train data and test on kaggle (test result  0.79426)
fit.5 <- glm(Survived ~ Pclass+Age+SibSp+Parch+Title, data=d, family=binomial(("logit")))
predict.5<-predict(fit.5,  newdata=t, type="response")
t$Survived <- as.numeric(as.numeric(predict.5)>0.5)
write.csv(t[,c("PassengerId", "Survived")],"amazonaws-5variables.csv", row.names=F)

#Fitting a linear model that includes 5 statistically significant variable and “TicketCount” converted to a factor variable.
fit.6.grp <- glm(Survived ~ Pclass+Age+SibSp+Parch+Title+I(TicketCount>2), data=d[inTrain,], family=binomial)
summary(fit.6.grp)

confusionMatrix(d[-inTrain,"Survived"], as.numeric(as.numeric(predict(fit.6.grp, d[-inTrain,])>0.5)))$overall[1]

# Code to prepeare test data for sumbiting on Kaggle. This code commented and for refernce only.
fit.6.grp <- glm(Survived ~ Pclass+Age+SibSp+Parch+Title+I(TicketCount>2), data=d, family=binomial)
predict.6.grp<-predict(fit.6.grp,  newdata=t, type="response")
t$Survived <- as.numeric(as.numeric(predict.6.grp)>0.5)
write.csv(t[,c("PassengerId", "Survived")],"amazonaws-6-variables.csv", row.names=F)

#Fitting a linear model that includes 5 statistically significant variable and 
#“TicketCount” and Embarked=S converted to a factor variable.
fit.6.grp2 <- glm(Survived ~ Pclass+Age+SibSp+Parch+Title+I(TicketCount>2)+I(Embarked=="S"), data=d[inTrain,], family=binomial)
summary(fit.6.grp2)

confusionMatrix(d[-inTrain,"Survived"], as.numeric(as.numeric(predict(fit.6.grp2, d[-inTrain,])>0.5)))$overall[1]

# Code to prepeare test data for sumbiting on Kaggle. This code commented and for refernce only.
fit.6.grp2 <- glm(Survived ~ Pclass+Age+SibSp+Parch+Title+I(TicketCount>2)+I(Embarked=="S"), data=d, family=binomial)
predict.6.grp2<-predict(fit.6.grp2,  newdata=t, type="response")
t$Survived <- as.numeric(as.numeric(predict.6.grp2)>0.5)
write.csv(t[,c("PassengerId", "Survived")],"amazonaws-6-grp2-variables.csv", row.names=F)


#Fitting a linear model that includes 5 statistically significant variable and 
#“TicketCount” and Embarked=S and Title=Mr converted to a factor variable.
fit.6.grp3 <- glm(Survived ~ Pclass+Age+SibSp+Parch+I(Title=="Mr")+I(TicketCount>2)+I(Embarked=="S"), data=d[inTrain,], family=binomial)
summary(fit.6.grp3)

confusionMatrix(d[-inTrain,"Survived"], as.numeric(as.numeric(predict(fit.6.grp3, d[-inTrain,])>0.5)))$overall[1]

# Code to prepeare test data for sumbiting on Kaggle. This code commented and for refernce only.
fit.6.grp3 <- glm(Survived ~ Pclass+Age+SibSp+Parch+I(Title=="Mr")+I(TicketCount>2)+I(Embarked=="S"), data=d, family=binomial)
predict.6.grp3<-predict(fit.6.grp3,  newdata=t, type="response")
t$Survived <- as.numeric(as.numeric(predict.6.grp3)>0.5)
write.csv(t[,c("PassengerId", "Survived")],"amazonaws-6-grp3-variables.csv", row.names=F)