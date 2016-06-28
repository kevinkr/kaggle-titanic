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
df.train <- train.raw

test.raw <- readData(Titanic.path, test.data.file, 
                     test.column.types, missing.types)
df.infer <- test.raw

## map missing data by provided feature
require(Amelia)
missmap(df.train, main="Titanic Training Data - Missings Map", 
        col=c("yellow", "black"), legend=FALSE)


barplot(table(df.train$Survived),
        names.arg = c("Perished", "Survived"),
        main="Survived (passenger fate)", col="black")

barplot(table(df.train$Pclass), 
        names.arg = c("first", "second", "third"),
        main="Pclass (passenger traveling class)", col="firebrick")
barplot(table(df.train$Sex), main="Sex (gender)", col="darkviolet")
hist(df.train$Age, main="Age", xlab = NULL, col="brown")
barplot(table(df.train$SibSp), main="SibSp (siblings + spouse aboard)", 
        col="darkblue")
barplot(table(df.train$Parch), main="Parch (parents + kids aboard)", 
        col="gray50")
hist(df.train$Fare, main="Fare (fee paid for ticket[s])", xlab = NULL, 
     col="darkgreen")
barplot(table(df.train$Embarked), 
        names.arg = c("Cherbourg", "Queenstown", "Southampton"),
        main="Embarked (port of embarkation)", col="sienna")
#str(df.train$Embarked)
install.packages("vcd")

#travelling class for mosaic plot
mosaicplot(df.train$Pclass ~ df.train$Survived, 
           main="Passenger Fate by Traveling Class", shade=FALSE, 
           color=TRUE, xlab="Pclass", ylab="Survived")

#mosic for fate by gender
mosaicplot(df.train$Sex ~ df.train$Survived, 
           main="Passenger Fate by Gender", shade=FALSE, color=TRUE, 
           xlab="Sex", ylab="Survived")

#Is it possible that "survival of the fittest" dictated the fate of passengers in certain parts of the ship? Perhaps, though it isn't apparent at first glance from the boxplot of Age by Survival.
boxplot(df.train$Age ~ df.train$Survived, 
        main="Passenger Fate by Age",
        xlab="Survived", ylab="Age")

#While passenger survival didn't vary as much across the three ports of embarkation as it did between genders and traveling classes, perhaps the Embarked feature will prove useful at some point.
mosaicplot(df.train$Embarked ~ df.train$Survived, 
           main="Passenger Fate by Port of Embarkation",
           shade=FALSE, color=TRUE, xlab="Embarked", ylab="Survived")

install.packages("corrgram")

require(corrgram)
corrgram.data <- df.train
## change features of factor type to numeric type for inclusion on correlogram
corrgram.data$Survived <- as.numeric(corrgram.data$Survived)
corrgram.data$Pclass <- as.numeric(corrgram.data$Pclass)
corrgram.data$Embarked <- revalue(corrgram.data$Embarked, 
                                  c("C" = 1, "Q" = 2, "S" = 3))
## generate correlogram
corrgram.vars <- c("Survived", "Pclass", "Sex", "Age", 
                   "SibSp", "Parch", "Fare", "Embarked")
corrgram(corrgram.data[,corrgram.vars], order=FALSE, 
         lower.panel=panel.ellipse, upper.panel=panel.pie, 
         text.panel=panel.txt, main="Titanic Training Data")


#Addressing missing ages
#Time to tackle those missing ages. A common approach to this type of situation is to replacing the missings with the average of the available values. In this case, that would mean replacing 177 missing Age values with 29.7.
summary(df.train$Age)

#This makes intuitive sense: Passengers in the upper classes (first and second) would tend to be wealthier, and in that period of U.S. history, acquiring wealth usually required a good deal of time (no dot-com kings in their 20s were aboard the Titanic on her maiden voyage). There are no missing values in Pclass, so we could replace the missing age for, say, a third class passenger with the average or median of the available ages for those in Pclass="3". Doing so would be an improvement over assigning 29.7 to all Age missings.
boxplot(df.train$Age ~ df.train$Pclass, 
        main="Passenger travelling by class",
        xlab="Pclass", ylab="Age")

#Inspection of the next feature -- Name -- reveals what could be an even better approach...
head(df.train$Name, n=10L)

#The title "Miss" should help with differentiation betweeen younger and older females. Also, note the way the title appears in the name: The format "Surname, Title. Firstname..." is consistent in Name across all records. I used that pattern to create a custom function which employs a regular expression and the regexpr function to extract the title from each name:
getTitle <- function(data) {
  title.dot.start <- regexpr("\\,[A-Z ]{1,20}\\.", data$Name, TRUE)
  title.comma.end <- title.dot.start
  + attr(title.dot.start, "match.length")-1
  data$Title <- substr(data$Name, title.comma.end+2,title.comma.end+attr(title.dot.start, "match.length")-2)
  return (data$Title)
}

#get titles and assign them their own column
df.train$Title <- getTitle(df.train)

unique(df.train$Title)

#To identify the titles which have at least one record with an age missing, I'll use the bystats function from the Hmisc package.

install.packages("Hmisc")

options(digits=2)
require(Hmisc)
bystats(df.train$Age, df.train$Title, 
        fun=function(x)c(Mean=mean(x),Median=median(x)))

#Now I can assign the titles with at least one missing Age value to a list...

titles.na.train <- c("Dr", "Master", "Mrs", "Miss", "Mr")

#...then pass that list to the following custom function I created for imputing the missing ages:
imputeMedian <- function(impute.var, filter.var, var.levels) {
  for (v in var.levels) {
    impute.var[ which( filter.var == v)] <- impute(impute.var[ 
      which( filter.var == v)])
  }
  return (impute.var)
}

#I apply the impute function from the Hmisc package on a per-title basis to assign the median of the available ages to the missing age(s). For example, the single record with a missing Age value and Title="Dr" will be assigned the median of the ages from the 6 records with Title="Dr" which do have age data.
df.train$Age[which(df.train$Title=="Dr")]

#After doing the age imputations, I check the Age data and find that the function seems to have done its job.
df.train$Age <- imputeMedian(df.train$Age, df.train$Title, 
                             titles.na.train)

df.train$Age[which(df.train$Title=="Dr")]
summary(df.train$Age)

#Embarked also has missing data
summary(df.train$Embarked)

#...reveals just two missings. It should be fine to replace those missings with "S", the most common value.
df.train$Embarked[which(is.na(df.train$Embarked))] <- 'S'

#While there are no missing Fare values, a summary does show at least one Fare=0...
summary(df.train$Fare)

#(That exceptionally high fare of $512.30 suggests that some tickets were purchased in groups. We'll address that later.) A zero fare might have been assigned to a baby. However, a closer look at records where Fare = 0 suggests otherwise...
subset(df.train, Fare < 7)[order(subset(df.train, Fare < 7)$Fare, 
                                 subset(df.train, Fare < 7)$Pclass), 
                           c("Age", "Title", "Pclass", "Fare")]

#The jump in fares from 0 to the 4-7 range suggests errors. I replaced the zero Fare values with the median fare from the respective passenger class using the imputMedian function introduced earlier.
## impute missings on Fare feature with median fare by Pclass
df.train$Fare[ which( df.train$Fare == 0 )] <- NA
df.train$Fare <- imputeMedian(df.train$Fare, df.train$Pclass, 
                              as.numeric(levels(df.train$Pclass)))

summary(df.train$Fare)

#I see the titles as more than merely a guide for imputation of missing ages. A passenger's title can reflect gender, his/her position on the ship (officers & royalty), and access to a lifeboat (where "Master" superceded "Mr"). Making the effort to get the Title feature model-ready seems worthwhile.

#Recall from the bystats results above that the training data contains 17 different titles. We already know that "Master" and "Mr" should separate the males into roughly two groups by age. The following script...

df.train$Title <- factor(df.train$Title,
                         c("Capt","Col","Major","Sir","Lady","Rev",
                           "Dr","Don","Jonkheer","the Countess","Mrs",
                           "Ms","Mr","Mme","Mlle","Miss","Master"))
boxplot(df.train$Age ~ df.train$Title, 
        main="Passenger Age by Title", xlab="Title", ylab="Age")
#https://drive.google.com/file/d/0B-yx9UUIpB6ubEZ5NU5WSFo1U0E/edit?pref=2&pli=1

#...produces this boxplot (too wide for display here) showing passenger age by title, including shading which illustrates the manner in which I consolidated the titles. I created and applied a custom function for revaluing the titles, then reclassified Title to a factor type, as follows:

## function for assigning a new title value to old title(s) 
changeTitles <- function(data, old.titles, new.title) {
  for (honorific in old.titles) {
    data$Title[ which( data$Title == honorific)] <- new.title
  }
  return (data$Title)
}
## Title consolidation
df.train$Title <- changeTitles(df.train, 
                               c("Capt", "Col", "Don", "Dr", 
                                 "Jonkheer", "Lady", "Major", 
                                 "Rev", "Sir"),
                               "Noble")
df.train$Title <- changeTitles(df.train, c("the Countess", "Ms"), 
                               "Mrs")
df.train$Title <- changeTitles(df.train, c("Mlle", "Mme"), "Miss")
df.train$Title <- as.factor(df.train$Title)
summary(df.train$Title)

