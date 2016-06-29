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

#All of the work done designing the new Title column can be considered a part of feature engineering. The other features I chose to add are generated using custom function featureEngrg, which can be applied to both the training data in df.train and the Kaggle-provided test data in df.infer.

require(plyr)     # for the revalue function 
require(stringr)  # for the str_sub function

## test a character as an EVEN single digit
isEven <- function(x) x %in% c("0","2","4","6","8") 
## test a character as an ODD single digit
isOdd <- function(x) x %in% c("1","3","5","7","9") 

## function to add features to training or test data frames
featureEngrg <- function(data) {
  ## Using Fate ILO Survived because term is shorter and just sounds good
  data$Fate <- data$Survived
  ## Revaluing Fate factor to ease assessment of confusion matrices later
  data$Fate <- revalue(data$Fate, c("1" = "Survived", "0" = "Perished"))
  ## Boat.dibs attempts to capture the "women and children first"
  ## policy in one feature.  Assuming all females plus males under 15
  ## got "dibs' on access to a lifeboat
  data$Boat.dibs <- "No"
  data$Boat.dibs[which(data$Sex == "female" | data$Age < 15)] <- "Yes"
  data$Boat.dibs <- as.factor(data$Boat.dibs)
  ## Family consolidates siblings and spouses (SibSp) plus
  ## parents and children (Parch) into one feature
  data$Family <- data$SibSp + data$Parch
  ## Fare.pp attempts to adjust group purchases by size of family
  data$Fare.pp <- data$Fare/(data$Family + 1)
  ## Giving the traveling class feature a new look
  data$Class <- data$Pclass
  data$Class <- revalue(data$Class, 
                        c("1"="First", "2"="Second", "3"="Third"))
  ## First character in Cabin number represents the Deck 
  data$Deck <- substring(data$Cabin, 1, 1)
  data$Deck[ which( is.na(data$Deck ))] <- "UNK"
  data$Deck <- as.factor(data$Deck)
  ## Odd-numbered cabins were reportedly on the port side of the ship
  ## Even-numbered cabins assigned Side="starboard"
  data$cabin.last.digit <- str_sub(data$Cabin, -1)
  data$Side <- "UNK"
  data$Side[which(isEven(data$cabin.last.digit))] <- "port"
  data$Side[which(isOdd(data$cabin.last.digit))] <- "starboard"
  data$Side <- as.factor(data$Side)
  data$cabin.last.digit <- NULL
  return (data)
}

## add remaining features to training data frame
df.train <- featureEngrg(df.train)

#Some color on the features I've added:

#Boat.dibs - assumes all females plus males under 15 get "dibs' on access to a lifeboat. Filtering by Title="Master" was considered, but the highest age in the training data for males addressed as "Master" was just 12, and I wanted to account for male teens with Title="Mr" who could pass for a child.
#Deck - levels are as shown in the Titanic cross-section displayed previously. Cabin data provided for just 23 percent of training data records, so it's tough to give this one much emphasis.
#Side - subject to the same concern (dearth of data) expressed for Deck
#I finish the data munging process by paring down the data frame to the columns I will use in model building.

train.keeps <- c("Fate", "Sex", "Boat.dibs", "Age", "Title", 
                 "Class", "Deck", "Side", "Fare", "Fare.pp", 
                 "Embarked", "Family")
df.train.munged <- df.train[train.keeps]

#Fitting a model
#I selected an 80/20 split for training data and testing data. The code:
  
## split training data into train batch and test batch
set.seed(23)
training.rows <- createDataPartition(df.train.munged$Fate, p = 0.8, list = FALSE)
train.batch <- df.train.munged[training.rows, ]
test.batch <- df.train.munged[-training.rows, ]

#Before I go pouring features into the popular Random Forest method, I'm going to give one of the simplest classification methods a crack at the Titanic prediction challenge. Logistic regression, which surfaced about 70 years ago, has been used extensively in multiple fields. I'll start simple by passing essentially the features provided in the raw training data (remember that we combined SibSp and Parch to form Family) through the R function for fitting general linearized models. When entering the model formula, I typically have a habit of listing the features in an order roughly corresponding to what I initially believe their importance will be. In this case, I've ordered them roughly by the two main themes I discussed earlier (women & children first policy and location on the ship). By setting the argument family to binomial with a logit link, I'm asking glm( ) to produce a logistic regression.
Titanic.logit.1 <- glm(Fate ~ Sex + Class + Age + Family + Embarked + Fare, 
                       data = train.batch, family=binomial("logit"))
          
#To assess this first model and the various binary logistic regressions that will appear in its wake, we will use the chi-square statistic, which is basically a measure of the goodness of fit of observed values to expected values. The bigger the difference (or deviance) of the observed values from the expected values, the poorer the fit of the model. The null deviance shows how well passenger survival is predicted by a "null" model using only a constant (grand mean). As we adjust the model's formula by adding and/or removing variables, we'll look for those changes which prompt a drop in the residual deviance, indicating an improvement in fit.
Titanic.logit.1

#The deviance was reduced by 332.2 points on 713-705=8 degrees of freedom (DF), a significant reduction...

1 - pchisq(332.2, df=8)

#In other words, the model put forth is significantly different from the null model. Overall, the model appears to have performed well -- but I'm willing to bet that we could improve on that residual deviance with a different combination of features. Calling anova(), an extractor function, generates the results of the analysis.
anova(Titanic.logit.1, test="Chisq")

#otice how the Sex and Class features accounted for the lion's share of the reduction in the deviance, providing some support to our hypotheses about life boat access and location on ship. Since Fare isn't doing much for us, let's see if the Fare.pp we created fares any better (pun intended).
Titanic.logit.2 <- glm(Fate ~ Sex + Class + Age + Family + Embarked + Fare.pp,                        
                       data = train.batch, family=binomial("logit"))
anova(Titanic.logit.2, test="Chisq")

#Hmm, that was no help. Dropping fares altogether and passing a slightly slimmer formula through the glm() function will give us a new baseline for model improvement.
glm(Fate ~ Sex + Class + Age + Family + Embarked, 
    data = train.batch, family=binomial("logit"))

#Time to shift the model fitting to a higher gear. Henceforth, I'm going to use the train function in Kuhn's caret package to fit binary logistic regression models, as well as models built using other methods.

#Modeling taken to an extreme on a training data set can leave you with a model which very accurately maps the training data, but does not generalize well to new samples. This phenomenon, commonly referred to as overfitting, can be addressed by resampling the training samples in a way which approximates the fitted model's performance on future data. I'm going to use a form of resampling known as 10-fold cross-validation (CV), repeated 3 times.

#Later, I plan to compare the fitted logit model to other model types using the receiver operating characteristic (ROC) curve. The twoClassSummary function in caret can calculate the figures I'll need for that if I give it class probabilities predicted by the logistic regression model.

#All of these things I want -- 3x 10-fold CV, estimation of class probabilities, metrics from twoClassSummary -- can be passed through the trainControl function.

## Define control function to handle optional arguments for train function
## Models to be assessed based on largest absolute area under ROC curve
cv.ctrl <- trainControl(method = "repeatedcv", repeats = 3,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE)

require(pROC)
#Below is the train function call using the same formula (sans Fare) that we recently passed through glm function. I use the metric argument to tell train to optimize the model by maximizing the area under the ROC curve (AUC). summary(), another extractor function, is called to generate regression coefficients with standard errors and a z-test, plus the residual deviance metric we were watching earlier.
set.seed(35)
glm.tune.1 <- train(Fate ~ Sex + Class + Age + Family + Embarked,
                    data = train.batch,
                    method = "glm",
                    metric = "ROC",
                    trControl = cv.ctrl)
glm.tune.1

summary(glm.tune.1)

#This is as good a time as any to introduce the concept of class compression. Think of it as collapsing particular levels on a categorical variable. One of the earlier bar graphs showed about 70 percent of the Titanic's passengers boarded the ship at Southampton. I'm going to use Embarked and the I() function, which inhibits interpretation & conversion of R objects, to create a new 2-level factor within the model formula. This factor is valued TRUE if a passenger's port of origin was Southampton ("S"), or FALSE otherwise.

set.seed(35)
glm.tune.2 <- train(Fate ~ Sex + Class + Age + Family + I(Embarked=="S"),
                    data = train.batch, method = "glm",
                    metric = "ROC", trControl = cv.ctrl)
summary(glm.tune.2)

#As I discussed earlier, the Title feature addresses more than one theme. For that reason, I believe it has real potential to improve this model. Besides, I put a good chunk of effort into it, so why not give it a go?
set.seed(35)

glm.tune.3 <- train(Fate ~ Sex + Class + Title + Age 
                    + Family + I(Embarked=="S"), 
                    data = train.batch, method = "glm",
                    metric = "ROC", trControl = cv.ctrl)

summary(glm.tune.3)

#Nice! That gave us our first material decline in the residual deviance. Since the Title feature seems to give us everything that Age did (and more), I'm going to drop Age from the formula. I will also collapse the titles “Miss” and “Mrs” and leave a duo of Title-related factors which should represent the “women and children first” theme well.
set.seed(35)
glm.tune.4 <- train(Fate ~ Class + I(Title=="Mr") + I(Title=="Noble") 
                    + Age + Family + I(Embarked=="S"), 
                    data = train.batch, method = "glm",
                    metric = "ROC", trControl = cv.ctrl)
summary(glm.tune.4)

#Remember that there were a lot of male passengers in third class. Given the “women and children first” policy already mentioned plus reports that the Titanic's internal layout was confusing (I recall reading that one crew member claimed it took him two weeks to become comfortable with finding his way around the ship), to say that “grown men in the lower decks had it tough” is such a gross understatement that I hesitated to put it in type. A feature reflecting those third-class men might make a further dent in that residual deviance. Indeed, it does...
set.seed(35)
glm.tune.5 <- train(Fate ~ Class + I(Title=="Mr") + I(Title=="Noble") 
                    + Age + Family + I(Embarked=="S") 
                    + I(Title=="Mr"&Class=="Third"), 
                    data = train.batch, 
                    method = "glm", metric = "ROC", 
                    trControl = cv.ctrl)
summary(glm.tune.5)

#Unfortunately, the other features did not contribute to further deviance compression. Taking a different approach to representing the “women and children first” policy didn't bear fruit (removing the title references in the formula and adding Boat.dibs produced a residual deviance of 565 -- no better than what we already have, using a new feature which some may find confusing). Given that Deck and Side combined (a) shaved just a few points off of the deviance, and (b) were derived from such a small subset of the training data, I decided to withdraw them from consideration.


