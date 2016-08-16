# Script for munging titanic data
# Mark Peterson

require(plyr)     # for the revalue function 
require(stringr)  # for the str_sub function
require(Amelia)   # For missing data mssmap
require(Hmisc)    # for impute and some bystats
require(RCurl)
require(foreign)

options(stringsAsFactors = FALSE)

colT.train <- c('integer',   # PassengerId
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
colT.test <- colT.train[-2]     # # no Survived column in test.csv
missing.types <- c("NA", "")

trainData <- read.csv(text=getURL("https://raw.githubusercontent.com/kevinkr/kaggle-titanic/master/Data/train.csv")
                      , colClasses = colT.train
                      , na.strings = missing.types)
head(trainData)
summary(trainData)

testData <- read.csv(text=getURL("https://raw.githubusercontent.com/kevinkr/kaggle-titanic/master/Data/test.csv")
                     , colClasses = colT.test
                     , na.strings = missing.types)
head(testData)
summary(testData)

# Set data types
table(trainData$Survived)
trainData$Survived <- revalue(trainData$Survived
                              , c("1" = "yes" , "0" = "no"))

testData$Survived <- NA

# Merge the data sets
allData <- rbind(trainData,testData[,names(trainData)])
row.names(allData) <- allData$PassengerId

# head(allData)
# table(allData$Survived, useNA = "ifany")

# Functions

# Impute medians from SOUPTONUTS -- my updates
imputeMedian <- function(
  impute.var
  , filter.var
  , var.levels = levels(filter.var)
) {
  for (v in var.levels) {
    impute.var[ filter.var == v] <-
      impute(impute.var[ filter.var == v])
  }
  return (impute.var)
}

# sum(is.na(imputeMedian(allData$Fare,allData$Pclass)))

allData$Embarked[is.na(allData$Embarked)] <- "S"

# Set total family size
allData$Family <- allData$Parch + allData$SibSp

# Port or Starboard
allData$cabinSide <- ifelse(
  as.numeric(str_sub(allData$Cabin,-1)) %% 2 == 0
  , "port"
  , "starboad"
)
allData$cabinSide[is.na(allData$cabinSide)] <- "unk"
table(allData$cabinSide, useNA = "ifany")

# Set the zero fares to class median
allData$Fare[allData$Fare == 0] <- NA
allData$Fare <- imputeMedian(allData$Fare,allData$Pclass)

# Set fares per person instead of ticket
nPerTix <- table(allData$Ticket)
allData$farePerPerson <-
  allData$Fare / nPerTix[allData$Ticket]

# Get titles -- use these for ages where possible
getTitle <- function(data) {
  title.dot.start <- regexpr("\\,[A-Z ]{1,20}\\.", data$Name, TRUE)
  title.comma.end <- title.dot.start + attr(title.dot.start, "match.length")-1
  data$Title <- substr(data$Name, title.dot.start+2, title.comma.end-1)
  return (data$Title)
}   

allData$Title <- getTitle(allData)

# Impute ages from full title
# tempAge <-
allData$Age <- 
  imputeMedian(allData$Age
               , interaction(allData$Title
                             , allData$Pclass
                             , drop = TRUE)
  )
# hist(tempAge, breaks = 0:100)
# hist(allData$Age, breaks = 0:100)

# Fix the title to be simple
changeTitles <- function(data, old.titles, new.title) {
  for (honorific in old.titles) {
    data$Title[ which( data$Title == honorific)] <- new.title
  }
  return (data$Title)
}

allData$Title <- changeTitles(allData, 
                              c("Capt", "Col", "Don", "Dona", "Dr", 
                                "Jonkheer", "Lady", "Major", 
                                "Rev", "Sir"),
                              "Noble")
allData$Title <- changeTitles(allData,
                              c("the Countess", "Ms"), 
                              "Mrs")
allData$Title <- changeTitles(allData
                              , c("Mlle", "Mme")
                              , "Miss")
allData$Title <- as.factor(allData$Title)
# table(allData$Title, useNA = "ifany")

# Split up tickets and grab agent -- needed for group
splitTix <- do.call(rbind,lapply(strsplit(allData$Ticket," "),function(x){
  if(length(x) == 1){
    agent <- "none"
  } else{
    agent <- paste(x[1:(length(x)-1)],collapse = "")
  }
  
  if(is.na(as.numeric(x[length(x)]))){
    agent <- paste(agent, x[length(x)], sep="_")
    ticketNumber <- 0
  } else{
    ticketNumber <- as.numeric(x[length(x)])
  }
  
  return(data.frame(agent = agent,
                    ticketNumber = ticketNumber))
}))

# The na's are already handled in the function
# splitTix[apply(splitTix,1,function(x){any(is.na(x))}),]

# Add a factor for agent to make tickets unique
splitTix$agent <- factor(splitTix$agent)
addBase <- ceiling(log10(max(splitTix$ticketNumber)))
1*10^addBase > max(splitTix$ticketNumber)

# Set final ticket number
splitTix$TicketOut <- 
  as.numeric(splitTix$agent) * 
  10^addBase +
  splitTix$ticketNumber
allData$agent <- splitTix$agent
allData$TicketOut <- splitTix$TicketOut

# Set groups -- can use fare per person instead of my hacky solution
# Keep in group if ticket number diff is less than this
groupThresh <- 5

thisSet <- allData[order(allData$TicketOut),]
tempDiffs <- c(1000,diff(thisSet$TicketOut))
tempGroup <- numeric()
currGroup <- 0
for(k in 1:nrow(thisSet)){
  if(tempDiffs[k] > groupThresh){
    currGroup <- currGroup + 1
  } else if(!(round(thisSet$Fare[k],3) %in% round(thisSet$Fare[k-1] / c(1/(1:10),10:1),3))){
    currGroup <- currGroup + 1
  }
  tempGroup[k] <- currGroup
}
out <- split(thisSet,tempGroup)

# Add group info
# Add number in group
# ID survival of others in group
# thisGroup <- out[[4]]
# Set bayesian prior
bayesPrior <- 4*(table(allData$Survived)/sum(!is.na(allData$Survived)))
groupInfo <- lapply(out, function(thisGroup){
  thisGroup$nInGroup <- nrow(thisGroup)
  thisGroup$groupSurvival <-
    unlist(lapply(1:nrow(thisGroup),function(x){
      othersSurvival <- thisGroup[-x,"Survived"]
      othersSurvival <- othersSurvival[!is.na(othersSurvival)]
      
      if(length(othersSurvival) != 0){
        temp <- (table(othersSurvival)+ bayesPrior)
        rateOut <- (temp/sum(temp))["yes"]
      } else{
        rateOut <- (bayesPrior/sum(bayesPrior))["yes"]
      }
      return(round(rateOut,4))
    }))
  
  return(thisGroup)
  
})

withGroups <- do.call(rbind,groupInfo)
head(withGroups)

write.csv(withGroups
          , paste("imputedData_"
                  #, format(Sys.time(),"%Y%b%d_%H%M")
                  , ".csv"
                  ,sep = "")
          , row.names = FALSE)

