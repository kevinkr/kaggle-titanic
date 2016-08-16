#mark peterson kaggle titanic example
#https://bitbucket.org/petersmp/titanic/src/983d6af19aea5835c68640fd7aa46f313f57a14d/scripts/playWithTitanicData.R?at=master&fileviewer=file-view-default

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
table(trainData$Survived)

missmap(trainData, main="Titanic Training Data - Missings Map", 
        col=c("yellow", "black"), legend=FALSE)

#Display barplots for all columns
catch <- lapply(names(trainData),function(thisCol){
  barplot(table(trainData[[thisCol]])
          ,main = thisCol)
})

par(mfrow = c(1,1))

#Consider survival rates for useful columns
usefulColNames <- c(
  "Pclass"
  , "Sex"
  , "SibSp"
  , "Parch"
  , "Embarked"
)

catch <- lapply(usefulColNames,function(thisCol){
  plot(table(trainData[[thisCol]],trainData[["Survived"]])
       ,main = thisCol
       ,col = gray.colors(2))
})

#Examine numeric data for survival rates
numericCols <- c(
  "PassengerId"
  , "Age"
  , "SibSp"
  , "Parch"
  , "Fare"
)

catch <- lapply(numericCols,function(thisCol){
  plot(trainData[[thisCol]] ~ trainData[["Survived"]]
       ,main = thisCol
       ,ylab = thisCol
  )
})

# Check any family
tempAnyFam <- factor( (trainData$Parch > 0) + 2*(trainData$SibSp > 0)
                      , levels = 0:3)
tempAnyFam <- factor(tempAnyFam, labels = c("none"
                                            ,"ParchOnly"
                                            ,"SibSpOnly"
                                            ,"ParchAndSibSp")
)
plot(
  table(tempAnyFam
        ,trainData$Survived
  ))

getTitle <- function(data) {
  title.dot.start <- regexpr("\\,[A-Z ]{1,20}\\.", data$Name, TRUE)
  title.comma.end <- title.dot.start + attr(title.dot.start, "match.length")-1
  data$Title <- substr(data$Name, title.dot.start+2, title.comma.end-1)
  return (data$Title)
}   

head(trainData$Name)
tempTitle <- getTitle(trainData)
head(tempTitle)
table(tempTitle)

cbind(unclass(by(trainData$Age,tempTitle,function(x){median(x,na.rm = TRUE)}))
      ,unclass(by(trainData$Age,tempTitle,function(x){mean(x,na.rm = TRUE)}))
      ,unclass(by(trainData$Age,tempTitle,function(x){sd(x,na.rm = TRUE)}))
      ,unclass(by(trainData$Age,tempTitle,function(x){sum(is.na(x))}))
)

tempTitle[!is.na(trainData$Age) & trainData$Age < 14 & trainData$Sex == "female"]
hist(trainData$Age[tempTitle == "Miss"])

## list of titles with missing Age value(s) requiring imputation
titles.na.train <- c("Dr", "Master", "Mrs", "Miss", "Mr")

imputeMedian <- function(impute.var, filter.var, var.levels) {
  for (v in var.levels) {
    impute.var[ which( filter.var == v)] <- impute(impute.var[ 
      which( filter.var == v)])
  }
  return (impute.var)
}

trainData$Age <- imputeMedian(trainData$Age
                              , tempTitle
                              , titles.na.train)


trainData$Title <- factor(tempTitle
                          , c("Capt","Col","Major","Sir","Lady","Rev"
                              ,"Dr","Don","Jonkheer","the Countess","Mrs"
                              ,"Ms","Mr","Mme","Mlle","Miss","Master"))
boxplot(trainData$Age ~ trainData$Title, 
        main="Passenger Age by Title", xlab="Title", ylab="Age")

## function for assigning a new title value to old title(s) 
changeTitles <- function(data, old.titles, new.title) {
  for (honorific in old.titles) {
    data$Title[ which( data$Title == honorific)] <- new.title
  }
  return (data$Title)
}
table(trainData$Title)

## Title consolidation
trainData$Title <- as.character(trainData$Title)
trainData$Title <- changeTitles(trainData, 
                                c("Capt", "Col", "Don", "Dr", 
                                  "Jonkheer", "Lady", "Major", 
                                  "Rev", "Sir"),
                                "Noble")
trainData$Title <- changeTitles(trainData, c("the Countess", "Ms"), 
                                "Mrs")
trainData$Title <- changeTitles(trainData, c("Mlle", "Mme"), "Miss")
trainData$Title <- as.factor(trainData$Title)
table(trainData$Title)

plot(Fare ~ I(SibSp +Parch), data = trainData)
trainData[trainData$Fare > 500,]
testData[is.na(testData$Fare),]
# The above were all on the same ticket, but not listed as family
# because the non- Cardeza's were servants
trainData[grep("^Ward",trainData$Name),]
trainData[grep("^Cardeza",trainData$Name),]
trainData[grep("^Lesurer",trainData$Name),]
testData[grep("^Cardeza",testData$Name),]
trainData[trainData$Ticket == "PC 17755",]
testData[testData$Ticket == "PC 17755",]

sort(table(trainData$Ticket)[table(trainData$Ticket)>1])
sum(is.na(trainData$Ticket))
nPerTix <- table(c(trainData$Ticket
                   , testData$Ticket))

farePerPerson <- trainData$Fare / nPerTix[trainData$Ticket]
hist(farePerPerson)
boxplot(farePerPerson ~ trainData$Pclass)
boxplot(trainData$Fare ~ trainData$Pclass)
trainData[farePerPerson == max(farePerPerson),]

# Why three rooms? Map shows it is bed plus two "sitting" with private promenade
# Who is on the other side?
trainData[grep("B54",trainData$Cabin),]
testData[grep("B54",trainData$Cabin),]
trainData[grep("B56",trainData$Cabin),]
testData[grep("B56",trainData$Cabin),]

# Not listed?
trainData[farePerPerson > 80 & farePerPerson < 100,]

# Who had more than one room?
trainData[grep(" ",trainData$Cabin),]

# The class 3 appear to be a weird issue, otherwise all are first class.
twoCabFirst1 <- grepl(" ",trainData$Cabin[trainData$Pclass == "1"])
boxplot(farePerPerson[trainData$Pclass == "1"] ~ twoCabFirst1)
trainData[grepl(" ",trainData$Cabin) & trainData$Pclass == "3",]

# Any other looks?

# Odd even?
head(trainData$Cabin)
cabinSide <- ifelse(
  as.numeric(str_sub(trainData$Cabin,-1)) %% 2 == 0
  , "port"
  , "starboad"
)
table(cabinSide, useNA = "ifany")  

# Some parties appear to have sequential instead of identical tickets
# Can I parse that?

# Get the tickets and fares
allTix <- c(trainData$Ticket, testData$Ticket)
allFares <- c(trainData$Fare, testData$Fare)

fareTable <- table(allFares)
length(fareTable)
barplot(fareTable)

tixTable <- table(allTix)
length(tixTable)
length(allTix)
barplot(tixTable)
tixTable[which.max(tixTable)]

trainData[ trainData$Ticket == names(tixTable)[which.max(tixTable)],]
testData[ testData$Ticket == names(tixTable)[which.max(tixTable)],]

# This does suggest that a "proportion of party known to survive" might be good
# Just make sure not to count that ind.

# Back to identifying parties with unique tickets
tixTable
# Appears to be two parts -- idenifier of some sort (travel agent?)
# plus a number. Will need to confirm that the number are still unique
# after stripping first part
# Better -- going to leave that in, just use a numeric for each agent

splitTix <- do.call(rbind,lapply(strsplit(allTix," "),function(x){
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
head(splitTix)
length(table(splitTix$ticketNumber))
length(table(allTix))
# so, 5 of the ticket numbers are none unique
length(table(splitTix$agent))
splitTix$agent <- factor(splitTix$agent)

addBase <- ceiling(log10(max(splitTix$ticketNumber)))
1*10^addBase > max(splitTix$ticketNumber)

# Set final ticket number
splitTix$TicketOut <- as.numeric(splitTix$agent) * 10^addBase +
  splitTix$ticketNumber
splitTix$fare <- allFares
head(splitTix)
splitTix[apply(splitTix,1,function(x){any(is.na(x))}),]
# For now -- set Fare as 0
splitTix$fare[is.na(splitTix$fare)] <- 0

sort(fareTable)
firstTest <- splitTix[splitTix$fare == 7.8958,]
table(firstTest$agent)
diff(sort(firstTest$ticketNumber))

allData <- rbind(trainData[,-c(2,13)],testData)
allData[splitTix$fare == 7.8958, ]
sum(splitTix$agent == "none")
7.8958 * sum(splitTix$fare == 7.8958)
386.89/ sum(splitTix$fare == 7.8958)
bigGroupNames <- allData$Name[splitTix$fare == 7.8958]
bigGroupLast <- unlist(lapply(strsplit(bigGroupNames,","),function(x){x[1]}))
# The group seems very odd, but a few shared names, and alot of "off"s
sort(bigGroupLast)
bigGroupLast[order(firstTest$TicketOut)]
# If they are a group, it is not clear that the numbers sort them within the group
# Can/ should I lump these?
# It is probably safe, but there may be issues.

# Figure out how to assign groups when the tickets are close and fares identical
# Specifically, how close is close?

allData <- cbind(allData, splitTix[,1:3])

splitByFare <- split(allData,allData$Fare)
length(splitByFare)
nClasses <- unlist(lapply(splitByFare,function(thisFare){
  length(table(as.character(thisFare$Pclass)))
}))
sum(nClasses != 1)
splitByFare[nClasses != 1]
names(splitByFare)[nClasses != 1]
# Only a few tix are not true; seems managable

# Adding agent to see what that takes care
splitByFare <- split(allData
                     , interaction(allData$Fare
                                   ,allData$agent
                                   ,drop = TRUE
                                   , sep = "_"))
length(splitByFare)
nClasses <- unlist(lapply(splitByFare,function(thisFare){
  length(table(as.character(thisFare$Pclass)))
}))
sum(nClasses != 1)
splitByFare[nClasses != 1]
names(splitByFare)[nClasses != 1]
unlist(lapply(splitByFare[nClasses != 1]
              , nrow))
# Still only a few -- can I manage this
hist(unlist(lapply(splitByFare
                   , nrow)))
sum(allData$Fare == 0, na.rm = TRUE)
table(allData$Pclass[allData$Fare == 0])
allData[allData$Fare == 0,c("Pclass","Name","Sex","Age","Cabin","ticketNumber","agent")]
# LINE are American Line employees

# Use 30_none as test case
tempTest <- splitByFare[["30_none"]]
sort(tempTest$TicketOut)
diff(sort(tempTest$TicketOut))
allData[grep("^Christy",allData$Name),]
tixDiffs <- diff(sort(allData$TicketOut))
hist(tixDiffs)
barplot(table(tixDiffs[tixDiffs<10]))
table(tixDiffs < 10)
table(tixDiffs < 3)
table(tixDiffs < 5)
# I am going to set a threshold of <5 to count as same group.

groupThresh <- 5
# Loop through to do groups
# Split the fares further
# thisSet <- splitByFare[["30_none"]]
reSplit <- lapply(splitByFare,function(thisSet){
  thisSet <- thisSet[order(thisSet$TicketOut),]
  tempDiffs <- c(1000,diff(thisSet$TicketOut))
  tempGroup <- numeric()
  currGroup <- 0
  for(k in 1:nrow(thisSet)){
    if(tempDiffs[k] > groupThresh){
      currGroup <- currGroup + 1
    }
    tempGroup[k] <- currGroup
  }
  out <- split(thisSet,tempGroup)
  out <- lapply(out,function(x){
    x$group <- x$TicketOut[1]
    return(x)
  })
  finalOut <- do.call(rbind,out)
  row.names(finalOut) <- finalOut$PassengerId
  return(finalOut)
})
reMerge <- do.call(rbind,reSplit)
row.names(reMerge) <- reMerge$PassengerId
barplot(table(table(reMerge$group)))

# 
head(reMerge)
# There is at least one set that looks like it should be a group:
reMerge[4:7,]

# Try without Fare split
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
length(out)
table(unlist(lapply(out,function(x){length(table(as.character(x$Pclass)))})))
nPerGroup <- unlist(lapply(out,nrow))
hist(nPerGroup)
barplot(table(factor(nPerGroup, levels = 1:max(nPerGroup))))
out[[which.max(nPerGroup)]][,c("Name","Sex","Age","Ticket","Fare")]
out <- lapply(out,function(x){
  x$group <- x$TicketOut[1]
  return(x)
})
finalOut <- do.call(rbind,out)
row.names(finalOut) <- finalOut$PassengerId
head(finalOut,7)
finalOut[grep("^Davies",finalOut$Name),]
# Caluculate and add survival of group as well.

# Double check ages -- too many of the Sages are 21
hist(trainData$Age, breaks = 0:100)
sort(table(trainData$Age), decreasing = FALSE)
# I don't trust the 21's or the 30's
# HUGE jump there
hist(trainData$Age[trainData$Age != 30 & trainData$Age != 21], breaks = 0:100)
# Still a few more at 35 thatn I like, but those are probably closer
## Make sure to set 21 and 30 to NA

