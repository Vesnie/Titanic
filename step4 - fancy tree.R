# Loading the fancy stuff
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Looking at the name, can now access an individual using the row no. 1 as an index
train$Name[1]

# Binding test and train data together
test$Survived <- NA
combi <- rbind(train, test)

# Convert to a string as opposed to factor
combi$Name <- as.character(combi$Name)
# Examine
combi$Name[1]

# Find the indexes for the tile piece of the name using string split (strsplit)
strsplit(combi$Name[1], split='[,.]')
strsplit(combi$Name[1], split='[,.]')[[1]]
strsplit(combi$Name[1], split='[,.]')[[1]][2]

# Engineered variable: Title
combi$Title <- strsplit(combi$Name, split='[,.]')[[1]][2]  # Doesn't work!
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})

# Convert to a factor, stripping spaces from beginning of titles:
combi$Title <- sub(' ', '', combi$Title)

# Looking at new feature
table(combi$Title)

# Combine small title groups
# '%in%' check to see if value is part of vector I'm comparing to
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

combi$Title <- factor(combi$Title)
# no. of family members passenger is travelling with, combining them to 'FamilySize':
combi$FamilySize <- combi$SibSp + combi$Parch + 1

# Engineered variable: did specific family have any issues?
#  ie no 2 family names that share the same name should have the same family size on a small ship
# extract the passengers' last names:
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

# ammend 'FamilySize' var temporarily to a string and combine with Surname to get
#net FamilyID var:
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")

#Lets' create a "small family" which consists of 2 or less members
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

# Inspect new feature...can see there are loads of small families
table(combi$FamilyID)

# naughty families that didn't work well with my assumptions...
# subsetting the dataframe to show only unexpected;y small FamilyID groups:
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'

# Convert to a factor
combi$FamilyID <- factor(combi$FamilyID)

# Split back into test and train sets
train <- combi[1:891,]
test <- combi[892:1309,]

# Build a new tree with new features
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, method="class")
fancyRpartPlot(fit)

# Making a prediction
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "engineeredfeaturestree.csv", row.names = FALSE)

