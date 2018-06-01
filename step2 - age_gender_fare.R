# Look at gender patterns to see if "women and children first" was accurate to the results
# We can see that the majority of passengers were male:
summary(train$Sex)

# Doing a 2 way comparison on no. of males and females that survived:
prop.table(table(train$Sex, train$Survived))

# Giving us the proportions in the 1st dimension (rows)
# we can see that the majority of females survived:
prop.table(table(train$Sex, train$Survived), 1)

# Create new column in test set with our prediction that everyone dies
test$Survived <- 0
# Update the prediction to say that all females will survive
test$Survived[test$Sex == 'female'] <- 1

# Create submission dataframe and output to file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "gendermodel.csv", row.names = FALSE)

# Lets look at the age patterns...
#We can see that there are alot of missing values, will assume they are the average age:
summary(train$Age)

# Creating a new column indicating whether or not the passenger was a child (< 18)
#Have also deleted the missing values by adding a zero in the 'n/a', to make sure it passed the boolean test
train$Child <- 0
train$Child[train$Age < 18] <- 1

# 'aggregate' command takes a formula with the target var on the left of the '~'
# and subsets it over on the right
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
aggregate(Survived ~ Child + Sex, data=train, FUN=length)

#Looking at proportions
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# Now looking at the fare and class of each passenger and incorporating that in the formula:
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
# ^^ SHows that majority of males don't do that well regardless of class/fare
# BUT can see that most of the class 3 women who paid > $20 miss out on lifeboat!


# Create new column in test set with our prediction that everyone dies
test$Survived <- 0

# Updating the prediction to say that all females will survive
test$Survived[test$Sex == 'female'] <- 1

# Updating again to say that females who pay more for a third class fare also die
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

# Create submission dataframe and output to file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "genderclassmodel.csv", row.names = FALSE)

