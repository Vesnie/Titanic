#!!!Ensure to set the working directory first...'Session -> set working directory'!!!
train <- read.csv("./train.csv")
test <- read.csv("./test.csv")

# Examine structure of dataframe using 'str'
str(train)

# Look at number of people who survived
# Can see that only 38% of passengers survived using prop.table
table(train$Survived)
prop.table(table(train$Survived))

# Create new column in test set with our prediction that everyone dies
test$Survived <- rep(0, 418)

# Create submission dataframe and output to file
# There is no 'Survived' column in the dataframe...so let's add it
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)

