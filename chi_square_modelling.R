library(data.table)
library(ggplot2)
library(corrplot)
library(ggcorrplot)

# Set a working directory to store all the related datasets and files.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Import using data.table fread function
raw.data <- fread("./dataset/Churn_Modelling.csv")

# drop useless rows i.e. row number, customer id, and name
data <- raw.data[,`:=`(RowNumber=NULL,CustomerId=NULL, Surname=NULL)]

# print summary of data
summary(data)

# we need to refactor the variables to the correct data type
data$Geography <- factor(data$Geography)
data$Gender <- factor(data$Gender)
data$HasCrCard <- factor(data$HasCrCard)
data$IsActiveMember <- factor(data$IsActiveMember)
data$Exited <- factor(data$Exited)
data$NumOfProducts <- factor(data$NumOfProducts)

# check that refactoring was done right 
summary(data)

# add ageAtStartOfTenure to data
data <- data[,`:=`(ageAtStartOfTenure = (Age-Tenure))]
summary(data)

# get only categorical varibles to run chi-square test
catVar <- select(data, -Exited)

is.factors<-sapply(catVar,is.factor)
catVar<-catVar[,..is.factors]

# do chi square test
chisq.test(data$Exited, catVar$Geography, correct = FALSE)
chisq.test(data$Exited, catVar$Gender, correct = FALSE)
chisq.test(data$Exited, catVar$NumOfProducts, correct = FALSE)
chisq.test(data$Exited, catVar$HasCrCard, correct = FALSE)
chisq.test(data$Exited, catVar$IsActiveMember, correct = FALSE)

# Geography.chisq <- chisq.test(data$Exited, catVar$Geography, correct = FALSE)
# Gender.chiqq <- chisq.test(data$Exited, catVar$Gender, correct = FALSE)
# NumOfProducts.chiqq <- chisq.test(data$Exited, catVar$NumOfProducts, correct = FALSE)
# HasCrCard.chiqq <- chisq.test(data$Exited, catVar$HasCrCard, correct = FALSE)
# isActiveMemeber.chiqq <- chisq.test(data$Exited, catVar$IsActiveMember, correct = FALSE)



## MODELLING 
data.model <-select(data, -HasCrCard)
summary(data.model)
str(data.model)

# Train-Test split
set.seed(2014)
train <- sample.split(Y = data.model$Exited, SplitRatio = 0.7)
trainset <- subset(data.model, train == T)
testset <- subset(data.model, train == F)

## LOGISTICS REGRESSION 
m1 <- glm(Exited ~ ., data=trainset, family="binomial")
summary(m1)

# Confusion Matrix on Trainset
prob.train <- predict(m1, type = 'response')
predict.churn.train <- as.factor(ifelse(prob.train > 0.5, 1, 0))
table1 <- table(trainset$Exited, predict.churn.train)
table1
prop.table(table1)
# Overall Accuracy --> 0.8404286
mean(predict.churn.train == trainset$Exited)

# Confusion Matrix on Testset
prob.test <- predict(m1, newdata = testset, type = 'response')
predict.churn.test <- as.factor(ifelse(prob.test > 0.5, 1, 0))
table2 <- table(testset$Exited, predict.churn.test)
table2
prop.table(table2)
# Overall Accuracy - 0.8413333
mean(predict.churn.test == testset$Exited)

## CART
m2 <- rpart(Exited ~ ., data = trainset, method = 'class',
            control = rpart.control(minsplit = 2, cp = 0))

printcp(m2)
plotcp(m2)
print(m2)

## Min CV Error at Tree #8 and #11
## Based on CP plot, Tree #4 is simplest tree under CV Error Cap so optimal tree is #4.
## Get geometric mean as CP value using #3 (0.03085554) and #4 (0.00584385)
cp1 <- sqrt(0.03085554*0.00584385)

m3 <- prune(m2, cp = cp1)
print(m3)
printcp(m3, digits = 3)
rpart.plot(m3, nn = T, main = "Optimal Tree in Churn Modelling")
m3$variable.importance
## Age, NumOfProducts and ageAtStartOfTenure most important.
summary(m3)

# Confusion Matrix
cart.predict <- predict(m3, newdata = testset, type = "class")

table3 <- table(Testset.Actual = testset$Exited, cart.predict, deparse.level = 2)
table3
round(prop.table(table3), 3)
# Overall Accuracy - 0.857
mean(cart.predict == testset$Exited)

