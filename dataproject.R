# install.packages("corrplot")
# install.packages("ggcorrplot")
# install.packages("data.table")

library(data.table)
library(ggplot2)
library(corrplot)
library(ggcorrplot)
library(caTools)

# Set a working directory to store all the related datasets and files.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Import using data.table fread function
raw.data <- fread("./dataset/Churn_Modelling.csv")

# drop useless rows i.e. row number, customer id, and name
data <- raw.data[,`:=`(RowNumber=NULL,CustomerId=NULL, Surname=NULL)]

summary(data) # no missing values in any of the remaining columns 

# we need to refactor the variables to the correct data type
data$Geography <- factor(data$Geography)
data$Gender <- factor(data$Gender)
data$HasCrCard <- factor(data$HasCrCard)
data$IsActiveMember <- factor(data$IsActiveMember)
data$Exited <- factor(data$Exited)

summary(data) # check that refactoring was done right 

# first we need to confirm that the credit score is using PICO (US credit scoring system)
# use ggplot to see the general distribution
ggplot(data[,.(CreditScore)], aes(CreditScore)) + geom_histogram(binwidth = 5)

# is there any particular country(ies) that are more likely to churn?
ggplot(data[,.(Geography,Exited)], aes(Geography,fill=Exited)) + geom_bar(position = "stack")
ggplot(data[,.(Geography,Exited)], aes(Geography,fill=Exited)) + geom_bar(position = "fill")
# seems like though France make up majority of customers, yet Germany-based ones churn more than proportionately 

# why might this be the case? maybe because Germany has higher average income? 
ggplot(data[,.(EstimatedSalary,Geography)], aes(x=EstimatedSalary, fill=Geography)) + geom_histogram(binwidth = 1000)
# no, no clear trend with regards to estimated salary
# do German customers tend to be older?
ggplot(data[,.(Age,Geography)], aes(x=Age, fill=Geography)) + geom_histogram(binwidth = 5)
# no clear trend for age
ggplot(data[,.(Balance,Geography)], aes(x=Balance, fill=Geography)) + geom_histogram(binwidth = 10000)
# no clear trend for bank balance
ggplot(data[,.(Tenure,Geography)], aes(x=Tenure, fill=Geography)) + geom_histogram(binwidth = 1)
ggplot(data, aes(x=CreditScore, fill=Geography)) + geom_histogram(binwidth = 5)
ggplot(data, aes(Geography, fill=HasCrCard)) + geom_bar(position="fill")
ggplot(data, aes(x=NumOfProducts, fill=Geography)) + geom_bar(position="fill")


as.numeric()# does gender have implications on salary (perhaps due to gender inequality?)
ggplot(data[,.(Gender,EstimatedSalary)], aes(Gender,EstimatedSalary)) + geom_boxplot() 
ggplot(data[,.(Gender,Balance)], aes(Gender,Balance)) + geom_boxplot() 
# seems like there is a fair amount of gender equality, at least on the financial front



# does credit score have any correlation with age, tenure, balance, num of products or estimated salary?
corr <- cor(data[,.(CreditScore,Age,Tenure,Balance,NumOfProducts,EstimatedSalary)], use = 'pairwise.complete.obs')
ggcorrplot(corr, lab = TRUE)
# seems like credit score has no correlation with any of these variables

# now check if gender affects credit score, since we know that all other variables have no correlation and should not have any influence on credit score
ggplot(data[,.(Gender,CreditScore)], aes(Gender,CreditScore)) + geom_boxplot() 
# seems like gender is not telling of any difference between records. we can therefore consider dropping this column 

# see the distribution for no of products used by customers
ggplot(data[,.(NumOfProducts)], aes(NumOfProducts)) + geom_histogram(binwidth = 1) # most customers have 1 or 2 products only, a small minority with 3 or 4
data[,.N,by=.(NumOfProducts,Exited)][order(NumOfProducts,Exited)]
# seems like for customers with 3 products, they are more likely to exit than to stay
# possible that most customers trying out this 3rd product are not satisfied and hence want to try another bank?
# for customers who are on their 4th product, none of them stay with the bank at all. 
# might be telling that the bank's 3rd and 4th product are not doing very well
# but of course we are assuming that customers wtih 3 products have the same 3rd product and likewise for the 4th product

# might be worth trying to guess what is this mystery 3rd (and 4th product)
# by looking at the profile of customers who have 3 and 4 products
data <- data[,hasManyProd:=(NumOfProducts>2)]
data <- data[,has4Prod:=(NumOfProducts==4)]
data <- data[,has3Prod:=(NumOfProducts==3)]
data <- data[,has4Prod:=(NumOfProducts==4)]

summary(data)

# do people with 3 or 4 products, compared with those with 1 or 2 products
# tend to say have a higher credit score? older? higher salary? etc?

# see if the age distribution is different 
ggplot(data, aes(x=Age, fill=hasManyProd)) + geom_histogram(binwidth=5)
# histogram is not a good visualisation for this, try box plot
ggplot(data, aes(x=hasManyProd, y=Age)) + geom_boxplot() # ok so we know that many-prod-ppl tend to be older
ggplot(data, aes(x=hasManyProd, y=EstimatedSalary)) + geom_boxplot() # no clear distinction, maybe manyprod ppl tend tob be abit more high-earning
ggplot(data, aes(x=hasManyProd, y=Balance)) + geom_boxplot() # no clear distinction for balance
ggplot(data, aes(x=hasManyProd, y=CreditScore)) + geom_boxplot() # no clear distinction for credit score
ggplot(data, aes(x=hasManyProd, y=Tenure)) + geom_boxplot() # those with many products tend to be those that have a longer tenure, but that is only natural. so does not count as an insight

# check hasManyProd against the discrete variables
# check against gender
ggplot(data, aes(x=hasManyProd, fill=Gender)) + geom_bar(position="fill")
ggplot(data, aes(x=has4Prod, fill=Gender)) + geom_bar(position="fill")
# seems like ladies tend to have more banking products

ggplot(data, aes(x=hasManyProd, fill=Geography)) + geom_bar(position="fill")
ggplot(data, aes(x=has4Prod, fill=Geography)) + geom_bar(position="fill")
# seems like Germans tend to have many products, even more so for 4prodppl

# for precaution, check that it is not really due to germany having more female customers
ggplot(data, aes(x=Geography, fill=Gender)) + geom_bar(position="fill") # ok, gender distribution is more or less the same in all countries

# check against isActive
ggplot(data, aes(x=hasManyProd, fill=IsActiveMember)) + geom_bar(position="fill") 
ggplot(data, aes(x=has4Prod, fill=IsActiveMember)) + geom_bar(position="fill")
# no clear distinction, but if anything, manyprodppl are slightly more likely to be active 

# my hypothesis now is that people who exit are either those who have tried the 3rd/4th product and are not satisfied
# and those who were considering the 3rd/4th product but thinks that this bank's offerings are not good enough
# and hence churn without even taking on 3rd/4th product

# lets convert the num of products into category type variable
data$NumOfProducts <- factor(data$NumOfProducts)

# check that it was done correctly
summary(data)

# check proportion of exits for each category of NumOfProduct
ggplot(data, aes(x=NumOfProducts, fill=Exited)) + geom_bar(position="fill") # seems like those with 2 products are most likely to stay

# why those with 1 product are more likely to churn that those with 2 products?
# maybe they are more financially savy and therefore want to exit quickly when they quickly realise thiat this bank CMI?
ggplot(data, aes(x=Exited, y=Age)) + geom_boxplot() + facet_grid(cols=vars(NumOfProducts)) # people who exited with 1 product tend to be much older (45 yo for exited vs 36 yo for stayers)
ggplot(data, aes(x=Exited, y=EstimatedSalary)) + geom_boxplot() + facet_grid(cols=vars(NumOfProducts)) # no clear difference in terms of salary
ggplot(data, aes(x=Exited, y=Tenure)) + geom_boxplot() + facet_grid(cols=vars(NumOfProducts)) # exited people with 1 product tend to be those with shorter tenures, duh
ggplot(data, aes(x=Exited, y=Balance)) + geom_boxplot() + facet_grid(cols=vars(NumOfProducts)) # exited people with 1 product tend to have much lower balance, perhaps due to their much shorter tenure
ggplot(data, aes(x=Exited, y=CreditScore)) + geom_boxplot() + facet_grid(cols=vars(NumOfProducts)) # no clear difference

# maybe those who churned with one product are those who started their tenure late too
data <- data[,`:=`(ageAtStartOfTenure = (Age-Tenure))]
ggplot(data[NumOfProducts==1], aes(x=Exited, y=ageAtStartOfTenure)) + geom_boxplot() # this plot seems to suggest this 
summary(data[NumOfProducts==1 & Exited==1, .(ageAtStartOfTenure)]) # get distribution data


# are the exit-on-1-product customers similar in age distribution as those with 3 or 4 products?
summary(data[hasManyProd==TRUE, .(ageAtStartOfTenure)]) # seems to be yes 
ggplot(data[hasManyProd==1], aes(y=ageAtStartOfTenure)) + geom_boxplot()

# so my guess here is that for longer-term customers going on their 3rd and/or 4th product, they are using Product C and D respectively
# and that Product C and D are not great, but yet these customers need good versions of C and D, so hence they are likely to leave the bank
# and since new customers (with only 1 product) to the bank are at around the same age as these long-term customers on their 3rd and 4th product, 
# it could be that they have similar needs, and hence went to try Product C or D, and ended up equally unsatisfied
# and hence they churn quickly 

# my guess is that the 3rd/4th product is some kind of retirement fund or savings plan since people who take this up are around 40 years old
# here, we can try creating new features to try to figure out what profile of people have 3 or 4 products

# the first one we can try creating is deposit rate i.e. balance/tenure 
# the second one would be saving-to-earnings ratio i.e. balance/tenure/estimated salary
#the third one is earnings/age. perhaps those who are earning alot for their age tend to have different expectations for their banks
# we create these columns first 
data <- data[,`:=`(depositRate = (Balance/(Tenure+1)), savingsToSalaryRatio = (Balance/(Tenure+1)/EstimatedSalary), salaryToAgeSq = (EstimatedSalary/Age^2))]

# now to plot them, startig with depositRate
ggplot(data, aes(x=hasManyProd, y=depositRate)) + geom_boxplot() # not too clear, see the math
data[, .(meanDepositRate=(mean(depositRate))), by=hasManyProd] # not much difference in deposit rate. if anything, those with many products have a slightly lower annual deposit sum on average
data[, .(meanDepositRate=(mean(depositRate))), by=.(Exited)] # contradiction, so this variable is probably useless

ggplot(data, aes(x=hasManyProd, y=savingsToSalaryRatio)) + geom_boxplot() # not too clear, see the math
data[, .(meanSavingToSalaryRatio=(mean(savingsToSalaryRatio))), by=hasManyProd] # those who has many products tend to save a much smaller fraction of their earnings
data[, .(meanSavingToSalaryRatio=(mean(savingsToSalaryRatio))), by=Exited] # those who exit tend to save a larger proportion of their salary: contradiction, so this variable is probably useless 

ggplot(data, aes(x=hasManyProd, y=salaryToAgeSq)) + geom_boxplot() # not too clear, see the math
ggplot(data, aes(x=salaryToAgeSq, fill=hasManyProd)) + geom_histogram(binwidth = 5) # not clear
ggplot(data, aes(x=salaryToAgeSq, fill=Exited)) + geom_histogram(binwidth = 5) # not clear here either
data[, .(meanSalaryToAgeSq=(mean(salaryToAgeSq))), by=hasManyProd] # people with many products tend to be earning lesser for their age, but it could be due to them being older in general
data[, .(meanSalaryToAgeSq=(mean(salaryToAgeSq))), by=.(Exited)] # people who churn tend to be earning lesser for their age, but again, it could be due to them being generally older
summary(data[hasManyProd==TRUE,.(salaryToAgeSq)]) 
summary(data[Exited==1,.(salaryToAgeSq)])
summary(data[,.(Age)])

# another way is to split the population into different age groups and check out the salary distribution in each group
data.df <- as.data.frame(data)
data.df$ageGroup <- cut(data.df$Age, breaks = c(0,25,30,35,40,45,50,55,60,65,70,75,80,100), labels=c("<25", "26-30", "31-35", "36-40", "41-45", "46-50", "51-55", "56-60", "61-65", "66-70", "71-75", "76-80", ">80"))
data <- as.data.table(data.df)
summary(data)
summary(data[hasManyProd==FALSE,.(salaryToAgeSq)])
summary(data[Exited==0,.(salaryToAgeSq)])
data[,.N,by=ageGroup]
# salary distribution: 1st quart: 51k, median: 100k, mean: 100k, 3rd quart: 149k, 

ggplot(data, aes(x=ageGroup, y=EstimatedSalary)) + geom_boxplot() # 

data[EstimatedSalary<51002, salaryQuartile:="1Q"]
data[EstimatedSalary>51002 & EstimatedSalary < 100193, salaryQuartile:="2Q"]
data[EstimatedSalary>100193 & EstimatedSalary <149388, salaryQuartile:="3Q"]
data[EstimatedSalary>149388, salaryQuartile:="4Q"]
data$salaryQuartile <- factor(data$salaryQuartile)
summary(data)
# how about the churn-on-1-product customers? do they tend to be earning lower for their age too?
ggplot(data[NumOfProducts==1], aes(x=Exited, y=salaryToAgeSq)) + geom_boxplot() # 
ggplot(data[NumOfProducts==1], aes(x=salaryToAgeSq, fill=Exited)) + geom_histogram(binwidth = 5) # no clear difference 
ggplot(data[NumOfProducts==1], aes(Exited,fill=salaryQuartile)) + geom_bar(position="fill")  # no clear difference 


summary(data)
# a possible story is that people who start their first product when they are older are actually looking for a bank for a particular banking need
# so people who start their tenure on this bank when they are old, and also have lower salary-to-age ratio, dont tend to have long tenures?
ggplot(data, aes(x=Age, y=Exited)) + geom_tile(aes(fill=salaryToAgeSq))


# might this salary-to-age ratio explain why females are more likely to churn?
ggplot(data, aes(x=salaryToAgeSq, fill=Gender)) + geom_histogram(binwidth = 5) 
ggplot(data, aes(x=Gender, y=salaryToAgeSq)) + geom_boxplot() # no, salary to age ratio seems to be the same for males and females

# might the salary-age ratio explain the higher churn rate among the Germans?
ggplot(data, aes(x=salaryToAgeSq, fill=Geography)) + geom_histogram(binwidth = 5) # not too clear here.
ggplot(data, aes(x=salaryToAgeSq, fill=Geography)) + geom_histogram(binwidth = 50) # not too clear here either. the greographic distributions seems to stay the same for all brackets

# perhaps Germans tend to churn more not because they have different characteristics but becos the Germans have more options to choose from or something


## LOGISTIC REGRESSION

# Train-Test split
set.seed(2014)
train <- sample.split(Y = data$Exited, SplitRatio = 0.7)
trainset <- subset(data, train == T)
testset <- subset(data, train == F)

m1 <- glm(Exited ~ . , family = binomial, data = trainset)
summary(m1)

# Confusion Matrix on Trainset
prob.train <- predict(m1, type = 'response')
predict.churn.train <- as.factor(ifelse(prob.train > 0.5, 1, 0))
table1 <- table(trainset$Exited, predict.churn.train)
table1
prop.table(table1)
# Overall Accuracy
mean(predict.churn.train == trainset$Exited)

# Confusion Matrix on Testset
prob.test <- predict(m1, newdata = testset, type = 'response')
predict.churn.test <- as.factor(ifelse(prob.test > 0.5, 1, 0))
table1 <- table(testset$Exited, predict.churn.test)
table1
prop.table(table1)
# Overall Accuracy
mean(predict.churn.test == testset$Exited)


## CART
m2 <- rpart(Exited ~ ., data = trainset, method = 'class',
            control = rpart.control(minsplit = 2, cp = 0))

printcp(m2)
plotcp(m2)
print(m2)

## 10th tree is optimal. Choose betw the 10th and 11th tree CP values.
cp1 <- sqrt(0.00233754*0.00280505)

m3 <- prune(m2, cp = cp1)
print(m3)
printcp(m3, digits = 3)
rpart.plot(m3, nn = T, main = "Optimal Tree in Churn Modelling")
m3$variable.importance
## Age and ageAtStartOfTenure most important.
summary(m3)

cart.predict <- predict(m3, newdata = testset, type = "class")

table2 <- table(Testset.Actual = testset$Exited, cart.predict, deparse.level = 2)
table2
round(prop.table(table2), 3)
# Overall Accuracy
mean(cart.predict == testset$Exited)


