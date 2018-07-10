hrdata <- read.csv("Sampleadm.csv", stringsAsFactors = T)
set.seed(1337)

#Selecting a random number in a uniform distribution
my_dataset <- hrdata[order(runif(600)), ]

str(my_dataset)

#Lets remove id we dont need that
my_dataset <- my_dataset[-10]

col1 <- round(runif(1)*32)+2 #the +2 protects the Age and Attrition Variables
col2 <- round(runif(1)*31)+2
col3 <- round(runif(1)*30)+2

cols <- names(my_dataset)
print(paste("I lost: ", cols[col1], ",", cols[col2], ",", cols[col3]))

my_dataset <- my_dataset[-col1]
my_dataset <- my_dataset[-col2]
my_dataset <- my_dataset[-col3]

write.csv(file="Emp.csv", my_dataset, row.names = F)

my_dataset <- read.csv("Emp.csv", header = T)

str(my_dataset)

#let's deal with these other factors: F1
my_dataset$Education <- factor(my_dataset$Education, levels = c(1,2,3,4,5), labels=c("BC", "C", "UG", "MSc", "PhD"))
my_dataset$EnvironmentSatisfaction <- factor(my_dataset$EnvironmentSatisfaction, levels = c(1:4), labels=c("Low", "Medium", "High", " VHigh"))
my_dataset$JobInvolvement <- factor(my_dataset$JobInvolvement, levels = c(1:4), labels=c("Low", "Medium", "High", "vHigh"))
my_dataset$JobLevel <- as.factor(my_dataset$JobLevel) #insufficient information to do more
my_dataset$JobSatisfaction <- factor(my_dataset$JobSatisfaction, levels = c(1:4), labels=c("Low", "Medium", "High", "vHigh"))
my_dataset$PerformanceRating <- factor(my_dataset$PerformanceRating, levels = c(1:4), labels=c("Low", "Good", "Excellent", "Outstanding"))
my_dataset$RelationshipSatisfaction <- factor(my_dataset$RelationshipSatisfaction, levels = c(1:4), labels=c("Low", "Medium", "High", "vHigh"))
my_dataset$StockOptionLevel <- as.factor(my_dataset$StockOptionLevel) #don't have more information
my_dataset$WorkLifeBalance <- factor(my_dataset$WorkLifeBalance, levels = c(1:4), labels=c("Bad", "Good", "Better", "Best"))

table(my_dataset$BusinessTravel)

#Now to fix factors where i don't have all levels
summary(my_dataset)
#I'm missing levels 0 and 1 in PerformanceRating, but the rest of my categoricals seem fine
my_dataset$PerformanceRating <- factor(my_dataset$PerformanceRating) 
#however, as JobRole displays other, let's check more precisely

table(my_dataset$JobRole)
#because i already encoded them above, all i have to do is reinvoke the factor command and my empty levels disappear

#a couple of the variables are pointless, so let's get rid of them
my_dataset <- my_dataset[,-9] #EmployeeCount -- always 1
my_dataset <- my_dataset[, -19] #over 18 -- always Y; strictly speaking we should confirm this, but under time pressure it's safe to assume this varibale isn't needed

#mising values:
sapply(my_dataset,function(x) sum(is.na(x)))

#outliers
boxplot(subset(my_dataset, select=c(1,6,18,20,21:24))) #nothing too obvious here
boxplot(subset(my_dataset, select=c(26:29))) #May be something in years at company
boxplot(my_dataset$DailyRate)
boxplot(my_dataset$HourlyRate)
boxplot(my_dataset$MonthlyIncome) #possibly some here too

summary(my_dataset$MonthlyRate)
#i prefer the attrition factor as 0/1 for convenience 
my_dataset$Attrition <- factor(my_dataset$Attrition, labels=c(0,1), levels=c("No", "Yes"))

#checking the class balance
table(my_dataset$Attrition) #its imbalanced 

#train and testing the dataset
library(caret)
sample <- createDataPartition(my_dataset$Attrition, p = .75, list = FALSE)
train <- my_dataset[sample, ]
test <- my_dataset[-sample, ]

#Making some own assumptions:
#Everyonestays 
str(test$Attrition) # remain (Y) is 0
b1 <- rep(0, dim(test)[1])
(accuracyB1 <- 1 - mean(b1 != test$Attrition))

#those who have higher satisfaction are less likely to leave
str(my_dataset$JobSatisfaction)
b2 <- rep(0, dim(test)[1])
b2[test$JobSatisfaction == 'Low'] <- 1
b2[test$JobSatisfaction == 'Medium'] <- 1
(accuracyB2 <- 1 - mean(b2 != test$Attrition))

#work out what columns we have:
str(subset(my_dataset, select=c(1,4,6,10,16:19,23:24,27,26:29)))

#now correlation testew
cor(subset(my_dataset, select=c(1,4,6,10,16:19,23:24,27,26:29)))

str(my_dataset)

#Making 2 visualization over the dependent varaible against numerical and categorical attribute
boxplot(my_dataset$DailyRate ~ my_dataset$Attrition)
spineplot(my_dataset$Education, my_dataset$Attrition)

#Make 2 gender-based observations of the dataset
spineplot(my_dataset$MaritalStatus, my_dataset$Attrition)
boxplot(my_dataset$DistanceFromHome ~ my_dataset$MaritalStatus)

#Start of Question: B4
plot(my_dataset$Age, my_dataset$MonthlyRate, main = "Scatter plot of Age vs. Monthly Rate")
plot(my_dataset$Age, my_dataset$DailyRate, main = "Scatter plot of Age vs. Daily Rate")
plot(my_dataset$Age, my_dataset$HourlyRate, main = "Scatter plot of Age vs. Hourly Rate")

#Pick one of the Likert scales and interpret it against the dependent variable
(counts <- table(my_dataset$Attrition, my_dataset$JobSatisfaction))
row.names(counts) <- c("Remained", "Left")
barplot(counts, main="Attrition Distribution by Job Satisfaction", legend = row.names(counts))

#alternative 
barplot(counts, main="Attrition Distribution by Job Satisfaction", xlab="Job Satistfaction", col=c("lightgreen","red"), legend = rownames(counts), beside=T)

#Prediction using Algorithms

#KNN Algorithm
n<-sapply(my_dataset,function(x){is.numeric(x)})
numerics<-my_dataset[,n]
summary(numerics)
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }
numericsNormal <- normalize(numerics)
summary(numericsNormal)
my_datasetKNN<-my_dataset[,!n]
my_datasetKNN<-cbind(my_datasetKNN,numericsNormal)
install.packages("dummies")
library(dummies)
#handle even categorical data to represent as 1 and 0
#not taking dependent variable
tkNN<-dummy.data.frame(my_dataset[,-2])
summary(tkNN)
training_KNN<-tkNN[sample,]
testing_KNN<-tkNN[-sample,]

#different ways to determine k
k2<-round(sqrt(dim(training_KNN)[2])) #sqrt of number of attribute
k1 <- round(sqrt(dim(testing_KNN)[1])) #sqrt of number of instances
k3 <- 7 #a number between 3 and 10
library(class)

knn1 <- knn(train = training_KNN, test = testing_KNN, cl = train$Attrition, k=k1)
knn2 <- knn(train = training_KNN, test = testing_KNN, cl = train$Attrition, k=k2)
knn3 <- knn(train = training_KNN, test = testing_KNN, cl = train$Attrition, k=k3)


#Accuracy Prediction
(knn1Acc <- 1- mean(knn1 != test$Attrition))
(knn2Acc <- 1- mean(knn2 != test$Attrition))
(knn3Acc <- 1- mean(knn3 != test$Attrition))

########################################################################

#C5.0 Algorithm Prediction- Decision Tree

library(C50)
nearZeroVar(train,saveMetrics = TRUE) #To chweck for the unwanted rows 
cFiftyModel<-C5.0(train$Attrition~.,data = train[1:8],trails=10)
class(cFiftyModel)
summary(cFiftyModel)
#plot(cFiftyModel)
cFiftyModelPrediction<-predict(cFiftyModel,newdata=test[,-2])
(cFiftyModelAccuracy <- mean(cFiftyModelPrediction == test$Attrition)) 

########################################################################


