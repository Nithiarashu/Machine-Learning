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

my_dataset <- read.csv("Emp.csv", stringsAsFactors = T)

str(my_dataset)

#let's deal with these other factors: F1
my_dataset$Education <- factor(my_dataset$Education, levels = c(1,2,3,4,5), labels=c("BC", "C", "UG", "MSc", "PhD"))
my_dataset$EnvironmentSatisfaction <- factor(my_dataset$EnvironmentSatisfaction, levels = c(1:4), labels=c("Low", "Medium", "High", "v. High"))
my_dataset$JobInvolvement <- factor(my_dataset$JobInvolvement, levels = c(1:4), labels=c("Low", "Medium", "High", "v. High"))
my_dataset$JobLevel <- factor(my_dataset$JobLevel) #insufficient information to do more
my_dataset$JobSatisfaction <- factor(my_dataset$JobSatisfaction, levels = c(1:4), labels=c("Low", "Medium", "High", "v. High"))
my_dataset$PerformanceRating <- factor(my_dataset$PerformanceRating, levels = c(1:4), labels=c("Low", "Good", "Excellent", "Outstanding"))
my_dataset$RelationshipSatisfaction <- factor(my_dataset$RelationshipSatisfaction, levels = c(1:4), labels=c("Low", "Medium", "High", "v. High"))
my_dataset$StockOptionLevel <- factor(my_dataset$StockOptionLevel) #don't have more information
my_dataset$WorkLifeBalance <- factor(my_dataset$WorkLifeBalance, levels = c(1:4), labels=c("Bad", "Good", "Better", "Best"))


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
str(subset(my_dataset, select=c(1,4,6,11,17,18,21,24,27,29:32)))

#now correlation testew
cor(subset(my_dataset, select=c(1,4,6,11,17,18,21,24,27,29:32)))

str(my_dataset)

#Making 2 visualization over the dependent varaible against numerical and categorical attribute
boxplot(my_dataset$DailyRate ~ my_dataset$Attrition)
spineplot(my_dataset$Education, my_dataset$Attrition)

#Make 2 gender-based observations of the dataset
spineplot(my_dataset$MaritalStatus, my_dataset$Attrition)
boxplot(my_dataset$DistanceFromHome ~ my_dataset$MaritalStatus)

#Is there a relationship between age and hour/day/month rate
library(graphics)
plot(my_dataset$Age, my_dataset$MonthlyRate, main = "Scatter plot of Age vs. Monthly Rate")
plot(my_dataset$Age, my_dataset$DailyRate, main = "Scatter plot of Age vs. Daily Rate")
plot(my_dataset$Age, my_dataset$HourlyRate, main = "Scatter plot of Age vs. Hourly Rate")

#Pick one of the Likert scales and interpret it against the dependent variable
(counts <- table(my_dataset$Attrition, my_dataset$JobSatisfaction))
row.names(counts) <- c("Remained", "Left")
barplot(counts, main="Attrition Distribution by Job Satisfaction", legend = row.names(counts))

