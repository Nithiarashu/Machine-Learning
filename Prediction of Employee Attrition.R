my_dataset <- read.csv("Sampleadm.csv", stringsAsFactors = T)


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

#i prefer the attrition factor as 0/1 for convenience 
my_dataset$Attrition <- factor(my_dataset$Attrition, labels=c(0,1), levels=c("No", "Yes"))

table(my_dataset$Attrition)
