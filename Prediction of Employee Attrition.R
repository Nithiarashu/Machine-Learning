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
prop.table(table(my_dataset$Attrition))
#train and testing the dataset
library(caret)
sample <- createDataPartition(my_dataset$Attrition, p = .75, list = FALSE)
train <- my_dataset[sample, ]
prop.table(table(train$Attrition))
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
cFiftyModel<-C5.0(train$Attrition~.,data = train,trails=10)
class(cFiftyModel)
summary(cFiftyModel)
#plot(cFiftyModel)
cFiftyModelPrediction<-predict(cFiftyModel,newdata=test[,-2])
(cFiftyModelAccuracy <- mean(cFiftyModelPrediction == test$Attrition)) 

########################################################################

# Naive Bayes Alternative doing it directly 
nb <- e1071::naiveBayes(Attrition ~., data=train)
nbP <- predict(nb, newdata=test[,-2], type = "class")
(nbAcc <- 1- mean(nbP != test$Attrition))

#Naive Bayes Prediction 

NBData<-my_dataset
str(NBData)
#table(NBData$ï..Age)
NBData$StockOptionLevel<-as.factor(NBData$StockOptionLevel)
str(NBData$StockOptionLevel)
NBData$WorkLifeBalance<-as.factor(NBData$WorkLifeBalance)

NBData$EmployeeCount<-NULL #Removing data which is not useful
str(NBData)
#Age
hist(NBData$ï..Age, breaks = 30)
install.packages("OneR")
library(OneR)
res1 <- bin(NBData$ï..Age, nbins = 10, method = "content", na.omit = TRUE)
NBData<-cbind(NBData,BinnedAge=res1)
table(NBData$BinnedAge,NBData$Over18)
#HourlyRate
hist(NBData$HourlyRate) #seems normally distributed
res2 <- bin(NBData$HourlyRate, nbins = 15, method = "content", na.omit = TRUE)
NBData<-cbind(NBData,BinnedHour=res2)
table(NBData$BinnedHour,NBData$Department)
#everything is now a factor/categorical data
str(NBData)  
trainingNB<-NBData[sample,]
testingNB<-NBData[-sample,]
nb <- e1071::naiveBayes(trainingNB, train$Attrition)
nbP <- predict(nb, newdata=testingNB[,-2], type = "class")
(nbAcc <- 1- mean(nbP != test$Attrition))

###################################################################################

#CART [Classification and Regression Tree]- Decision Tree

library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
library(RColorBrewer)
regressionTree <- rpart::rpart(Attrition ~ ., data=train, method="class")
install.packages("rattle")
library(rattle)
fancyRpartPlot(regressionTree)
rpartPrediction <- predict(regressionTree, test, type = "class")
(CARTModelAccuracy <- mean(rpartPrediction == test$Attrition))

#Pruning in CART to remove the overfit automatically 
# what does manual pruning help in ? how is it working?
treeToPrune <- rpart(Attrition ~ ., data=train, method="class", control=rpart.control(minsplit=,cp=0))
prunedTree <- prp(treeToPrune,snip=TRUE)$obj
fancyRpartPlot(prunedTree)

###################################################################################

#Random Forest
install.packages("randomForest")
library(randomForest)
forest <- randomForest(Attrition ~ ., data=train, importance=TRUE, ntree=2000)
varImpPlot(forest)
rf <- predict(forest, test[,-2], type = "class")
(forestAcc <- 1- mean(rf != test$Attrition))

#Check for it !
caret::confusionMatrix(rf, test$Attrition, positive = "Yes")

###################################################################################

#Conditional Inference Trees
library(partykit)
install.packages("party")
library(party)
cTree <- ctree(Attrition ~., data=train)
print(cTree)
plot(cTree, type="simple")
cForest <- cforest(Attrition ~., data=train, controls=party::cforest_unbiased(ntree=2000, mtry=3))
cForestPrediction <- predict(cForest, newdata = test)
(cForestAccuracy <- 1 - mean(cForestPrediction != test$Attrition))

###################################################################################

#K-Mean worked out but accuracy of code has to improve 
str(my_dataset)
my_dataset_c <- subset(my_dataset, select=c(2,3,5,7:9,11:15,20:22,25)) #isolate the categoricals
my_dataset_k <- cbind(numerics, my_dataset_c) #put them back together (Have to take data from KNN for the numeric vaules

kmeansClusters <- list()
kmeansScores <- c()
for (i in 3:10) {
  clusters <- kmeans(numerics, i)
  name <- paste0("kmeans", i)
  kmeansClusters[[name]] <- clusters
  kmeansScores <- rbind(kmeansScores, sum(clusters$withinss))
}

row.names(kmeansScores) <- c(3:10)
colnames(kmeansScores) <- c("k")

plot(kmeansScores, xlab="k", ylab="within groups sum of squares")
#intuition would suggest 4 clusters... let's try another measure and find out
#i'd have been fine with up to here as an answer, but to improve our intutition of a possibly good value for k...
install.packages("clusterSim")
library(clusterSim)
kmeansScores <- c()
for (i in 3:10) {
  clusters <- kmeans(numerics, i)
  name <- paste0("kmeans", i)
  dbindex <- index.DB(numerics, clusters$cluster, centrotypes="centroids")
  kmeansScores <- rbind(kmeansScores, dbindex$DB)
}

row.names(kmeansScores) <- c(3:10)
colnames(kmeansScores) <- c("k")

plot(kmeansScores, xlab="k", ylab="DBIndex")
#lower is better, so it's probably not k=2; k = 5 seems ok again, 10 has also done well this time.

#let's try plotting
install.packages("fpc")
library(fpc)
plotcluster(numerics, kmeansClusters[["kmeans2"]]$cluster)
plotcluster(numerics, kmeansClusters[["kmeans4"]]$cluster)
plotcluster(numerics, kmeansClusters[["kmeans5"]]$cluster)
plotcluster(numerics, kmeansClusters[["kmeans10"]]$cluster)

#for interest (going beyond what's asked, but it's good to think about this a little) let's use the whole dataset -- we need to switch to medoids because of the categorical variables
library(cluster)

kmedoidsClusters <- list()
kmedoidsScores <- c()

gower_dist <- daisy(my_dataset_k, metric = "gower", type = list(logratio = 3))

for (i in 2:20) { #for fun let's also increase the max k value as well
  clusters <- pam(gower_dist, k=i, diss=T) #note we switched to the full set of attibutes now so need to use medoids
  name <- paste0("kmedoids", i)
  kmedoidsClusters[[name]] <- clusters
  dbindex <- index.DB(gower_dist, clusters$clustering, centrotypes = "medoids", d=gower_dist)
  kmedoidsScores <- rbind(kmedoidsScores, dbindex$DB)
}

row.names(kmedoidsScores) <- c(2:20)
colnames(kmedoidsScores) <- c("k")

plot(kmedoidsScores, xlab="k", ylab="DBIndex")
#now k=2 (which makes sense) and k=7 are looking like good candidate values of k

###################################################################################################

table(my_dataset$Attrition)
myIris <- numerics[,] #removing the dependent
myIris <- sapply(myIris, FUN=function(x) { scale(x, scale = T, center=T)})
res.1 <- kmeans(myIris, 2) #2 is the number of cluster and need to know how to find a better cluster count 
str(res.1)
df <- data.frame(cluster = res.1$cluster, Attrition = my_dataset$Attrition)
table (factor(df$cluster), df$Attrition)

myIris <- numerics[,] #removing the dependent
myIris <- sapply(myIris, FUN=function(x) { scale(x, scale = T, center=F)})
res.2 <- kmeans(myIris, 2)
df <- data.frame(cluster = res.2$cluster, Attrition = my_dataset$Attrition)
table (factor(df$cluster), df$Attrition)

pca <- prcomp(numerics[,], scale. = T, center = T)
res.3 <- kmeans(pca$x[, 1:2], 2)
df <- data.frame(cluster = res.3$cluster, Attrition = my_dataset$Attrition)
table (factor(df$cluster), df$Attrition)

pca <- prcomp(numerics[,], scale. = T, center = F)
res.4 <- kmeans(pca$x[, 1:2], 2)
df <- data.frame(cluster = res.4$cluster, Attrition = my_dataset$Attrition)

table (factor(df$cluster), df$Attrition)

str(res.4)
totss <- c(res.1[[3]], res.2[[3]], res.3[[3]], res.4[[3]])
tos.withinss <- c(res.1[[5]], res.2[[5]], res.3[[5]], res.4[[5]])
betweenss <- c(res.1[[6]], res.2[[6]], res.3[[6]], res.4[[6]])
performance <- data.frame(totss, tos.withinss, betweenss)
row.names(performance) <- c("w/o pca +c", "w/o pca", "w pca +c", "w pca")
performance

plot(iris[,c(1:4)], col=res.1$cluster)
plot(iris[,c(1:4)], col=res.4$cluster)


###################################evaluation - DB index and silhoutte #####################################3
install.packages("clusterSim")
library(cluster)
install.packages("xtable")
library(xtable)
install.packages("shiny")
library(shiny)

library(clusterSim)

library(cluster) #for the silhouette
results <- list()
tot.withinss <- c()
betweenss <- c()
dbindex <- c()
silhouettes <- c()

for (k in 2:10) {
  results[[k]] <- kmeans(myIris, k)
  tot.withinss[k] <- results[[k]]$tot.withinss
  betweenss[k] <- results[[k]]$betweenss
  dbindex[k] <- clusterSim::index.DB(myIris, results[[k]]$cluster, centrotypes="centroids")$DB
  s <- silhouette(results[[k]]$cluster, daisy(myIris))
  silhouettes[k] <- mean(s[,3])
}

par(mfrow=c(2,2))
plot(tot.withinss, xlab="k")
plot(betweenss, xlab="k")
plot(dbindex, xlab="k")
plot(silhouettes, xlab="k")
plot(silhouette(results[[3]]$cluster, daisy(myIris)))


library(fpc)
plotcluster(myIris, res.2$cluster)

############################ Outlier treatment using euclidean distance ####################
res.2$centers #coords of centroids
centers <- res.2$centers[res.2$cluster, ] #vector of all centers for each point
distances <- sqrt(rowSums((myIris - centers)^2)) #Euclidean pair-wise distances
summary(distances)
sd(distances)
outliers <- order(distances, decreasing=T)[1:5]

# pick those more than 2 standard deviations from the mean
outliers <- (distances > (mean(distances) + (2 * sd(distances))) |
               distances < (mean(distances) - (2 * sd(distances))))

print(myIris[outliers,])


# plot clusters (choose 2 dimensions for ease of display)
plot(myIris[,c("DailyRate.Length", "DistanceFromHome.Width")], pch="o", col=res.2$cluster, cex=0.3)
# plot cluster centers
points(res.2$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=8, cex=1.5)
# plot outliers
points(myIris[outliers, c("Sepal.Length", "Sepal.Width")], pch="+", col=4, cex=1.5)

##################Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- myIris
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
#############################################################################

#K-mediods for including both categorical and numerical in our model

##Aastha code for k-medoids goes here 

#############################Agglomerative Heirarchical ######################################
d <- dist(myIris, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D")
plot(fit) # display dendogram
groups <- cutree(fit, k=3) # cut tree into 3 clusters
# draw dendogram with red borders around the 3 clusters
rect.hclust(fit, k=3, border="red")

########################################################################################

#Density Model (HAVE TO CHECK)
ds <- dbscan(myIris, eps=.25, MinPts=5)
table(ds$cluster, my_dataset$Attrition)
plot(ds, myIris) #myirish from KNN 
plotcluster(myIris, ds$cluster)

#Not working till model
results <- list()
d <- dist(myIris)
for (i in 1:2) {
  ds <- dbscan(myIris, eps=i/100, MinPts = 5)
  results[[i]] <- cluster.stats(d, ds$cluster)
  results[[i]]$eps <- i/100
}

#prune the NULLS
results[sapply(results, is.null)] <- NULL
#pick what you want to plot, e.g. average silhouette width


############################# Model Base clustering #################################
library(mclust)
fit <- Mclust(myIris)
plot(fit, what = "classification") # plot results

avg.silwidth <- lapply(results, FUN = function(x) {
  return (x$avg.silwidth)
})


summary(fit)
fit$classification

####################################################################################

#SVM
