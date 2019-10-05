setwd('D:/UC/Sem 2/FLex 1/Data Mining 1')
data(iris)
aggregate(.~Species, iris, sd)
iris.head()

head(iris)

data <- iris
data[,-5] <- scale(iris[,-5])
set.seed(12845384)

ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.8,0.2))
trainData <- data[ind==1,]
testData <- data[ind==2,]

library("class")
Knn_k1 <- knn(trainData[,-5], testData[,-5],
                            trainData$Species, k=1, prob=F)

Knn_k2 <- knn(trainData[,-5], testData[,-5],
                            trainData$Species, k=2, prob=F)

Knn_k3 <- knn(trainData[,-5], testData[,-5],
                            trainData$Species, k=3, prob=F)

Knn_k4 <- knn(trainData[,-5], testData[,-5],
                            trainData$Species, k=50, prob=F)
table(testData$Species, Knn_k1)
table(testData$Species, Knn_k2)
table(testData$Species, Knn_k3)
table(testData$Species, Knn_k4)

sum(Knn_k1==testData$Species)/length(testData$Species)*100
sum(Knn_k2==testData$Species)/length(testData$Species)*100
sum(Knn_k3==testData$Species)/length(testData$Species)*100
sum(Knn_k4==testData$Species)/length(testData$Species)*100




KnnTestPrediction <- list()
accuracy <- numeric()

for(k in 1:100){
  
  KnnTestPrediction[[k]] <- knn(trainData[,-5], testData[,-5], trainData$Species, k, prob=TRUE)
  accuracy[k] <- sum(KnnTestPrediction[[k]]==testData$Species)/length(testData$Species)*100
  
}

plot(accuracy, type="b", col="dodgerblue", cex=1, pch=20,
     xlab="k, number of neighbors", ylab="Classification accuracy", 
     main="Accuracy vs Neighbors")

# Add lines indicating k with best accuracy
abline(v=which(accuracy==max(accuracy)), col="darkorange", lwd=1.5)

# Add line for max accuracy seen
abline(h=max(accuracy), col="grey", lty=2)

# Add line for min accuracy seen 
abline(h=min(accuracy), col="grey", lty=2)






predicted_Species <- knn(trainData[,-5],testData[,-5],trainData$Species,k=1)
predicted_Species

#checking the accuracy
accuracy<-mean(trainData$Species==predicted_Species)
accuracy


###OPTIMAL K VALUE###
#now trying to find the optimal value of k which has maximum accuracy
predicted_Species <- NULL
k_Val <- 1:15
accuracy <- NULL

for(i in k_Val) {
  set.seed(1234)
  predicted_Species <- knn(newDataaTrain[,-5],newDataaTest[,-5],newDataaTrain$Species,k=i)
  accuracy[i] <- mean(newDataaTrain$Species==predicted_Species)
}


par(mfrow=c(2,2))
boxplot(Sepal.Width ~ Species, data=iris,
        xlab="Species",
        ylab="Sepal Width", col="lightpink3")
boxplot(Sepal.Length ~ Species, data=iris,
        xlab="Species",
        ylab="Sepal Length", col = "dark green" )
boxplot(Petal.Length ~ Species, data=iris,
        xlab="Species",
        ylab="Petal Length", col = "gray")
boxplot(Petal.Width ~ Species, data=iris,
        xlab="Species",
        ylab="Petal Width", col = "antiquewhite1")











