seed = read.table('http://archive.ics.uci.edu/ml/machine-learning-databases/00236/seeds_dataset.txt', header=F)
seed = seed[,1:7]
colnames(seed) = c("area", "perimeter","campactness", "length", "width", "asymmetry", "groovelength")

seed <- scale(seed) 

k2 <- kmeans(seed,centers = 2, nstart = 25) #2 cluster solution
fit$withinss
fit$tot.withinss

#Display number of clusters in each cluster
table(fit$cluster)

fviz_cluster(fit, data = seed)

k3 <- kmeans(seed, centers = 3, nstart = 25)
k4 <- kmeans(seed, centers = 4, nstart = 25)
k5 <- kmeans(seed, centers = 5, nstart = 25)
library(factoextra)
# plots to compare
p1 <- fviz_cluster(fit, geom = "point", data = seed) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = seed) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = seed) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = seed) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

wss <- (nrow(seed)-1)*sum(apply(seed,2,var))
for (i in 2:12) wss[i] <- sum(kmeans(seed,
                                     centers=i)$withinss)
plot(1:12, wss, type="b", xlab="Number of Clusters"
     ,ylab="Within groups sum of squares")
summary(fit)

fit@.Data
install.packages('clValid')
library(clValid)
dunn(fit)
?dunn
library(clValid)
intern <- clValid(seed, 2:3, clMethods="kmeans",validation="internal")
summary(intern)

k2$tot.withinss
k3$tot.withinss
