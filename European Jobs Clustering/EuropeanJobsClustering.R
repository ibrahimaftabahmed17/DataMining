data <- read.table("D:/UC/Sem 2/Flex 2/Data Mining 2/europeanJobs.txt",header = TRUE)

data
m=as.matrix(cbind(data$Agr,data$Min,data$Man,data$PS,data$Con,data$SI,data$Fin
                  ,data$SPS,data$TC),ncol=9)

cl=(kmeans(m,3))
cl$size
cl$withinss
data$cluster=factor(cl$cluster)
centers=as.data.frame(cl$centers)
library(factoextra)
set.seed(12825384)
samp <- floor(0.90 * nrow(m))
index.train <- sample(seq_len(nrow(m)), size = samp)
train.data <- m[index.train, ]
#bank.test <- bank.data[-index.train, ]


kmm = kmeans(train.data,3,nstart = 50,iter.max = 15)
kmm

k.max <- 15
wss <- sapply(1:k.max, 
              function(k){kmeans(train.data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

qplot(x=1:k.max,y=wss,
      xlab="Number of clusters K",
      ylab="Total within-clusters sum of squares") + geom_smooth(se=FALSE)
 
fit.4 = kmeans(train.data,4,nstart = 50,iter.max = 15)
fit.5 = kmeans(train.data,5,nstart = 50,iter.max = 15)
fit.2 = kmeans(train.data,2,nstart = 50,iter.max = 15)
p1 <- fviz_cluster(fit.2, data = train.data)
p2 <- fviz_cluster(kmm, data = train.data)
p3 <- fviz_cluster(fit.4, data = train.data)
p4 <- fviz_cluster(fit.5, data = train.data)
library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)
View(data)
cluster1<-train.data[kmm$cluster==1,]
cluster2<-train.data[kmm$cluster==2,]
cluster3<-train.data[kmm$cluster==3,]
cluster1
data[,2:10]==cluster1

fit.2$tot.withinss
kmm$tot.withinss
library(fpc)

wss <- (nrow(train.data)-1)*sum(apply(train.data,2,var))
for (i in 2:12) wss[i] <- sum(kmeans(train.data,
                                     centers=i)$withinss)
plot(1:12, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

prediction.strength(train.data, Gmin=2, Gmax=10, M=10,cutoff=0.8)


data.dist=dist(train.data)
#Obtain clusters using the Wards method
hclust=hclust(data.dist, method="ward")
plot(hclust)

?prediction.strength
prediction.strength(data.dist,
                    M=10,
                    clustermethod = hclustCBI(data.dist,3,cut="level", method="ward"))

intern <- clValid(seed, 2:5, clMethods=c("kmeans","hierarchical"),
                  validation="internal")

summary(intern)


#############################################################
