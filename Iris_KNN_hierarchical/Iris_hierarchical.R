# Load data
data(iris)
iris <- iris[, -5]
head(iris)
# Compute distances and hierarchical clustering
dd <- dist(scale(iris[,1:4]), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")

plot(hc,hang = -1, cex = 0.6)
hcd <- as.dendrogram(hc)
plot(hcd, type = "rectangle", ylab = "Height")
nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), 
                cex = 0.7, col = "blue")
plot(hcd, ylab = "Height", nodePar = nodePar, leaflab = "none")
plot(as.phylo(hc), type = "fan")
install.packages("ape")
library(ape)
plot(as.phylo(hc), type = "fan")
scale(iris)


m <- hclust(dist(iris[,1:4]), method="ave")
plot(m)
clusters = cutree(m, 3)
table(clusters, iris$Species)
d <- dist(iris)
install.packages('dendextend')
require2 <- function (package, ...) {
  if (!require(package)) install.packages("package"); library(package)
}

## require2('installr')
## install.Rtools() # run this if you are using Windows and don't have Rtools installed

# Load devtools:
require2("devtools")
devtools::install_github('talgalili/dendextend')
<!-- require2("Rcpp") -->
  <!-- devtools::install_github('talgalili/dendextendRcpp') -->
  
  # Having colorspace is also useful, since it is used
  # In various examples in the vignettes
  require2("colorspace")
And then you may load the package using:
  
  library(dendextend)



library(package)
library(magrittr) # need to run every time you start R and want to use %>%
library(dplyr)
library(ggplot2)

dend <- iris[,-5] %>% dist %>% hclust %>% as.dendrogram %>%
  set("branches_k_color", k=3) %>% set("branches_lwd", c(1.5,1,1.5)) %>%
  set("branches_lty", c(1,1,3,1,1,2)) %>%
  set("labels_colors") %>% set("labels_cex", c(.9,1.2)) %>% 
  set("nodes_pch", 19) %>% set("nodes_col", c("orange", "black", "plum", NA))
#

# plot the dend in usual "base" plotting engine:
plot(dend)
ggd1 <- as.ggdend(dend)
library(ggplot2)
# the nodes are not implemented yet.
ggplot(ggd1)



