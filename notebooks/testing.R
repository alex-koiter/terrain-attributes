# # Load required library
# library(boot)
# 
# # Define a function to compute R-squared from bootstrap samples
# rsq <- function(formula, data, indices) {
#   d <- data[indices, ]  # Allows boot to select resampled cases
#   fit <- lm(formula, data = d)
#   return(summary(fit)$r.square)
# }
# 
# # Perform bootstrapping with 1000 replications
# # Example: predicting mpg from wt and disp
# results <- boot(
#   data = mtcars,
#   statistic = rsq,
#   R = 1000,
#   formula = mpg ~ wt
# )
# 
# # View bootstrap results
# print(results)
# 
# # Plot the bootstrap distribution of R-squared
# plot(results)
# boot.ci(results, type = "all")
# 
# ## Not run:
# data(SNPdata)
# set.seed(123)
# stability_result = measure_stability(y = SNPdata[,1], X=SNPdata[,-1], num.trees=500)
# stability_result # Stability of random forest with 500 trees
# 
# ## The `unsupervised' case:
# set.seed(17)
# iris.urf <- randomForest(iris[, -5])
# MDSplot(iris.urf, iris$Species)
# ## stratified sampling: draw 20, 30, and 20 of the species to grow each tree.
# (iris.rf2 <- randomForest(iris[1:4], iris$Species,
#                           sampsize=c(20, 30, 20)))
# 
# library(randomUniformForest)
# data(iris)
# iris.uruf = unsupervised.randomUniformForest(iris[,-5])
# plot(iris.uruf)
# iris.uruf2 = unsupervised.randomUniformForest(iris[,-5], mtry = 1, nodesize = 2)
# plot(iris.uruf2)
# 
# rf2 <- randomForest(x = iris[,-5], mtry = 2, ntree = 2000, proximity = TRUE)
# rf2
# rf2$proximity
# prox <- rf2$proximity
# pam.rf <- pam(prox, 3)
# pred <- cbind(pam.rf$clustering, iris$Species)
# table(pred[,2], pred[,1])


install.packages("factoextra")
library(cluster)
library(factoextra)
library(tidyverse)
# Example using the iris dataset
data(iris)
max_k <- 4
sil_widths <- numeric(max_k - 1)

for (i in 2:max_k) {
  pam_result <- pam(iris[, 1:4], k = i, keep.diss = TRUE)
  sil_widths[i - 1] <- pam_result$silinfo$avg.width
}

# Find the optimal k
optimal_k <- which.max(sil_widths) + 1
print(paste("Optimal number of clusters:", optimal_k))

plot(2:max_k, sil_widths, type = "b", xlab = "Number of clusters (k)", ylab = "Average Silhouette Width")

pam_final <- pam(iris[, 1:4], k = optimal_k, keep.diss = TRUE)
fviz_silhouette(pam_final, label = TRUE) 


library(tidyverse)
library(randomForest)
iris.pc <- prcomp(iris[,1:4], center = TRUE, scale. = FALSE)$x %>% as.data.frame()

rf.fit <- randomForest(x = iris[,1:4], y = NULL, ntree = 10000, proximity = TRUE, oob.prox = TRUE)
hclust.rf <- hclust(as.dist(1-rf.fit$proximity), method = "ward.D2")

plot(hclust.rf)

# library(dendextend)
# dend <- as.dendrogram(hclust.rf)
# dend <- color_branches(dend, h=3) # Color branches based on a height cut
# plot(dend)



rf.cluster = cutree(hclust.rf, k=2)
iris.pc$rf.clusters <- rf.cluster
table(rf.cluster, iris$Species)
library(tidyverse)
ggplot(data = iris.pc, aes(x= PC1, y = PC2, colour = factor(rf.clusters))) +
  geom_point()


# Define function to compute WSS for a single cluster
wss <- function(d) {
  sum(scale(d, scale = FALSE)^2)
}

# Wrapper function to apply WSS calculation across clusters for a given k
wrap <- function(i, hc, x) {
  cl <- cutree(hc, i)  # Cut tree into i clusters
  spl <- split(x, cl)  # Split data by cluster membership
  total_wss <- sum(sapply(spl, wss))  # Sum WSS across all clusters
  total_wss
}

# Example using the iris dataset (excluding species column)
data <- iris[, 1:4]

# Perform hierarchical clustering
# hc <- hclust(dist(data))

# Compute WSS for k = 1 to 10
k_values <- 1:10
wss_values <- sapply(k_values, wrap, hc = hclust.rf, x = iris[, 1:4])

# Plot the elbow plot
plot(k_values, wss_values, 
     type = "b", 
     pch = 19, 
     xlab = "Number of Clusters (k)", 
     ylab = "Total Within-Cluster Sum of Squares (WSS)",
     main = "Elbow Method for Optimal k")
