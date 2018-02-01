
cluster_kmeans <- function(data,num){
set.seed(101)   # set seed for reproducible work
wss <- (nrow(data)-1)*sum(apply(mydata,2,var))  # wss is within group sum of squares

for (i in 2:15) wss[i] <- sum(# checking model fit for 2 to 15 clusters
  kmeans(data,  centers = i)$withinss)  # note use of kmeans() func

plot(1:num, wss, type="b", 
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")

fit <- kmeans(mydata, num)

aggregate(data, by = list(fit$cluster), FUN = mean)

cluster_data <- data.frame(data, fit$cluster)  # put cluster number as identifier in a separate column

return(cluster_data)
} # function ends here



# function for Hieararchical clustering
clustering_hier <- function(data, num){
  d <- dist(data, method = "euclidean") # distance matrix
fit <- hclust(d, method = "ward.D") 

plot(fit) # display dendogram
groups <- cutree(fit, k = num) # cut tree into 5 clusters

# draw dendogram with red borders around the 5 clusters 
dendo <- rect.hclust(fit, k = num, border ="red")

return(dendo)
} # function ends here 


