# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

install.packages(c("cluster", "rattle","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function

df <- scale(wine[-1])

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(df)

# Exercise 2:
#   * How many clusters does this method suggest?
# A bend in the graph produced by wssplot(df) at 3 clusters indicates the appropriate number of clusters to use is 3.
#   * Why does this method work? What's the intuition behind it?
# The graph produced by wssplot(df) is a plot of the sums of squares within each group against the number of clusters possible in a K-means solution.
# The number of clusters is limited to 15, so this plot graphs the sum of squares within each group against possible clusters ranging from 1 to 15.
# This method partitions observations into k groups within a data set such that the sum of squares of the observation compared to the center of their assigned cluster is minimized their 
# This plot shows significant reductions in the within groups sum of squares from 1 cluster to 2 clusters to 3 clusters. The bend in the graph from 3 to 4 clusters
# indicates there is not a significant reduction in the within groups sum of squares by increasing the number of clusters from 3 to 4 illustrating that the appropriate
# number of clusters to use is 3.
#   * Look at the code for wssplot() and figure out how it works
# # A data set is read (data), the maximum number of clusters (nc) is set to 15 and the seed is set to 1234 to ensure reproducibile results.

# nrow(data)-1 counts the number of rows of data and subtracts 1 to account for the header.

# apply(data,2,var) uses the function var (variance) and is applied to columns (represented by 2) in data. The sum() wrapper then sums the variances. 

# Variance is calculated as the sum of squared differences of each data point from the mean, divided by the number of observations in a population or sample size.



# For each iteration [i] beginning with the 2nd row of the data set (2:nc) through the maximum number of clusters (nc=15), first set the seed to 1234 with set.seed(seed) 

# where seed was previously defined as 1234. 

# Then sum the result of the kmeans() function on data with centers set to i such that the sum of squares within groups ($withinss) is returned 

# (as opposed to between or total).



# Plot 1:nc (or 1 to 15) on the x-axis and wss on the y-axis using type = b (both). Set the label of the x-axis to "Number of Clusters" and the label of the y-axis to

# "Within groups sum of squares".


# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
# The resulting bar plot also suggests the appropriate number of clusters is 3. 

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(df, 3, nstart=25)

fit.km$size

fit.km$centers

aggregate(wine[-1], by=list(cluster=fit.km$cluster), mean)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

ct.km <- table(wine$Type, fit.km$cluster)

ct.km

install.packages("flexclust")
library(flexclust)

# Quantify the agreement between type and cluster using an adjusted Rand index provided by the flexclust package.

# The adjusted Rand index ranges from -1 (no agreement) to 1 (perfect agreement). 

# Here we have a Rand index of almost 0.9 indicating the clustering we used is pretty good.

randIndex(ct.km)

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?


library(cluster)

clusplot(df, clus=fit.km$cluster, main = "Cluster Plot of Scaled Data")

# The distance among the 3 clusters in the visualization indicate that further partitioning would not improve results. 

# The cluster plot shows that the clusters on the left side of the graph could be combined into 1 cluster. However, the distances between the outermost data points

# in this cluster from the center would significantly increase indicating a reduction of clusters from 3 to 2 would not be a good idea. Thus, this cluster plot

# indicates 3 clusters is appropriate.
