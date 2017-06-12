# Importing the shopping data

shopclust<- read.csv('Shopper.csv',header = TRUE,stringsAsFactors = FALSE)
View(shopclust)

# Data Structure
str(shopclust)

# Data Summary
summary(shopclust)

# We have data having NA values, eliminating this data entirely along with CID column
# Leaving out monetary,age and both visit columns

shopclust_1<- shopclust[c(2,3,4,5,7)]

# Boxplot of shopclust1

boxplot(shopclust_1)

# From the boxplot we can see that electronics data is spread over large value counts than
# rest of the categories, this will result in bias while creating clustering.
# Therefore, let us standardize the variables

# Calculating the Mean and Std.Dev before standardizing

colMeans(shopclust_1)
apply(shopclust_1,2,sd)

#Standardizing

shopclust_2<- data.frame(scale(shopclust_1))

# Calculating the Mean and Std.Dev after standardizing

colMeans(shopclust_2)
apply(shopclust_2,2,sd)


# We have 5 variables that we are considering for the clusters
# Do we need all 5?

library(ClustOfVar)

var_tree<- hclustvar(shopclust_2)
var_tree
plot(var_tree)

# How many clusters would give us the maximum stability
stability(var_tree)  # 4 clusters

var_list<- cutreevar(var_tree,4)
var_list
summary(var_list)  # We need to remove either of Clothes or Books.Mag value
var_list$E

# Dropping the unrequired values

shopclust_3<- shopclust_2[c(1,2,3,4)]

# Decide the final number of clusters

set.seed(100)

withinSS<- (nrow(shopclust_3)-1)*sum(apply(shopclust_3,2,var))
for(i in 2:15) withinSS[i]<- sum(kmeans(shopclust_3,
                                        centers = i)$withinss)

plot(1:15,withinSS,type = "b",xlab = "Number of Clusters",
     ylab = "Within group sum of squares")

# We can see that 8 will be the optimum number of clusters

water_try<- kmeans(shopclust_3,centers = 8)
water_try


# Having 8 clusters will give a within cluster variance of approx. 35%, it is reasonable

## Lets plot the 8 clusters

shopclust_3 <- data.frame(shopclust_3, water_try$cluster)
View(shopclust_3)

## Creating a 3d plot for the clusters

library(scatterplot3d)

shopclust_4<- shopclust_3

shopclust_4$pcolor[shopclust_4$water_try.cluster==1]<- "red"
shopclust_4$pcolor[shopclust_4$water_try.cluster==2]<- "blue"
shopclust_4$pcolor[shopclust_4$water_try.cluster==3]<- "green"
shopclust_4$pcolor[shopclust_4$water_try.cluster==4]<- "yellow"
shopclust_4$pcolor[shopclust_4$water_try.cluster==5]<- "orange"
shopclust_4$pcolor[shopclust_4$water_try.cluster==6]<- "black"
shopclust_4$pcolor[shopclust_4$water_try.cluster==7]<- "brown"
shopclust_4$pcolor[shopclust_4$water_try.cluster==8]<- "purple"

scatterplot3d(shopclust_4$Electronics,shopclust_4$Groceries,shopclust_4$Clothes,shopclust_4$Alcohal,
              pch=16,
              highlight.3d=FALSE,
              color = shopclust_4$pcolor,
              main="3D Scatterplot")
















