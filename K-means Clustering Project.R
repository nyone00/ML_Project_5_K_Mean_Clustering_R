#Open both data sets
df1 <- read.csv('winequality-red.csv',sep=';')
df2 <- read.csv('winequality-white.csv',sep=';')

# or df1$label <- 'red'
df1$label <- sapply(df1$pH,function(x){'red'})
df2$label <- sapply(df2$pH,function(x){'white'})

head(df2)


# Combine df1 and df2 into a single data frame
wine <- rbind(df1,df2)
str(wine)

# EDA
# create a histogram of residual suagr from the wine data
# color by red and white wines
pl <- ggplot(wine,aes(x=residual.sugar)) + geom_histogram(aes(fill=label),color='black',bins=50)
pl + scale_fill_manual(values = c('red','white')) + theme_bw()

# create a histogram of citric.acid from the wine data. color by red and white wines
ggplot(wine,aes(x=citric.acid)) + geom_histogram(aes(fill=label),color='black',bins=50) + scale_fill_manual(values = c('red','white')) + theme_bw()

# create a histogram of alcohol from the wine data. color by red and white wines
ggplot(wine,aes(x=alcohol)) + geom_histogram(aes(fill=label),color='black',bins=50) + scale_fill_manual(values = c('red','white')) + theme_bw()

# create a scatterplot of residual.sugar versus citric.acid, color by red and white wine
ggplot(wine,aes(citric.acid,residual.sugar)) + geom_point(aes(color=label),alpha=0.3) + theme_dark()

# create a scatterplot of volatile.acidity versus residual.sugar, color by red and white wine
ggplot(wine,aes(volatile.acidity,residual.sugar)) + geom_point(aes(color=label),alpha=0.3)                                                  

# grab the wine data without the label
clus.data <- wine[,1:12]

# Building the Clusters
# call the kmeans function on clus.data and assign the results to wine.cluster
wine.cluster <- kmeans(clus.data,2,nstart = 10)

# print out the wine.cluster and explore it
print(wine.cluster$centers)

# evaluating the clusters
table(wine$label,wine.cluster$cluster)

#        1       2
# red   1515     84
# white 1310    3588
