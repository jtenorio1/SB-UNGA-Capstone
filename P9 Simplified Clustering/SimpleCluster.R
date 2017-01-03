#this script performs clustering on a simplified dataset allocating one row of
#average values to represent a country.


#Normalize new dataset and determine number of clusters to evaluate
#-------------------------------------------------------------------------------------------------------------------
temp <- unvotes
temp[,-c(1:2)] <- scale(temp[,-c(1:2)]) #normalize the data using scaling function
unvotesnorm <- temp

save(undatascaled,file="undatascaled.Rda")

#now let's find out what number of clusters works best to most reduce the sum of squares. We will choose the 
#number of clusters where the marginal gain for the cluster starts to drop.We take 10 samples
#cmopute the average, and determine on average where this occurs. 
temp1 <- unvotesnorm[,c(3:17)]
wss <- (nrow(temp1)-1)*sum(apply(temp1,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(temp1, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
wss

#wssm = matrix(NA,10,15)
wssm[10,] = wss

write.csv(wssm,file = "wssclusters.csv")

#Results: Our average marginal gain stops increaseing by more than 1% after 6 clusters on average. 

#Use the filtered data to assign 6 clusters 
#-----------------------------------------------------------------------------------------------------------
k = 6 #number of desired clusters
set.seed = (sample(1:1000,1)) #seed for iterations 
KMC = kmeans(unvotesnorm[3:17], centers = k, iter.max = 1000)
str(KMC)
KMC #shows us a summary of the results 
rowclusters = KMC$cluster #extracting cluster information into a variable (ie what cluster each row is assigned)
KMC$centers #gives centraoid values for each of the input variables (scaled values)
temp <- KMC$cluster

#create a table showing the number of times a country falls in each cluster
#k6filtered <- data.frame(unclass(table(undatascaled$CountryName, KMC$cluster)))
#colnames(k6filtered) <- c("C1", "C2","C3", "C4", "C5", "C6")
#write.csv(k6filtered,file = "k6result.csv")


library(cluster)
clusplot(unvotesnorm, KMC$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=T,cex = 0.4,
         labels=2, lines=0)

unvotesk6 <- unvotes
unvotesk6$Cluster <- rowclusters
write.csv(unvotesk6,file = "unvotesk6.csv")
#--------------------------------------------------------------------------------------------------------------
library(ggplot2)


p1 <- ggplot(countryclusters, aes(x=Final.Cluster, y=Idealpoint, col = Final.Cluster))+ geom_jitter()+xlab("cluster")+ scale_fill_discrete(name = "New Legend Title")
p2 <- ggplot(countryclusters, aes(x=Final.Cluster, y=me, col = Final.Cluster))+ geom_jitter()
p3 <- ggplot(countryclusters, aes(x=Final.Cluster, y=nu, col = Final.Cluster))+ geom_jitter()
p4 <- ggplot(countryclusters, aes(x=Final.Cluster, y=di, col = Final.Cluster))+ geom_jitter()

multiplot(p1, p2, p3, p4, cols=2)
p1


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

