#THE SCRIPT BELOW OUTLINES CODE FOR ANALYZING BINNED (BY SESSION) UN DATA FOR EACH COUNTRY.
#USING THE BINNED DATA, WE WILL ASSESS THE RELATIONSHIPS BETWEEN THE WAY EACH COUNTRY VOTES
#AND DETERMINE WHICH COUNTRIES VOTE SIMILARLY. 

suppressMessages(library(dplyr))
suppressMessages(library(tidyr))

#heirchical clustering 
distances = dist(unbindata[6:42], method = "euclidean") #calculate the distances between all the points
clustercountries = hclust(distances, method = "ward") #cluster the distances
plot(clustercountries) #plot the dendogram 
rect.hclust(clustercountries, k = 10, border = "red") #draw a rect around the number of clusters desired
countryclusters = cutree(clustercountries, k=10) #split the data into the desired number of clusters 
countryclusters #this will output the vector indicating which cluster each row was assigned to
tapply(unbindata$Idealpoint, countryclusters, mean) #calculates the mean idealpoint for each cluster



#Normalize Data and Determine number of clusters to evaluate
#-------------------------------------------------------------------------------------------------------------------
temp <- unbindata
temp[,-c(1:5)] <- scale(temp[,-c(1:5)]) #normalize the data using scaling function
votescaled <- temp


set.seed(Sys.time())

#now let's find out what number of clusters works best to most reduce the sum of squares. We will choose the 
#number of clusters where the marginal gain for the cluster starts to drop.We take 10 samples
#cmopute the average, and determine on average where this occurs. 
temp1 <- temp[,c(6:42)]
wss <- (nrow(temp1)-1)*sum(apply(temp1,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(temp1, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
wss

#wssm = matrix(NA,10,15)
wssm[10,] = wss

write.csv(wssm,file = "wssclusters.csv")

#RESULTS: The marginal gain on average starts to decrease after 3 clusters. It then starts to again increase (on average)
#at 6 clusters, so we will analyze these two scenarios. 


#K-means clustering of data
#-----------------------------------------------------------------------------------------------------------------
k = 3 #number of desired clusters
set.seed = (1) #seed for iterations 
KMC = kmeans(votescaled[6:42], centers = k, iter.max = 1000)
str(KMC)
KMC #shows us a summary of the results 
rowclusters = KMC$cluster #extracting cluster information into a variable (ie what cluster each row is assigned)
KMC$centers #gives centraoid values for each of the input variables (scaled values)
temp <- KMC$cluster

#create a table showing the number of times a country falls in each cluster
k3result <- data.frame(unclass(table(unbindata$CountryName, KMC$cluster)))
colnames(k3result) <- c("C1", "C2","C3")
write.csv(k3result,file = "k3result.csv")

unbindatak3 <- unbindata
unbindatak3$k3cluster <- rowclusters
write.csv(unbindatak3,file = "unbindatak3.csv")


k = 6 #number of desired clusters
set.seed = (1) #seed for iterations 
KMC = kmeans(votescaled[6:42], centers = k, iter.max = 1000)
str(KMC)
KMC #shows us a summary of the results 
rowclusters = KMC$cluster #extracting cluster information into a variable (ie what cluster each row is assigned)
KMC$centers #gives centraoid values for each of the input variables (scaled values)
temp <- KMC$cluster

#create a table showing the number of times a country falls in each cluster
k6result <- data.frame(unclass(table(unbindata$CountryName, KMC$cluster)))
colnames(k6result) <- c("C1", "C2","C3", "C4", "C5", "C6")
write.csv(k6result,file = "k6result.csv")

unbindatak6 <- unbindata
unbindatak6$k6cluster <- rowclusters
write.csv(unbindatak6,file = "unbindatak6.csv")

#CONCLUSION: using a k means of 3 clusters is much too broad and does not allow for easily discernable factors. 
#using a k-mean of 6 clusters allows greater granularity and maes more sense, however, it highlights a few
#confounding factors in the data:
#1) If we select the cluster based on the majority of the # of rows for a country that fall in a cluster
#   we will notice that many countries have only been present for very few sessions (e.g Switzerland 
#   barely joined the UN in 2001 so has only participated in 12 sessions). 
#2) Using 6 clusters divides the countries pretty clearly into goegraphic regions (i.e. Western Countries
#   , middle east, Asia, etc.). This is likely due to the fact that we are including this info as an input
#   to the clustering.

#NEXT STEPS: We will exclude countries with participation in less than half of the UN sessions to ensure
#countries in the clustering set have a sample size of sessions large enough to properly characterize them.
#We will also exclude geographic info as an input to the clustering. This will allow us to evaluate how countries vote
#based on topic and if the clusters fall along geographic lines without the bias of a geogrphic factor


#Create New subset of data:
#------------------------------------------------------------------------------------------------------------
temp <- unbindata[unbindata$ccode %in% names(which(table(unbindata$ccode) >= 33)),] #remove ccodes appearing less than
#33 times, meaning present in less than 33 sessions. 
undata <- temp

save(undata,file="undata.Rda")

#Normalize new dataset and determine number of clusters to evaluate
#-------------------------------------------------------------------------------------------------------------------
temp <- undata
temp[,-c(1:27)] <- scale(temp[,-c(1:27)]) #normalize the data using scaling function
undatascaled <- temp

save(undatascaled,file="undatascaled.Rda")
#now let's find out what number of clusters works best to most reduce the sum of squares. We will choose the 
#number of clusters where the marginal gain for the cluster starts to drop.We take 10 samples
#cmopute the average, and determine on average where this occurs. 
temp1 <- undatascaled[,c(28:42)]
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
set.seed = (1) #seed for iterations 
KMC = kmeans(undatascaled[28:42], centers = k, iter.max = 1000)
str(KMC)
KMC #shows us a summary of the results 
rowclusters = KMC$cluster #extracting cluster information into a variable (ie what cluster each row is assigned)
KMC$centers #gives centraoid values for each of the input variables (scaled values)
temp <- KMC$cluster

#create a table showing the number of times a country falls in each cluster
k6filtered <- data.frame(unclass(table(undatascaled$CountryName, KMC$cluster)))
colnames(k6filtered) <- c("C1", "C2","C3", "C4", "C5", "C6")
write.csv(k6filtered,file = "k6result.csv")

undatak6 <- undata
undatak6$k6cluster <- rowclusters
write.csv(undatak6,file = "undatak6.csv")



test<- subset(unbindata,unbindata$C.AS==1)







