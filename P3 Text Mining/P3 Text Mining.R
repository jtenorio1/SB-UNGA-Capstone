#load packages
suppressMessages(library(tm))
suppressMessages(library(SnowballC))
suppressMessages(library(ggplot2))

#create text info to use or analysis
#create data for text mining 
voteshortvars <- c("X", "short")
voteinfo.short <- voteinfo[voteshortvars]

votelongvars <- c("X", "descr")
voteinfo.long <- voteinfo[votelongvars]

write.csv(voteinfo, file = "voteinfoP3.csv")
write.csv(voteinfo.long, file = "votelong.csv")
write.csv(voteinfo.short, file = "voteshort.csv")

#preprocessing
#-------------------------------------------------------------------------------------------------
#corpus is a collection of documents - we use the tm function to do this
#voteshort = read.csv("voteshort.csv", stringsAsFactors = F)
#votelong = read.csv("votelong.csv", stringsAsFactors = F)

corpus = Corpus(VectorSource(voteinfo.short$short))
corpus[[1]]
str(voteshort)


#lowercase all text and reomve punctuaions 
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)

#remove stopwords
corpus = tm_map(corpus, removeWords, stopwords("english"))

#stem the document
corpus = tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, PlainTextDocument)   

#convert corpus to df for later use
shortdf <-data.frame(text=unlist(sapply(corpus, `[`, "content")), 
                      stringsAsFactors=F)

#extracting word frequencies 
#---------------------------------------------------------------------------------------------------
frequencies = DocumentTermMatrix(corpus)
frequencies
inspect(frequencies)
str(frequencies)
#we have a total of 1478 terms in our dataset

inspect(frequencies[1000:1005,505:515]) 
#we can inspect the matrix - let's look at documents (rows) 1000-1005 and look at the words 505-515
#the data is sparse if there are a lot of 0's in the matrix

findFreqTerms(frequencies, lowfreq = 20) 
#we can look at the most popular terms are. Here we're looking for terms that appear at least 20 times.
#151 appear at least 20 times. More terms means more independent variables. which means it takes longer 
#to build the model.the ratio of IV to obs will affect how good the model can generalize. 
#We need to remove some terms to make model more effective


#organize items by frequency 
ordfreq <- colSums(as.matrix(frequencies))   
length(ordfreq)   
ord <- order(ordfreq)


#least freq words
ordfreq[head(ord)]  

#most frequent words
ordfreq[tail(ord)]

#view the top 20
topfreq <- sort(colSums(as.matrix(frequencies)), decreasing=TRUE)   
head(topfreq, 20)   


#This will identify all terms that appear frequently (in this case, 50 or more times).
findFreqTerms(frequencies, lowfreq=50)   # Change "50" to whatever is most appropriate for your text data.
wf <- data.frame(word=names(topfreq), freq=topfreq)   #simply the df of all the terms sort from most to least frequent
head(wf)  
str(wf)

#plot words that appear at least x times
p <- ggplot(subset(wf, freq>100), aes(word, freq)) #shows histogram of words with freq > 100
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   


#Finding insights
#-------------------------------------------------------------------------------------------------------------------
#now let's Start by removing sparse terms (i.e. narrowing dataset):   
sparse = removeSparseTerms(frequencies, 0.985) # only keeps terms that appear in 2% or more of the rows (18 terms in this case)
inspect(sparse)  
sparse


#organize items by frequency 
ordsparse <- colSums(as.matrix(sparse))   
length(ordsparse)   
ordsp <- order(ordsparse)
ordsparse
str(ordsparse) #view structure
ordsparse[1] # view first entry

#view the top 20
topsparse <- sort(colSums(as.matrix(sparse)), decreasing=TRUE)   
head(topsparse, 30)   


#we find that 26 terms appear in 1.5% or more of the data. The intent is to use these 26 terms to identify 
#buckets for each vote based on the short description.Many of these terms will have strong associations
#with other terms in this list of 26 so we need to see which buckets make the most sense. For example,
#we will find "south" is very commonly associiated with "africa" so the bukcet would actually be something
#like "South Africa."


#****the following used for regression which is not used***
#now lets convert the sparse matrix into a data frame
#shortsparse = as.data.frame(as.matrix(sparse)) #converts sparse to a df called shortsparse
#colnames(shortsparse) = make.names(colnames(shortsparse))
# we need to run the make.names function to ensure all our variable names are appropriate names.


#term correlations & word clouds!
#-----------------------------------------------------------------------------------------------------------------
#If you have a term in mind that you have found to be particularly meaningful to your analysis, 
#then you may find it helpful to identify the words that most highly correlate with that term.
#If words always appear together, then correlation=1.0

findAssocs(frequencies, c("weapon"), corlimit=0.25) # specifying a correlation limit of 0.25 

#human is associated with "rights" and "advanc"; bucket = Human Rights & Advancement 
#africa is associated with 

#make a word cloud
set.seed(142)   















