#text mining
Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
install.packages(Needed, dependencies=TRUE)

install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")    


suppressMessages(library(tm))
suppressMessages(library(SnowballC))
suppressMessages(library(RColorBrewer))
suppressMessages(library(ggplot2))
suppressMessages(library(wordcloud))
suppressMessages(library(biclust))
suppressMessages(library(cluster))
suppressMessages(library(igraph))
suppressMessages(library(fpc))


#create data for text mining 
voteshortvars <- c("X", "short")
voteinfo.short <- voteinfo[voteshortvars]

votelongvars <- c("X", "descr")
voteinfo.long <- voteinfo[votelongvars]

write.csv(voteinfo, file = "voteinfoP3.csv")
write.csv(voteinfo.long, file = "votelong.csv")
write.csv(voteinfo.short, file = "voteshort.csv")

#write.table(voteinfo.texts, "voteinfo.txt", sep="\t")
#write.table(voteinfo.short, "voteshort.txt", sep="\t")
#write.table(voteinfo.long, "voteslong.txt", sep="\t")


cname <- file.path("C:", "texts")
cname   
dir(cname)   
docs <- Corpus(DirSource(cname)) 
summary(docs)
inspect(docs)

#remove punctuation and make sure # of char reduced
docs <- tm_map(docs, removePunctuation)
inspect(docs)


#remove punctuation and make sure # of char reduced
docs <- tm_map(docs, removeNumbers)
inspect(docs)


#convert all char to lowercase
docs <- tm_map(docs, tolower) 
inspect(docs)

#remove stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
inspect(docs)


#removing common wor dendings (e.g. "ing" "es" "s")
docs <- tm_map(docs, stemDocument)  
inspect(docs)


#remove unnecessary whitespace
docs <- tm_map(docs, stripWhitespace)   
inspect(docs)

#wrap upf by telling R to treat your preprocessed documents as text documents.
docs <- tm_map(docs, PlainTextDocument)   



#Stage the Data
#---------------------------------------------------------------------------------------------------------------
#create a matrix of words and their counts
dtm <- DocumentTermMatrix(docs)   
dtm   
inspect(dtm)
inspect(dtm[1:5, 1:20])


#transpose the Matrix
tdm <- TermDocumentMatrix(docs)   
tdm   


#organize items by frequency 
freq <- colSums(as.matrix(dtm))   
length(freq)   
ord <- order(freq)

#  Start by removing sparse terms:   
dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   
inspect(dtms)  

#least freq words
freq[head(ord)]  

#most frequent words
freq[tail(ord)]


#frequency of frequencies
tail(table(freq), 20)  

#For a less, fine-grained look at term freqency we can view a table of the terms we selected 
#when we removed sparse terms, above. - makes it easier to see high frequency words 
freq <- colSums(as.matrix(dtms))   
freq  

#view the top 14
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 20)   

#This will identify all terms that appear frequently (in this case, 50 or more times).
findFreqTerms(dtm, lowfreq=500)   # Change "50" to whatever is most appropriate for your text data.
wf <- data.frame(word=names(freq), freq=freq)   
head(wf)  

#plot words that appear at least x times
p <- ggplot(subset(wf, freq>900), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   


#term correlations & word clouds!
#-----------------------------------------------------------------------------------------------------------------
#If you have a term in mind that you have found to be particularly meaningful to your analysis, 
#then you may find it helpful to identify the words that most highly correlate with that term.
#If words always appear together, then correlation=1.0

findAssocs(dtm, c("war"), corlimit=0.98) # specifying a correlation limit of 0.98   

#make a word cloud
set.seed(142)   
wordcloud(names(freq), freq, min.freq=500)   



#clustering by similarity
#-----------------------------------------------------------------------------------------------------------------
#first, remove a lot of the uninteresting or infrequent words. 
#If you have not done so already, you can remove these with the following code.
dtmss <- removeSparseTerms(dtm, 0.01) # This makes a matrix that is only 15% empty space, maximum.   
inspect(dtmss)   
str(dtmss)

#edit dtmss to reduce the number of col in the list based on those words that are less frequent 

#calculate distance between words & then cluster them according to similarity.
d <- dist(t(dtmss), method="euclidian")   
fit <- hclust(d=d, method="ward.D")   
fit   
plot(fit, hang=-1)   
