#load packages
suppressMessages(library(tm))
suppressMessages(library(SnowballC))
suppressMessages(library(ggplot2))

#load votetext.Rda file 


#preprocessing
#-------------------------------------------------------------------------------------------------
#corpus is a collection of documents - we use the tm function to do this

corpus = Corpus(VectorSource(votetext$votetext))
corpus[[1]]

#lowercase all text and reomve punctuaions 
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)

#remove stopwords
corpus = tm_map(corpus, removeWords, stopwords("english"))

#remove numbers
corpus = tm_map(corpus, removeNumbers)   

#stem the document
corpus = tm_map(corpus, stemDocument)

#clean up white space
corpus = tm_map(corpus, stripWhitespace) 

#make it a plain text doc
corpus <- tm_map(corpus, PlainTextDocument)   

#convert corpus to df for later use
#shortdf <-data.frame(text=unlist(sapply(corpus, `[`, "content")), 
#                      stringsAsFactors=F)

#extracting word frequencies 
#---------------------------------------------------------------------------------------------------
frequencies = DocumentTermMatrix(corpus)
frequencies
inspect(frequencies)
str(frequencies)
#we have a total of 1513 rows to look at and a total of 3376 terms to examine

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
sparse = removeSparseTerms(frequencies, 0.94)
sparse
#only keeps terms that appear in 1.95% or more of the rows (18 terms in this case)
#the lower the threshold, the more rows the non-sparse terms are required to be in 
#(i.e. a threshold of 0.4 requires the terms to be in at least 60% of the rows)

#below is a table to keep track of sparsities 
sparsity <- matrix(c(0.6,0,100,
                     0.7,3,66,
                     0.8,3,66,
                     0.85,5,73,
                     0.9,14,83,
                     0.92,16,84,
                     0.93,19,85,
                     0.94,25,87,
                     0.95,34,89,
                     0.955,37,90,
                     0.96,41,90,
                     0.98,105,94,
                     0.9805,108,95,
                     0.981,119,95,
                     0.982,121,95),ncol=3,byrow = T)
colnames(sparsity)<-c("threshold","terms", "sparsity")
sparsity<-as.table(sparsity)
sparsity

#CONCLUSION: we will use a sparsity of .94 b/c this results in 25 terms that can classify 87% of the data.
#The unclassifiable rows (i.e 197 of them), will be classified as "Other" and will only account for
#~4% of the overall data. 

inspect(sparse)  
sparse

#organize sparse terms by frequency & view 
ordsparse <- colSums(as.matrix(sparse))   
length(ordsparse)   
ordsp <- order(ordsparse)
ordsparse
str(ordsparse) #view structure
ordsparse[1] # view first entry

#view the top 20
topsparse <- sort(colSums(as.matrix(sparse)), decreasing=TRUE)   
head(topsparse, 30)   


#we find that 25 terms appear in 6% or more of the data. The intent is to use these 25 terms to identify 
#buckets for each vote based on the text. Many of these terms will have strong associations
#with other terms in this list of 25 so we need to see which buckets make the most sense. For example,
#we will find "south" is very commonly associated with "africa" so the bukcet would actually be something
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

findAssocs(frequencies, c("states"), corlimit=0.25) # specifying a correlation limit of 0.25 

#human is associated with "rights" and "advanc"; bucket = Human Rights & Advancement 
#africa is associated with 




#CONCLUSION:
#Out of the list of 25 words, possible categories can be narrowed to the following:
#  1. procedural/structural (ps)
#  2. Of or relating to Africa (af)
#  3. Budgetary (bu)
#  4. of or relating to the security council (sc)
#  5. of international interest/impact (in)
#  6. relating to peace (pc)
#  7. relating to delegation of power (pw)
#  8. special issues/initiaitves (sp)
#  9. relating to multiple states (st)
#  10. of or relating to the united nations (un) 
#  11. N/A









