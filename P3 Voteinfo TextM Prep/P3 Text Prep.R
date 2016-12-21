#load packages
suppressMessages(library(tm))
suppressMessages(library(SnowballC))
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(reshape2))

#OBJECTIVE: Format voteinfo data to identify additional descirptive fields for each vote that
#is not currently classified into one of the six categories provided.

str(voteinfo)
voteinfo <- transform(voteinfo, 
                      session = as.factor(session),
                      rcid = as.factor(rcid),
                      importantvote = as.factor(importantvote),
                      date = as.Date(date),
                      unres = as.character(unres),
                      short = as.character(short),
                      descr = as.character(descr))

#PART 1: Identify the rows without a categorization in one of the six category types.
#use a sum to find which votes lack a category 
voteinfo <- voteinfo %>% mutate(sumassigned = me+nu+di+hr+co+ec)
no<-sum(voteinfo$sumassigned == 0)
yes<-sum(voteinfo$sumassigned >=1)
#check
yes+no
#CONCLUSION: there are a total of 1513/5325 rows without an assignment; we will use text 
#mining to find these values a home. 

#PART 2: There are two cols of text data we want to use and analyze at once: the short
#descr and the long descr. We will combine them both into a single col to expidite the
#text mining process. 
vunassigned <- subset(voteinfo, voteinfo$sumassigned == 0)
votetext <- vunassigned %>% unite(votetext, short, descr, sep = " ", remove = F)

#PART 3: now let's create the new data file with only the text info we will be analyzing. 
votetextdata <- c("X", "votetext")
votetext <- votetext[votetextdata]
save(votetext,file="votetext.Rda")

#CONCLUSION: We will use the votetext file for text mining in P4. 


