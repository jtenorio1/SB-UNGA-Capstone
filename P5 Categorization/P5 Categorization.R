#install library
install.packages(gtools)

#load Packages
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))


voteinfo<- voteinfo[,-1]
voteinfo <- transform(voteinfo, 
                      session = as.factor(session),
                      rcid = as.factor(rcid),
                      importantvote = as.factor(importantvote),
                      date = as.Date(date),
                      unres = as.character(unres),
                      short = as.character(short),
                      descr = as.character(descr))
str(voteinfo)


voteinfotemp <- voteinfo[grep("SECURITY COUNCIL",voteinfo$descr),]

#P1 CREATE TEXT COLUMN FOR CATEGORIZATION
#using the terms identified in P4 Text Mining, we will go through and add categorizations.
#in our text mining we used a singe column that combined short & descr so we need to repeat this operation:
voteinfo <- voteinfo %>% unite(votetext, short, descr, sep = " ", remove = F)

#P2 SPLIT DATA INTO TWO SETS: ONE WITH UNCATEGORIZED VOTES; THE OTHER WITH CATEGORIZED VOTES
#WE WILL USE THE UNASSIGNED DATASET TO ASSIGN CATEGORIES TO THEM
voteinfo <- voteinfo %>% mutate(sumassigned = me+nu+di+hr+co+ec)
no<-sum(voteinfo$sumassigned == 0)
yes<-sum(voteinfo$sumassigned >=1)

voteinfoc <- subset(voteinfo, sumassigned >= 1)
voteinfou <- subset(voteinfo, sumassigned == 0)

#P3 ASSIGN CATEGORIES TO THE UNCLASSIFIED VOTES DATASET
#CATEGORY 1 - PROCEDURAL/STRUCTURAL: The procedural category will be marked if the text contains the following words:
#adopt, adopted, approve, draft, paragraph, resolution, said
voteinfops<- filter(voteinfou, grepl('ADOPT|ADOPTED|APPROVE|DRAFT|PARAGRAPH|RESOLUTION',voteinfou$votetext, ignore.case = T)) #find the rows containing the terms
psx <- voteinfops$X # store the X ID's where the terms are present as a list
temp<- voteinfou[voteinfou$X %in%  psx,] #pull the X's containing the target terms in the  list
temp<- temp %>% mutate(ps = 1) #assign all the rows with the target terms to the category 
temp1<- merge(x = voteinfou,y = temp[,c("X","ps")],by = "X", all.x = T) #add in the new category col to the uncategorized dataset
temp1[["ps"]][is.na(temp1[["ps"]])]<- 0 #replace NA's with 0's to our newly added category 
sum(temp1$ps) #check the sum of the new category col to ensure all the assignments were added (839)
voteinfou<-temp1 #update the table

#CATEGORY 2 - AFRICA: The procedural category will be marked if the text contains the following words:
#africa
voteinfoaf<- filter(voteinfou, grepl('AFRICA',voteinfou$votetext, ignore.case = T)) #find the rows containing the terms
afx <- voteinfoaf$X # store the X ID's where the terms are present as a list
temp<- voteinfou[voteinfou$X %in%  afx,] #pull the X's containing the target terms in the  list
temp<- temp %>% mutate(af = 1) #assign all the rows with the target terms to the category 
temp1<- merge(x = voteinfou,y = temp[,c("X","af")],by = "X", all.x = T) #add in the new category col to the uncategorized dataset
temp1[["af"]][is.na(temp1[["af"]])]<- 0 #replace NA's with 0's to our newly added category 
sum(temp1$af) #check the sum of the new category col to ensure all the assignments were added (145)
voteinfou<-temp1 #update the table

#CATEGORY 3 - SECURITY COUNCIL: The security council category will be marked if the text contains the following words:
#security council
voteinfosc<- filter(voteinfou, grepl('SECURITY COUNCIL',voteinfou$votetext, ignore.case = T)) #find the rows containing the terms
scx <- voteinfosc$X # store the X ID's where the terms are present as a list
temp<- voteinfou[voteinfou$X %in%  scx,] #pull the X's containing the target terms in the  list
temp<- temp %>% mutate(sc = 1) #assign all the rows with the target terms to the category 
temp1<- merge(x = voteinfou,y = temp[,c("X","sc")],by = "X", all.x = T) #add in the new category col to the uncategorized dataset
temp1[["sc"]][is.na(temp1[["sc"]])]<- 0 #replace NA's with 0's to our newly added category 
sum(temp1$sc) #check the sum of the new category col to ensure all the assignments were added (52)
voteinfou<-temp1 #update the table

#CATEGORY 4 - SPECIAL: The special category will be marked if the text contains the following words:
#special
voteinfosp<- filter(voteinfou, grepl('SPECIAL',voteinfou$votetext, ignore.case = T)) #find the rows containing the terms
spx <- voteinfosp$X # store the X ID's where the terms are present as a list
temp<- voteinfou[voteinfou$X %in%  spx,] #pull the X's containing the target terms in the  list
temp<- temp %>% mutate(sp = 1) #assign all the rows with the target terms to the category 
temp1<- merge(x = voteinfou,y = temp[,c("X","sp")],by = "X", all.x = T) #add in the new category col to the uncategorized dataset
temp1[["sp"]][is.na(temp1[["sp"]])]<- 0 #replace NA's with 0's to our newly added category 
sum(temp1$sp) #check the sum of the new category col to ensure all the assignments were added (52)
voteinfou<-temp1 #update the table

#CATEGORY 5 - United Nations: The united nations category will be marked if the text contains the following words:
#united nations
voteinfoun<- filter(voteinfou, grepl('UNITED NATIONS',voteinfou$votetext, ignore.case = T)) #find the rows containing the terms
unx <- voteinfoun$X # store the X ID's where the terms are present as a list
temp<- voteinfou[voteinfou$X %in%  unx,] #pull the X's containing the target terms in the  list
temp<- temp %>% mutate(un = 1) #assign all the rows with the target terms to the category 
temp1<- merge(x = voteinfou,y = temp[,c("X","un")],by = "X", all.x = T) #add in the new category col to the uncategorized dataset
temp1[["un"]][is.na(temp1[["un"]])]<- 0 #replace NA's with 0's to our newly added category 
sum(temp1$un) #check the sum of the new category col to ensure all the assignments were added (52)
voteinfou<-temp1 #update the table

#CATEGORY 6 - International: The international category will be marked if the text contains the following words:
#international
voteinfoint<- filter(voteinfou, grepl('international',voteinfou$votetext, ignore.case = T)) #find the rows containing the terms
intx <- voteinfoint$X # store the X ID's where the terms are present as a list
temp<- voteinfou[voteinfou$X %in%  intx,] #pull the X's containing the target terms in the  list
temp<- temp %>% mutate(int = 1) #assign all the rows with the target terms to the category 
temp1<- merge(x = voteinfou,y = temp[,c("X","int")],by = "X", all.x = T) #add in the new category col to the uncategorized dataset
temp1[["int"]][is.na(temp1[["int"]])]<- 0 #replace NA's with 0's to our newly added category 
sum(temp1$int) #check the sum of the new category col to ensure all the assignments were added (52)
voteinfou<-temp1 #update the table

#CATEGORY 7 - budget: The budget category will be marked if the text contains the following words:
#budget
voteinfobu<- filter(voteinfou, grepl('budget',voteinfou$votetext, ignore.case = T)) #find the rows containing the terms
bux <- voteinfobu$X # store the X ID's where the terms are present as a list
temp<- voteinfou[voteinfou$X %in%  bux,] #pull the X's containing the target terms in the  list
temp<- temp %>% mutate(bu = 1) #assign all the rows with the target terms to the category 
temp1<- merge(x = voteinfou,y = temp[,c("X","bu")],by = "X", all.x = T) #add in the new category col to the uncategorized dataset
temp1[["bu"]][is.na(temp1[["bu"]])]<- 0 #replace NA's with 0's to our newly added category 
sum(temp1$bu) #check the sum of the new category col to ensure all the assignments were added (52)
voteinfou<-temp1 #update the table

#CATEGORY 8 - peace: The peace category will be marked if the text contains the following words:
#peace
voteinfopc<- filter(voteinfou, grepl('peace',voteinfou$votetext, ignore.case = T)) #find the rows containing the terms
pcx <- voteinfopc$X # store the X ID's where the terms are present as a list
temp<- voteinfou[voteinfou$X %in%  pcx,] #pull the X's containing the target terms in the  list
temp<- temp %>% mutate(pc = 1) #assign all the rows with the target terms to the category 
temp1<- merge(x = voteinfou,y = temp[,c("X","pc")],by = "X", all.x = T) #add in the new category col to the uncategorized dataset
temp1[["pc"]][is.na(temp1[["pc"]])]<- 0 #replace NA's with 0's to our newly added category 
sum(temp1$pc) #check the sum of the new category col to ensure all the assignments were added (52)
voteinfou<-temp1 #update the table

#P4 EXAMINE: Let's see how many of the unclassified rows we now managed to classify into 1 of the 8
#newly added categories 
voteinfou <- voteinfou %>% mutate(sumassigned2 = ps+af+sc+sp+un+int+bu+pc)
no<-sum(voteinfou$sumassigned2 == 0)
yes<-sum(voteinfou$sumassigned2 >=1)

voteinfoc <- subset(voteinfo, sumassigned >= 1)
voteinfou <- subset(voteinfo, sumassigned == 0)

#CONCLUSION: we managed to classify 1279/1513 (~85%) of the previously unclassified rows
summary(voteinfou$sumassigned2)


#P5 CLEAN UP: We'll  finish this part of the project by merging the two dataset (classified and unclassified)
#adn removing uneeded columns
voteinfoc$ps <- 0
voteinfoc$af <- 0
voteinfoc$sc <- 0
voteinfoc$un <- 0
voteinfoc$int <- 0
voteinfoc$bu  <- 0
voteinfoc$pc <- 0
voteinfoc$sp <- 0
voteinfoc$sumassigned2 <- 0 

voteinfomerged <- rbind(voteinfou, voteinfoc) #merge the newley categorized data with the old categorized data
temp <- inner_join(voteinfomerged, voteinfo, by = "X") #check to ensure no rows were lost along the way 

drops <- c("sumassigned2","sumassigned")
voteinfomerged <- voteinfomerged[,!(names(voteinfomerged) %in% drops)]

write.csv(voteinfomerged, file = "P5outputvoteinfo.csv")
save(voteinfomerged,file="P5outputvoteinfo.Rda")

