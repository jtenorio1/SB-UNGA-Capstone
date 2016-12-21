#load packages
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))

#PART 1: PREP VOTEINFO DATASET
#import & format voteinfo
voteinfo <- voteinfomerged
voteinfo <- transform(voteinfo, 
                      session = as.factor(session),
                      rcid = as.factor(rcid),
                      importantvote = as.factor(importantvote),
                      date = as.Date(date),
                      unres = as.character(unres),
                      short = as.character(short),
                      descr = as.character(descr),
                      ps = as.integer(ps),
                      af = as.integer(af),
                      sc = as.integer(sc),
                      sp = as.integer(sp),
                      un = as.integer(un),
                      int = as.integer(int),
                      bu= as.integer(bu),
                      pc = as.integer(pc))
str(voteinfo)

#let's ensure the rcid is the uniqque identifier we can use to tie to the voteinfo dataset
temp <- data.frame(table(voteinfo$rcid))
temp[temp$Freq > 1,]

#we're going to merge the voteinfo table with the rawvotes table in order to get granularity of the rows
#by country. This results in a HUGE dataset so let's drop the session column (already in rawvotes) and the descr
#column (this data is captured by the categories and unres)
drops <- c("descr", "session")
voteinfo <- voteinfo[,!(names(voteinfo)) %in% drops]

#we can drop the two text cols (short and votetext), since they are both contained within descr
drops <- c("short", "votetext")
voteinfo <- voteinfo[,!(names(voteinfo)) %in% drops]

#PART 2: PREP RAWVOTES DATASET
rawvotes <- transform(rawvotes,
                      ccode = as.factor(ccode),
                      session = as.factor(session),
                      rcid = as.factor(rcid),
                      vote = as.factor(vote))
str(rawvotes)


#Part 3: MERGE RAWVOTES & VOTEINFO
temp1<- merge(x = rawvotes,
              y = voteinfo[,c("X","rcid","abstain", "yes", "no", "importantvote", "date","unres",
                              "me","nu","di","hr","co","ec","ps","af","sc","sp","un","int","bu","pc")],
              by = "rcid", 
              all.x = T)

#reorder and rename
temp1 <- temp1[,c(2,3,7,1,4,5,6,8:27)]
colnames(temp1)[8] <- "sum.abstain"
colnames(temp1)[9] <- "sum.yes"
colnames(temp1)[10] <- "sum.no"

#add col for total votes on that rcid
temp1 <- temp1 %>% mutate(sum.votes = sum.abstain +sum.no +sum.yes)
temp1 <- temp1[,c(1:10,28,11:27)]
rawinfo <- temp1

#CONCLUSION: we have a table with the rawvotes merged with the voteinfo. Now we can analyze each country's
#individual vote on a particular topic based on the categories from voteinfo. 


#PART 4: MERGE IDEALS AND RAW INFO
#we can insert the specific country name/info from the ideals table using ccode
str(rawinfo)
str(ideals)
ideals <- transform(ideals,
                    Year = as.character(Year),
                    ccode = as.factor(ccode),
                    session = as.factor(session),
                    CountrySession = as.character(CountrySession))
ideals <- transform(ideals, Year = as.Date(Year, "%Y"))

#inorder to merge both ideals and rawinfo, we need to create a foreign key in each table
#by concatenating the session with the ccode

temp <- ideals %>% mutate(session.cc = paste(session, ccode,"x",  sep = "."))
str(temp)
temp1<- rawinfo %>%  mutate(session.cc = paste(session, ccode, "x", sep = "."))
str(temp1)

temp3<- merge(x = temp1,
              y = temp[,c("ccode","CountryAbb", "CountryName", "CountrySession", "Idealpoint", "session.cc")],
              by = "session.cc", 
              all.x = T)

drops <- c("X.x", "X.y","ccode.y", "session.cc")
temp3 <- temp3[,!(names(temp3)) %in% drops]

temp3 <- temp3[,c(1:4, 27:30,5:26)]

temp3 <- temp3[order(temp3$rcid),]


rawccinfo <- temp3
colnames(rawccinfo)[3] <- "ccode"

#CONCLUSION: We now have a table containing inidivudal country votes for each UN resolution, and each resolution 
#(with some exceptions) is categroized into 1 or more of 13 categories. We also have the idealpoints for each
#country by session. 

#PART 5: MERGE RAWCCINFO WITH COUNTRYINFO
#the last piece of information we want to incorporate is the regional info for each countty
countryinfo <- transform(countryinfo,
                         ccode = as.factor(ccode),
                         Region.Code = as.factor(Region.Code))
temp <- countryinfo
str(temp)
str(rawccinfo)

temp1 <- merge(x = rawccinfo,
               y = temp[,c("ccode","Region.Code","Macro.Region", "Sub.Region")],
               by = "ccode",
               all.x = T)

temp1 <- temp1[order(temp1$rcid),]
temp2 <- temp1[,c(4,16,15,3,1, 5:14,17:33)]

unvotes <- temp2

#CONCLUSION: we now have a master dataset containing all the info we will need for analysis; however
#we still need to do some cleanup and reformatting before clustering. 


#PART 6: FINAL PREP OF CLUSTER DATASET
#we need to get rid of rows where the vote is 8 (absent) or 9 (not a member) because
#these countries do not impact the vote and are not counted in the vote total. Furthermore,
#there is no tracking of the idealpoint for non-member countries 
str(unvotes)

temp <- transform(unvotes,vote = as.factor(vote))
str(temp)

count(unvotes, unvotes$vote == 9) #254,521
count(unvotes, unvotes$vote == 8) #71,095
count(unvotes, unvotes$vote == 1) #566,456
count(unvotes, unvotes$vote == 2) #91,838
count(unvotes, unvotes$vote == 3) #52,981
#total = 1,036,891


temp1 <- subset(unvotes, vote == 1 | vote == 2 | vote == 3)
#total = 711,275
#after inspecing data I noticed there are some na's for the category data because some of the 
#rcid's in the raw votes file were removed during scrubbing in the voteinfo file. 

temp2 <- subset(unvotes, is.na(unres))
unique(temp2["rcid"]) #30 unique rcid's where value's are na, totaling to 5910 rows (<1% of data)
#we will drop these rows
temp3 <- temp1[complete.cases(temp1),] #708,051

unvotes <- temp3

#now we have only complete data. Next step is to transform the vote column into discrete variables for the 3 outcomes
#a yes is a vote in support so is +
#an abstention is a vote of no impact so is 0
#a no is a vote in opposition so is -
temp <- unvotes
temp$v.weight <- ifelse(temp$vote == 1, 1,
                 ifelse(temp$vote == 2, 0,
                 ifelse(temp$vote == 3, -1, 
                        0)))
temp <- temp[, c(1:10,33,11:32)]
unique(temp$Sub.Region)

#similarly lets create columns indicating the region
str(temp)
temp$S.AM <- ifelse(temp$Sub.Region == "South America", 1,0)
temp$N.AM <- ifelse(temp$Sub.Region == "Northern America", 1,0)
temp$W.EU <- ifelse(temp$Sub.Region == "Western Europe", 1,0)
temp$N.EU <- ifelse(temp$Sub.Region == "Northern Europe", 1,0)
temp$E.EU <- ifelse(temp$Sub.Region == "Eastern Europe", 1,0)
temp$S.EU <- ifelse(temp$Sub.Region == "Southern Europe", 1,0)
temp$CAR <- ifelse(temp$Sub.Region == "Caribbean", 1,0)
temp$W.AF <- ifelse(temp$Sub.Region == "Western Africa", 1,0)
temp$E.AF <- ifelse(temp$Sub.Region == "Eastern Africa", 1,0)
temp$S.AF <- ifelse(temp$Sub.Region == "Southern Africa", 1,0)
temp$S.AS <- ifelse(temp$Sub.Region == "Southern Asia", 1,0)
temp$W.AS <- ifelse(temp$Sub.Region == "Western Asia", 1,0)
temp$N.AF <- ifelse(temp$Sub.Region == "Northern Africa", 1,0)
temp$C.AM <- ifelse(temp$Sub.Region == "Central America", 1,0)
temp$SEA <- ifelse(temp$Sub.Region == "South-Eastern Asia", 1,0)
temp$ANZ <- ifelse(temp$Sub.Region == "Australia and New Zealand", 1,0)
temp$E.AS <- ifelse(temp$Sub.Region == "Eastern Asia", 1,0)
temp$M.AF <- ifelse(temp$Sub.Region == "Middle Africa", 1,0)
temp$MEL <- ifelse(temp$Sub.Region == "Melanesia", 1,0)
temp$POL <- ifelse(temp$Sub.Region == "Polynesia", 1,0)
temp$MIC <- ifelse(temp$Sub.Region == "Micronesia", 1,0)
temp$C.AS <- ifelse(temp$Sub.Region == "Central Asia", 1,0)

temp <- temp[, c(1:10,12:15, 11, 16:30, 34:55, 31:33)]
temp <- temp[, c(1:8, 10:15, 9, 16:55)]

unvotes <- temp

str(unvotes)

unvotes <- transform(unvotes, 
                     v.weight = as.integer(v.weight),
                     Idealpoint = as.numeric(Idealpoint),
                     importantvote = as.integer(importantvote),
                     S.AM = as.integer(S.AM),
                     N.AM = as.integer(N.AM),
                     W.EU = as.integer(W.EU),
                     N.EU = as.integer(N.EU),
                     E.EU = as.integer(E.EU),
                     S.EU = as.integer(S.EU),
                     CAR = as.integer(CAR),
                     W.AF = as.integer(W.AF),
                     E.AF = as.integer(E.AF),
                     S.AF = as.integer(S.AF),
                     S.AS = as.integer(S.AS),
                     W.AS = as.integer(W.AS),
                     N.AF = as.integer(N.AF),
                     C.AM = as.integer(C.AM),
                     SEA = as.integer(SEA),
                     ANZ = as.integer(ANZ),
                     E.AS = as.integer(E.AS),
                     M.AF = as.integer(M.AF),
                     MEL = as.integer(MEL),
                     POL = as.integer(POL),
                     MIC = as.integer(MIC),
                     C.AS = as.integer(C.AS))

write.csv(unvotes, file = "P6outputscatterprep.csv")
save(unvotes,file="P6outputscatterprep.Rda")
