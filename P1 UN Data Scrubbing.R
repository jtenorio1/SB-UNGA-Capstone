#install packages
install.packages("dplyr")
install.packages("tidyr")
install.packages("reshape2")
install.packages("ggplot2")
install.packages("caTools")
install.packages("ROCR")

#load packages
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(reshape2))
suppressMessages(library(ggplot2))
suppressMessages(library(caTools))
suppressMessages(library(ROCR))


# Importing the Relevant Data - import all with strings as factors 
#----------------------------------------------------------------------------------------------------------------
str(Country.Info.Master)
str(descriptionsnew)
str(RawVotingdata)
str(Idealpoints)  #import with headings
str(COW.Country.Codes)

#rename for ease of access
countryinfo <- tbl_df(Country.Info.Master)
voteinfo <- tbl_df(descriptionsnew)
rawvotes <- tbl_df(RawVotingdata)
ideals <- tbl_df(Idealpoints)
countrycodes <- tbl_df(COW.Country.Codes)

#keep only needed columns
countryinfo <- countryinfo[,1:7]
ideals <- ideals[,c(1:8, 16:21)]
countrycodes <- countrycodes[, 1:3]
voteinfo <- voteinfo[, c(1:8, 11:18)]

#properly format data
countryinfo <- transform(countryinfo, ccode = as.factor(ccode))

voteinfo$date <- as.Date(voteinfo$date, "%m/%d/%Y")

voteinfo <- transform(voteinfo, 
                      short = as.character(short),
                      descr = as.character(descr))

rawvotes <- transform(rawvotes, 
                      ccode = as.factor(ccode),
                      vote = as.factor(vote),
                      session = as.factor(session),
                      rcid = as.factor(rcid)
                      )

ideals <- transform(ideals,
                    ccode = as.factor(ccode),
                    session = as.factor(session)
                    )
countrycodes$ccode <- as.factor(countrycodes$ccode)


#final check of relevant data and proper data format
str(countryinfo) 
str(voteinfo)
str(rawvotes)
str(ideals)
str(countrycodes)



#ADDRESS MISSING VALUES
#----------------------------------------------------------------------------------------------------------------
#identify which tables contain missing values, what values are missing, AND reduce/eliminate where possible 

#countryinfo - cleaned with no NA's
blanks <- countryinfo[rowSums(is.na(countryinfo)) > 0, ]  #PASS, but I know there's some cells with "#N/A"
countryinfo.NA <- subset(countryinfo, countryinfo$Macro.Region == "#N/A" | countryinfo$Sub.Region == "#N/A")
#so there's 5 countries where the region specification is missing... do we really need these? 
#let's see if they're in the raw votes data at all:
countryinfo.m <- subset(rawvotes, rawvotes$ccode == 402) #5275 instances
countryinfo.m <- subset(rawvotes, rawvotes$ccode == 267) #0 instances 
countryinfo.m <- subset(rawvotes, rawvotes$ccode == 327) #0 instances 
countryinfo.m <- subset(rawvotes, rawvotes$ccode == 332) #0 instances 
countryinfo.m <- subset(rawvotes, rawvotes$ccode == 335) #0 instances 
#so we can drop the rows with 0 instances:
countryinfo <- countryinfo[!countryinfo$ccode == 267, ]
countryinfo <- countryinfo[!countryinfo$ccode == 327, ]
countryinfo <- countryinfo[!countryinfo$ccode == 332, ]
countryinfo <- countryinfo[!countryinfo$ccode == 335, ]
#but now we need to fill in the #N/A value for ccode = 402
countryinfo.402 <- countryinfo[countryinfo$ccode == 402, ]
#by looking at the established year (1975) and investigating we can fill in the missing info for this data
countryinfo$Region.Code[countryinfo$ccode == 402] <- 433
countryinfo$Sub.Region[countryinfo$ccode == 402] <- "Western Africa"
countryinfo$Macro.Region[countryinfo$ccode == 402] <- "Africa"
#double check there's no NA's
countryinfo.NA <- subset(countryinfo, countryinfo$Macro.Region == "#N/A" | countryinfo$Sub.Region == "#N/A") #PASS



#voteinfo - cleaned with no NA's
voteinfo.temp <- voteinfo[rowSums(is.na(voteinfo)) > 0, ]  #OK, 31 (~0%) - let's drop these 31 instances since they represent >1%
voteinfo <- voteinfo[ !rowSums(is.na(voteinfo)) > 0, ]

#rawvotes
rawvotes.blanks <- rawvotes[rowSums(is.na(rawvotes)) > 0, ]  #OK, 15714/1052605 rows (~1%)
summary(rawvotes.blanks) 
#all blanks are for session 69
temp <- rawvotes[rawvotes$session == '69',]
#let's see if there's any other commonality (besides session) between tese NA's
temp <- temp$rcid
temp <- data.frame(temp)
temp <- temp %>% unique()
#pertains to 81 specific countries - let's see if the issues are similar
colnames(temp) <- "rcid"
temp.m <- merge(x = temp, y = voteinfo[,c("rcid","session", "descr")], by = "rcid") #merge temp rcid with voteinfo descr
#no immediately identifiable reason why all these rcid's from session 69 are missing values... will drop these instances
summary(temp) #multiple rcid's with NA's in votes column
rawvotes <- rawvotes[!rowSums(is.na(rawvotes)) > 0, ] #1052605-15714=1036891 remaining values
#final check to make sure no remaining NA's
rawvotes.blanks <- rawvotes[rowSums(is.na(rawvotes)) > 0, ] #PASS NO MORE NA's


#ideals
ideals.blank <- ideals[rowSums(is.na(ideals)) > 0, ]  #FAIL, 2094/9505 (~22%) - main fields for NA are pct agree with other countries
  #we don't intend to use the pcnt agreements between other countries so let's drop these cols
  ideals <- ideals[, 1:8]
  #now let's see where the NA's are
  blanks <- ideals[rowSums(is.na(ideals)) > 0, ] #PASS - removing these cols eliminated all balnks 

#countrycodes
blanks <- countrycodes[rowSums(is.na(countrycodes)) > 0, ]  #PASS


#RESULTS: we now have all the data tables properly formatted and all the NA values removed. 
#formatting our df's and eliminating NA's will allows to derive a model for product a
#pass/fail on a particular vote. Let's save the scrubbed data in the files below:

write.table(countryinfo, file = "countryinfo")
write.table(voteinfo, file = "voteinfo")
write.table(rawvotes, file = "rawvotes")
write.table(ideals, file = "ideals")
write.table(countrycodes, file = "countrycodes")

write.csv(countryinfo, file = "countryinfo")
write.csv(voteinfo, file = "voteinfo")
write.csv(rawvotes, file = "rawvotes")
write.csv(ideals, file = "ideals")
write.csv(countrycodes, file = "countrycodes")

#Q: Is it better at all to save data as a csv/table/etc?


#Scrubbing Round 2 & Data Formatting 
#----------------------------------------------------------------------------------------------------------------

#let's check the data structures
str(voteinfo)
voteinfo <- transform(voteinfo, 
                      session = as.factor(session),
                      rcid = as.factor(rcid),
                      importantvote = as.factor(importantvote),
                      date = as.Date(date),
                      unres = as.character(unres),
                      short = as.character(short),
                      descr = as.character(descr),
                      me = as.factor(me),
                      nu = as.factor(nu),
                      di = as.factor(di),
                      hr = as.factor(hr),
                      co = as.factor(co),
                      ec = as.factor(ec))

#from here we note there are a few typos in the data related to the ec field.
#it should be a factor with 2 levels, but we're coming up with 4 due to 
#two typos: "" & "0\"" & "," let's see where these are:
voteinfo.duds <- subset(voteinfo, ec == "" | ec == "0\"") #3 instances - let's replace with 0's
voteinfo$ec[voteinfo$X == 642] <- 0
voteinfo$ec[voteinfo$X == 646] <- 0
voteinfo$ec[voteinfo$X == 1483] <- 0
#doublecheck
voteinfo.duds <- subset(voteinfo, ec == "" | ec == "0\"")
voteinfo.duds <- subset(voteinfo, X == 642 | X == 646 | X == 1483)
str(voteinfo) #PASS

#resave the new voteinfo
write.csv(voteinfo, file = "voteinfo")

#countrycodes
str(countrycodes)
countrycodes <- transform(countrycodes,
                          ccode = as.factor(ccode))
#PASS

#countryinfo
str(countryinfo)
countryinfo <- transform(countryinfo,
                         ccode = as.factor(ccode),
                         Region.Code = as.factor(Region.Code))

#ideals
str(ideals)
ideals <- transform(ideals,
                    Year = as.character(Year),
                    ccode = as.factor(ccode),
                    session = as.factor(session),
                    CountrySession = as.character(CountrySession))
ideals <- transform(ideals, Year = as.Date(Year, "%Y"))
str(ideals)

#rawvotes
str(rawvotes)
rawvotes <- transform(rawvotes,
                      ccode = as.factor(ccode),
                      session = as.factor(session),
                      rcid = as.factor(rcid),
                      vote = as.factor(vote))


write.csv(countryinfo, file = "countryinfo")
write.csv(rawvotes, file = "rawvotes")
write.csv(ideals, file = "ideals")
write.csv(countrycodes, file = "countrycodes")
