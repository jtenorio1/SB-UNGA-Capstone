#install packages
install.packages("dplyr")
install.packages("tidyr")
install.packages("reshape2")
install.packages("ggplot2")

#load packages
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(reshape2))
suppressMessages(library(ggplot2))

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
                      session = as.factor(session))
#ideals <- transform(ideals, Year = as.character(Year))
#ideals$Year <- as.Date(ideals$Year,"%Y")
#ideals$Year <- strftime(ideals$Year, "%Y")
ideals <- transform(ideals,
                    ccode = as.factor(ccode),
                    session = as.factor(session)
                    )
countrycodes$ccode <- as.factor(countrycodes$ccode)

#Q: Should the year of the vote in the ideals data be formatted as something other than an int?

#final check of relevant data and proper data format
str(countryinfo)
str(voteinfo)
str(rawvotes)
str(ideals)
str(countrycodes)



# Data Scrubbing Below
#----------------------------------------------------------------------------------------------------------------
#identify which tables contain missing values, what values are missing, AND  
blanks <- countryinfo[rowSums(is.na(countryinfo)) > 0, ]  #PASS
blanks <- voteinfo[rowSums(is.na(voteinfo)) > 0, ]  #OK, 31 (~0%)
blanks <- rawvotes[rowSums(is.na(rawvotes)) > 0, ]  #OK, 12714 (~1%)
blanks <- ideals[rowSums(is.na(ideals)) > 0, ]  #FAIL, 2094 (~22%) - main fields for NA are pct agree with other countries
  temp <- ideals[,9:14]
  blanks <- temp[rowSums(is.na(temp)) > 0, ] #FAIL, 2094 are NA (100% of subset), so let's remove these fields (not necessarily needed anyway)
  ideals <- ideals[, 1:8]
  blanks <- ideals[rowSums(is.na(ideals)) > 0, ] #PASS
blanks <- countrycodes[rowSums(is.na(countrycodes)) > 0, ]  #PASS

raw.votes %>% select(ccode) %>% count() %>% unique()
raw.votes %>% select(vote) %>% unique()
sum(is.na(raw.votes$ccode))

transform(ccode = as.factor(ccode)

count(raw.votes, 'stateabb')
factor(raw.votes$ccode)



#statistics below on votedesc
#----------------------------------------------------------------------------------------------------------------
rawvotes %>% group_by(ccode, vote) %>% count()




#Data Plotting Below 
#----------------------------------------------------------------------------------------------------------------

#Plot of # of countries voting vs UN session (see how UN membership has changed)
ggplot(raw.votes, aes(x = session, y = vote, col = factor(Macro.Region))) +
  geom_jitter()

temp <- subset(raw.votes, stateabb %in% c("USA"))

ggplot(subset(raw.votes, stateabb %in% c("USA")), aes(x = session, y = vote)) +
  geom_jitter()




write.csv(country.master, file = "output.csv")
