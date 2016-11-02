#install packages
install.packages("dplyr")
install.packages("tidyr")
install.packages("reshape2")

#load packages
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(reshape2))


# Data Tidying Below
#----------------------------------------------------------------------------------------------------------------

#Import datasets as tables
rawvotes <- tbl_df(RawVotingdata)
countrycodes <- tbl_df(COW.Country.Codes)
ideals <- tbl_df(Idealpoints)
votedesc <- tbl_df(descriptionsnew)

#insert proper col names for ideals
colnames(ideals) = ideals[1,]
ideals = ideals[-1,]

#make a lookup table for country codes & remove duplicates based on ideals info
countrycodes <- ideals[,2:3]
countrycodes <- unique(countrycodes[c("ccode", "CountryAbb")])



#Data Plotting Below 
#----------------------------------------------------------------------------------------------------------------

#Plot of # of countries voting vs UN session (see how UN membership has changed)









write.csv(rawvotes, file = "output.csv")
