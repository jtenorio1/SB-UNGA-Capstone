#load packages
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(reshape2))
suppressMessages(library(ggplot2))
suppressMessages(library(caTools))
suppressMessages(library(ROCR))


#load data & maintain data structures
#---------------------------------------------------------------------------------------------------------------

#let's check the data structures
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

#countrycodes
countrycodes <- transform(countrycodes,
                          ccode = as.factor(ccode))

#countryinfo
countryinfo <- transform(countryinfo,
                         ccode = as.factor(ccode),
                         Region.Code = as.factor(Region.Code))

#ideals
ideals <- transform(ideals,
                    Year = as.character(Year),
                    ccode = as.factor(ccode),
                    session = as.factor(session),
                    CountrySession = as.character(CountrySession))
ideals <- transform(ideals, Year = as.Date(Year, "%Y"))


#rawvotes
rawvotes <- transform(rawvotes,
                      ccode = as.factor(ccode),
                      session = as.factor(session),
                      rcid = as.factor(rcid),
                      vote = as.factor(vote))

str(rawvotes)
str(ideals)
str(countryinfo)
str(countrycodes)
str(voteinfo)



#Adding & Manipulating Data (column changes)
#----------------------------------------------------------------------------------------------------------------
#voteinfo
#votes are passed by a two thirds majority so we need to add a few columns
#Add column for total number of countries voting
voteinfo <- voteinfo %>% mutate(sum.votes = abstain + yes + no)
#reorder
voteinfo <- voteinfo[, c(1,2,3,4,5,6,7,19,8:ncol(voteinfo))]
#percent abstain, yes, and no
voteinfo <- voteinfo %>% mutate(p.abstain = abstain/sum.votes)
voteinfo <- voteinfo %>% mutate(p.yes = yes/sum.votes)
voteinfo <- voteinfo %>% mutate(p.no = no/sum.votes)
#vote adopt/reject
voteinfo <- voteinfo %>% mutate(vote.result = ifelse(p.yes >= (2/3), 1, 0)) #1 = ADOPT & 0 = REJECT
#reorder
voteinfo <- voteinfo[, c(1,2,3,4,5,6,7,8, 21,22,23,24, 9:ncol(voteinfo))]
#cleanup: drop unnecessary columns
drops <- c(2,24,25,26,27,28)
voteinfo <- voteinfo[,-drops]
voteinfo <- voteinfo[,-1]

write.csv(voteinfo, file = "voteinfo")

#countrycodes
#drop duplicate identifier col
countrycodes <- countrycodes[, -c(1,2)]
write.csv(countrycodes, file = "countrycodes")


#countryinfo
#drop duplicate identifier cols
countryinfo <- countryinfo[, -c(1,2)]
write.csv(countryinfo, file = "countryinfo")

#ideals
#drop duplicate ID cols & n_full col
ideals <- ideals[, -c(1,2,7)]
write.csv(ideals, file = "ideals")

#rawvotes
#drop duplicate ID col
rawvotes <- rawvotes[,-c(1,2)]
write.csv(rawvotes, file = "rawvotes")


#Data Plotting Below 
#----------------------------------------------------------------------------------------------------------------

#Plot of # of countries voting vs UN session (see how UN membership has changed)
ggplot(raw.votes, aes(x = session, y = vote, col = factor(Macro.Region))) +
  geom_jitter()

temp <- subset(raw.votes, stateabb %in% c("USA"))

ggplot(subset(raw.votes, stateabb %in% c("USA")), aes(x = session, y = vote)) +
  geom_jitter()



#Linear regression models Below 
#----------------------------------------------------------------------------------------------------------------
summary(ideals)
model1 = lm(Year ~ ccode, data = ideals)
summary(model1)
model1$residuals
SSE = sum(model1$residuals^2)
model2 = lm(Year ~ ccode + Idealpoint, data = ideals)
summary(model2)
cor(ideals$session, ideals$Idealpoint)
cor(ideals)

#test data....
predictTest = predict(model2, testdata = idealstest) #idealtest is the test file

#create new variable based on arithmetic (e.g. subtraction)
#data$varname = data$var1 - data$vr2


#when do the number of member nations plateau?
#how many "yes" votes does it take for a resolution to pass? 



write.csv(country.master, file = "output.csv")
