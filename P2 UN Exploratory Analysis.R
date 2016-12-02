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
