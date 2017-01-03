#load packages
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))

str(unvotes)

#explore dataset
#let's see how many of the rows have a category assigned to them:
temp <- unvotes %>%  mutate(sumcat = me+nu+di+hr+co+ec+ps+af+sc+sp+un+int+bu+pc)
temp1 <- subset(temp, sumcat == 0)
#33,035 of 708,051 rows are uncategorized (4.6%), but we still have ideal points and other measures to judge similarity 


#we want to add in columns to inidcate if teh country voted in favor,against, or asbtained on a vote of a 
#particular category. We can quantify this by multiplying the category columns by the vote weight
#i.e. in favor = 1, against = -1, and abstain = 0. Based on the average for each vote, we can see if a 
#country tends to vote in favor or against a particular vote category (on average)
temp1 <- temp %>% mutate(w.me = me*v.weight)
temp1 <- temp1 %>% mutate(w.nu = nu*v.weight)
temp1 <- temp1 %>% mutate(w.di = di*v.weight)
temp1 <- temp1 %>% mutate(w.hr = hr*v.weight)
temp1 <- temp1 %>% mutate(w.co = co*v.weight)
temp1 <- temp1 %>% mutate(w.ec = ec*v.weight)
temp1 <- temp1 %>% mutate(w.ps = ps*v.weight)
temp1 <- temp1 %>% mutate(w.af = af*v.weight)
temp1 <- temp1 %>% mutate(w.sc = sc*v.weight)
temp1 <- temp1 %>% mutate(w.sp = sp*v.weight)
temp1 <- temp1 %>% mutate(w.un = un*v.weight)
temp1 <- temp1 %>% mutate(w.int = int*v.weight)
temp1 <- temp1 %>% mutate(w.bu = bu*v.weight)
temp1 <- temp1 %>% mutate(w.pc = pc*v.weight)


unmaster <- temp1
votecluster <- temp1[,c(1:16,31:52,57:70,56)]


str(votecluster)

#each rcid represents a vote on one topic. Rcid's repeat because multiple countries vote on a single topic. 
example <- subset(votecluster, session == 1)


mean(subset(votecluster, session == 30 & ccode == 2)$w.sp) #0.1428571

#CREATE THE MATRICES TO STORE THE SESSION & CCODE INFO FOR EAHC OF THE CATEGORY VALUES
#-----------------------------------------------------------------------------------------------------------------------
#so the challenge is to take the average of each measure, for each session, for each country. Sounds like
#a for loop

#first we need to figure out how many sessions there are:
lsessions<-length(unique(votecluster$session)) #output = 67 - now we need to store these 67 values in a vector
sessionslist <- unique(votecluster$session)
slist <- as.numeric(levels(sessionslist))[sessionslist]
slist <- sort(slist)

#then figure out how many ccodes there are for the session
lccodes<-length(unique(votecluster$ccode)) #output = 199 - now we need to store these 199 values in a vector
ccodeslist <- unique(votecluster$ccode)
cclist <- as.numeric(levels(ccodeslist))[ccodeslist]
cclist <- sort(cclist)



#MATRIX FOR SP
x = matrix(NA,67,199)
for(i in 1:67){
  for(j in 1:199){
    x[i,j] = mean(subset(votecluster, session == slist[i] & ccode == cclist[j])$w.sp)
  }
}

sp <- x

spmat <- as.data.frame(as.table(sp))
temp <- c("session", "ccode", "sp")
colnames(spmat) <- paste(temp)

#MATRIX FOR ME
me = matrix(NA,67,199)
for(i in 1:67){
  for(j in 1:199){
    me[i,j] = mean(subset(votecluster, session == slist[i] & ccode == cclist[j])$w.me)
  }
}

colnames(me) <- paste(cclist)
colnames(me)
rownames(me) <- paste(slist)
rownames(me)

memat <- as.data.frame(as.table(me))
temp <- c("session", "ccode", "me")
colnames(memat) <- paste(temp)

#MATRIX FOR nu
nu = matrix(NA,67,199)
for(i in 1:67){
  for(j in 1:199){
    nu[i,j] = mean(subset(votecluster, session == slist[i] & ccode == cclist[j])$w.nu)
  }
}
colnames(nu) <- paste(cclist)
rownames(nu) <- paste(slist)

numat <- as.data.frame(as.table(nu))
temp <- c("session", "ccode", "nu")
colnames(numat) <- paste(temp)


#MATRIX FOR di
di = matrix(NA,67,199)
for(i in 1:67){
  for(j in 1:199){
    di[i,j] = mean(subset(votecluster, session == slist[i] & ccode == cclist[j])$w.di)
  }
}
colnames(di) <- paste(cclist)
rownames(di) <- paste(slist)

dimat <- as.data.frame(as.table(di))
temp <- c("session", "ccode", "di")
colnames(dimat) <- paste(temp)


#MATRIX FOR hr
hr = matrix(NA,67,199)
for(i in 1:67){
  for(j in 1:199){
    hr[i,j] = mean(subset(votecluster, session == slist[i] & ccode == cclist[j])$w.hr)
  }
}
colnames(hr) <- paste(cclist)
rownames(hr) <- paste(slist)


hrmat <- as.data.frame(as.table(hr))
temp <- c("session", "ccode", "hr")
colnames(hrmat) <- paste(temp)

#MATRIX FOR co
co = matrix(NA,67,199)
for(i in 1:67){
  for(j in 1:199){
    co[i,j] = mean(subset(votecluster, session == slist[i] & ccode == cclist[j])$w.co)
  }
}
colnames(co) <- paste(cclist)
rownames(co) <- paste(slist)

comat <- as.data.frame(as.table(co))
temp <- c("session", "ccode", "co")
colnames(comat) <- paste(temp)


#MATRIX FOR ec
ec = matrix(NA,67,199)
for(i in 1:67){
  for(j in 1:199){
    ec[i,j] = mean(subset(votecluster, session == slist[i] & ccode == cclist[j])$w.ec)
  }
}
colnames(ec) <- paste(cclist)
rownames(ec) <- paste(slist)

ecmat <- as.data.frame(as.table(ec))
temp <- c("session", "ccode", "ec")
colnames(ecmat) <- paste(temp)

#MATRIX FOR ps
ps = matrix(NA,67,199)
for(i in 1:67){
  for(j in 1:199){
    ps[i,j] = mean(subset(votecluster, session == slist[i] & ccode == cclist[j])$w.ps)
  }
}
colnames(ps) <- paste(cclist)
rownames(ps) <- paste(slist)

psmat <- as.data.frame(as.table(ps))
temp <- c("session", "ccode", "ps")
colnames(psmat) <- paste(temp)

#MATRIX FOR af
af = matrix(NA,67,199)
for(i in 1:67){
  for(j in 1:199){
    af[i,j] = mean(subset(votecluster, session == slist[i] & ccode == cclist[j])$w.af)
  }
}
colnames(af) <- paste(cclist)
rownames(af) <- paste(slist)

afmat <- as.data.frame(as.table(af))
temp <- c("session", "ccode", "af")
colnames(afmat) <- paste(temp)


#MATRIX FOR sc
sc = matrix(NA,67,199)
for(i in 1:67){
  for(j in 1:199){
    sc[i,j] = mean(subset(votecluster, session == slist[i] & ccode == cclist[j])$w.sc)
  }
}
colnames(sc) <- paste(cclist)
rownames(sc) <- paste(slist)

scmat <- as.data.frame(as.table(sc))
temp <- c("session", "ccode", "sc")
colnames(scmat) <- paste(temp)

#MATRIX FOR int
int = matrix(NA,67,199)
for(i in 1:67){
  for(j in 1:199){
    int[i,j] = mean(subset(votecluster, session == slist[i] & ccode == cclist[j])$w.int)
  }
}
colnames(int) <- paste(cclist)
rownames(int) <- paste(slist)

intmat <- as.data.frame(as.table(int))
temp <- c("session", "ccode", "int")
colnames(intmat) <- paste(temp)


#MATRIX FOR bu
bu = matrix(NA,67,199)
for(i in 1:67){
  for(j in 1:199){
    bu[i,j] = mean(subset(votecluster, session == slist[i] & ccode == cclist[j])$w.bu)
  }
}
colnames(bu) <- paste(cclist)
rownames(bu) <- paste(slist)

bumat <- as.data.frame(as.table(bu))
temp <- c("session", "ccode", "bu")
colnames(bumat) <- paste(temp)

#MATRIX FOR pc
pc = matrix(NA,67,199)
for(i in 1:67){
  for(j in 1:199){
    pc[i,j] = mean(subset(votecluster, session == slist[i] & ccode == cclist[j])$w.pc)
  }
}
colnames(pc) <- paste(cclist)
rownames(pc) <- paste(slist)

pcmat <- as.data.frame(as.table(pc))
temp <- c("session", "ccode", "pc")
colnames(pcmat) <- paste(temp)

#MATRIX FOR un
un = matrix(NA,67,199)
for(i in 1:67){
  for(j in 1:199){
    un[i,j] = mean(subset(votecluster, session == slist[i] & ccode == cclist[j])$w.un)
  }
}
colnames(un) <- paste(cclist)
rownames(un) <- paste(slist)

unmat <- as.data.frame(as.table(un))
temp <- c("session", "ccode", "un")
colnames(unmat) <- paste(temp)


#COMBINE ALL THE MATRICES INTO A SINGLE DATA FRAME RESULTING IN A TABLE OF 67 SESSIONS BY 199 CCODES = 13,333 ROWS  
#------------------------------------------------------------------------------------------------------------------
undata <- merge(memat, numat, all = T)
undata <- merge(undata, dimat, all = T)
undata <- merge(undata, hrmat, all = T)
undata <- merge(undata, comat, all = T)
undata <- merge(undata, ecmat, all = T)
undata <- merge(undata, psmat, all = T)
undata <- merge(undata, afmat, all = T)
undata <- merge(undata, scmat, all = T)
undata <- merge(undata, spmat, all = T)
undata <- merge(undata, unmat, all = T)
undata <- merge(undata, intmat, all = T)
undata <- merge(undata, bumat, all = T)
undata <- merge(undata, pcmat, all = T)

unbindata <- undata

#NOW TO FINALLY ARRIVE AT THE CLUSTER DATASET, WE WILL COMBINE THE BINNED DATA WITH THE OTHER VARIABLES
#-----------------------------------------------------------------------------------------------------------------
#let's start by merging country specific data (i.e. data unique by country)
ccodeinfo <- votecluster[,c("ccode","CountryAbb", "CountryName",
                            "S.AM","N.AM","W.EU","N.EU","E.EU","S.EU",
                            "CAR","W.AF","E.AF","S.AF","S.AS","W.AS",
                            "N.AF","C.AM","SEA","ANZ","E.AS","M.AF",
                            "MEL","POL","MIC","C.AS")]

ccodeinfo <- unique(ccodeinfo[,c("ccode","CountryAbb", "CountryName",
                                 "S.AM","N.AM","W.EU","N.EU","E.EU","S.EU",
                                 "CAR","W.AF","E.AF","S.AF","S.AS","W.AS",
                                 "N.AF","C.AM","SEA","ANZ","E.AS","M.AF",
                                 "MEL","POL","MIC","C.AS")])


temp2 <- merge(x = unbindata, 
               y = ccodeinfo[,c("ccode","CountryAbb", "CountryName",
                                "S.AM","N.AM","W.EU","N.EU","E.EU","S.EU",
                                "CAR","W.AF","E.AF","S.AF","S.AS","W.AS",
                                "N.AF","C.AM","SEA","ANZ","E.AS","M.AF",
                                "MEL","POL","MIC","C.AS")],
               by = "ccode",
               all.x = T)

unbindata <- temp2

#We need to assign a unique identifier for each session/ccode combination in the votecluster data set. We 
#will do this by adding a session.cc ID column in votecluster
temp <- votecluster %>%  mutate(session.cc = paste(votecluster$session, votecluster$ccode, sep = "."))
temp1 <- unbindata %>% mutate(session.cc = paste(unbindata$session, unbindata$ccode, sep = "."))

tempA <- unique(temp[,c("session.cc","Idealpoint")])


tempB <- merge(x = temp1, 
               y = tempA[,c("session.cc","Idealpoint")],
               by = "session.cc",
               all.x = T)

unbindata <- tempB
unbindata <- unbindata[,c(1:3, 18:19, 42, 20:41, 4:17)]

temp <- subset(unbindata, is.na(unbindata$Idealpoint))

#THE FINAL STEP WILL BE TO REMOVE DATAPOINTS WITH NA VALUES. THESE SHOULD BE VALUES WITHOUT IDEALPOINTS BECAUSE THE COUNTRIES 
#WERE NOT MEMBERS FOR THAT PARTICULAR VOTE/SESSION.WHEN WE CREATED THE MATRICES, THE FOR LOOP HAD TO CYCLE THROUGH ALL THE COUNTRIES
#EVEN WHEN THEY WEREN'T IN THE VOTE, WHICH RESULTS IN OUR NA'S.
ven <- setdiff(unbindata, temp)
unbindata <- ven

str(unbindata)


save(unmaster,file="unmaster.Rda")
save(votecluster,file="votecluster.Rda")
save(unbindata,file="unbindata.Rda")
save(ccodeinfo,file="ccodeinfo.Rda")


