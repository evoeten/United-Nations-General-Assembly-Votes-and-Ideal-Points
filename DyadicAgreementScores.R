load(paste(Path,"Data\\UNVotes.RData",sep=""))

## Keep only columns that we will use 
library(tidyverse)
df <- completeVotes %>%   select(vote, ccode, rcid, session)

#Create dyadic data
df <- df %>% expand(ccode1=ccode, ccode2=ccode, rcid) %>%
  filter(ccode1!=ccode2) %>% 
  left_join(., df, by=c("ccode1"="ccode", "rcid")) %>%
  left_join(., df, by=c("ccode2"="ccode", "rcid")) 
df<-  filter(df,vote.x<4) 
df <-  filter(df,vote.y<4)
dfrc <- df %>% group_by(rcid) %>% summarise(distinct=n_distinct(vote.x)) %>% ungroup()
df <- left_join(df,dfrc,by="rcid")
df <- filter(df,distinct>1) ##Throw out unanimous votes
df$Agree <- (1-abs(df$vote.x-df$vote.y)/2)

dfAgree <- df  %>% group_by(session.x, ccode1, ccode2) %>% 
 summarise(agree = mean(Agree, na.rm = TRUE)) %>%
  mutate(year=session.x+1945)
dfIdeal <- read.csv( file=paste(Path, "Output\\Idealpointestimates",FileSuffix, ".csv", sep=""))
dfIdeal <- select(dfIdeal, ccode, session, IdealPoint, NVotes)
dfAgree <- left_join(dfAgree, dfIdeal,by=c("ccode1"="ccode", "session.x"="session" ))
dfAgree <- left_join(dfAgree, dfIdeal,by=c("ccode2"="ccode", "session.x"="session" ))
dfAgree$IdealPointDistance <- abs(dfAgree$IdealPoint.x-dfAgree$IdealPoint.y)
write.csv(dfAgree, file = paste(Path, "Output\\AgreementScores",FileSuffix, ".csv", sep="")) 
save(dfAgree,file = paste(Path, "Output\\AgreementScores",FileSuffix, ".Rdata", sep=""))
