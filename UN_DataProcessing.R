## Data processing for U.N. project
## 	April 2020

##
##	a) Create a vectors for votes, country id, session id, and vote id
##	b) Create utility variables for number of countries, number of votes, total observations
##	c) ...

## NOTE THAT WE KNOW THIS IS CLUNKY AND SLOW 
## Set parameters for data organization
	## Set values for Path and FileSuffix in Rcpp file


##
## Load and select data with non-missing votes
##	Raw data is in vectorized format: 1= yes; 2=abstain; 3=no; 8=absent
##

## Load full raw data file
Matched	= read.csv(file = paste(Path, "Data\\Matches.csv",sep=""), head=TRUE) 
load(paste(Path,"Data\\UNVotes.RData",sep=""))

	
  ## Keep only columns that we will use 
library(tidyverse)
completeVotes$new<- if_else(completeVotes$year > 1989 & (completeVotes$para==1 | completeVotes$amend==1), 1, 0, 0)
completeVotes <- filter(completeVotes,new==0)
	UNall	= completeVotes[, c("rcid", "ccode", "member", "vote", "Country", "Countryname", "year", "session", "importantvote", "me", "nu", "di", "hr", "co", "ec", "resid", "para", "amend")]   ## was "df" in other versions
	UNall[1:5, ]
	UNall[is.na(UNall$rcid)==1,][1:5,]
	

	## Set single South Africa 1993 (only 1 observation after being out of UN since 1974)
	SouthAfricaCut = as.numeric(UNall$ccode == 560 & UNall$session == 48 & UNall$vote < 4)	## table(SouthAfricaCut)
	UNall = UNall[SouthAfricaCut == 0, ]								## dim(UNall)

	## Cut observations
		## 1) Eliminate absences/not in UN observations 
		## 2) cut observations for session 19 (only 1 vote) 	## table(UNall$rcid[UNall$session == 19])
		## CHECK: from previous data: Cut Zanzibar (ccode == 511) -  only 2 votes and not listed in names.dta ## table(UNall$session[UNall$ccode == 511])
if(DataCode == "All") 		{
     UN	= UNall[which(UNall$vote<4 & UNall$session!=19 & UNall$ccode != 511), ]	}	## which prevents NAs from entering
if(DataCode == "Important") 	{
	UN	= UNall[which(UNall$vote<4 & UNall$session!=19 & UNall$ccode != 511 & UNall$importantvote==1), ]	}	
if(DataCode == "NoNukes") 	{
  	UN	= UNall[which(UNall$vote<4 & UNall$session!=19 & UNall$ccode != 511 & is.na(UNall$nu)==0 & UNall$nu==0),	] }	
if(DataCode == "MiddleEast") 	{		## 29th session onward (could be 22nd onward?)
	UN	= UNall[which(UNall$vote<4 & UNall$session!=19 & UNall$ccode != 511 & is.na(UNall$me)==0 & UNall$me==1 & UNall$session> 28),	] }	
if(DataCode == "HumanRights") 	{	## 25th session onward
	UN	= UNall[which(UNall$vote<4 & UNall$session!=19 & UNall$ccode != 511 & is.na(UNall$hr)==0 & UNall$hr==1 & UNall$session> 24 & UNall$ccode != 713),	] }	
	## For HumanRights: remove Taiwan (ccode = 713) b/c only one year of data
if(DataCode == "Nuclear") 	{	## 33rd  session onward (could be 22nd or 29th onward?)
	UN	= UNall[which(UNall$vote<4 & UNall$session!=19 & UNall$ccode != 511 & is.na(UNall$nu)==0 & UNall$nu==1 & UNall$session> 32 & UNall$ccode != 626),	] }	
	## For nuclear: remove South Sudan (ccode = 626) b/c only one year of data
if(DataCode == "Economic") 	{	## 26th session onward
	UN	= UNall[which(UNall$vote<4 & UNall$session!=19 & UNall$ccode != 511 & is.na(UNall$ec)==0 & UNall$ec==1 & UNall$session> 25),	] }	
if(DataCode == "Colonial") 	{	## 14th session onward (could be from 1st session onward)
	UN	= UNall[which(UNall$vote<4 & UNall$session!=19 & UNall$ccode != 511 & is.na(UNall$co)==0 & UNall$co==1 & UNall$session> 13),	] }	
if(DataCode == "Disarmament") 	{	## 29th session onward
	UN	= UNall[which(UNall$vote<4 & UNall$session!=19 & UNall$ccode != 511 & is.na(UNall$di)==0 & UNall$co==1 & UNall$session> 28),	] }	
#rm(UNall)		 	dim(UN)				## CHECK: dim(UN); names(UN); table(UN$session);	sum(is.na(UN$session))



##
## Matched resolutions - set all matched resolutions to common vote ID number
##


Matched 	= Matched[order(Matched$rcid2),]
NMatch	= length(Matched$rcid1)
UN$rcidL	= UN$rcid		## Creates a new roll call ID variable (where "L" is for "linked")

## Two issues
## a) Some matches are snakes (e.g. 470 matched to 512 which is matched to 545 which is matched to 577)
##	For these we start at highest value (577) and set to 545, then set 545 to 512 and then set 512 to 470
## 	Also, we sort by Matched$rcid2 to that we always set to earlier for 
## b) Some matches are multiples (e.g., 5310 matched to 5389 and 5312 matched to 5389) [** Note: will double check these * ]
##	For these, we set to minimum value associated with that and set rcidL to that
for (rr in 0:(NMatch-1)) 		UN$rcidL[UN$rcidL==Matched$rcid2[NMatch-rr]] = min(  Matched$rcid1[Matched$rcid2 == Matched$rcid2[NMatch-rr]]	)
	## For each rcid2, find minimum of rcid1 associated with that and set rcidL to that
	## ORIG for (rr in 0:(NMatch-1)) 		UN$rcidL[UN$rcidL==Matched$rcid2[NMatch-rr]] = Matched$rcid1[NMatch-rr]


##
## Purge data set of unanimous votes and country-years with no variation
##

VoteList 		= sort(unique(UN$rcidL))
TTlink 		= length(VoteList )
VoteTally		= cbind(VoteList , matrix(NA, TTlink, 4))
for(tt in 1:TTlink){
	if(tt %% 300 ==0)  print(tt)
	VoteTally[tt, 2] = length(UN$vote[UN$rcidL==VoteList[tt] & UN$vote==1])
	VoteTally[tt, 3] = length(UN$vote[UN$rcidL==VoteList[tt] & UN$vote==2])
	VoteTally[tt, 4] = length(UN$vote[UN$rcidL==VoteList[tt] & UN$vote==3])
	}	# END for(ii in 1:TTlink){
VoteTally[, 5]	= apply(VoteTally[, 2:4], 1, sum)
VotePct 		= VoteTally[, 2:4]/ VoteTally[,5]
UnamVoteList	= VoteTally[VotePct[,1]==1.0 | VotePct[,2]==1.0 | VotePct[,3]==1.0, 1]
CutCode 		= 0*UN$vote
for (cc in 1:length(UnamVoteList))	CutCode[UN$rcidL==UnamVoteList[cc]]= 1

UN2			= UN[CutCode==0,]	## Cut unanimous votes
UN2$CSession 	= round(UN2$ccode + UN2$session/100, 2)
AllData 		= data.frame(UN2$CSession, UN2$ccode, UN2$session, UN2$rcid, UN2$rcidL, UN2$vote)	#, UN2$voetenshortcode
AllData 		= AllData[order(UN2$rcidL, UN2$ccode, UN2$session), ]			## Sort data by (linked) roll call number (rcidL)

write(t(AllData), file = paste(Path, "Output\\AllData_", FileSuffix, ".txt",sep=""), ncol=length(AllData[1,]))
	CSession	= AllData[,1];	Country	= AllData[,2]
	Session	= AllData[,3];	VoteID	= AllData[,5]
	yObs		= AllData[,6]		

## Re-calculate VoteTally for data after cutting unanimous votes and session 19
	VoteList 		= sort(unique(UN2$rcidL))
	TTlink 		= length(VoteList)
	VoteTally		= cbind(VoteList , matrix(NA, TTlink, 4))
	for(tt in 1:TTlink){
		if(tt %% 300 ==0)  print(tt)
		VoteTally[tt, 2] = length(UN2$vote[UN2$rcidL==VoteList[tt] & UN2$vote==1])
		VoteTally[tt, 3] = length(UN2$vote[UN2$rcidL==VoteList[tt] & UN2$vote==2])
		VoteTally[tt, 4] = length(UN2$vote[UN2$rcidL==VoteList[tt] & UN2$vote==3])
		}	# END for(ii in 1:TTlink){
	VoteTally[, 5]	= apply(VoteTally[, 2:4], 1, sum)
	VotePct 		= VoteTally[, 2:4]/ VoteTally[,5]
	MatchedDum 		= VoteList %in% Matched$rcid1
	VoteTally		= cbind(VoteTally, MatchedDum)
	write(t(VoteTally), 	file = paste(Path, "Output\\VoteTally_", FileSuffix, ".txt",sep=""), 	ncol=length(VoteTally[1,]))

	write(VoteList, 		file = paste(Path, "Output\\VoteList_", FileSuffix, ".txt",sep=""), ncol = 1 	)
## CHECK dim(VoteTally)
## CHECK VoteTally[1:4,]


##
## Save vectors with number of votes per country-year and number of countries per vote
##
IndN 		= as.vector(table(CSession))	## Number of votes by each country (if 0, need to delete)
VoteN		= as.vector(table(VoteID))	  ## Number of countries for each vote 
write(t(IndN),  file = paste(Path, "Output\\IndN_",  FileSuffix, ".txt",sep=""), ncol=1)
write(t(VoteN), file = paste(Path, "Output\\VoteN_", FileSuffix, ".txt",sep=""), ncol=1)
	## CHECK: TEMP = cbind(IndN, CountryList, SessionList) TEMP[CountryList > 254 & CountryList <261, ]

##
## Starting and ending rows for each vote
##	This speeds up computation so that do not have to find observations for each vote in each iteration
##
ObsN		= length(CSession)	## Length of any variable in voting data is fine
ObsID		= 1:ObsN
StartVote	= rep(NA, TTlink)
EndVote	= rep(NA, TTlink)
for (tt in 1:TTlink)	{			## This is possible b/c sorted AllData by (linked) roll call number (rcidL) above
	StartVote[tt] 	= min(ObsID[VoteID == VoteList[tt]])		
	EndVote[tt]		= max(ObsID[VoteID == VoteList[tt]])		}
VoteStartEnd 		= cbind(StartVote, EndVote)
write(t(VoteStartEnd), file = paste(Path, "Output\\VoteStartEnd_", FileSuffix, ".txt",sep=""), ncol=length(VoteStartEnd[1,]))

## DIAGNOSTIC: Indicates that the number of votes per roll call is same as number implied by starting and ending observations of vote (true if properly sorted)
	mean(EndVote - StartVote + 1 == VoteN)		# Should equal 1

##
## Create a matrix that indicates observation numbers for all observations with each country-session combo
##	This speeds up computation so that do not have to find observations for country-year in each iteration
##
CountrySessionList= sort(unique(CSession))
NN			= length(CountrySessionList)
IObsMat 		= matrix(NA, NN, max(IndN))				# Matrix w/ columns equal to maximum number of votes for individual country
for (ii in 1:NN) IObsMat[ii, 1:IndN[ii]] = ObsID[CSession == CountrySessionList[ii]]

write(t(IObsMat), file = paste(Path, "Output\\IObsMat_", FileSuffix, ".txt",sep=""), ncol=length(IObsMat[1,]))

##
## Create vectors listing countries and sessions for country-year combos
##

CountryList 	= floor(CountrySessionList)
SessionList 	= round(100*(CountrySessionList - floor(CountrySessionList)), 2)
write(t(CountryList), file = paste(Path, "Output\\CountryList_", FileSuffix, ".txt",sep=""), ncol=1)
write(t(SessionList), file = paste(Path, "Output\\SessionList_", FileSuffix, ".txt",sep=""), ncol=1)

## DIAGNOSTIC FOR "ALL"- should be all be true
	#Country[IObsMat [136, 1:VoteN[136]]] == floor(CountrySessionList[136])
	#Session[IObsMat [136, 1:VoteN[136]]] == SessionList[136]

##
## Priors across time 
##	- depend on data in year t relative to year before
##	- need to identify countries with gap year in voting data
##

UniqueCountry 	= unique(Country)
MinCountryYear	= rep(NA, length(UniqueCountry))
SmoothVector	= rep(1, length(IndN))
for (cc in 1:length(UniqueCountry)) MinCountryYear[cc] = min(Session[Country==UniqueCountry[cc]])

for (nn in 1:length(IndN)) {		# The weight of smoothing depends on how much data in current year versus previous year (lower values mean more smoothing (weight on previous year theta is higher))
	if(length(UN2$ccode[UN2$ccode==CountryList[nn] & UN2$session== SessionList[nn]-1]) > 0)
		SmoothVector[nn] 		= IndN[nn] / 
						 (IndN[nn] + length(UN2$ccode[UN2$ccode==CountryList[nn] & UN2$session== SessionList[nn]-1])		)
	if(SessionList[nn] == MinCountryYear[match(CountryList[nn], UniqueCountry)]) 
		SmoothVector[nn] 		= IndN[nn] / 
						 (IndN[nn] + length(UN2$ccode[UN2$ccode==CountryList[nn] & UN2$session== min(SessionList[CountryList==CountryList[nn] & SessionList>SessionList[nn]])])		)
		}

GapYear = (SmoothVector==1)		# Identify countries that have gaps in years with votes
for (nn in 1:length(IndN)) {		
	if(GapYear[nn] ==1)
		SmoothVector[nn] = IndN[nn] / 
					(IndN[nn] + length(UN2$ccode[UN2$ccode==CountryList[nn] & UN2$session== max(UN2$session[UN2$ccode==CountryList[nn] & UN2$session < SessionList[nn]])]	) )}

write(t(SmoothVector), 	file = paste(Path, "Output\\SmoothVector_", FileSuffix, ".txt",sep=""), ncol=1)
write(t(GapYear), 	file = paste(Path, "Output\\GapYear_", FileSuffix, ".txt",sep=""), ncol=1)


##
## Generate starting values for gamma (vote cutpoint) parameters
##   Do this once for each data set
##
	USVote	= rep(NA, TTlink); 
	for (tt in 1:TTlink) if (length(AllData[AllData[,2]==2   & AllData[,5]==VoteList[tt],6]) > 0) USVote[tt] 		= AllData[AllData[,2]==2   & AllData[,5]==VoteList[tt],6]

	UKVote	= rep(NA, TTlink); 
	for (tt in 1:TTlink) if (length(AllData[AllData[,2]==200 & AllData[,5]==VoteList[tt],6]) > 0) UKVote[tt] 		= AllData[AllData[,2]==200 & AllData[,5]==VoteList[tt],6]

	USUK		= cbind(USVote, UKVote)				## DIAGNOSTIC: VoteSum	= cbind(VoteTally, USVote, UKVote, IsraelVote)
	RussiaVote	= rep(NA, TTlink); 
	for (tt in 1:TTlink) if (length(AllData[AllData[,2]==365 & AllData[,5]==VoteList[tt],6]) > 0) RussiaVote[tt] 	= AllData[AllData[,2]==365 & AllData[,5]==VoteList[tt],6]
	USUKRussia		= cbind(USVote, UKVote, RussiaVote)			## cor(USUKRussia, , use = "pairwise.complete.obs")

	write(t(USUKRussia), 	file = paste(Path, "Output\\USUKRussia_", FileSuffix, ".txt",sep=""), 	ncol=length(USUKRussia[1,]))

	
	
	## Generate starting values for beta, which is polarity of vote (1 means US & UK more likely to vote no; -1 means US & UK more likely to vote yes)
	## Original	Beta 					= 0 -1* (apply(USUK, 1, mean, na.rm=T)<2.0) + (apply(USUK, 1, mean, na.rm=T)>2.0)
			Beta 					= 0 -1* (apply(USUK, 1, mean, na.rm=T)==1)  + (apply(USUK, 1, mean, na.rm=T)>2.0)
	Beta[is.na(Beta)==1]		= 0
	#Beta[USVote==RussiaVote]	= 0		## This is new -- needed for nuclear issue area where US and Soviet Union voted together more often - but not sufficient to get on -3/+3 scale that rest of results are on.
								## cbind(USUKRussia, Beta, Beta2, VoteTally)
		#write(t(Beta), 	file = paste(Path, "Output\\Beta_",  FileSuffix, ".txt",sep=""), 	ncol=1)

## VoteCode indicates whether we observed votes 
	## VoteCode = 1 if observed votes in all three categories (Yea/Abstain/Nay)
	## VoteCode = 2 if observed votes in Yea/Abstain
	## VoteCode = 3 if observed votes in Abstain/Nay
	## VoteCode = 4 if observed votes in Yea/Nay
VoteCode			= rep(0, TTlink)
	for (vvv in 1:TTlink) {
		if(VoteTally[vvv , 2]> 0 & VoteTally[vvv , 3]> 0 & VoteTally[vvv , 4]>  0) VoteCode[vvv] = 1
		if(VoteTally[vvv , 2]> 0 & VoteTally[vvv , 3]> 0 & VoteTally[vvv , 4]== 0) VoteCode[vvv] = 2
		if(VoteTally[vvv , 2]==0 & VoteTally[vvv , 3]> 0 & VoteTally[vvv , 4]>  0) VoteCode[vvv] = 3
		if(VoteTally[vvv , 2]> 0 & VoteTally[vvv , 3]==0 & VoteTally[vvv , 4]>  0) VoteCode[vvv] = 4
		} 	# END for (vvv in 1:TTlink) {			## CHECK: cbind(VoteCode, VoteTally)
	write(t(VoteCode), 	file = paste(Path, "Output\\VoteCode_",  FileSuffix, ".txt",sep=""), 	ncol=1)


## gStart is matrix of starting values for vote cutpoints
	## USE PREVIOUS VERSION FOR NOW
	## use simple perentage on -1 to 1 scale.  If 80% of observations =1, then 80 of that area is less than 0.6 (etc)
	gStart 	= cbind(rep(-Inf, TTlink), -1 + 2* VotePct[,1], -1 + 2*(VotePct[,1] + VotePct[,2]), rep(Inf, TTlink))  
	write(t(gStart), 	file = paste(Path, "Output\\gStart_", FileSuffix, ".txt",sep=""), 	ncol=length(gStart[1,]))	## CHECK: cbind(gStart, VoteCode, VoteTally)

NewGammaStart 	=  0
if(NewGammaStart 	== 1){
## New approach to gamma start values
## Step 1) 	Use previous ideal points from "all" votes
	ThetaSummaryAll		= read.table(file = paste(Path, "Output\\ThetaSummaryRW_July2015.txt", sep=""))		## Manually identify most recent theta results
		names(ThetaSummaryAll)[1:4] = c("Country", "Session", "VoteN", "ThetaEst")

	## Convert to vector of thetas for each vote (ThetaVector)
	Theta = ThetaSummaryAll$ThetaEst
	sourceCpp(paste(Path, "RcppFunctions\\PopulateThetaVector.cpp", 	sep="") )
	ThetaVector = PopulateThetaVector(IObsMat, Theta, IndN, length(UN2[,1]))	

## Step 2) 	Find cutpoints implied by min/max theta's in ranges.  
##		Example: Beta>0 then gamma1 is between min(\theta|y=2) and max(\theta|y=1) (am using 25th and 75th percentiles at this point)
LB11 = rep(-4, TTlink)
LB12 = LB11
LB21 = rep(4, TTlink)
LB22 = LB21


for(tt in 1:TTlink) {
		#CHECK vv = RClist[tt] 	#print(c(tt, vv, Beta[tt], VoteCode[tt]))
Theta.temp 	= ThetaVector[VoteStartEnd[tt,1]:VoteStartEnd[tt,2]]
Y.temp 	= yObs       [VoteStartEnd[tt,1]:VoteStartEnd[tt,2]]
BetaEst  	= mean(Theta.temp [Y.temp == max(Y.temp)])- mean(Theta.temp [Y.temp == min(Y.temp)])
if(BetaEst > 0) {
	if(VoteCode[tt]==1)	{
		LB11[tt] = quantile(Theta.temp [Y.temp ==1], 0.75)
		LB12[tt] = quantile(Theta.temp [Y.temp ==2], 0.25)
		LB21[tt] = quantile(Theta.temp [Y.temp ==2], 0.75)
		LB22[tt] = quantile(Theta.temp [Y.temp ==3], 0.25)		}
	if(VoteCode[tt]==2)	{
		LB11[tt] = quantile(Theta.temp [Y.temp ==1], 0.75)
		LB12[tt] = quantile(Theta.temp [Y.temp ==2], 0.25)		}
	if(VoteCode[tt]==3)	{
		LB21[tt] = quantile(Theta.temp [Y.temp ==2], 0.75)
		LB22[tt] = quantile(Theta.temp [Y.temp ==3], 0.25)		}
	if(VoteCode[tt]==4)	{
		LB11[tt] = quantile(Theta.temp [Y.temp ==1], 0.75)
		LB12[tt] = (LB11[tt]+ quantile(Theta.temp [Y.temp ==3], 0.25))/2
		LB21[tt] = LB12[tt]
		LB22[tt] = quantile(Theta.temp [Y.temp ==3], 0.25)		}	}
if(BetaEst<0) {
	if(VoteCode[tt]==1)	{
		LB11[tt] = quantile(Theta.temp [Y.temp ==3], 0.75)
		LB12[tt] = quantile(Theta.temp [Y.temp ==2], 0.25)
		LB21[tt] = quantile(Theta.temp [Y.temp ==2], 0.75)
		LB22[tt] = quantile(Theta.temp [Y.temp ==1], 0.25)		}
	if(VoteCode[tt]==2)	{
		LB11[tt] = quantile(Theta.temp [Y.temp ==2], 0.75)
		LB12[tt] = quantile(Theta.temp [Y.temp ==1], 0.25)		}
	if(VoteCode[tt]==3)	{
		LB11[tt] = quantile(Theta.temp [Y.temp ==3], 0.75)
		LB12[tt] = quantile(Theta.temp [Y.temp ==2], 0.25)		}
	if(VoteCode[tt]==4)	{
		LB11[tt] = quantile(Theta.temp [Y.temp ==3], 0.75)
		LB12[tt] = (LB11[tt]+ quantile(Theta.temp [Y.temp ==1], 0.25))/2
		LB21[tt] = LB12[tt]
		LB22[tt] = quantile(Theta.temp [Y.temp ==1], 0.25)		}	}
} ## END for(tt in 1:TTlink) {

## CHECK: tempLB = cbind(LB11, LB12, LB21, LB22)
xCANDGamma1 = apply(cbind(LB11, LB12), 1, min) + runif(TTlink)*abs(LB12 - LB11)
xCANDGamma2 = apply(cbind(LB21, LB22), 1, min) + runif(TTlink)*abs(LB22 - LB21)

for(tt in 1:TTlink) {	## Ensure gamma1 < gamma2 by flipping as needed (around 200 votes or so, typically low beta)
	if(xCANDGamma1[tt] > xCANDGamma2[tt]) {
		tempHold 	= xCANDGamma2[tt]
		xCANDGamma2[tt] = xCANDGamma1[tt]
		xCANDGamma1[tt] = tempHold	}	}	# END for(tt in 1:TT)
	gStart 	= cbind(rep(-Inf, TTlink), xCANDGamma1, xCANDGamma2, rep(Inf, TTlink))  
	write(t(gStart), 	file = paste(Path, "Output\\gStart_", FileSuffix, ".txt",sep=""), 	ncol=length(gStart[1,]))	## CHECK: cbind(gStart, VoteCode, VoteTally)

} ## END if(NewGammaStart == 1){

##
## US, Russia (etc) agreement scores plus matrix ("USUKRussiaBrazilChinaIndiaIsrael") with votes of selected countries
##   These are not used in estimation process, but can be useful for diagnostics
##
if(CreateAgreeMatrix ==1) {
	
 
  
  CSList 		= sort(unique(CSession))
	PctUSAgree 		= PctRUSSAgree 	= PctBrazilAgree 	= PctChinaAgree 	= PctIndiaAgree 	= PctIsraelAgree 	= PctCanadaAgree = rep(NA, NN)
	yobCountMatrix 	= matrix(NA, NN, 3)

	for (jjj in 1:NN){
	# Votes on which Country.Session has voted
		tempVoteID 	= VoteID[CSession == CSList[jjj]] 
		tempVoteObs 	= yObs[CSession == CSList[jjj]] 
	# Votes in Session on which US has voted
		UStempV 	= VoteID[	Country == 2  	&	Session == SessionList[jjj]]
		UStempYY 	= yObs  [	Country == 2  	&	Session == SessionList[jjj]]
	# Votes in Session on which Russia has voted
		RUSStempV 	= VoteID[	Country == 365  	&	Session == SessionList[jjj]]
		RUSStempYY 	= yObs  [	Country == 365  	&	Session == SessionList[jjj]]
	# Votes in Session on which Brazil has voted
		BraziltempV 	= VoteID[	Country == 140  	&	Session == SessionList[jjj]]
		BraziltempYY 	= yObs  [	Country == 140  	&	Session == SessionList[jjj]]
	# Votes in Session on which China has voted
		ChinatempV 		= VoteID[	Country == 710  	&	Session == SessionList[jjj]]
		ChinatempYY 	= yObs  [	Country == 710  	&	Session == SessionList[jjj]]
	# Votes in Session on which India has voted
		IndiatempV 		= VoteID[	Country == 750  	&	Session == SessionList[jjj]]
		IndiatempYY 	= yObs  [	Country == 750  	&	Session == SessionList[jjj]]
	# Votes in Session on which Israel has voted
		IsraeltempV 	= VoteID[	Country == 666  	&	Session == SessionList[jjj]]
		IsraeltempYY 	= yObs  [	Country == 666  	&	Session == SessionList[jjj]]
	# Votes in Session on which Canada has voted
		CanadatempV 	= VoteID[	Country == 20  	&	Session == SessionList[jjj]]
		CanadatempYY 	= yObs  [	Country == 20  	&	Session == SessionList[jjj]]

	# Votes on which both US/Russia/etc and Country has voted in Session
	CTempVotesUS 	= intersect(tempVoteID , UStempV)
	CTempVotesRUSS 	= intersect(tempVoteID , RUSStempV)
	CTempVotesBrazil 	= intersect(tempVoteID , BraziltempV)
	CTempVotesChina 	= intersect(tempVoteID , ChinatempV)
	CTempVotesIndia 	= intersect(tempVoteID , IndiatempV)
	CTempVotesIsrael 	= intersect(tempVoteID , IsraeltempV)
	CTempVotesCanada 	= intersect(tempVoteID , CanadatempV)

	PctUSAgree[jjj] 	= sum(tempVoteObs[match(CTempVotesUS, 		tempVoteID)] == UStempYY[ 		match(CTempVotesUS, UStempV)])   / 			length(CTempVotesUS )
	PctRUSSAgree[jjj] 	= sum(tempVoteObs[match(CTempVotesRUSS, 	tempVoteID)] == RUSStempYY[ 	match(CTempVotesRUSS, RUSStempV)]) /		length(CTempVotesRUSS )
	PctChinaAgree[jjj] 	= sum(tempVoteObs[match(CTempVotesChina,	tempVoteID)] == ChinatempYY[ 	match(CTempVotesChina, ChinatempV)]) / 	length(CTempVotesChina )
	PctBrazilAgree[jjj] = sum(tempVoteObs[match(CTempVotesBrazil,	tempVoteID)] == BraziltempYY[ 	match(CTempVotesBrazil, BraziltempV)]) / 	length(CTempVotesBrazil )
	PctIndiaAgree[jjj] 	= sum(tempVoteObs[match(CTempVotesIndia, 	tempVoteID)] == IndiatempYY[ 	match(CTempVotesIndia, IndiatempV)]) /		length(CTempVotesIndia )
	PctIsraelAgree[jjj] = sum(tempVoteObs[match(CTempVotesIsrael,	tempVoteID)] == IsraeltempYY[ 	match(CTempVotesIsrael, IsraeltempV)]) /	length(CTempVotesIsrael )
	PctCanadaAgree[jjj] = sum(tempVoteObs[match(CTempVotesCanada,	tempVoteID)] == CanadatempYY[ 	match(CTempVotesCanada, CanadatempV)]) /	length(CTempVotesCanada )

	yobCountMatrix[jjj, 1] 	= length(yObs[yObs==1 & CSession == CSList[jjj]] )
	yobCountMatrix[jjj, 2] 	= length(yObs[yObs==2 & CSession == CSList[jjj]] )
	yobCountMatrix[jjj, 3] 	= length(yObs[yObs==3 & CSession == CSList[jjj]] )

	if (jjj %% 50 == 0) print(jjj)
	}

	AgreeMatrix 			= cbind(1:NN, CSList , PctUSAgree, PctRUSSAgree, PctBrazilAgree, PctChinaAgree,  PctIndiaAgree, PctIsraelAgree, yobCountMatrix)
	write(t(AgreeMatrix), 	file = paste(Path, "Output\\AgreeMatrix_", FileSuffix, ".txt",sep=""), ncol=length(AgreeMatrix[1,]))


	IsraelVote	= rep(NA, TTlink); 
	for (tt in 1:TTlink) if (length(AllData[AllData[,2]==666 & AllData[,5]==VoteList[tt],6]) > 0) IsraelVote[tt] 	= AllData[AllData[,2]==666  & AllData[,5]==VoteList[tt],6]

	BrazilVote	= rep(NA, TTlink); 
	for (tt in 1:TTlink) if (length(AllData[AllData[,2]==140 & AllData[,5]==VoteList[tt],6]) > 0) BrazilVote[tt] 	= AllData[AllData[,2]==140  & AllData[,5]==VoteList[tt],6]

	ChinaVote	= rep(NA, TTlink); 
	for (tt in 1:TTlink) if (length(AllData[AllData[,2]==710 & AllData[,5]==VoteList[tt],6]) > 0) ChinaVote[tt] 	= AllData[AllData[,2]==710  & AllData[,5]==VoteList[tt],6]

	RussiaVote	= rep(NA, TTlink); 
	for (tt in 1:TTlink) if (length(AllData[AllData[,2]==365 & AllData[,5]==VoteList[tt],6]) > 0) RussiaVote[tt] 	= AllData[AllData[,2]==365  & AllData[,5]==VoteList[tt],6]

	IndiaVote	= rep(NA, TTlink); 
	for (tt in 1:TTlink) if (length(AllData[AllData[,2]==750 & AllData[,5]==VoteList[tt],6]) > 0) IndiaVote[tt] 	= AllData[AllData[,2]==750  & AllData[,5]==VoteList[tt],6]

	USUKRussiaBrazilChinaIndiaIsrael		= cbind(USVote, UKVote, RussiaVote, BrazilVote, ChinaVote, IsraelVote, IndiaVote)				## DIAGNOSTIC: VoteSum	= cbind(VoteTally, USVote, UKVote, IsraelVote)
	write(t(USUKRussiaBrazilChinaIndiaIsrael), 	file = paste(Path, "Output\\USUKRussiaBrazilChinaIndiaIsrael_", FileSuffix, ".txt",sep=""), 	ncol=length(USUKRussiaBrazilChinaIndiaIsrael[1,]))

} ## END  if(CreateAgreeMatrix ==1)
