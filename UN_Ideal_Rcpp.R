
############################################################
##
## IRT for ordinal data: see Johnson and Albert 1999: Sec. 7.3; Sec. 5.2.3; Sec. 4.3.2 (p. 135) for core M-H/Gibbs sampler
## Ordinal data model using UN data
## Code written by Michael Bailey with modifications by Erik Voeten
##
############################################################
## This code implements the ideal point estimation from: Bailey, Michael A., Anton Strezhnev, and Erik Voeten. "Estimating dynamic state preferences from United Nations voting data." Journal of Conflict Resolution 61, no. 2 (2017): 430-456.
## In addition, this code allows users to choose subsets of issues to focus on.
## Working with this code does presume some familiarity with R.
## And it takes time! We recommend that you start with models that run just a few hundred iterations to see that all is running well and then run the main model overnight.
## 
## Guide to using R code for estimates
## 1) If you are running this code for the first time on a data set
##	a) Set "Continuation" to 0.  If you have already run this code and would like to start where you left off in terms of the burn-in, set "Continuation" to be 1.  This is useful when you want to check results midway or when your estimation run was interrupted (e.g., computer shut down)
##	b) Set value of "Path" for your computer and set value of "FileSuffix" to whatever name you want (typically date and perhaps subset (e.g., "important votes") or perhaps value of smoothing parameter (e.g.,Smooth01"))
##	c) Place the 2 raw data files in folder defined by "Path".  One is the UN voting data ("UNVotes.Rdata").  The second is the file indicating which votes are matched with others (e.g., "Matches.csv")
##	d) Install Rcpp package and include RcppFunctions folder as subfolder of "Path"
## 	e) Set "RunDataOrganizer" to 1.  This will create data files in format needed for this code to run.  This only needs to be done once for each data set.
## 2) Run code based on K (number of iterations), Burn, Thin and Smooth parameters.  PrintSave is for display purposes to monitor progress.
## 3) The main output is "ThetaSummary.txt" in "Path".  This displays ideal points and other values for each country-year.  Other output (such as vote cutpoints) is also produced as described at end of this program.


## Code organization
	require(compiler)
	enableJIT(3)		## Just in time compiler: 0 turns this off; 1 compiles closures before an R script attempts to use them; 2 compiles closures before duplicating; 3 compiles all loops (for, while, repeat). Use 0 or 3.
	ptm <- proc.time()	## Record processor time at beginning of session	

## Initialize parameters
	Continuation	= 0		## 1 if use last values as starting values
	RunDataOrganizer	= 1		## 1 if need to create data files from raw data
	Path			= "~\\GitHub\\United-Nations-General-Assembly-Votes-and-Ideal-Points\\"   ## This indicates a subfolder where the organizing .R code (UN_DataProcessing.R), Rcpp functions and data reside
	DataCode		= "Important"
				## "All"			# For all non-missing data
				## "NoNukes"  		# Cut nuclear related votes				
				## "Important"	# Important votes only (from 38th session onward)
				## "MiddleEast"		# Middle east related votes only (for which UN$me == 1 )  -- note there are around 3100 NAs for this variable (15 votes or so; assume me = 0 for now); limit data to 29th session onward
				## "HumanRights"		# Human rights related votes only (for which UN$hr == 1 ) -- note there are around 3100 NAs for this variable (15 votes or so; assume hr = 0 for now); limit data to 25th session onward
				## "Colonial"		# Colonialism related votes only (for which UN$co == 1 ) -- note there are around 3100 NAs for this variable (15 votes or so; assume hr = 0 for now); limit data to 14th session onward
				## "Nuclear"		# Nuclear related votes only (for which UN$nu == 1 ) -- note there are around 3100 NAs for this variable (15 votes or so; assume hr = 0 for now); limit data to 27th session onward
				## "Economic"		# Economic related votes only (for which UN$ec == 1 ) -- note there are around 3100 NAs for this variable (15 votes or so; assume hr = 0 for now); limit data to 26th session onward
				## "Disarmament"		# Disarmament related votes only (for which UN$di == 1 ) -- note there are around 3100 NAs for this variable (15 votes or so; assume hr = 0 for now); limit data to XX session onward
								# etc - see names(UN) for other issue codes: EconVotes, coVotes, diVotes, NuclearVotes, HumanRightsVotes
								# Issue categories are not (at this point) exclusive
	FileSuffix 		= paste(DataCode, "_Apr2020", sep="")		## Include data and any subsetting information
	K			    = 20000	## Number of MCMC iterations
	Burn			= 4000	## Number of burn in iterations 
	Thin			= 50		## The final estimates based on thin.  If Thin = 20, take only posterior values from every 20th interation.  Minimum = 1
	Smooth		= 0.5		## The smaller this is, the more priors matter. For the main ideal point estimates, the smoother is set at .5. Note that for issue-specific ideal points it may make sense to set this smaller than .5.
	PrintSave		= 50		## Print kk and save every PrintSave iterations
	CreateAgreeMatrix = 0		## Set to 1 if want to create agreement matrix for selected countries; 0 otherwise (VERY SLOW!!); not necessary for MCMC, save time by set to 0. Moreover, you can create the full set of dyadic agreement codes with the specialized code Dyadicagreements,R
	FitStats		= 0		## Set to 1 if want to calculate fit stats (log likelihood, MSE, percent correct)

##
## Load and select data , priors, storage variables
##

## Run organizer if needed to generate data files from raw data
	if(RunDataOrganizer == 1 & Continuation == 0) {
		print("Running data organizer . . . ")
		source(paste(Path, "UN_DataProcessing.R", sep=""))	## The file name is encoded MANUALLY ##
	} ## END if(RunDataOrganizer == 1 & Continuation == 0) 

## Compile Rcpp functions: these functions are stored in RcppFunctions folder
	library(Rcpp)			
	library(RcppZiggurat)		## zrnorm(N)
#install.packages("pkgbuild")
#install.packages("devtools")
#install.packages("rtools")
#library(devtools)
#library(pkgbuild)
#library(rtools)
#find_rtools()
	sourceCpp(paste(Path, "RcppFunctions\\PopulateThetaVector.cpp", 	sep="") )
	sourceCpp(paste(Path, "RcppFunctions\\VarBetaFunction.cpp", 	sep="") )	
	sourceCpp(paste(Path, "RcppFunctions\\BetaMeanFunction.cpp", 	sep="") )		

## Load data
AllData = read.table(file 	= paste(Path, "Output\\AllData_", FileSuffix, ".txt"	, sep="") )
	CSession	= AllData[,1];	Country	= AllData[,2]
	Session	= AllData[,3];	VoteID	= AllData[,5]
	yObs		= AllData[,6];	
	ObsID 	= 1:length(Country)
	ObsN		= length(ObsID)
	RClist 	= sort(unique(VoteID))
VoteStartEnd 	= read.table(file =	  paste(Path, "Output\\VoteStartEnd_", 	FileSuffix, ".txt", 	sep=""))
IObsMat		= as.matrix(read.table(file = paste(Path, "Output\\IObsMat_", 	FileSuffix, ".txt", 	sep="")))
VoteN			= unlist(read.table(file =paste(Path, "Output\\VoteN_", 		FileSuffix, ".txt", 	sep="")))
IndN			= unlist(read.table(file =paste(Path, "Output\\IndN_", 			FileSuffix, ".txt", 	sep="")))
CountryList		= unlist(read.table(file =paste(Path, "Output\\CountryList_", 	FileSuffix, ".txt", 	sep="")))
SessionList		= unlist(read.table(file =paste(Path, "Output\\SessionList_", 	FileSuffix, ".txt", 	sep="")))
SmoothVector	= unlist(read.table(file =paste(Path, "Output\\SmoothVector_", 	FileSuffix, ".txt", 	sep="")))
GapYear		= unlist(read.table(file =paste(Path, "Output\\GapYear_", 		FileSuffix, ".txt", 	sep="")))
VoteCode		= unlist(read.table(file =paste(Path, "Output\\VoteCode_", 		FileSuffix, ".txt", 	sep="")))
VoteList		= read.table(file =paste(Path, "Output\\VoteList_", 		FileSuffix, ".txt", 	sep=""))

NN 			= length(unique(CSession))
TT 			= length(unique(VoteID))
kk			= 0				# iteration counter
Theta 		= rep(NA, NN); 		Beta 			= rep(0, TT)
ThetaMean		= rep(0, NN);		BetaMean		= rep(NA, TT)
ThetaMeanObs 	= 1:length(ThetaMean)
VarTheta		= rep(NA, NN);			VarBeta 	= rep(NA, TT)
ThetaStore		= matrix(NA, K/Thin, NN); 	BetaStore	= matrix(NA, K/Thin, TT); 	Gamma1Store	= BetaStore; 	Gamma2Store = BetaStore

## Priors ((Johnson & Albert p. 223: 	# A ~N(0, 1); epsilonIJ ~N(0, SigmaT);  PrSigmaTJ	= Inverse Gamma)
alpha 		= 0.2; lambda = 0.1	# From Johnson and Albert, p. 165 - prior parameters of inverse gamma distribution (CHECK ON THESE)
sigmaMH		= 0.2/3			# Johnson and Albert, p. 135 0.05/C where C is category; use larger value to get more mixing on gamma
LagThetaPrior 	= rep(0, NN)		# For non-Random Walk change "+ LagThetaPrior[ii]/S2ThetaPrior" to  "+ ThetaPrior/S2ThetaPrior" and use: ThetaPrior = 0.0;
S2ThetaPrior 	= 1.0											# Variance prior for theta
BetaPrior 		= 0.0;			S2BetaPrior 	= 1.0				# Mean and variance prior for beta
R 			= rep(NA, TT)
UniqueCountry 	= sort(unique(Country))
MinCountryYear	= rep(NA, length(UniqueCountry))
for (cc in 1:length(UniqueCountry)) MinCountryYear[cc] = min(Session[Country==UniqueCountry[cc]])

 
ptm <- proc.time()			# Record processor time at beginning of session	
for (cc in 1:length(UniqueCountry)) MinCountryYear[cc] = min(Session[Country==UniqueCountry[cc]])
print("CPU time in seconds is ")	
(proc.time() - ptm)			# Current processor time minus time at the beginning of the session


if(Continuation == 0) { 
	## STARTING VALUES: Theta, Beta, Z ("t" in Johnson and Albert, p. 166), Gamma1, Gamma2
		TotalK 				= 0
		ThetaVector				= 0*Country
		ThetaMean[CountryList==2] 	= 3.0	# US - only second session is used for priors for all these
		ThetaMean[CountryList==20] 	= 3.0	# Canada
		ThetaMean[CountryList==200] 	= 3.0	# UK
		ThetaMean[CountryList==666] 	= 3.0	# Israel
		ThetaMean[CountryList==365] 	=-2.0	# Russia 
		ThetaMean[CountryList==265] 	=-2.0	# East Germany
		if(DataCode=="Nuclear")	{ ## Place US, UK and Israel on the right, Russia in middle and Brazil on the left
			ThetaMean[CountryList==2] 	= 3.0	# US - only second session is used for priors for all these
			ThetaMean[CountryList==200] 	= 3.0	# UK
			ThetaMean[CountryList==666] 	= 3.0	# Israel
			ThetaMean[CountryList==365] 	= 0.0	# Russia 
			ThetaMean[CountryList==140] 	=-2.0	# Brazil
			}	## END if(DataCode == "Nuclear")
				
			
			gammaMat 		= as.matrix(read.table(file =paste(Path, "Output\\gStart_", FileSuffix, ".txt", 	sep="") ))
			Gamma1			= gammaMat[,2]
			Gamma2			= gammaMat[,3]
			CANDGamma1			= gammaMat[,2]
			CANDGamma2			= gammaMat[,3]
			USUKRussia			= read.table(file =paste(Path, "Output\\USUKRussia_", FileSuffix, ".txt", 	sep="") )
			Beta[USUKRussia$V1 > USUKRussia$V3] = 1 
			Beta[USUKRussia$V1 < USUKRussia$V3] = -1
			# Beta 				= 0 - (apply(USUKRussia[, 1:2], 1, mean, na.rm=T)==1)  + (apply(USUKRussia[, 1:2], 1, mean, na.rm=T)==3)
			# Beta[USUKRussia$V1==USUKRussia$V3]	= 0		## This is new (esp. for nuclear votes where US/USSR same)
			if(DataCode=="Nuclear") { ## Set votes to high beta when US and Brazil are on opposite sides
				Beta[USUKRussiaBrazilChinaIndiaIsrael[,1]==3 & USUKRussiaBrazilChinaIndiaIsrael[,4]== 1] =  2	## US nay and Brazil yea: positive discrimination
				Beta[USUKRussiaBrazilChinaIndiaIsrael[,1]==1 & USUKRussiaBrazilChinaIndiaIsrael[,4]== 3] = -2	## US nay and Brazil yea: negative discrimination
				}	## END if(DataCode == "Nuclear")
		# Beta[is.na(Beta)==1]		= 0	
		BetaVector				= rep(Beta, VoteN)
		## Start value for z (perceived trait which is t in Johnson and Albert)
			Gamma1Vector 		= rep(Gamma1, VoteN)
			Gamma2Vector 		= rep(Gamma2, VoteN)
			gLo				= -999        *(yObs==1) 	+ Gamma1Vector*(yObs==2) 	+ Gamma2Vector *(yObs==3)
			gHi				= Gamma1Vector*(yObs==1) 	+ Gamma2Vector*(yObs==2) 	+ 999          *(yObs==3)
			U 				= runif(ObsN)
			Z 				= 0 + qnorm( pnorm(gLo - 0) +  U * 	 (  pnorm(gHi- 0) - pnorm( gLo- 0) )	)
			# see Gelfand,Hills, Racine-Poon and Smith (1990, 977) or Greene 3e, 179 for truncated sampling (here: theta=0 for all individuals)
			# ySim = 1: Z lower bound = -INF & upper bd = gamma1; ySim = 2: Z lower bd = gamma1 & upper bd. = gamma3; ySim = 3: Z lower bd = gamma2 & upper bd = INF
} ## END: if(Continuation == 0)

if(Continuation == 1) { 
	Z		= unlist(read.table(file = paste(Path, "Output\\ZSaveRW_", 	FileSuffix, ".txt", sep="")))
	Gamma1	= unlist(read.table(file = paste(Path, "Output\\Gamma1RW_", 	FileSuffix, ".txt", sep="")))
	Gamma2	= unlist(read.table(file = paste(Path, "Output\\Gamma2RW_", 	FileSuffix, ".txt", sep="")))
	gLo		= unlist(read.table(file = paste(Path, "Output\\gLoSaveRW_", 	FileSuffix, ".txt", sep="")))
	gHi		= unlist(read.table(file = paste(Path, "Output\\gHiSaveRW_", 	FileSuffix, ".txt", sep="")))
	ThetaVector	= unlist(read.table(file = paste(Path, "Output\\ThetaVectorSaveRW_", 	FileSuffix, ".txt", sep="")))
	BetaVector	= unlist(read.table(file = paste(Path, "Output\\BetaVectorSaveRW_", 	FileSuffix, ".txt", sep="")))
	BurnKPrev	= unlist(read.table(file = paste(Path, "Output\\BurnKRW_", 			FileSuffix, ".txt", sep="")))
	TotalK	= sum(BurnKPrev)		## This does not work if previous session cutoff mid-session
	} ## END: if(Continuation == 1) { 

##
##	Hybrid Metropolis-Hastings/Gibbs sampler
##
StartTime = date()
while(kk< K+Burn){
kk	= kk + 1;			if (kk %% 50 ==0) cat(DataCode, kk, "\n")

## STEP 1: SIMULATE THETAS ("Z" in Johnson and Albert, p. 166)
	## On prior, see Gelman, Carlin, Stern and Rubin p. 260, p. 254.
# RANDOM WALK PRIOR -- allow theta mean prior to equal value of theta in that country in previous period
	LagThetaPrior 								= ThetaMean[match(CountryList +(SessionList-1)/100,  CountryList +SessionList/100)] 
	LagThetaPrior[is.na(LagThetaPrior)==1 & SessionList==20]	= LagThetaPrior [ThetaMeanObs[is.na(LagThetaPrior)==1 & SessionList==20] -1]
	LagThetaPrior[is.na(LagThetaPrior)==1]				= 0

		for(cc in 1:length(UniqueCountry)) 	{	
  			LagThetaPrior[CountryList== UniqueCountry[cc] & SessionList== MinCountryYear[cc]] =  ThetaMean[CountryList==UniqueCountry[cc] & 
				SessionList== min(SessionList[CountryList==UniqueCountry[cc] & SessionList > MinCountryYear[cc]])]
			} # END for (cc in 1:leng 		# Set prior on first year to be ideal point from second year in previous MCMC iteration	

		for(nn in 1:NN)	if(GapYear[nn]==1) LagThetaPrior[nn] = ThetaMean[CountryList==CountryList[nn] & SessionList== max(SessionList[CountryList==CountryList[nn] & SessionList< SessionList[nn]])]
			# For countries with gap in sessions, use most recent session as LagThetaPrior
		for (ii in 1:NN){ indexValue 		= IObsMat[ii, 1:IndN[ii]]
					BstarPrior 		= c(BetaVector[indexValue  ], 1/(SmoothVector[ii]*Smooth))
					Bstar 		= c(BetaVector[indexValue  ], 1)
					Zstar			= c(Z[indexValue  ],  LagThetaPrior[ii] )
					#Zstar		= c(Z[indexValue  ],  LagThetaPrior[CountryList==CountryList[ii] & SessionList==SessionList[ii]] )
					VarTheta[ii] 	= 1/(t(BstarPrior)%*% Bstar)
					ThetaMean[ii]	= VarTheta[ii] *(t(BstarPrior)%*%Zstar)
			} ## END: for (ii in 1:NN)	{
Theta		= ThetaMean + sqrt(VarTheta)*zrnorm(NN)		## Uses zrnorm from RcppZiggurat package (for speed)		#if(sum(is.nan(Theta)) >0) stop("Theta problem")
Theta[Theta >  5] 	= 5	## Hard max on theta		
Theta[Theta < -5]  	=-5	## Hard min on theta

Theta 	= (Theta - mean(Theta)) / sqrt(var(Theta))				## Scale identification
ThetaVector = PopulateThetaVector(IObsMat, Theta, IndN, length(ThetaVector))	## REPLACES for (ii in 1:NN)	ThetaVector[IObsMat[ii, 1:IndN[ii]] ] = Theta[ii]
													## CHECK: length(ThetaVector)
													## CHECK cbind(Theta[CountryList==2 | CountryList ==365], SessionList[CountryList==2 | CountryList ==365])

## STEP 2: Generate candidate gammas for updating Gamma1, Gamma2 (See Johnson and Albert, p. 136) [NOTE: these Gamma parameters are actually Gamma*Beta]
	# see Gelfand,Hills, Racine-Poon and Smith (1990, 977) or Greene (3e, 179) for truncated sampling
	U1 			= runif(TT)    						## TT draws from uniform distribution
	CANDGamma1		= Gamma1 + sigmaMH * qnorm(U1*pnorm((Gamma2 - Gamma1)/sigmaMH)+ pnorm((-7    - Gamma1)/sigmaMH)*(1-U1))	# Truncate gamma1 at -7 on low end
	U 			= runif(TT)    						## TT draws from uniform distribution
	CANDGamma2 		= Gamma2 + sigmaMH * qnorm(U*pnorm((7 - Gamma2)/sigmaMH) + pnorm((CANDGamma1 - Gamma2)/sigmaMH)*(1-U ))	# Truncate gamma2 at  7 on high end
	CANDGamma1Vector 	= rep(CANDGamma1, VoteN)
	CANDGamma2Vector 	= rep(CANDGamma2, VoteN)
	CANDgLo		= -99              *(yObs==1)	+ CANDGamma1Vector *(yObs==2) 	+ CANDGamma2Vector *(yObs==3)
	CANDgHi		= CANDGamma1Vector *(yObs==1) 	+ CANDGamma2Vector *(yObs==2) 	+ 99               *(yObs==3)
	LikelihoodVector 	=	(	pnorm(CANDgHi- BetaVector* ThetaVector) - pnorm(CANDgLo- BetaVector* ThetaVector) )	/
					(	pnorm(    gHi- BetaVector* ThetaVector) - pnorm(    gLo- BetaVector* ThetaVector) )
	LikelihoodVector[is.nan(LikelihoodVector)==1]	= 1
	for (tt in 1:TT) {	R[tt] 	=  	prod(LikelihoodVector[VoteStartEnd[tt,1]:VoteStartEnd[tt,2]] 	) * 
						(1-pnorm((CANDGamma1[tt]- Gamma2[tt])  /sigmaMH))    / (1 - pnorm((Gamma1[tt] - CANDGamma2[tt])/sigmaMH) 	)
					if(is.na(R[tt])==1) R[tt] = 1		}  ## CHECK 	for (tt in 1:TT) if(is.na(R[tt])==1) print(tt)
	AcceptTemp 		= (runif(TT)<R) 
	Gamma1 		= AcceptTemp * CANDGamma1 + (1-AcceptTemp)*Gamma1
	Gamma2 		= AcceptTemp * CANDGamma2 + (1-AcceptTemp)*Gamma2
	#if(sum(is.na(Gamma1)) + sum(is.na(Gamma2))>0) stop("gamma problem")
	Gamma1Vector 	= rep(Gamma1, VoteN)
	Gamma2Vector 	= rep(Gamma2, VoteN)
	gLo		= -99          *(yObs==1) 	+ Gamma1Vector *(yObs==2) + Gamma2Vector *(yObs==3)
	gHi		= Gamma1Vector *(yObs==1) 	+ Gamma2Vector *(yObs==2) + 99           *(yObs==3)

## STEP 3: generate latent variable ("t" in Johnson and Albert, p. 166)
U 	= runif(ObsN)
Z 	= BetaVector* ThetaVector + qnorm( 	pnorm(gLo - BetaVector* ThetaVector) +  U * (pnorm(gHi - BetaVector* ThetaVector) - pnorm( gLo- BetaVector* ThetaVector) )	)
	#CHECK if(max(Z)> 100) stop("Max Z >100")
Z[Z>9]	= 9; Z[Z < -9]	= -9;				## CHECK Z[VoteStartEnd[tt,1]:VoteStartEnd[tt,2]]

## STEP 4: generate \beta_v estimates
#VarBeta 	= VarBetaFunction(as.matrix(VoteStartEnd), ThetaVector, S2BetaPrior)
#zBetaMean	= BetaMeanFunction(as.matrix(VoteStartEnd), ThetaVector, Z, VarBeta, BetaPrior, S2BetaPrior)
for (tt in 1:TT)	{	VarBeta[tt] 	= 1/(	sum(ThetaVector[VoteStartEnd[tt,1]:VoteStartEnd[tt,2]]^2)	+ 1/S2BetaPrior)
				BetaMean[tt]	= VarBeta[tt] * sum(ThetaVector[VoteStartEnd[tt,1]:VoteStartEnd[tt,2]]* Z[VoteStartEnd[tt,1]:VoteStartEnd[tt,2]])+ BetaPrior/S2BetaPrior
			} ## END: for (tt in 1:TT)	{
Beta	 	= BetaMean + sqrt(VarBeta)* zrnorm(TT)	## Uses zrnorm from RcppZiggurat package (for speed)

if(max(abs(Beta))> 10) print(c(kk, "Abs(Max Beta) >10"))
BetaVector 	= rep(Beta, VoteN)

## STEP 5: Store samples
	if(kk>Burn & ((kk-Burn) %% Thin == 0)) { 
		ThetaStore[(kk-Burn)/Thin,  ] 	= Theta
		BetaStore[(kk-Burn)/Thin, ] 		= Beta
		Gamma1Store[(kk-Burn)/Thin, ] 	= Gamma1
		Gamma2Store[(kk-Burn)/Thin, ] 	= Gamma2
		if (kk %% PrintSave ==0){ 		write(c(Burn, kk, TotalK), 	file = paste(Path, "Output\\BurnKRW_", 	FileSuffix, ".txt", sep=""), 		ncol=1)
								write(Z, 				file = paste(Path, "Output\\ZSaveRW_", 	FileSuffix, ".txt", sep=""), 		ncol=1)
								write(gLo, 				file = paste(Path, "Output\\gLoSaveRW_", 	FileSuffix, ".txt", sep=""), 		ncol=1)
								write(gHi, 				file = paste(Path, "Output\\gHiSaveRW_", 	FileSuffix, ".txt", sep=""), 		ncol=1)
								write(ThetaVector,		file = paste(Path, "Output\\ThetaVectorSaveRW_", 	FileSuffix, ".txt", sep=""), 	ncol=1)
								write(Theta,			file = paste(Path, "Output\\ThetaSaveRW_", 		FileSuffix, ".txt", sep=""), 	ncol=1)
								write(BetaVector, 		file = paste(Path, "Output\\BetaVectorSaveRW_", 	FileSuffix, ".txt", sep=""), 	ncol=1)
								write(Gamma1, 			file = paste(Path, "Output\\Gamma1RW_", 		FileSuffix, ".txt", sep=""), 	ncol=1)
								write(Gamma2, 			file = paste(Path, "Output\\Gamma2RW_", 		FileSuffix, ".txt", sep=""), 	ncol=1)
			} ## END: 		if (kk %% PrintSave ==0){
	} 	## END: if(kk>Burn & ((kk-Burn) %% Thin == 0) { 
} 	## END: while(kk< K){
EndTime = date(); 

## Save for continuation purposes
write(c(kk, Burn, K, TotalK), file = paste(Path, "Output\\BurnKRW_", 		FileSuffix, ".txt", sep=""), 	ncol=1)
write(Z, 				file = paste(Path, "Output\\ZSaveRW_", 		FileSuffix, ".txt", sep=""), 	ncol=1)
write(gLo, 				file = paste(Path, "Output\\gLoSaveRW_", 		FileSuffix, ".txt", sep=""), 	ncol=1)
write(gHi, 				file = paste(Path, "Output\\gHiSaveRW_", 		FileSuffix, ".txt", sep=""), 	ncol=1)
write(ThetaVector,		file = paste(Path, "Output\\ThetaVectorSaveRW_", 	FileSuffix, ".txt", sep=""), 	ncol=1)
write(BetaVector, 		file = paste(Path, "Output\\BetaVectorSaveRW_", 	FileSuffix, ".txt", sep=""), 	ncol=1)
write(Gamma1, 			file = paste(Path, "Output\\Gamma1RW_", 		FileSuffix, ".txt", sep=""), 	ncol=1)
write(Gamma2, 			file = paste(Path, "Output\\Gamma2RW_", 		FileSuffix, ".txt", sep=""), 	ncol=1)

# Estimates
ThetaEst 		= apply(ThetaStore, 	2, 	mean)
ThetaEstDist	= t(apply(ThetaStore, 2, quantile, probs = c(0, 0.05, 0.1, 0.5, 0.9, 0.95, 1)))
BetaEst 		= apply(BetaStore, 	2, 	mean)

ThetaSummary 	= cbind(CountryList, SessionList, IndN, ThetaEst, ThetaEstDist)
write(t(ThetaSummary), 	file = paste(Path, "Output\\ThetaSummaryRW_", 	FileSuffix, ".txt", sep=""), ncol = length(ThetaSummary[1,]))
write(t(BetaEst), 	file = paste(Path, "Output\\BetaEstRW_", 		FileSuffix, ".txt", sep=""), ncol = 1)
write(t(BetaStore), 	file = paste(Path, "Output\\BetaStoreRW_", 		FileSuffix, ".txt", sep=""), ncol = length(BetaStore[1,]))
write(t(ThetaStore), 	file = paste(Path, "Output\\ThetaStoreRW_",		FileSuffix, ".txt", sep=""), ncol = length(ThetaStore[1,]))
write(t(Gamma1Store), 	file = paste(Path, "Output\\Gamma1StoreRW_", 	FileSuffix, ".txt", sep=""), ncol = length(Gamma1Store[1,]))
write(t(Gamma2Store), 	file = paste(Path, "Output\\Gamma2StoreRW_", 	FileSuffix, ".txt", sep=""), ncol = length(Gamma2Store[1,]))



## Create output with vote information (rcid, gamma1est, gamma2est, beta, US vote, UK etc, resolution name, votes)
BetaEst 	= read.table(file = paste(Path, "Output\\BetaEstRW_", 		FileSuffix, ".txt", sep=""))
Gamma1Store = read.table(file = paste(Path, "Output\\Gamma1StoreRW_", 	FileSuffix, ".txt", sep=""))
Gamma2Store = read.table(file = paste(Path, "Output\\Gamma2StoreRW_", 	FileSuffix, ".txt", sep=""))
VoteTally 	= read.table(file = paste(Path, "Output\\VoteTally_", 		FileSuffix, ".txt", sep=""))
VoteList 	= VoteTally[,1]
VoteCode 	= read.table(file = paste(Path, "Output\\VoteCode_", 		FileSuffix, ".txt", sep=""))
USplus 	= read.table(file = paste(Path, "Output\\USUKRussiaBrazilChinaIndiaIsrael_", 	FileSuffix, ".txt", sep=""))
Gamma1Est 	= apply(Gamma1Store, 2,	mean)
Gamma2Est 	= apply(Gamma2Store, 2,	mean)		## CHECK: cbind(Gamma2Est, rep(NA, TT), CountriesPerVote)

VoteMatrix = cbind(1:length(t(BetaEst)), VoteTally, VoteCode, BetaEst, Gamma1Est, Gamma2Est, USplus)
	names(VoteMatrix)[1:18] = c("tt", "rcidL", "Yes", "Abstain", "No", "Total", "MatchedDum", "VoteCode", "BetaEst", "Gamma1Est", "Gamma2Est", "US", "UK", "Russia", "Brazil", "China", "Israel", "India")
write(t(VoteMatrix), 	file = paste(Path, "Output\\VoteMatrix_", FileSuffix, ".txt", sep=""), sep = "\t", ncol=length(VoteMatrix[1,]))
## Need to populate: Matches	= rep(NA, length(unique(VoteID)))


## Display how much time used so far
print(StartTime); print(EndTime)
print("CPU time in minutes is ")	#proc.time() - ptm
(proc.time() - ptm)/60		## Current processor time minus time at the beginning of the session
print(c("Number of iterations so far: ", TotalK + Burn + K))