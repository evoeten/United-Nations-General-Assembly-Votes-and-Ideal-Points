# United Nations General Assembly Votes and Ideal Points

Citation: Bailey, Michael A., Anton Strezhnev, and Erik Voeten. "Estimating dynamic state preferences from United Nations voting data." Journal of Conflict Resolution 61, no. 2 (2017): 430-456.

This code implements the ideal point estimation from Bailey et al (2017). The model is based a general IRT model for ordinal data: see Johnson and Albert 1999: Sec. 7.3; Sec. 5.2.3; Sec. 4.3.2 (p. 135) for core M-H/Gibbs sampler.

This code allows users to run the dynamic ideal point model on a subset of issues.

Note that this code was not designed to be an R package. This code dates back about a decade (although it has been adjusted) and there are some aspects of the code that are very particular to UN data, such as the missing 19th session, the way we define starting values and so on. 

Moreover, this code is not optimized to run fast, although it runs faster than an alternative application we coded in Stan. Nevertheless, it's MCMC and this is a very large dataset. We recommend that you start with models that run just a few hundred iterations to see that all is running well and then run the main model overnight, unless you run the model on small subsets of the data.

The Data is in the Data subfolder. All the Output is in the Output subfolder except for some Figures created from the Output.
 
The main file to work with if you would like to estimate ideal points is UN_ideal_Rcpp.R. 

 1) If you are running this code for the first time on a data set
	a) Set "Continuation" to 0.  If you have already run this code and would like to start where you left off in terms of the burn-in, set "Continuation" to be 1.  This is useful when you want to check results midway or when your estimation run was interrupted (e.g., computer shut down)
	b) Set value of "Path" for your computer and set value of "FileSuffix" to whatever name you want (typically date and perhaps subset (e.g., "important votes") or perhaps value of smoothing parameter (e.g.,Smooth01"))
	c) Place the 2 raw data files in a subfolder (/Data) of "Path".  One is the UN voting data ("UNVotes.Rdata").  The second is the file indicating which votes are matched with others ("Matches.csv")
	d) Install Rcpp package and include RcppFunctions folder as subfolder of "Path"
 	e) Set "RunDataOrganizer" to 1.  This will create data files in format needed for this code to run.  This only needs to be done once for each data set (this is time consuming). The file UN_DataProcessing.R processes the data and should be placed in the “Path” folder.

2) Run code based on K (number of iterations), Burn, Thin and Smooth parameters.  PrintSave is for display purposes to monitor progress.

3) The comments in the .R files may help resolve other questions.

4) The main output is "ThetaSummary.txt" in "Path/Output".  This displays ideal points and other values for each country-year.  Other output (such as vote cutpoints) is also produced as described at end of this program.

5) The code Analysis.RmD produces some basic figures for the ideal point estimates and produces an nice output file that can be used for further analysis. Run this code directly after UN_Ideal_Rcpp.R.

6) The example output files in the folder are based on sessions 1-74. Note that votes for the last two sessions may be adjusted at a later date (the 74th session is technically still running). Look at the Codebook (in the Data folder) for details on the raw data.

7) The code DyadicAgreementScores produces an undirected dyadic dataset of agreement scores and ideal point distances based on all votes. You can adjust to compute for issue specific votes.

8) IssueIdealpoints.Rmd produces some basic graphs of the various issue specific ideal points. This codes also produces "Issueidealpointsestimates.csv" in the Output folder, which contains estimates for issue ideal points.
 
