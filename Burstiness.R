############################################################
##			     Burstiness
##
## Code to compute burstiness as used in Riedl & Woolley (2017).
##
## Please cite: Riedl, C., Woolley, A. (2017). "Teams vs. Crowds: A Field Test of the Relative Contribution of Incentives, Member Ability, and Emergent Collaboration to Crowd-Based Problem Solving Performance," Academy of Management Discoveries, in press.
############################################################




############################################################
## Function to calculate bursty nature of wait times (in minutes) between events.
## From: Goh and Barabasi, 2008, Burstiness and memory in complex systems, EPL (Europhysics Letters), 81(4), 48002, 2008.
############################################################
getBurstiness <- function(x) {
	if(length(x) == 1) return(0)
	( sd(x, na.rm=TRUE) - mean(x, na.rm=TRUE) ) / ( sd(x, na.rm=TRUE) + mean(x, na.rm=TRUE) )
}


################################################
## Helper function to compute wait time between events given by dates.
## Given a vector of (ordered) datetimes, compute waittimes between them
################################################
getWaitTime <- function( x, units="mins" ) {
	if(length(x)==1) return(NA)
	# Sanity check: make sure x is ordered
	xdup <- sort(x)
	if( all.equal(x, xdup ) != TRUE ) {
		warning( "x is not ordered. result may not be meaningful" )
	}
	out <- rep( NA, length(x) )
	for(i in 2:length(x) ) {
		out[i] <- difftime( x[i], x[i-1], units=units)
	}
	out
}

################################################
## Another helper function to get some random dates -- for illustration purposes only
################################################
getRndDatetimes <- function(N, st="2016/01/01", et="2016/01/31") {
    st <- as.POSIXct(as.Date(st))
    et <- as.POSIXct(as.Date(et))
    dt <- as.numeric(difftime(et,st,unit="sec"))
    ev <- sort(runif(N, 0, dt))
    st + ev
}

################################################
## Illustration of how to compute burstiness
################################################
events <- getRndDatetimes(100)	# Get 100 random datetimes for when events occurred
events <- sort(events)			# Make sure events are sorted by time
events

# Get inter-event wait time
waitTimes <- getWaitTime(events)

# Waittimes are one element shorter than events
waitTimes

# Compute burstiness
getBurstiness(waitTimes)



