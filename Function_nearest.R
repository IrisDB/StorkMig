#------------------------------------------------#
#                                                #
#--------------- Function nearest ---------------#
#                                                #
#------------------------------------------------#


# Project: Shared code for Collective Migration Group at MPI-AB
# Authors: ?????? (code-author + initial comments) & Iris Bontekoe (comments), Andrea Flack provided the code
# Date started: 14 May 2020
# Date last modified: 15 June 2020
# R version: 3.6.2
# Description: This function finds the nearest burst out of a sequence for the start time of any given burst


nearest <- function(probe, target, ends=c(-Inf,Inf)) { # Start of function nearest

# Important: Both `probe` and `target` must be vectors of numbers in ascending order.
# Return an array `i` of indexes into `target`, parallel to array `probe`.
# For each index `j` in `target`, probe[i[j]] is nearest to target[j].

#cat("Please make sure that all input vectors are ordered in ascending order!\n")

# The input of the function is (probe) the first timestamp of a given burst in the data
# And (target) all start times of the burst sequence bseq

	glb <- function(u, v) { # Start of function glb
	
	# Explanation for  u = the first timestamp of a given burst
	#	and v = bseq, but the same is true for bseq combined with -inf and inf (see below function)


		# Calculate the length of v, which represents how many bursts there are in the sequence bseq
		n <- length(v)

		# Combine the bseq timestamps and the first timestamp of the given burst
		z <- c(v, u)

		# Save the order of the above in i and j. 
		#	Note that index numbers are saved and not the values itself
		#	The first timestamp of the given burst has the indexnumber n+1 (n = length of bseq)
		j <- i <- order(z)

		# The index number of the timestamp of the given burst is replaced by -1
		j[j > n] <- -1
		
		# The cumulative maximum index number of j is determined
		#	For example, if the j would be 1 2 5 6 4 7 9 8, then
		#	k would be 1 2 5 6 6 7 9 9
		k <- cummax(j)

		# The result given by this function (glb) is the cummulative maximum
		#	index number in k at the location where the first timestamp of the
		#	given burst was placed when ordering z (3 steps above). 
		#	This is where i has the value of n+1 (n = length of bseq)
		return (k[i > n])

	} # End of function glb

	# Combine -inf (or any other first value entered for ends), the start times
	#	in bseq and inf (or any other second value entered for ends) into y
	y <- c(ends[1], target, ends[2])

	# Run the function glb on the first timestamp of the given burst and 
	#	y (bseq combined with -inf and inf)
	i.lower <- glb(probe,y)

	# Do the same for the inversed of the first timestamp of the given burst
	#	and the inversed of y
	i.upper <- length(y) + 1 - rev(glb(rev(-probe), rev(-y)))

	# i.lower represents the position within y of the time at which the burst (from bseq)
	#	before the timestamp of the given burst
	# i.upper represents the position within y of the time at which the burst
	#	after the timestamp of the given burst

	# Get the timestamp in y at the position of i.lower
	y.lower <- y[i.lower]

	# Get the timestamp in y at the position of i.upper
	y.upper <- y[i.upper]

	# If the difference between the timestamp of the given burst
	#	and y.lower is smaller than the difference between the 
	#	timestamp of the given burst (probe) and y.upper, than
	#	lower.nearest will be TRUE, otherwise FALSE
	#	In short, this line test if probe is closer to y.lower
	#	or to y.upper. If the first is the case TRUE is saved in lower.nearest
	lower.nearest <- probe - y.lower < y.upper - probe

	# If lower.nearest (see line above) is TRUE, i is assigned the value of 
	#	i.lower-1, otherwise the value of i.upper-1 is assigned to i
	# i will indicate the position of the start time of the burst in bseq that
	#	the given burst will be assigned to
	i <- ifelse(lower.nearest, i.lower, i.upper) - 1

	# If i is smaller than 1 or larger than the length of bseq, i will be NA
	i[i < 1 | i > length(target)] <- NA

	# The function nearest() will give the value of i as output
	return (i)

} # End of function nearest


# End of comments by Iris

# Graphical illustration.
# #
# set.seed(17)
# x <- sort(round(runif(8), 3))
# y <- sort(round(runif(12), 1))
# i <- nearest(x, y)
# plot(c(0,1), c(3/4,9/4), type="n", bty="n", yaxt="n", xlab="Values", ylab="")
# abline(v = (y[-1] + y[-length(y)])/2, col="Gray", lty=3)
# invisible(apply(rbind(x, y[i]), 2, function(a) arrows(a[1], 1, a[2], 2, length=0.15)))
# points(x, rep(1, length(x)), pch=21, bg="Blue")
# points(y, rep(2, length(y)), pch=21, bg="Red", cex=sqrt(table(y)[as.character(y)]))
# text(c(1,1), c(1,2), c("x","y"), pos=4)
# #
# # Timing.
# #
# x <- runif(1e6)
# y <- runif(1e6)
# system.time({
#   x <- sort(x); y <- sort(y)
#   nearest(x,y)
# }
# )
