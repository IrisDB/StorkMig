#----------------------------------------------#
#                                              #
#----------- Function CalculateODBA -----------#
#                                              #
#----------------------------------------------#

# Project: White Stork Affenberg Releases
# Author: Iris Bontekoe
# Date started: 10 December 2020
# Date last modified: 10 December 2020
# R version: 4.0.3 
# Description: Removes non-ACC data,
#	data after dead and data before release (where applicable)


#-------------------------------------#
#- Define the function and arguments -#
#-------------------------------------#

CalculateODBA<-function(DataFrame1,DataFrame2){ # Start of function CalculateODBA


	#--- Capture input names ---#
	DataFrameName<-as.character(deparse(substitute(DataFrame1)))


	#--- Calculate ODBA ---#
	# Make an empty data frame to later enter the calculated ODBA values
	ODBA_list<-list()

	# Calculate the ODBA values
	ODBA_list<-lapply(DataFrame2,function(i){
	
		# Substract the mean of X from X
		Diff_X<-abs(i$X_mps-mean(i$X_mps));
		# Substract the mean of Y from Y
		Diff_Y<-abs(i$Y_mps-mean(i$Y_mps));
		# Substract the mean of Z from Z
		Diff_Z<-abs(i$Z_mps-mean(i$Z_mps));
	
		# Calculate the average of the X, Y and Z calculated above and fill the data frame
		ODBA_list[length(ODBA_list)+1]<-mean(Diff_X)+mean(Diff_Y)+mean(Diff_Z);
		
		return(ODBA_list)
	})

	DataFrame1$ODBA<-unlist(ODBA_list)

	# Run the garbage collector to free memory
	gc()


	#--- Return the DataFrame ---#
	# Print end statement to inform the user
	cat(DataFrameName,"now contains ODBA\n")
	
	return(DataFrame1)


} # End of function CalculateODBA

