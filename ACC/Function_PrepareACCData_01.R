#-----------------------------------------------#
#                                               #
#----------- Function PrepareACCData -----------#
#                                               #
#-----------------------------------------------#

# Project: White Stork Affenberg Releases
# Author: Iris Bontekoe
# Date started: 1 December 2020
# Date last modified: 5 October 2022
# R version: 4.0.3 
# Description: Removes non-ACC data,
#	data after dead and data before release (where applicable)


#-------------------------------------#
#- Define the function and arguments -#
#-------------------------------------#

PrepareACCData<-function(DataFrame,Released=TRUE){ # Start of function PrepareACCData


	#--- Capture input names ---#
	DataFrameName<-as.character(deparse(substitute(DataFrame)))


	#--- Remove unnecessary columns ---#
	# Only keep columns that are necessary for further steps and analyses
	
	DataFrame<-DataFrame[,c(
		"timestamp",
		"tag.local.identifier",
		"individual.local.identifier",
		"eobs.accelerations.raw",
		"sensor.type")]
	

	#--- Remove the acceleration data ---#
	# Get rid of the accelleration data. The ACC data will be loaded separately
	#	when necessary

	DataFrame<-droplevels(DataFrame[DataFrame$sensor.type=="acceleration",])


	#--- Change timestamps to POSIXct ---#

	DataFrame$timestamp<-as.POSIXct(DataFrame$timestamp,format="%Y-%m-%d %H:%M:%OS",tz="UTC")
	
	
	#--- Round data to seconds ---#
	
	DataFrame$timestamp<-as.POSIXct(round(as.POSIXct(DataFrame$timestamp,tz="UTC"),"secs"),tz="UTC")


	if(Released==TRUE){
		#--- Remove data before the release and after death ---#

		# Add the release and death time to the data frame
		DataFrame[,c("Release","Death")]<-ReleaseDeath[match(DataFrame$tag.local.identifier,ReleaseDeath$tag.local.identifier),c("Release","Death")]
		DataFrame$Release<-as.POSIXct(DataFrame$Release,format="%d-%m-%Y %H:%M",tz="UTC")
		DataFrame$Death<-as.POSIXct(DataFrame$Death,format="%d-%m-%Y %H:%M",tz="UTC")


		# Remove data after the release and death
		DataFrame<-droplevels(DataFrame[(is.na(DataFrame$Release)|DataFrame$timestamp>=DataFrame$Release)&(is.na(DataFrame$Death)|DataFrame$timestamp<=DataFrame$Death),])
		
	}else{
		#--- Remove data after death ---#

		# Add the release and death time to the data frame
		DataFrame[,"Death"]<-ReleaseDeath[match(DataFrame$tag.local.identifier,ReleaseDeath$tag.local.identifier),"Death"]
		DataFrame$Death<-as.POSIXct(DataFrame$Death,format="%d-%m-%Y %H:%M",tz="UTC")


		# Remove data after the release and death
		DataFrame<-droplevels(DataFrame[is.na(DataFrame$Death)|DataFrame$timestamp<=DataFrame$Death,])
		
	}


	#--- Change data type of the individual IDs ---#
	# Change individual IDs from integer (continuous) to character (discrete)

	DataFrame$tag.local.identifier<-as.character(DataFrame$tag.local.identifier)


	#--- Remove duplicated timestamps ---#
	# Run the garbage collector to free memory
	gc()
	
	#DataFrame<-RemoveDuplicatedTimestamps(DataFrame)
	

	#--- Split the ACC into colums X Y Z ---#
	# Create a new data frame where the splitted ACC values can be entered into
	DataFrame2<-data.frame(timestamp=as.POSIXct(character(0)),tag.local.identifier=character(0),X_raw=numeric(0),Y_raw=numeric(0),Z_raw=numeric(0),ACC_num=numeric());

	# Split the ACC values
	SplittedACC<-strsplit(DataFrame[,"eobs.accelerations.raw"]," ")

	# Fill the new data frame with the splitted values
	DataFrame2<-lapply(seq_along(SplittedACC),function(i) {
		DataFrame2[nrow(DataFrame2):(length(SplittedACC[[i]])/3),"X_raw"]<-as.numeric(SplittedACC[[i]][seq(1,length(SplittedACC[[i]]),3)]);
		DataFrame2[,"Y_raw"]<-as.numeric(SplittedACC[[i]][seq(2,length(SplittedACC[[i]]),3)]);
		DataFrame2[,"Z_raw"]<-as.numeric(SplittedACC[[i]][seq(3,length(SplittedACC[[i]]),3)]);
		DataFrame2[,"timestamp"]<-DataFrame[i,"timestamp"];
		DataFrame2[,"tag.local.identifier"]<-DataFrame[i,"tag.local.identifier"];
		DataFrame2[,"ACC_num"]<-seq(1,length(SplittedACC[[i]]),3);
		return(DataFrame2)
	})

	# Bind the data frames together to one data frame
	#DataFrame2<-rbindlist(DataFrame2)


	#--- Transform the values to g or m/s2 ---#
	# Calibrate ACC data # Code from Andrea adjusted by me
	# calibaration
	cal = data.frame(matrix(6,1))
	cal$xzero = 2042
	cal$cx = 0.0020
	cal$yzero = 2042
	cal$cy = 0.0020
	cal$zzero = 2049
	cal$cz = 0.0023
	g=9.80665

	# convert ACC raw values to meaningful unit (m/s)
	DataFrame2<-lapply(DataFrame2,function(i){

		# Calculate the ACC values in m/s
		i[,"X_mps"]<-(i$X_raw-cal$xzero)*cal$cx*g;
		i[,"Y_mps"]<-(i$Y_raw-cal$yzero)*cal$cy*g;
		i[,"Z_mps"]<-(i$Z_raw-cal$zzero)*cal$cz*g;
		return(i)
	})


	# Run the garbage collector to free memory
	gc()


	#--- Return the DataFrame ---#
	# Print end statement to inform the user
	cat("The data is prepared.\n- Only ACC data\n- before death\n- after release (if applicable) \n- relevant columns\n- ACC split\n- ACC converted to m/s\n")
	cat("The output is a list of data frames with one data frame for each burst\n\n")
	
	assign(DataFrameName,DataFrame,pos=1)
	return(DataFrame2)


} # End of function PrepareACCData

