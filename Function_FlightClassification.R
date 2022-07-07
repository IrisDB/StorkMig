
#### In translation

#-------------------------------------------------------------#
#                                                             #
#--------------- Function FlightClassification ---------------#
#                                                             #
#-------------------------------------------------------------#

# Project: 
# Authors: Iris Bontekoe
# Date started: 14 May 2020
# Date last modified: 21 June 2022
# R version: 
# Description: This script determines for every data point whether the stork was flying and whether it was climbing or gliding.
# Translated from Python script with the same name

# Note: Please ask the author before you use any part of this code

data<-read.table("C:/Users/ibontekoe/Desktop/R_Analyses/DATA/DataAff_TempWind_Q.csv",header=T,sep=",",na.strings=c("","NA"))
data$timestamp<-as.POSIXct(data$timestamp)

data<-data[data$timestamp>as.POSIXct("2019-09-18")&data$timestamp<as.POSIXct("2019-09-20"),]
data<-data[data$tag.local.identifier==6998|data$tag.local.identifier==7004|data$tag.local.identifier==7026|data$tag.local.identifier==7030,]

nrow(data)

MinGroundSpeed=2.5
RunningWindowLength=15
MinFlightTime=15
MinNonFlightTime=5
MinClimbingRate=0.2
MaxDecliningRate=0

library(zoo)

# Define function FlightClassification that calculates climbing rates and classifies flight, climbing and gliding segments
FlightClassification<-function(data,MinGroundSpeed=2.5,RunningWindowLength=15,MinFlightTime=15,MinNonFlightTime=5,MinClimbingRate=0.2,MaxDecliningRate=0){ # Start function FlightClassification

	#---------------------------------#
    	#- Preparation of the data frame -#
    	#---------------------------------#
   
    	# Sort the data frame by individual and date
    	#data<-data[order(data$tag.local.identifier,data$timestamp,data$BurstID),]
    
    	# Split the data by BurstID
    	data.burst<-split(data,data$BurstID)
    
	#----------------------------#
	#- Calculate climbing rates -#
	#----------------------------#

	# Calculate climbing rates within each burst
	data.burst<-lapply(data.burst,function(i){
		i<-data.burst[[24]]  
	
		# Order the data by timestamp
		i<-i[order(i$timestamp),];
	
		# Calculate the time difference between consecutive timestamps
      		i$TimeDiff<-c(i[-1,"timestamp"]-i[-nrow(i),"timestamp"],as.difftime("NA"));
	
		# Calculate the height difference
		i$HeightDiff<-c(i[-1,]$height.above.ellipsoid-i[-nrow(i),]$height.above.ellipsoid,NA);
	    
		# Calculate the climbing rate in m/s altitude gain, based on the height difference and time difference
     		i$ClimbingRate<-i$HeightDiff/as.numeric(i$TimeDiff);

		# Calculate the running window/smoothed climbing rate
		i$Smoothed_height_above_ellipsoid <- rollapply(i$height_above_ellipsoid, width=RunningWindowLength, FUN = mean, fill = NA);

		i$HeightDiff_S<-c(i[-1,]$Smoothed_height_above_ellipsoid-i[-nrow(i),]$Smoothed_height_above_ellipsoid,NA);

		i$SmoothedClimbingRate<-i$HeightDiff_S/as.numeric(i$TimeDiff);
	
		return(i)
	})
	
    	#-------------------#
    	#- Classify flight -#
    	#-------------------#

	data.burst<-lapply(data.burst,function(i){
    
        	# Set Flying to T when the ground speed is higher than MinGroundSpeed and to F if not
        	i$Flying<-i$ground.speed>=MinGroundSpeed
    
       		# Give each flight segment an ID (non-flight will also get an ID first)
        	i$FlyingID<-cumsum(i$Flying==F)

        	# Replace the IDs with na when Flying is False
        	i[i$Flying==F,]$FlyingID<-NA
        
        	i$RowID<-1:nrow(i)

        	# Check if there is another segment less than MinNonFlightTime away
        	if(length(unique(i[!(is.na(i$FlyingID)),]$FlyingID))>1){
            		for(id in unique(i[!(is.na(i$FlyingID)),]$FlyingID)){
                		if(id<=min(i[!(is.na(i$FlyingID)),]$FlyingID)){
                    			idx<-max(i[!(is.na(i$FlyingID))&i$FlyingID==id,]$RowID)
                    			indices<-(idx+1):(idx+MinNonFlightTime+1)
                    			indices<-indices[indices %in% i$RowID]
			  		if(nrow(i[!(is.na(i$FlyingID))&(i$RowID %in% indices),])>0){
						i[is.na(i$FlyingID)&(i$RowID %in% indices),]$FlyingID<-id
			  		}
					
                 		}elif(id>=max(i[!(is.na(i$FlyingID)),]$FlyingID)){
                    			idx<-min(i[!(is.na(i$FlyingID))&i$FlyingID==id,]$RowID)
                    			indices<-(idx-MinNonFlightTime):idx
                    			indices<-indices[indices %in% i$RowID]
                    			if(nrow(i[!(is.na(i$FlyingID))&(i$RowID %in% indices),])>0){
                        			i[is.na(i$FlyingID)&(i$RowID %in% indices),]$FlyingID<-id
			  		}
					
  				}else{
                    			idx<-max(i[!(is.na(i$FlyingID))&i$FlyingID==id,]$RowID)
		    			indices<-(idx+1):(idx+MinNonFlightTime+1)
		    			indices<-indices[indices %in% i$RowID]
                    			if(nrow(i[!(is.na(i$FlyingID))&(i$RowID %in% indices),])>0){
                        			i[is.na(i$FlyingID)&(i$RowID %in% indices),]$FlyingID<-id
		    			}

		    			idx<-min(i[!(is.na(i$FlyingID))&i$FlyingID==id,]$RowID)
                    			indices<-(idx-MinNonFlightTime):idx
                   			indices<-indices[indices %in% i$RowID]
                    			if(nrow(i[!(is.na(i$FlyingID))&(i$RowID %in% indices),])>0){
                        			i[is.na(i$FlyingID)&(i$RowID %in% indices),]$FlyingID<-id
                    			}
		 		}
            		}
        	}

		# Give merged segments the same number
		i$FlyingID2<-cumsum(i$FlyingID==F)

        	# Replace the values with nan where FlyingID is nan
        	i[is.na(i$FlyingID),]$FlyingID2<-NA
        	i$FlyingID<-i$FlyingID2

        	# Replace the IDs with na if the segment is shorter than MinFlightTime
        	for(id2 in unique(i[!(is.na(i$FlyingID)),]$FlyingID)){
			if(nrow(i[!(is.na(i$FlyingID))&i$FlyingID==id2,])<MinFlightTime){
				i[!(is.na(i$FlyingID))&i$FlyingID==id2,]$FlyingID<-NA
			}
		}

		#---------------------#
		#- Classify climbing -#
		#---------------------#
  
		# Do this for every flying segment separately
		if(length(unique(i[!(is.na(i$FlyingID)),]$FlyingID))>1){
			i$Climbing<-NA
			i$ClimbingID<-NA
			for(F_ID in unique(i[!(is.na(i$FlyingID)),]$FlyingID)){
            
				IBD<-i[!(is.na(i$FlyingID))&data$FlyingID==F_ID,]

				# Set Climbing to T when the climbing rate is higher than MinClimbingRate and to F if not
            		IBD$Climbing<-IBD$SmoothedClimbingRate>=MinClimbingRate

            		# Give each flight segment an ID (non-flight will also get an ID first)
            		IBD$ClimbingID<-cumsum(IBD$Climbing==F)

            		# Replace the IDs with na when Climbing is False
            		IBD[IBD$Climbing==F,]$ClimbingID<-NA

            		# Check if there is another segment less than MinNonFlightTime away
            		if(nrow(IBD[!(is.na(IBD$ClimbingID)),])>1){
                			for(CID in unique(IBD[!(is.na(IBD$ClimbingID)),]$ClimbingID)){
						
						if(CID<=min(IBD[!(is.na(IBD$ClimbingID)),]$ClimbingID)){
							idx<-max(IBD[!(is.na(IBD$ClimbingID))&IBD$ClimbingID==CID,]$RowID)
                    				indices<-(idx+1):(idx+MinNonFlightTime+1)
                    				indices<-indices[indices %in% IBD$RowID]
			  				if(nrow(IBD[!(is.na(IBD$ClimbingID))&(IBD$RowID %in% indices),])>0){
								IBD[is.na(IBD$ClimbingID)&(IBD$RowID %in% indices),]$ClimbingID<-CID
			  				}

                 				}elif(CID>=max(IBD[!(is.na(IBD$ClimbingID)),]$ClimbingID)){
                    				idx<-min(IBD[!(is.na(i$ClimbingID))&IBD$ClimbingID==CID,]$RowID)
                    				indices<-(idx-MinNonFlightTime):idx
                    				indices<-indices[indices %in% i$RowID]
                    				if(nrow(IBD[!(is.na(IBD$ClimbingID))&(IBD$RowID %in% indices),])>0){
                        				IBD[is.na(IBD$ClimbingID)&(IBD$RowID %in% indices),]$ClimbingID<-CID
			  				}
                            
                    			 }else{
                    				idx<-max(IBD[!(is.na(IBD$ClimbingID))&IBD$ClimbingID==CID,]$RowID)
		    					indices<-(idx+1):(idx+MinNonFlightTime+1)
		    					indices<-indices[indices %in% i$RowID]
                    				if(nrow(IBD[!(is.na(IBD$ClimbingID))&(IBD$RowID %in% indices),])>0){
                        				IBD[is.na(i$ClimbingID)&(i$RowID %in% indices),]$ClimbingID<-CID
		    					}

		    					idx<-min(IBD[!(is.na(IBD$ClimbingID))&IBD$ClimbingID==CID,]$RowID)
                    				indices<-(idx-MinNonFlightTime):idx
                   				indices<-indices[indices %in% i$RowID]
                    				if(nrow(IBD[!(is.na(IBD$ClimbingID))&(IBD$RowID %in% indices),])>0){
                        				IBD[is.na(IBD$ClimbingID)&(IBD$RowID %in% indices),]$ClimbingID<-CID
                    				}
		 				}
					}
        			}

            		# Give merged segments the same number
				IBD$ClimbingID2<-cumsum(IDB$ClimbingID==F)

				# Replace the values with nan where ClimbingID is nan
        			IBD[is.na(IBD$ClimbingID),]$ClimbingID2<-NA
        			IBD$ClimbingID<-IBD$ClimbingID2

        			# Replace the IDs with na if the segment is shorter than MinFlightTime
        			for(CID2 in unique(IBD[!(is.na(IBD$ClimbingID)),]$ClimbingID)){
					if(nrow(IBD[!(is.na(IBD$ClimbingID))&IBD$ClimbingID==CID2,])<MinFlightTime){
						IBD[!(is.na(IBD$ClimbingID))&IBD$ClimbingID==CID2,]$ClimbingID<-NA
					}
				}

            		# Enter the ClimbingIDs in the data
            		i[!(is.na(i$FlyingID))&data$FlyingID==F_ID,]$ClimbingID<-IBD$ClimbingID   

			}
		}

        #--------------------#
        #- Classify gliding -#
        #--------------------#
        
		# Do this for every flying segment separately
		if(length(unique(i[!(is.na(i$FlyingID)),]$FlyingID))>1){
			i$Gliding<-NA
			i$GlidingID<-NA
			for(F_ID in unique(i[!(is.na(i$FlyingID)),]$FlyingID)){
            
				IBD<-i[!(is.na(i$FlyingID))&data$FlyingID==F_ID,]

				# Set Gliding to T when the climbing rate is higher than MinClimbingRate and to F if not
            		IBD$Gliding<-IBD$SmoothedClimbingRate<= MaxDecliningRate

				# Give each flight segment an ID (non-flight will also get an ID first)
            		IBD$GlidingID<-cumsum(IBD$Gliding==F)

            		# Replace the IDs with na when Gliding is False
            		IBD[IBD$Gliding==F,]$GlidingID<-NA

            		# Check if there is another segment less than MinNonFlightTime away
            		if(nrow(IBD[!(is.na(IBD$GlidingID)),])>1){
                			for(GID in unique(IBD[!(is.na(IBD$GlidingID)),]$GlidingID)){
						
						if(GID<=min(IBD[!(is.na(IBD$GlidingID)),]$GlidingID)){
							idx<-max(IBD[!(is.na(IBD$GlidingID))&IBD$GlidingID==GID,]$RowID)
                    				indices<-(idx+1):(idx+MinNonFlightTime+1)
                    				indices<-indices[indices %in% IBD$RowID]
			  				if(nrow(IBD[!(is.na(IBD$GlidingID))&(IBD$RowID %in% indices),])>0){
								IBD[is.na(IBD$GlidingID)&(IBD$RowID %in% indices),]$GlidingID<-GID
			  				}

                 				}elif(GID>=max(IBD[!(is.na(IBD$GlidingID)),]$GlidingID)){
                    				idx<-min(IBD[!(is.na(i$GlidingID))&IBD$GlidingID==GID,]$RowID)
                    				indices<-(idx-MinNonFlightTime):idx
                    				indices<-indices[indices %in% i$RowID]
                    				if(nrow(IBD[!(is.na(IBD$GlidingID))&(IBD$RowID %in% indices),])>0){
                        				IBD[is.na(IBD$GlidingID)&(IBD$RowID %in% indices),]$GlidingID<-GID
			  				}
                            
                    			 }else{
                    				idx<-max(IBD[!(is.na(IBD$GlidingID))&IBD$GlidingID==GID,]$RowID)
		    					indices<-(idx+1):(idx+MinNonFlightTime+1)
		    					indices<-indices[indices %in% i$RowID]
                    				if(nrow(IBD[!(is.na(IBD$GlidingID))&(IBD$RowID %in% indices),])>0){
                        				IBD[is.na(i$GlidingID)&(i$RowID %in% indices),]$GlidingID<-GID
		    					}

		    					idx<-min(IBD[!(is.na(IBD$GlidingID))&IBD$GlidingID==GID,]$RowID)
                    				indices<-(idx-MinNonFlightTime):idx
                   				indices<-indices[indices %in% i$RowID]
                    				if(nrow(IBD[!(is.na(IBD$GlidingID))&(IBD$RowID %in% indices),])>0){
                        				IBD[is.na(IBD$GlidingID)&(IBD$RowID %in% indices),]$GlidingID<-GID
                    				}
		 				}
					}
        			}

            		# Give merged segments the same number
				IBD$GlidingID2<-cumsum(IDB$GlidingID==F)

				# Replace the values with nan where GlidingID is nan
        			IBD[is.na(IBD$GlidingID),]$GlidingID2<-NA
        			IBD$GlidingID<-IBD$GlidingID2

        			# Replace the IDs with na if the segment is shorter than MinFlightTime
        			for(GID2 in unique(IBD[!(is.na(IBD$GlidingID)),]$GlidingID)){
					if(nrow(IBD[!(is.na(IBD$GlidingID))&IBD$GlidingID==GID2,])<MinFlightTime){
						IBD[!(is.na(IBD$GlidingID))&IBD$GlidingID==GID2,]$GlidingID<-NA
					}
				}

            		# Enter the GlidingIDs in the data
            		i[!(is.na(i$FlyingID))&data$FlyingID==F_ID,]$GlidingID<-IBD$GlidingID   
			}
		}

		return(i)
	}

	# Merge the data
	data<-rbindlist(data.burst)

	# Make the data available outside the function
	return(data)
}
                                                                 
