
#### In translation

#-------------------------------------------------------------#
#                                                             #
#--------------- Function FlightClassification ---------------#
#                                                             #
#-------------------------------------------------------------#

# Project: 
# Authors: Iris Bontekoe
# Date started: 14 May 2020
# Date last modified: 2 June 2022
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

		     }

# End of translation


                    idx = min(FlyingID[FlyingID==i].index)
                    indices = [*range(idx-MinNonFlightTime,idx)]
                    indices = [j for (j, v) in zip(indices, [item in FlyingID.index for item in indices]) if v]
                    if len(FlyingID[indices].dropna())>0:
                        idxs = FlyingID[indices].isnull()
                        idxs = idxs[idxs].index
                        FlyingID[idxs] = i

        # Give merged segments the same number
        FlyingID2 = FlyingID.isnull().cumsum()

        # Replace the values with nan where FlyingID is nan
        FlyingID2[FlyingID.isnull()] = np.nan
        FlyingID = FlyingID2

        # Replace the IDs with na if the segment is shorter than MinFlightTime
        for i in FlyingID.dropna().unique():
            if len(FlyingID[FlyingID==i])<MinFlightTime:
                FlyingID[FlyingID==i] = np.nan

        # Enter the FlyingIDs in the data
        data.loc[data["BurstID"]==BurstID,"FlyingID"] = FlyingID

        #---------------------#
        #- Classify climbing -#
        #---------------------#

        
        # Do this for every flying segment separately
        for F_ID in FlyingID.dropna().unique():
            
            # Set Climbing to T when the climbing rate is higher than MinClimbingRate and to F if not
            Climbing = data[(data["BurstID"]==BurstID)&(data["FlyingID"]==F_ID)]["SmoothedClimbingRate"] >= MinClimbingRate

            # Give each flight segment an ID (non-flight will also get an ID first)
            ClimbingID = (Climbing == False).cumsum()

            # Replace the IDs with na when Flying is False
            ClimbingID[(Climbing == False)] = np.nan

            # Check if there is another segment less than MinNonFlightTime away
            if len(ClimbingID.dropna().unique())>1:
                for i in ClimbingID.dropna().unique():

                    if i <= min(ClimbingID.dropna().unique()):
                        idx = max(ClimbingID[ClimbingID==i].index)
                        indices = [*range(idx+1,idx+MinNonFlightTime+1)]
                        indices = [j for (j, v) in zip(indices, [item in ClimbingID.index for item in indices]) if v]
                        if len(ClimbingID[indices].dropna())>0:
                            idxs = ClimbingID[indices].isnull()
                            idxs = idxs[idxs].index
                            ClimbingID[idxs] = i
                            
                    elif i >= max(ClimbingID.dropna().unique()):
                        idx = min(ClimbingID[ClimbingID==i].index)
                        indices = [*range(idx-MinNonFlightTime,idx)]
                        indices = [j for (j, v) in zip(indices, [item in ClimbingID.index for item in indices]) if v]
                        if len(ClimbingID[indices].dropna())>0:
                            idxs = ClimbingID[indices].isnull()
                            idxs = idxs[idxs].index
                            ClimbingID[idxs] = i
                            
                    else:
                        
                        idx = max(ClimbingID[ClimbingID==i].index)
                        indices = [*range(idx+1,idx+MinNonFlightTime+1)]
                        indices = [j for (j, v) in zip(indices, [item in ClimbingID.index for item in indices]) if v]
                        if len(ClimbingID[indices].dropna())>0:
                            idxs = ClimbingID[indices].isnull()
                            idxs = idxs[idxs].index
                            ClimbingID[idxs] = i
                        
                        idx = min(ClimbingID[ClimbingID==i].index)
                        indices = [*range(idx-MinNonFlightTime,idx)]
                        indices = [j for (j, v) in zip(indices, [item in ClimbingID.index for item in indices]) if v]
                        if len(ClimbingID[indices].dropna())>0:
                            idxs = ClimbingID[indices].isnull()
                            idxs = idxs[idxs].index
                            ClimbingID[idxs] = i

            # Give merged segments the same number
            ClimbingID2 = ClimbingID.isnull().cumsum()

            # Replace the values with nan where FlyingID is nan
            ClimbingID2[ClimbingID.isnull()] = np.nan
            ClimbingID = ClimbingID2

            # Replace the IDs with na if the segment is shorter than MinFlightTime
            for i in ClimbingID.dropna().unique():
                if len(ClimbingID[ClimbingID==i])<MinFlightTime:
                    ClimbingID[ClimbingID==i] = np.nan

            # Enter the ClimbingIDs in the data
            data.loc[(data["BurstID"]==BurstID)&(data["FlyingID"]==F_ID),"ClimbingID"] = ClimbingID          

        #--------------------#
        #- Classify gliding -#
        #--------------------#
        
        # Do this for every flying segment separately
        for F_ID in FlyingID.dropna().unique():
            
            # Set Gliding to T when the climbing rate is higher than MinClimbingRate and to F if not
            Gliding = data[(data["BurstID"]==BurstID)&(data["FlyingID"]==F_ID)]["SmoothedClimbingRate"] <= MaxDecliningRate

            # Give each flight segment an ID (non-flight will also get an ID first)
            GlidingID = (Gliding == False).cumsum()

            # Replace the IDs with na when Flying is False
            GlidingID[(Gliding == False)] = np.nan

            # Check if there is another segment less than MinNonFlightTime away
            if len(GlidingID.dropna().unique())>1:
                for i in GlidingID.dropna().unique():
                    
                    if i <= min(GlidingID.dropna().unique()):
                        idx = max(GlidingID[GlidingID==i].index)
                        indices = [*range(idx+1,idx+MinNonFlightTime+1)]
                        indices = [j for (j, v) in zip(indices, [item in GlidingID.index for item in indices]) if v]
                        if len(GlidingID[indices].dropna())>0:
                            idxs = GlidingID[indices].isnull()
                            idxs = idxs[idxs].index
                            GlidingID[idxs] = i
                            
                    elif i >= max(GlidingID.dropna().unique()):
                        idx = min(GlidingID[GlidingID==i].index)
                        indices = [*range(idx-MinNonFlightTime,idx)]
                        indices = [j for (j, v) in zip(indices, [item in GlidingID.index for item in indices]) if v]
                        if len(GlidingID[indices].dropna())>0:
                            idxs = GlidingID[indices].isnull()
                            idxs = idxs[idxs].index
                            GlidingID[idxs] = i
                            
                    else:
                        
                        idx = max(GlidingID[GlidingID==i].index)
                        indices = [*range(idx+1,idx+MinNonFlightTime+1)]
                        indices = [j for (j, v) in zip(indices, [item in GlidingID.index for item in indices]) if v]
                        if len(GlidingID[indices].dropna())>0:
                            idxs = GlidingID[indices].isnull()
                            idxs = idxs[idxs].index
                            GlidingID[idxs] = i
                        
                        idx = min(GlidingID[GlidingID==i].index)
                        indices = [*range(idx-MinNonFlightTime,idx)]
                        indices = [j for (j, v) in zip(indices, [item in GlidingID.index for item in indices]) if v]
                        if len(GlidingID[indices].dropna())>0:
                            idxs = GlidingID[indices].isnull()
                            idxs = idxs[idxs].index
                            GlidingID[idxs] = i

            # Give merged segments the same number
            GlidingID2 = GlidingID.isnull().cumsum()

            # Replace the values with nan where FlyingID is nan
            GlidingID2[GlidingID.isnull()] = np.nan
            GlidingID = GlidingID2

            # Replace the IDs with na if the segment is shorter than MinFlightTime
            for i in GlidingID.dropna().unique():
                if len(GlidingID[GlidingID==i])<MinFlightTime:
                    GlidingID[GlidingID==i] = np.nan

            # Enter the GlidingIDs in the data
            data.loc[(data["BurstID"]==BurstID)&(data["FlyingID"]==F_ID),"GlidingID"] = GlidingID
            
    # Make the data available outside the function
    return(data)
}
                                                                 
