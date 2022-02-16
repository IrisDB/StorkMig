
#### In translation

#-------------------------------------------------------------#
#                                                             #
#--------------- Function FlightClassification ---------------#
#                                                             #
#-------------------------------------------------------------#

# Project: 
# Authors: Iris Bontekoe
# Date started: 14 May 2020
# Date last modified: 16 February 2022
# R version: 
# Description: This script determines for every data point whether the stork was flying and whether it was climbing or gliding.
# Translated from Python script with the same name

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
        
        # Order the data by timestamp
		i<-i[order(i$timestamp),];
        i$TimeDiff<-c(i[-1,"timestamp"]-i[-nrow(i),"timestamp"],as.difftime("NA"));
		i$HeightDiff<-c(i[-1,]$height.above.ellipsoid-i[-nrow(i),]$height.above.ellipsoid,NA);
        i$ClimbingRate<-i$HeightDiff/as.numeric(i$TimeDiff)

        return(i)
        })
# End of translation    
    

    
    # Enter climbing rates into the data frame, only within bursts

    # Make a list that indicates if a data point belongs to a burst
    InBurst = [i for i, x in enumerate(data["BurstID"].diff()) if x == 0]
    
    # Enter ClimbingRate into the data, but only for the rows belonging to a burst
    data.loc[data.index.isin(InBurst),"ClimbingRate"] = [ClimbingRate[idx] for idx in InBurst]
    
    # Shift the data one up to have the climbing rate between the current location and the next
    data.ClimbingRate = data.ClimbingRate.shift(-1)
    
    # Calculate the running window/smoothed climbing rate
    for BurstID in BurstIDs:
        data.loc[data["BurstID"]==BurstID,"Smoothed_height-above-ellipsoid"] = uniform_filter1d(data.loc[data["BurstID"]==BurstID,"height-above-ellipsoid"], size=RunningWindowLength)
    
    data["SmoothedClimbingRate"] = np.nan
    
    HeightDiff = data["Smoothed_height-above-ellipsoid"].diff()
    ClimbingRate = (HeightDiff/TimeDiff).tolist()
    
    # Enter ClimbingRate into the data, but only for the rows belonging to a burst
    data.loc[data.index.isin(InBurst),"SmoothedClimbingRate"] = [ClimbingRate[idx] for idx in InBurst]
    
    # Shift the data one up to have the climbing rate between the current location and the next
    data.SmoothedClimbingRate = data.SmoothedClimbingRate.shift(-1)
    

    for BurstID in BurstIDs:
        
        #-------------------#
        #- Classify flight -#
        #-------------------#
            
        # Set Flying to T when the ground speed is higher than MinGroundSpeed and to F if not
        Flying = data[data["BurstID"]==BurstID]["ground-speed"] >= MinGroundSpeed
    
        # Give each flight segment an ID (non-flight will also get an ID first)
        FlyingID = (Flying == False).cumsum()

        # Replace the IDs with na when Flying is False
        FlyingID[(Flying == False)] = np.nan

        # Check if there is another segment less than MinNonFlightTime away
        if len(FlyingID.dropna().unique())>1:
            for i in FlyingID.dropna().unique():
                
                if i <= min(FlyingID.dropna().unique()):
                    idx = max(FlyingID[FlyingID==i].index)
                    indices = [*range(idx+1,idx+MinNonFlightTime+1)]
                    indices = [j for (j, v) in zip(indices, [item in FlyingID.index for item in indices]) if v]
                    if len(FlyingID[indices].dropna())>0:
                        idxs = FlyingID[indices].isnull()
                        idxs = idxs[idxs].index
                        FlyingID[idxs] = i

                elif i >= max(FlyingID.dropna().unique()):
                    idx = min(FlyingID[FlyingID==i].index)
                    indices = [*range(idx-MinNonFlightTime,idx)]
                    indices = [j for (j, v) in zip(indices, [item in FlyingID.index for item in indices]) if v]
                    if len(FlyingID[indices].dropna())>0:
                        idxs = FlyingID[indices].isnull()
                        idxs = idxs[idxs].index
                        FlyingID[idxs] = i

                else:
                    idx = max(FlyingID[FlyingID==i].index)
                    indices = [*range(idx+1,idx+MinNonFlightTime+1)]
                    indices = [j for (j, v) in zip(indices, [item in FlyingID.index for item in indices]) if v]
                    if len(FlyingID[indices].dropna())>0:
                        idxs = FlyingID[indices].isnull()
                        idxs = idxs[idxs].index
                        FlyingID[idxs] = i

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
            
    # Save the data
    data.to_pickle(data_folder+file_name_out)
}
                                                                 
# Run the function

#=================#
#=== Affenberg ===#
#=================#

# Define objects
file_name_in = "DataAff_TempWind_Q.pkl"
file_name_out = "DataAff_TempWind_Q.pkl"

# Execute the function and print the run time
start = datetime.datetime.now()
FlightClassification()
print(datetime.datetime.now())
print(datetime.datetime.now()-start)
