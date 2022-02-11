#-------------------------------------------------------#
#                                                       #
#--------------- Function SplitGPSBursts ---------------#
#                                                       #
#-------------------------------------------------------#

# Project: White stork migration 
# Authors: Andrea Flack & Iris Bontekoe (code was written/adjusted on moveobjects by Andrea, Iris adjusted the code and added comments)
# Date started: 14 May 2020
# Date last modified: 10 Feb 2022
# R version: 3.6.2
# Description: This function identifies GPS bursts, gives each burst a unique ID, and a second ID that is based on time sequence so that it is possible to identify simultaneously recorded GPS bursts.


## input data is movestack


## input data is movestack
## input data is movestack

SplitGPSBursts_move <- function(data,MaxTimeDiff=10,MinBurstLength=120){ # Start function SplitGPSBursts_move
  
  # Load pakages and functions where necessary
  if(!require("lubridate")){install.packages("lubridate");library(lubridate)}
  if(!require("raster")){install.packages("raster");library(raster)}
  if(!require("data.table")){install.packages("data.table");library(data.table)}
  
  source("nearest.R")
  

  #---------------------------------------#
  #- Capture the input name of DataFrame -#
  #---------------------------------------#
  
  DataFrameName<-as.character(deparse(substitute(data)))
  
  #---------------------------------#
  #- Preparation of the data frame -#
  #---------------------------------#
  
  # Give every row in the data frame a unique number to be able to link rows after 
  #	splitting the data frame
  data$UniqueNumber<-1:nrow(data)
  
  #----------------------------#
  #- Split data by individual -#
  #----------------------------#

  data.ind <- split(data)
  
  #----------------------#
  #- Identifying bursts -#
  #----------------------#
  
  # Find which rows of the dataframe differ only 1 second from the previous row
  #	This is done for every sub-dataframe within data.ind, so for the data of every individual separately
  #	Index-numbers of these rows will be saved in rows_onesecdiff
  #	Note: If the tag takes a burst, a position is taken every second, so the time difference between
  #		locations in the same burst is 1 second
  rows_onesecdiff <- lapply(data.ind, function(i) {which(difftime(i$timestamp[-1],i$timestamp[-nrow(i)],units="secs")==1)})
  
  
  # Enter the index-numbers saved in rows_onesecdiff in the respective rows in the individual-split-dataframe
  #	This is done for every individual separately
  #	index-numbers are saved in the new column "new"
  # 	Note that if the difference between row 5 and 6 was 1 second, then the rownumber 5 will be entered in row 5
  #	and that NA is entered in row 6
  
  for(i in 1:length(data.ind)){
    data.ind[[i]][rows_onesecdiff[[i]],"new"] <- rows_onesecdiff[[i]]
  }
  
  
  #-------------------#
  #- Separate bursts -#
  #-------------------#
  
  # A break of at least 10 seconds to be counted as separte bursts
  # For every row of which the index-number is in rows_onesecdiff, the time difference with the next row is calculated
  #	If this difference is more than 10 seconds, the index of the index-number in rows_onesecdiff
  #	is saved in "Breaks", together with a 0 and with the lenght of rows_onesecdiff
  #	This happens for every individual separately
  
  Breaks <- lapply(1:length(data.ind),function(i) 
    {
    b=c(0,
      which(diff(timestamps(data.ind[[i]])[rows_onesecdiff[[i]]]) > MaxTimeDiff),
      length(rows_onesecdiff[[i]]))
    attributes(b) = NULL
    return(b)
    }
    )

  # Create the list BurstData to save the burst data in
  BurstData <- list()
  
  #-------------------------#
  #- Reduce data to bursts -#
  #-------------------------#
  
  for (i in 1:length(Breaks))
    {
    tmp1 <- (sapply(seq(length(Breaks[[i]]) - 1), function(j) {list(rows_onesecdiff[[i]][(Breaks[[i]][j] + 1):Breaks[[i]][j+1]])}))
    # Burst has to be at least 120 (MinBurstLength) seconds long
    tmp2 <- (lapply(tmp1[lapply(tmp1,length)>MinBurstLength], function(i) {as.integer(seq(i[1],i[length(i)]+1))}))
    
    BurstData[[i]] <- data.ind[[i]][unlist(tmp2),]
    
    if(length(tmp2)>0){
      BurstData[[i]]$Bno <- mapply(rep, list(1:length(tmp2)), list(do.call(rbind, lapply(tmp2, function(x) length(x)))))
    }
  }
  #--------------------------------#
  #- Assign BurstIDs to DataFrame -#
  #--------------------------------#
  
  # Assign names of data.ind to BurstData
  names(BurstData) <- names(data.ind)
  
  ## remove individuals that do not have bursts
  if (length(which(lapply(BurstData,nrow)==0))!=0)
  BurstData = BurstData[-which(lapply(BurstData,nrow)==0)]
  
  # Rejoin the data data frames of all individuals
  BurstData2<-moveStack( BurstData, forceTz="UTC")
  
  # Assign individual BurstIDs based on tag number and Bno
  BurstData2$BurstID<-paste0(BurstData2$tag_local_identifier,"_",BurstData2$Bno)
  data$BurstID =NA
  data$BurstID<-BurstData2$BurstID[match(data$UniqueNumber,BurstData2$UniqueNumber)]
  
  data<-data[!(is.na(data$BurstID)),]
  
  
  #--------------------------#
  #- Assign burst to a time -#
  #--------------------------#
  
  # Make a data frame with the time on which bursts should have started
  
  min.date=min(floor_date(timestamps(data),"day"))
  max.date=max(floor_date(timestamps(data),"day"))
  bseq <- data.frame(
    StartTime=c(
      seq(min.date,max.date+days(1),by="15 mins")
      ,
      seq(min.date+hours(10)+minutes(40),max.date+days(1)+hours(10)+minutes(40),by="1 days")
    )
    
    ,
    EndTime=c(
      seq(min.date+minutes(12),max.date+days(1)+minutes(12),by="15 mins")
      ,
      seq(min.date+hours(10)+minutes(40)+minutes(12),max.date+days(1)+hours(10)+minutes(40)+minutes(12),by="1 days")
    )
  )
  
  # Order bseq by StartTime
  bseq <- bseq[order(bseq$StartTime),]
  
  # Give each time window an id
  bseq$ID <- seq(1:nrow(bseq))
  
  trackId = data@trackId
  # Match bursts with sequence 
  data.split =split(data, data$BurstID)
  data.split = lapply(data.split,
                      function(i) {
                      i$Burst_A <- bseq[nearest(unique(as.numeric(timestamps(i)))[1],as.numeric(bseq$StartTime)),"ID"]
                      return(i)
  })
  ## re-stacking it very slow. Need to find out whether not splitting is faster. 
  data = moveStack(data.split, forceTz="UTC",trackId = trackId)
  data@trackId = trackId
  #--------------------------------------------#
  #- Save data frame and print end-statements -#
  #--------------------------------------------#
  
  # Save data to the R-environment
  assign(DataFrameName,data,pos=1)
  
  # Print information for the user
  cat("\nThe input data frame now contains the extra columns BurstID and Burst_A\n\n")
  cat("Thank you very much for your patience and have a nice day!\n")
  
} # End function SplitGPSBursts_move
