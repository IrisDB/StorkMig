#=====================================================================#
#                                                                     #
#                            Iris Bontekoe                            #
#                                                                     #
#                                                                     #
#                                                                     #
#                    Prepare "DataAff_ACC.Rdata"                      #
#                                                                     #
#=====================================================================#

# Project: White Stork - Affenberg Releases
# Author: Iris Bontekoe
# Date started: 1 December 2020
# Date last modified: 8 December 2020
# Date data download: 1 December 2020
# R version: 4.0.3
# Description: This script contains everything to make the DataAff_ACC.Rdata file


#================#
#= Preparations =#
#================#

# Change work directory
setwd("C:/Users/ibontekoe/Desktop/R_Analyses")

# Set memory limit to enable
#memory.limit(size=30000)

# Set standard time zone to UTC
Sys.setenv(TZ="UTC")

# Load functions and packages
source("SCRIPTS/Function_PrepareACCData_01.R")
#source("SCRIPTS/Function_.R")


#=============#
#= Load data =#
#=============#

# Load the ACC data
DataAff<-read.csv("DATA/White Stork Affenberg releases MPIAB_ACC.csv",header=T,sep=",")
#DataAff<-DataAff[DataAff$tag.local.identifier%in%c(7013,6999,8041,8052),]

# Load the table with release and death dates and times
ReleaseDeath<-read.csv("DATA/Release_Death.csv",sep=";")

# Run the garbage collector to free memory
gc()


#================#
#= Prepare data =#
#================#

# Prepare data
DataAff2<-PrepareACCData(DataAff,Released=TRUE)

# Run the garbage collector to free memory
gc()


#===========================#
#= Save data as RData file =#
#===========================#

# Save the data
save(DataAff,file="DATA/DataAff_ACC.RData")
save(DataAff2,file="DATA/DataAff2_ACC.RData")

# Quit R
#	Go to the script "" in a new R session to continue
quit(save="no")


