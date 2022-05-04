#=====================================================================#
#                                                                     #
#                            Iris Bontekoe                            #
#                                                                     #
#                                                                     #
#                                                                     #
#                            Calculate ODBA                           #
#                                                                     #
#=====================================================================#

# Project: White Stork - Affenberg Releases
# Author: Iris Bontekoe
# Date started: 4 December 2020
# Date last modified: 8 December 2020
# R version: 4.0.3
# Description: This script calculates ODBA


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
source("SCRIPTS/Function_CalculateODBA_01.R")
#source("SCRIPTS/Function_.R")


#=============#
#= Load data =#
#=============#

# Load the ACC data
load(file="DATA/DataAff_ACC.RData")
load(file="DATA/DataAff2_ACC.RData")


#==================#
#= Calculate ODBA =#
#==================#

DataAff<-CalculateODBA(DataAff,DataAff2)	


#===========================#
#= Save data as RData file =#
#===========================#

# Save the data
save(DataAff,file="DATA/DataAff_ODBA.RData")



#####################
library(lubridate)
library(ggplot2)

DataAff$month<-floor_date(DataAff$timestamp,unit="months")
DataAff$year<-floor_date(DataAff$timestamp,unit="years")
DataAff$day<-floor_date(DataAff$timestamp,unit="days")
DataAff$week<-floor_date(DataAff$timestamp,unit="weeks")


bla<-aggregate(ODBA~tag.local.identifier+day,data=DataAff,mean)

ggplot(bla,aes(day,ODBA,color=tag.local.identifier))+geom_point()+
geom_smooth(se=F)+theme_classic()

ggplot(bla,aes(day,ODBA))+geom_point()+
geom_smooth(se=F)+theme_classic()+facet_wrap(~tag.local.identifier)




library(ggplot2)
ggplot(rbindlist(DataAff[1:12]),aes(x=ACC_num))+
geom_line(aes(y=X),color="Red")+
geom_line(aes(y=Y),color="Green")+
geom_line(aes(y=Z),color="Blue")+
facet_wrap(~timestamp)+theme_classic()+
geom_text(data=rbindlist(ODBA_df[1:12]),aes(label=paste0("ODBA = ",round(ODBA,digits=0)),x=60,y=2200))

blablabla<-rbindlist(ODBA_df)
blabla<-aggregate(ODBA~tag.local.identifier+timestamp,data=blablabla,mean)

blablabla2<-blablabla[blablabla$ODBA>20,]
blabla<-aggregate(ODBA~tag.local.identifier+timestamp,data=blablabla2,mean)

library(lubridate)
blabla$month<-floor_date(blabla$timestamp,unit="months")
blabla$year<-floor_date(blabla$timestamp,unit="years")
blabla$day<-floor_date(blabla$timestamp,unit="days")
blabla$week<-floor_date(blabla$timestamp,unit="weeks")


bla<-aggregate(ODBA~tag.local.identifier+day,data=blabla,mean)

ggplot(bla,aes(day,ODBA,color=tag.local.identifier))+geom_point()+
geom_smooth(se=F)+theme_classic()

ggplot(blabla,aes(week,ODBA,color=tag.local.identifier))+geom_point()+
geom_smooth(se=F)+theme_classic()


