#Set working directory and load my usual libraries
setwd("/Users/djcoo/Desktop/UVA/UVA Spring 2020/STAT 4996/")
#setwd("C:/Users/JanPatrick/Documents/UVA/UVA Spring 2020/STAT 4996")
library(tidyverse)
library(stringr)
library(splitstackshape)
library(tidycensus)
library(stringi)
library(plyr)

presdf<-read.csv('1976-2016-president.csv') #read in the file
presdf<-presdf[,c('year','state','state_po','state_fips','party','candidatevotes','totalvotes')] #label columns
presdf<-subset(presdf,presdf$party=='republican' | presdf$party=='democrat') #subset by just republican and democrat
presdf$percentpartyvotes<-presdf$candidatevotes/presdf$totalvotes*100 #get percentage of votes won by party
presdf$percentpartyvotes<-round(presdf$percentpartyvotes,4) #round to 4 decimals
presdf$state_fips<-fips_codes[match(presdf$state_po,fips_codes$state),2] #replace fip codes for fips_code IMPORTANT
turnout<-read.csv("registration_pct.csv") #read in file
turnout<-turnout[,2:15] #keep useful columns
turnout$fips<-str_pad(turnout$fips, width=2, pad="0") #add leading zero to FIPS values with 1 digit.
colnames(turnout)<-c('state','state.abb','fips','2016','2012','2008','2004','2000',
                     '1996','1992','1988','1984','1980','1976') #name columns
#turnout$fips<-fips_codes[match(turnout$state_po,fips_codes$state),2] #replace fip codes for fips_code IMPORTANT
presdf<-arrange(presdf,year,state_fips) #organize data by year then state fips
presdf$yes<-seq_len(nrow(presdf)) #assign a number to all rows in additional column
presdf<-rbind(presdf,data.frame(year="2000",state='Minnesota',state_po='MN',state_fips=27,
                                  party='democrat',candidatevotes=0,totalvotes=0,
                                  percentpartyvotes=0,yes=659.1)) #insert missing column with 'position' 659.1
presdf<-rbind(presdf,data.frame(year="2004",state='Minnesota',state_po='MN',state_fips=27,
                                party='democrat',candidatevotes=0,totalvotes=0,
                                percentpartyvotes=0,yes=761.1)) #insert missing column with 'position' 761.1
presdf<-rbind(presdf,data.frame(year="2012",state='Minnesota',state_po='MN',state_fips=27,
                                party='democrat',candidatevotes=0,totalvotes=0,
                                percentpartyvotes=0,yes=964.1)) #insert missing column with 'position' 964.1
presdf<-arrange(presdf,yes) #organize the data by new column (used to just insert missing columns where needed)
presdf<-presdf[-c(757,1028,1065,1066),] #get rid of useless columns
presdf<-arrange(presdf,desc(year),state_fips) #organize by descending year and then state fips
presdf<-presdf[,-9] #get rid of made up column 'yes'
turnout<-arrange(turnout,fips) #organize by fip codes
turnout[is.na(turnout)]<-0 #inset a zero for all NA's
presdf$voterturnout<-c(rep(turnout$`2016`,each=2),rep(turnout$`2012`,each=2),rep(turnout$`2008`,each=2),
                       rep(turnout$`2004`,each=2),rep(turnout$`2000`,each=2),rep(turnout$`1996`,each=2),
                       rep(turnout$`1992`,each=2),rep(turnout$`1988`,each=2),rep(turnout$`1984`,each=2),
                       rep(turnout$`1980`,each=2),rep(turnout$`1976`,each=2)) #insert new column.I inserted the voter turn out
                                                                              #rate by creating a new column 
Voting_demo<-read.csv("/Users/djcoo/Desktop/UVA/UVA Spring 2020/STAT 4996/Votingdata_demographic.csv", 
                     header = TRUE) # Read in voting and registration by demographics data; then clean the table
Voting_demo<-Voting_demo[-c(1,3,5,7,9,11,13,15,17,19,21,23,25,26:297),] # This removes unnecessary rows including general election years
Voting_demo<-Voting_demo[-c(3,5,7,9,11,13,17,19,21,22,23,25,27)]
Voting_demo$Voting_age_Pop_thous_V<-as.numeric(as.character(gsub(",","", Voting_demo$Voting_age_Pop_thous_V))) # removes commas 
Voting_demo$Voting_age_Pop_thous_V<-stri_pad_right(Voting_demo$Voting_age_Pop_thous_V, 
                                                  width=9, pad="0") #add trailing zero to Voting_age_Pop_thous_V for 9 digits
presdf<-merge(presdf, Voting_demo, by.x="year", by.y="year", sort = TRUE) #merge two datasets "presdf" and "Voting_demo".

