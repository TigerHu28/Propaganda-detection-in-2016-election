#set working directory and load libraries
setwd("/Users/djcoo/Desktop/UVA/UVA Spring 2020/STAT 4996/")
library(stringi)
library(stringr)
library(data.table)
library(tigris)
library(tidycensus)
library(usmap)

###

#We want to remove unnecessary info from our "voting" csv, then merge state fips codes.
voting=read.csv("/Users/djcoo/Desktop/UVA/UVA Spring 2020/STAT 4996/VotingData_76to16.csv", 
                header = TRUE) # Read in registration data; then clean the table
Voting_demo=read.csv("/Users/djcoo/Desktop/UVA/UVA Spring 2020/STAT 4996/Votingdata_demographic.csv", header = TRUE)
voting=voting[c(1,3,5,7,9,11,13,15,17,19,21,22,23)]
voting=voting[-c(52:320),]
names(voting)=c("State", "2016", "2012", "2008", "2004", "2000", "1996", "1992", "1988", "1984", "1980", 
                "1976", "1972")
fips = data.frame(state.abb) # Create state abbrivations table
DC_votes=subset(voting[-c(1:8,10:51),]) #Adding state abbriviation to DC
voting=voting[-c(9),]
DC_votes=cbind(state.abb = "DC", DC_votes) #merge voting and fips for DC and other states
voting=cbind(fips, voting)
registration_pct=rbind(voting, DC_votes)
registration_pct$state.abb #merging the state fips codes to state abbriviations.
fips_code=fips(c("AL", "AK", "AZ", "AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS",
                 "KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC",
                 "ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY","DC"))
registration_pct=cbind(registration_pct, fips_code)
registration_pct <- registration_pct[c(2,1,15,3:14)] #Reorder columns where fips code are adjacent to State.
names(registration_pct)=c("State", "State.abb", "FIPS", "2016 (%)", "2012 (%)", "2008 (%)", "2004 (%)", #Changing names
                         "2000 (%)", "1996 (%)", "1992 (%)", "1988 (%)", "1984 (%)", "1980 (%)", "1976 (%)", "1972 (%)")

#write.csv(registration_pct, paste("registration_pct", Sys.Date(), ".csv"))
write.csv(registration_pct,'registration_pct.csv')

