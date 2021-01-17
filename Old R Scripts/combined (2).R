##### Creating df1 #####
#setwd("C:/Users/Jan/OneDrive/Documents/UVA/UVA Spring 2020/STAT 4996") #setting the working directory
setwd("/Users/djcoo/Desktop/UVA/UVA Spring 2020/STAT 4996/")
library(tidyverse) #loading the packages that will be used, may need to install some packages
library(stringr)
library(splitstackshape)
library(tidycensus)
library(stringi)
library(plyr)
library(data.table)
dffacebook<-read.csv("FacebookAds.csv") #reading in the data
#fix the instances where clicks>impressions
dffacebook[c(1545, 2175, 2706, 3349, 3445),4]=c(152, 8610, 545, 19510, 119800)
dffacebook[c(1545),3]=c(100) #replace this with 100 according to the picture
#View(subset(adsall, rpm>5))
#this outlier is an incorrect adspend/typo, so fix
fixind=which(dffacebook$AdSpend==27500.00)
dffacebook[fixind,21]=c(2500)
df1<-dffacebook #copy dffacebook to make df1
df1<-df1[!(is.na(df1$Impressions)),] #keep all the rows that don't have missing values for impressions
df1<-df1[!(is.na(df1$Clicks)),] #keep all the rows that don't have missing values for clicks
df1$CreationDate<-str_sub(df1$CreationDate,start=1,end=8) #grabs the date for each ad
df1$CreationDate<-as.Date(df1$CreationDate,format='%m/%d/%y') #turns the dates into R recognizable date form
df1<-df1[!(is.na(df1$CreationDate)),] #keep all the rows that have a creation date
df1$EndDate<-str_sub(df1$EndDate,start=1,end=8) #grabs the date for each ad
df1$EndDate<-as.Date(df1$EndDate,format='%m/%d/%y') #turns the dates into R recognizable date form
df1[which(is.na(df1$EndDate)==TRUE),]$EndDate<-'2017/05/26'
df1$AdDuration<-df1$EndDate-df1$CreationDate
df1<-subset(df1,df1$Impressions!=0) #keep all the rows that have at least one impression
df1<-subset(df1,df1$Clicks!=0) #keep all the rows that have at least one Clicks ###
df1<-subset(df1,df1$Location!="") #keep all the rows that have a location
df1$AdWordCount<-str_count(df1$AdText,"\\w+") #get a count of all the words for each row
df1<-subset(df1,df1$AdWordCount!=0) #keep all the rows that have an ad
df1<-df1[,c("AdID","AdText","AdWordCount","Clicks","Impressions","Location","CreationDate","EndDate","AdDuration","AdSpend")] #subset the data by specified columns
df1<-subset(df1,df1$Location!="Living in: United States") #get rid of specific locations
df1<-subset(df1,df1$Location!="Living In: United States")
df1<-subset(df1,df1$Location!="United States")
df1<-df1[str_detect(df1$Location,"Canada")==FALSE,]
df1<-df1[str_detect(df1$Location,"Germnay")==FALSE,]
df1<-df1[str_detect(df1$Location,"United Kingdom")==FALSE,]
df1<-df1[str_detect(df1$Location,"Russia")==FALSE,]

###
# Loop for state dummies
l = as.list(state.name) #Create list for state dummies
l[[51]] = 'District of Columbia' #Adds DC to state dummies
df2 = as.data.frame(matrix(,nrow = nrow(df1), ncol = length(l))) #Creates dataframe with the length of observations in df2
names(df2) = l #dataframe 'l' is equal to df2; joins the state dummies into the df2 dataframe

# The loop counts each time a state dummy coinsides with a "Location" that have the same characters
for (i in 1:length(l)) {
  df2[,i] = ifelse(str_detect(df1$Location, l[[i]]),1,0)
}

df1 = cbind(df1,df2) #df1 is joined with df2, giving df1 the additional predictors

# This counts each time the state dummy "District of Columbis "coinsides with a 
# "Location" that is "Washington". The "Location" column does not include 
# "District of Columbia", only "Washington" represents D.C.
df1$Washington[which(df1$`District of Columbia` == 1)] = 0   
rownames(df1) = 1:nrow(df1) #Reindexing the rows according to the count of observations
###

removewords<-c("Living In:","United States","United States:","Latitude .....",
               "Longitude .....","[()]","km","mi","[+]") #name the characters that we want to get rid of in location
df3<-as.data.frame(sapply(df1,function(x)gsub(paste(removewords,collapse="|"),"",x))) #clean up the location rows
#df1$Impressions<-as.numeric(df1$Impressions)
df1$Location<-df3$Location
df1$Location<-str_replace_all(df1$Location,"[[:digit:]]", "") #get rid off all numbers in location
df1$Location<-str_replace_all(df1$Location,":",";") #replace all : for ; in order to split rows 
df1<-separate_rows(df1,Location,sep=";") #splits locations into different rows by ;
df1[322,'Location']<-c('Ferguson, St. Louis Missouri; Cleveland Ohio') #rename location cells that were inputed incorrectly
df1[540,'Location']<-c('Austin, Houston Texas')
df1[566,'Location']<-c('Massachusetts; Ferguson Missouri')
df1[569,'Location']<-c('New Jersey; New York New York; Ohio')
df1[726,'Location']<-c('Illinois; Virginia')
df1[736,'Location']<-c('Georgia; Alabama')
df1[c(932,939),'Location']<-c('Illinois; Michigan')
df1[c(927,923),'Location']<-c('Arizona; Texas')
df1<-separate_rows(df1,Location,sep=";") #split the cells by ; again
df1$StateFips<-fips_codes[match(stri_extract_last_words(df1$Location),
                                stri_extract_last_words(fips_codes$state_name)),2] #put fips codes by state
df1[732,]$StateFips<-54 #deal with West Virginia
df1[c(680,764,766,767,832,835,869,907,965),]$StateFips<-45 #deal with South Carolina
#census_api_key('f64f5a5d0f9deacfbbc623e6e01bad70bbe7af34',overwrite=TRUE) #for Jan
dfpop<-as.data.frame(get_estimates(geography="state",product="population",year=2016,keep_geo_vars=TRUE)) #create a location dataframe
dfpop$GEOID<-as.numeric(dfpop$GEOID) #make fip codes numeric
dfpop$GEOID<-str_pad(dfpop$GEOID,2,pad="0") #add a leading zero to single digit zip codes
df1$StatePopulation<-dfpop[match(df1$StateFips,dfpop$GEOID),4] #match the dataframes by fips code and input state population
df1$StateName<-fips_codes[match(df1$StateFips,fips_codes$state_code),3] #match dataframes by fips codes and input state name

##### Creating presdf #####
#setwd("/Users/djcoo/Desktop/UVA/UVA Spring 2020/STAT 4996/")
#setwd("C:/Users/Jan/OneDrive/Documents/UVA/UVA Spring 2020/STAT 4996") #setting the working directory
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
                       rep(turnout$`1980`,each=2),rep(turnout$`1976`,each=2)) #insert new column. I inserted the voter turn out
#rate by creating a new column 
#Voting_demo<-read.csv("/Users/djcoo/Desktop/UVA/UVA Spring 2020/STAT 4996/Votingdata_demographic.csv", 
#                     header = TRUE) # Read in voting and registration by demographics data; then clean the table
PctPov<-read.csv("PctPov_state_18_15.csv",header = TRUE)
MedHHInc<-read.csv("MedHHInc_state_18_15_18dollars.csv",header = TRUE)
MedHHInc$MedHHInc_18<-as.numeric(as.character(gsub(",","", MedHHInc$MedHHInc_18))) # removes commas
MedHHInc$MedHHInc_17<-as.numeric(as.character(gsub(",","", MedHHInc$MedHHInc_17))) # removes commas 
MedHHInc$MedHHInc_16<-as.numeric(as.character(gsub(",","", MedHHInc$MedHHInc_16))) # removes commas 
MedHHInc$MedHHInc_15<-as.numeric(as.character(gsub(",","", MedHHInc$MedHHInc_15))) # removes commas 
PctPov$Num_Pov18<-as.numeric(as.character(gsub(",","", PctPov$Num_Pov18))) # removes commas 
PctPov$Num_Pov17<-as.numeric(as.character(gsub(",","", PctPov$Num_Pov17))) # removes commas 
PctPov$Num_Pov16<-as.numeric(as.character(gsub(",","", PctPov$Num_Pov16))) # removes commas 
PctPov$Num_Pov15<-as.numeric(as.character(gsub(",","", PctPov$Num_Pov15))) # removes commas 
PctPov[,c(2,4,6,8)]<-PctPov[,c(2,4,6,8)]*1000 #add trailing zero for population in thousands
presdf<-merge(presdf, PctPov, by.x="state", by.y="state", sort = TRUE) #merge two datasets "presdf" and "Voting_demo".
presdf<-merge(presdf, MedHHInc, by.x="state", by.y="state", sort = TRUE) #merge two datasets "presdf" and "Voting_demo".

###### creating df2 #####
presdfrep<-subset(presdf,presdf$party=='republican' & presdf$year=='2016') #subset all republican and 2016 data
colnames(presdfrep)[5] <- "republican" #rename three columns
colnames(presdfrep)[6] <- "repcandidatevotes"
colnames(presdfrep)[8] <- "reppercentvotes"
presdfdem<-subset(presdf,presdf$party=='democrat' & presdf$year=='2016') #subset all democrat and 2016 data
colnames(presdfdem)[5] <- "democrat" #rename three columns
colnames(presdfdem)[6] <- "demcandidatevotes"
colnames(presdfdem)[8] <- "dempercentvotes"
presdfdem<-presdfdem[,c(4,5,6,8)] #subset democrat dataframe by columns of interest
presdfboth<-merge(presdfrep,presdfdem,by.x="state_fips",by.y="state_fips",sort = TRUE) #merge subsetted democrat data to full republican data
presdfboth2<-presdfboth[,c(1:6,8,22:24,7,9:21)] #re-order some columns
df2<-merge(df1,presdfboth2,by.x="StateFips",by.y="state_fips",sort = TRUE) #merge df1 and new combines republican and democrat data
df2<-arrange(df2,AdID) #organize data by AdID
df2<-df2[,c(2:6,7,8:11,63:66,1,67:87,12:62)] #re-order some columns
colnames(df2)[14]<-"election_year" #rename the column year to election_year

##### Dummy variables for States (For analysis) #####
#df2$StateFips=as.numeric(df2$StateFips) #changing "state_fips" into numeric value for dummy variables.
#df2<-fastDummies::dummy_cols(df2,select_columns='state_po') #turning state_po into binary varibales
#setnames(df2,old=colnames(df2[c(36:86)]),new=str_sub(colnames(df2[c(36:86)]),start=-2)) #renaming the new binary columns

##### Looking at the distribution of the dates #####
adcreationdate<-df2[!duplicated(df2[,c("AdID")]),] #create a new dataframe with individual ads
datespread<-max(adcreationdate$CreationDate)-min(adcreationdate$CreationDate) #get the distribution of dates
datespread/14 #finding the number of biweekly data we have (for density bins)
ggplot(adcreationdate,aes(x=CreationDate))+geom_histogram(aes(y=..density..),position='identity',alpha=0.5,bins=51) #plotting the date distribution 
cat('Number of dates after 2016-11-29 is:', length(which(adcreationdate$CreationDate>'2016-11-29')))

##### Creating Vars for Demographics #####

##### Number of ads in each state #####
sort(table(df2$state_po),decreasing=TRUE)

##### For events, dates, and key words #####
#adcreationdate<-df2[!duplicated(df2[,c("AdID")]),] #create a new dataframe with individual ads (already done above)
length(which(adcreationdate$CreationDate>="2016-07-07" & adcreationdate$CreationDate<="2016-08-20")) #finding the number of ads between two dates
which(adcreationdate$CreationDate>="2016-07-07" & adcreationdate$CreationDate<="2016-08-20") #printing which ads are between two dates

adcreationdate[5,2] #printing a specific AdText
str_detect(adcreationdate[5,2],fixed("hate",ignore_case=TRUE)) #checking a key word presence in a specific AdText

textsamp<-subset(adcreationdate,adcreationdate$CreationDate>="2016-07-07" & adcreationdate$CreationDate<="2016-08-20",select=c("AdID","AdText")) #subsetting ads between two dates
table(str_detect(textsamp$AdText,fixed("black",ignore_case=TRUE))) #checking how many ads had a key word

race<-c('black|cop|hate') #set of key words
table(str_detect(tolower(df2$AdText),race)) #checking how many ads had a key word present in the set
str_count(df2$AdText, race) #counts the number of times a word in race is present in each set

##### Finding the frequency of all the words #####
#install.packages('tm')
#install.packages('wordcloud')
library(tm)
library(wordcloud)
uniquetext<-df2[!duplicated(df2[,c("AdID")]),] #creating a dataframe with unique AdID so we don't double count words (drop duplicates)
docs <- Corpus(VectorSource(uniquetext$AdText)) #creating AdText in uniquetext into a Corpus format
dtm <- TermDocumentMatrix(docs) #making all the AdText into another format
m <- as.matrix(dtm) #creating the new string as a matrix
v <- sort(rowSums(m),decreasing=TRUE) #sorting the frequency of words
d <- data.frame(word = names(v),freq=v) #creating a dataframe which has the word and its frequency
head(d, 10)

findAssocs(dtm, terms = "police", corlimit = 0.3) #finds the words with the highest correlations with black (most commonly matched?)

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

##### Specific Ad Target Count by State #####
x <- as.data.frame(table(df2$AdID))
df2 <- merge(df2, x,by.x="AdID",by.y="Var1")
colnames(df2)[88]<-"Ad_Freq_state"
df2<-df2[,c(1:10,88,11:87)] #sorts the columns by interest

#Reindex AdID in a different column
x <- interaction(df2$AdID) 
x <- factor(x, levels=levels(x)[order(unique(df2$AdID))])
df2$AdID.idx <- as.integer(x)
df2<-df2[,c(1,89,2:88)]

##### Creating AdCount and CTR #####
#AdCount<-sort(table(df2$state_po),decreasing=TRUE) #creating AdCount, number of ads in each state (double counts)
#df2<-merge(df2,AdCount,by.x="state_po",by.y="Var1") #merges AdCount into df2 to have AdCount in df2
#colnames(df2)[87]<-'AdCount' #renames the new column as AdCount
#df2<-arrange(df2,AdID) #arranges df2 by AdID
#df2<-df2[,c(2:10,87,11:13,1,14:86)] #sorts the columns by interest
df2$CTR<-df2$Clicks/df2$Impressions #creates CTR
df2<-df2[,c(1:6,90,7:89)] #sorts the columns by interest

##### Creating a Binary for AdYear if it was election year or not #####
df2$AdYear<-format(as.Date(df2$CreationDate, format="%d/%m/%Y"),"%Y") #creates a new column for the year the ad was created
df2$AdYear<-ifelse(df2$AdYear==2016,1,0) #makes it 1 if ad was created in 2016 or 0 if otherwise
df2<-df2[,c(1:8,91,9:90)] #sorts the columns by interest

##### Read in Race and Ethnicity data and merge to df2 #####
Race_Ethnicity<-read.csv("Race_Ethnicity_Clean.csv") #reading in the data
df2<-merge(df2, Race_Ethnicity, by.x="state",by.y="State")

for ( col in 1:ncol(df2)){
  colnames(df2)[col] <-  sub("X", "", colnames(df2)[col]) #Remove unnecessary X from columns
}

colnames(df2)[92]<-"State"
colnames(df2)

##### Don't worry, be happy :) #####

# install and load the 'plm' package
## install.packages("plm")
library(plm)
library(lmtest)
library(tidycensus)
library(MASS)
library(Matrix)
library(lfe)
library(car)
library(gcookbook)
require(moonBook)
require(ggplot2)
require(ggiraph)
require(ggiraphExtra)
library(robustbase)


# Dummies
#+ MD + MI + OH + MI + GA + TX + CA + FL + WI + LA + MI + VA + DC + NC + MI 
#+ PA + WA + AL + NM - NM

# Demographics

#For copy and pasting
#Pct_Pov17 + Pct_Pov16 + Pct_Pov15 + MedHHInc_17 + MedHHInc_16 + MedHHInc_15 + 

#Mean Percent Poverty for each state
df2$PctPov_Mean<-((df2$Pct_Pov17 + df2$Pct_Pov16 + df2$Pct_Pov15)/3*100)

#Mean Median HH Income for each state
df2$MedHHInc_Mean<-((df2$MedHHInc_17 + df2$MedHHInc_16 + df2$MedHHInc_15)/3*100)

median(df2$AdCount)

######### Regressions ##########

`Alabama` + `Arkansas` + `Arizona` + `California` + `District of Columbia` + 
`Delaware` + `Florida` + `Georgia` + `Iowa` + `Idaho` + `Illinois` + `Kansas` + 
`Louisiana` + `Massachusetts` + `Maryland` + `Michigan` + `Minnesota` + 
`Missouri` + `Mississippi` + `North Carolina` + `North Dakota` + 
`New Jersey` + `New Mexico` + `New York` + `Ohio` + `Oklahoma` + `Pennsylvania` + 
`South Carolina` + `Tennessee` + `Texas` + `Virginia` + `Vermont` + `Washington` + 
`Wisconsin` + `West Virginia`

### Best bet Model: New York as Baseline ###

    ClickEfficacy1  <- lmrob(log(CTR*100) ~ AdDuration + Ad_Freq_state + `Alabama` + `Arkansas` + `Arizona` + 
                           `California` + `District of Columbia` + `Delaware` + `Florida` + `Georgia` + `Iowa` + 
                           `Idaho` + `Illinois` + `Kansas` + `Louisiana` + `Massachusetts` + `New York` + `Michigan` + 
                           `Minnesota` + `Missouri` + `Mississippi` + `North Carolina` + `New Jersey` + `New Mexico` + 
                           `Ohio` + `Oklahoma` + `Pennsylvania` + `South Carolina` + `Tennessee` + `Texas` + `Virginia` + 
                           `Vermont` + `Washington` + `Wisconsin` + `West Virginia`, data = df2, setting="KS2014")
    
    ClickEfficacy1 <- lm(log(CTR*100) ~ AdWordCount + AdDuration + Ad_Freq_state + AdYear + `Alabama` + `Arkansas` + `Arizona` + 
                              `California` + `District of Columbia` + `Delaware` + `Florida` + `Georgia` + `Iowa` + 
                              `Idaho` + `Illinois` + `Kansas` + `Louisiana` + `Massachusetts` + `New York` + `Michigan` + 
                              `Minnesota` + `Missouri` + `Mississippi` + `North Carolina` + `New Jersey` + `New Mexico` + 
                              `Ohio` + `Oklahoma` + `Pennsylvania` + `South Carolina` + `Tennessee` + `Texas` + `Virginia` + 
                              `Vermont` + `Washington` + `Wisconsin` + `West Virginia`, data = df2)
    
    summary(ClickEfficacy1)

    summary(ClickEfficacy1, robust=T)  # For fixed-effects
    anova(ClickEfficacy1)
    step1 <- stepAIC(ClickEfficacy1, direction="both")
    #Save Predicted plots and residuals
    df2$predicted <- predict(ClickEfficacy1)
    df2$residuals <- residuals(ClickEfficacy1)
    df2 %>% select(CTR, predicted, residuals) %>% head()
    #Residual Plot
    library(ggplot2)
    ggplot(df2, aes(x = AdWordCount, y = CTR, color=AdID.idx)) +
      geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
      geom_segment(aes(xend = AdWordCount, yend = predicted), alpha = .2) +  # alpha to fade lines
      geom_point() +
      geom_point(aes(y = predicted), shape = 1) +
      theme_bw() +  # Add theme for cleaner look
    geom_point(aes(color = residuals)) +  # Color mapped here
      scale_color_gradient2(low = "blue", high = "red") +  # Colors to use here
      guides(color = FALSE) +
      geom_point(aes(y = predicted), shape = 1) +
      theme_bw()

#### Diagnostics ####

plot(ClickEfficacy1)
    
# Assessing Outliers
outlierTest(ClickEfficacy1) # Bonferonni p-value for most extreme obs
qqPlot(ClickEfficacy1, main="QQ Plot") #qq plot for studentized resid
leveragePlots(ClickEfficacy1) # leverage plots

# Influential Observations
# added variable plots
av.Plots(ClickEfficacy1)
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(df2)-length(ClickEfficacy1$coefficients)-2))
plot(ClickEfficacy1, which=4, cook.levels=cutoff)
# Influence Plot
influencePlot(ClickEfficacy1, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )


# Normality of Residuals
# qq plot for studentized resid
qqPlot(ClickEfficacy1, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(ClickEfficacy1)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)


# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(ClickEfficacy1)
# plot studentized residuals vs. fitted values
spreadLevelPlot(ClickEfficacy1)


# Evaluate Collinearity
vif(ClickEfficacy1) # variance inflation factors
sqrt(vif(ClickEfficacy1)) > 2 # problem?


# Evaluate Nonlinearity
# component + residual plot
crPlots(ClickEfficacy1)
# Ceres plots
ceresPlots(ClickEfficacy1)


# Test for Autocorrelated Errors
durbinWatsonTest(ClickEfficacy1)

# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(ClickEfficacy1)
summary(gvmodel)


###


qqnorm(df2$Clicks)
qqline(df2$Clicks)

qqnorm(df2$Impressions)
qqline(df2$Impressions)

qqnorm(df2$CTR)
qqline(df2$CTR)    
    
    

### Extract Coefficients for Secondary model with demographics

    df2_1=df2
    
    model <- ddply(df2_1, "state", function(x) {
      model <- lm(CTR*100 ~ AdWordCount + AdDuration + Ad_Freq_state + AdYear + `Alabama` + `Arkansas` + `Arizona` + 
                    `California` + `District of Columbia` + `Delaware` + `Florida` + `Georgia` + `Iowa` + 
                    `Idaho` + `Illinois` + `Kansas` + `Louisiana` + `Massachusetts` + `Maryland` + `Michigan` + 
                    `Minnesota` + `Missouri` + `Mississippi` + `North Carolina` + `New Jersey` + `New Mexico` + 
                    `Ohio` + `Oklahoma` + `Pennsylvania` + `South Carolina` + `Tennessee` + `Texas` + `Virginia` + 
                    `Vermont` + `Washington` + `Wisconsin` + `West Virginia`, data = df2)
      coef(model)
    })
    
    colnames(model) <- paste(colnames(model), "coeff", sep = "_")
    colnames(model)
    df2_1 <- merge(df2_1, model, by.x="state",by.y="state_coeff")
    colnames(df2_1)
    

### Secondary model with demographics

    AdEfficacy <- lm(Texas_coeff ~ StatePopulation + Pct_Pov16 + MedHHInc_16 + `2016_Pop_Male` + `2016_Pop_Female`
                     + `2016_Pop_White` + `2016_Pop_Black` + `2016_Pop_Asian` + `2016_Pop_Other_race` + `2016_Pop_Hispanic_Latino_`
                     + `2016_Pop_Two_or_more_races`, data = df2_1)
    summary(AdEfficacy)

    AdEfficacy <- lm(Oklahoma_coeff ~ StatePopulation + PctPov_Mean + MedHHInc_Mean, data = df2_1)
    summary(AdEfficacy)

    #Summary
    summary(AdEfficacy)
    #Save Predicted plots and residuals
    df2$predicted <- predict(AdEfficacy)
    df2$residuals <- residuals(AdEfficacy)
    df2 %>% select(Impressions, predicted, residuals) %>% head()
    #Residual Plot
    library(ggplot2)
    ggplot(df2, aes(x = Clicks, y = Impressions, color=AdID.idx)) +
      #xlim(c(0,10000)) + 
      #ylim(c(0,250000)) +
      geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
      geom_segment(aes(xend = Clicks, yend = predicted), alpha = .2) +  # alpha to fade lines
      geom_point() +
      geom_point(aes(y = predicted), shape = 1) +
      theme_bw()  # Add theme for cleaner look
    
    
###Alt. Model###

    ClickEfficacy2 <- lm(Impressions ~ Clicks + `Alabama` + `Arkansas` + `Arizona` + `California` + `District of Columbia` + 
                           `Delaware` + `Florida` + `Georgia` + `Iowa` + `Idaho` + `Illinois` + `Kansas` + 
                           `Louisiana` + `Massachusetts` + `Maryland` + `Michigan` + `Minnesota` + 
                           `Missouri` + `Mississippi` + `North Carolina` + `North Dakota` + 
                           `New Jersey` + `New Mexico` + `Ohio` + `Oklahoma` + `Pennsylvania` + 
                           `South Carolina` + `Tennessee` + `Texas` + `Virginia` + `Vermont` + `Washington` + 
                           `Wisconsin` + `West Virginia`, data = df2)
    #Summary
    summary(ClickEfficacy2)
    #Save Predicted plots and residuals
    df2$predicted <- predict(ClickEfficacy2)
    df2$residuals <- residuals(ClickEfficacy2)
    df2 %>% select(Impressions, predicted, residuals) %>% head()
    #Residual Plot
    library(ggplot2)
    ggplot(df2, aes(x = Clicks, y = Impressions, color=AdID.idx)) +
      #xlim(c(0,10000)) + 
      #ylim(c(0,250000)) +
      geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
      geom_segment(aes(xend = Clicks, yend = predicted), alpha = .2) +  # alpha to fade lines
      geom_point() +
      geom_point(aes(y = predicted), shape = 1) +
      theme_bw()  # Add theme for cleaner look

    
######

# OR library(tidyverse)

qqplot(rt(1000,df=3), x, main="t(3) Q-Q Plot",
       ylab="Sample Quantiles")
abline(0,1)

fitdistr(df2$CTR*100, "exponential")

####
qplot(state_po, CTR*100, data = df2, facets = . ~ state_po)
boxplot(CTR*100 ~ MedHHInc_17, data = df2, col = "blue")


equation1=function(x){coef(ClickEfficacy1)[2]+coef(ClickEfficacy1)[1]+coef(ClickEfficacy1)[3]}

ggplot(radial,aes(y=CTR*100,x=AdCount,color=df2))+geom_point()+
  stat_function(fun=equation1,geom="line",color=scales::hue_pal()(2)[1])+
  stat_function(fun=equation2,geom="line",color=scales::hue_pal()(2)[2])


ggplot(df2, aes(y=CTR*100,x=MedHHInc_17,color=state_po))+geom_point()+geom_smooth(method="lm",se=FALSE)+facet_wrap(.~state_po, scales="free")


########
class(df2$White_pct_V)

cor(as.numeric(df2$AdCount),as.numeric(df2$Clicks))
coeftest(ClickEfficacy1, vcov. = vcovHC, type = "HC1")

# estimate the fixed effects regression with plm()
ClickEfficacy2 <- plm(Clicks ~ CTR, 
                      data = df2,
                      index = c("StateFips"), 
                      model = "within")

# print summary using robust standard errors
coeftest(ClickEfficacy2, vcov. = vcovHC, type = "HC1")

unique(df2$state_po)