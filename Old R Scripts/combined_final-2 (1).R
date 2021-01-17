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
df3 = as.data.frame(matrix(,nrow = nrow(df1), ncol = length(l))) #Creates dataframe with the length of observations in df2
names(df3) = l #dataframe 'l' is equal to df2; joins the state dummies into the df2 dataframe

# The loop counts each time a state dummy coinsides with a "Location" that have the same characters
for (i in 1:length(l)) {
  df3[,i] = ifelse(str_detect(df1$Location, l[[i]]),1,0)
}

df1 = cbind(df1,df3) #df1 is joined with df2, giving df1 the additional predictors

# This counts each time the state dummy "District of Columbis "coinsides with a 
# "Location" that is "Washington". The "Location" column does not include 
# "District of Columbia", only "Washington" represents D.C.
df1$Washington[which(df1$`District of Columbia` == 1)] = 0 
df3$Washington[which(df3$`District of Columbia` == 1)] = 0
rownames(df1) = 1:nrow(df1) #Reindexing the rows according to the count of observations
###

#census_api_key('f64f5a5d0f9deacfbbc623e6e01bad70bbe7af34',overwrite=TRUE) #for Jan
dfpop<-as.data.frame(get_estimates(geography="state",product="population",year=2016,keep_geo_vars=TRUE)) #create a location dataframe
dfpop$GEOID<-as.numeric(dfpop$GEOID) #make fip codes numeric
dfpop$GEOID<-str_pad(dfpop$GEOID,2,pad="0") #add a leading zero to single digit zip codes

#df1$StatePopulation<-dfpop[match(df1$StateFips,dfpop$GEOID),4] #match the dataframes by fips code and input state population
#df1$StateName<-fips_codes[match(df1$StateFips,fips_codes$state_code),3] #match dataframes by fips codes and input state name

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
turnout$FIPS<-str_pad(turnout$FIPS, width=2, side = 'left', pad="0") #add leading zero to FIPS values with 1 digit.
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

##### Dummy variables for States (For analysis) #####
#df2$StateFips=as.numeric(df2$StateFips) #changing "state_fips" into numeric value for dummy variables.
#df2<-fastDummies::dummy_cols(df2,select_columns='state_po') #turning state_po into binary varibales
#setnames(df2,old=colnames(df2[c(36:86)]),new=str_sub(colnames(df2[c(36:86)]),start=-2)) #renaming the new binary columns

##### Looking at the distribution of the dates #####

datespread = max(df1$CreationDate)-min(df1$CreationDate)

datespread/14 #finding the number of biweekly data we have (for density bins)
ggplot(df1,aes(x=CreationDate))+geom_histogram(aes(y=..density..),position='identity',alpha=0.5,bins=51) #plotting the date distribution 
cat('Number of dates after 2016-11-29 is:', length(which(df1$CreationDate>'2016-11-29')))

##### Creating Vars for Demographics #####

##### Number of ads in each state #####
sort(table(df2$state_po),decreasing=TRUE)
statefreq = colSums(df3)

##### For events, dates, and key words #####
#adcreationdate<-df2[!duplicated(df2[,c("AdID")]),] #create a new dataframe with individual ads (already done above)
adcreationdate = df1
length(which(adcreationdate$CreationDate>="2016-07-07" & adcreationdate$CreationDate<="2016-08-20")) #finding the number of ads between two dates
which(adcreationdate$CreationDate>="2016-07-07" & adcreationdate$CreationDate<="2016-08-20") #printing which ads are between two dates

adcreationdate[5,2] #printing a specific AdText
str_detect(adcreationdate[5,2],fixed("hate",ignore_case=TRUE)) #checking a key word presence in a specific AdText

textsamp<-subset(adcreationdate,adcreationdate$CreationDate>="2016-07-07" & adcreationdate$CreationDate<="2016-08-20",select=c("AdID","AdText")) #subsetting ads between two dates
table(str_detect(textsamp$AdText,fixed("black",ignore_case=TRUE))) #checking how many ads had a key word

race<-c('black|cop|hate') #set of key words
table(str_detect(tolower(df2$AdText),race)) #checking how many ads had a key word present in the set
str_count(df1$AdText, race) #counts the number of times a word in race is present in each set

##### Finding the frequency of all the words #####
#install.packages('tm')
#install.packages('wordcloud')
library(tm)
library(wordcloud)
 #creating a dataframe with unique AdID so we don't double count words (drop duplicates)

uniquetext = df1
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

df1$Ad_Freq_state = rowSums(df3)


##### Creating AdCount and CTR #####
#AdCount<-sort(table(df2$state_po),decreasing=TRUE) #creating AdCount, number of ads in each state (double counts)
#df2<-merge(df2,AdCount,by.x="state_po",by.y="Var1") #merges AdCount into df2 to have AdCount in df2
#colnames(df2)[87]<-'AdCount' #renames the new column as AdCount
#df2<-arrange(df2,AdID) #arranges df2 by AdID
#df2<-df2[,c(2:10,87,11:13,1,14:86)] #sorts the columns by interest
df2 = df1
df2$CTR<-df2$Clicks/df2$Impressions #creates CTR
df2$AdDuration<-as.numeric(as.character(df2$AdDuration))



##### Creating a Binary for AdYear if it was election year or not as well as all years for ads #####
df2$AdYear<-format(as.Date(df2$CreationDate, format="%d/%m/%Y"),"%Y") #creates a new column for the year the ad was created
df2$ElectYear<-ifelse(df2$AdYear==2016,1,0) #makes it 0 if ad was created in 2016 or 0 if otherwise
df2$AdYear15<-ifelse(df2$AdYear==2015,1,0) #makes it 1 if ad was created in 2015 or 0 if otherwise
df2$AdYear16<-ifelse(df2$AdYear==2016,1,0) #makes it 1 if ad was created in 2016 or 0 if otherwise
df2$AdYear17<-ifelse(df2$AdYear==2017,1,0) #makes it 1 if ad was created in 2017 or 0 if otherwise


##### Read in Race and Ethnicity data and merge to df2 #####
Race_Ethnicity<-read.csv("Race_Ethnicity_Clean.csv") #reading in the data

##### Read in Connectivity and merge to df2 #####
Connectivity<-read.csv("morevariables.csv") #reading in the data

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
library(rlang)
library(olsrr)


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

### Exploratory Data Analysis (EDA) ###
    
    # AdWordCount #

    ggplot(data = df2) +
      geom_histogram(mapping = aes(x = AdWordCount), binwidth = 10)

    ggplot(data = df2) +
      geom_point(mapping = aes(x = AdWordCount, y = CTR*100))
    
    ggplot(data = df2, mapping = aes(x = AdWordCount, y = CTR*100)) + 
      geom_boxplot(mapping = aes(group = cut_width(AdWordCount, .1)))
    
    # AdDuration #
    
    ggplot(data = df2) +
      geom_histogram(mapping = aes(x = AdDuration), binwidth = 20)
    
    ggplot(data = df2) +
      geom_point(mapping = aes(x = AdDuration, y = CTR*100))
    
    ggplot(data = df2, mapping = aes(x = AdDuration, y = CTR*100)) + 
      geom_boxplot(mapping = aes(group = cut_width(AdDuration, .1)))
    
    # ElectYear #
    
    ggplot(data = df2) +
      geom_point(mapping = aes(x = ElectYear, y = CTR*100))
    
    ggplot(data = df2, mapping = aes(x = ElectYear, y = CTR*100)) + 
      geom_boxplot(mapping = aes(group = cut_width(ElectYear, .1)))
    
    # Ad_Freq_state #
    
    ggplot(data = df2) +
      geom_histogram(mapping = aes(x = Ad_Freq_state), binwidth = .5)
    
    ggplot(data = df2) +
      geom_point(mapping = aes(x = Ad_Freq_state, y = CTR*100))
    
    ggplot(data = df2, mapping = aes(x = Ad_Freq_state, y = CTR*100)) + 
      geom_boxplot(mapping = aes(group = cut_width(Ad_Freq_state, .1)))
    
    # connectivity: possibly an inverse transformation #
    
    df2$connectivity=((df2$connectivity)^-1)
    
    ggplot(data = df2) +
      geom_histogram(mapping = aes(x = connectivity, binwidth = .01))
    
    ggplot(data = df2) +
      geom_point(mapping = aes(x = connectivity, y = CTR*100))
    
    ggplot(data = df2, mapping = aes(x = connectivity, y = CTR*100)) + 
      geom_boxplot(mapping = aes(group = cut_width(connectivity, .01)))
    
    # RPM: possibly an inverse transformation #
    
    df2$ln_rpm=(log(df2$rpm))
    
    ggplot(data = df2) +
      geom_histogram(mapping = aes(x = rpm, binwidth = 1))
    
    ggplot(data = df2) +
      geom_point(mapping = aes(x = log(rpm), y = CTR*100))
    
    ggplot(data = df2, mapping = aes(x = rpm, y = CTR*100)) + 
      geom_boxplot(mapping = aes(group = cut_width(rpm, .01)))
    
    # tor: possibly an inverse transformation #
    
    ggplot(data = df2) +
      geom_histogram(mapping = aes(x = tor, binwidth = .01))
    
    ggplot(data = df2) +
      geom_point(mapping = aes(x = tor, y = CTR*100))
    
    ggplot(data = df2, mapping = aes(x = tor, y = CTR*100)) + 
      geom_boxplot(mapping = aes(group = cut_width(tor, .01)))
    

######### Regressions ##########

`Alabama` + `Arkansas` + `Arizona` + `California` + `District of Columbia` + 
`Delaware` + `Florida` + `Georgia` + `Iowa` + `Idaho` + `Illinois` + `Kansas` + 
`Louisiana` + `Massachusetts` + `Maryland` + `Michigan` + `Minnesota` + 
`Missouri` + `Mississippi` + `North Carolina` + `North Dakota` + 
`New Jersey` + `New Mexico` + `New York` + `Ohio` + `Oklahoma` + `Pennsylvania` + 
`South Carolina` + `Tennessee` + `Texas` + `Virginia` + `Vermont` + `Washington` + 
`Wisconsin` + `West Virginia`

### Best Model: Maryland as Baseline ###

    
    df_corr<-df2[,c("AdWordCount","AdDuration","Ad_Freq_state","ElectYear")]
    ClickEfficacy_corr <- lm((CTR*100) ~ AdWordCount + AdDuration + Ad_Freq_state + ElectYear, data = df2)
    
    library(stargazer)
    library(performance)
    library(corrplot)
    
    vif(ClickEfficacy_corr)
    check_collinearity(ClickEfficacy_corr)
    CorrMatrix <- cor(df_corr) #Correlation Matrix as dataframe
    corrplot.mixed(CorrMatrix, lower.col = "black", number.cex = .7)

    summary(df2)
    
    df_corr<-df2[,c("AdWordCount","AdDuration","Ad_Freq_state","ElectYear")]
    # + Ad_Freq_state*ElectYear
    ClickEfficacy1 <- lm((CTR*100) ~ AdWordCount + AdDuration + Ad_Freq_state + ElectYear + `Alabama` + `Arkansas` + 
                           `Arizona` + `California` + `District of Columbia` + `Delaware` + `Florida` + `Georgia` + 
                           `Iowa` + `Idaho` + `Illinois` + `Kansas` + `Louisiana` + `Massachusetts` + `New York` + 
                           `Michigan` + `Minnesota` + `Missouri` + `Mississippi` + `North Carolina` + 
                           `New Jersey` + `New Mexico` + `New York` + `Ohio` + `Oklahoma` + `Pennsylvania` + 
                           `South Carolina` + `Tennessee` + `Texas` + `Virginia` + `Vermont` + `Washington` + 
                           `Wisconsin` + `West Virginia`, data = df2)
    
    #ClickEfficacy1 output
    library(jtools)
    library(huxtable)
    library(officer) 
    library(flextable)
    
    ols_plot_cooksd_bar(ClickEfficacy1)
    #ols_plot_dfbetas(ClickEfficacy1)
    ols_plot_dffits(ClickEfficacy1)
    ols_plot_resid_lev(ClickEfficacy1) 
    summ(ClickEfficacy1, digits = 5, render = 'normal_print')
    
### Box-Cox: Transformation for non-linearity ###
    
    summary(ClickEfficacy1)
    plot(ClickEfficacy1)
    
    
    bc=boxcox(ClickEfficacy1, lambda=seq(-2, 2))
    best.lam=bc$x[which(bc$y==max(bc$y))] # 0.383838383... 
    
    ClickEfficacy1_tr1 <- lm((CTR*100)^.38 ~ AdWordCount + AdDuration + Ad_Freq_state + ElectYear + `Alabama` + 
                              `Arkansas` + `Arizona` + `California` + `District of Columbia` + `Delaware` + `Florida` + 
                              `Georgia` + `Iowa` + `Idaho` + `Illinois` + `Kansas` + `Louisiana` + `Massachusetts` + `New York` + 
                              `Michigan` + `Minnesota` + `Missouri` + `Mississippi` + `North Carolina` + `New Jersey` + `New Mexico` + 
                              `Ohio` + `Oklahoma` + `Pennsylvania` + `South Carolina` + `Tennessee` + `Texas` + `Virginia` + 
                              `Vermont` + `Washington` + `Wisconsin` + `West Virginia`, data = df2)
    
### Box-Tidwell: Transformation of predictors ###

    boxTidwell((CTR*100)^.38 ~ AdWordCount + Ad_Freq_state, other.x=~ AdDuration + ElectYear + `Alabama` + 
                 `Arkansas` + `Arizona` + `California` + `District of Columbia` + `Delaware` + `Florida` + 
                 `Georgia` + `Iowa` + `Idaho` + `Illinois` + `Kansas` + `Louisiana` + `Massachusetts` + `New York` + 
                 `Michigan` + `Minnesota` + `Missouri` + `Mississippi` + `North Carolina` + `New Jersey` + `New Mexico` + 
                 `Ohio` + `Oklahoma` + `Pennsylvania` + `South Carolina` + `Tennessee` + `Texas` + `Virginia` + 
                 `Vermont` + `Washington` + `Wisconsin` + `West Virginia`, data = df2)
    
    # Recommended transformation of Ad_Freq_state #
    df2$sq_Ad_Freq_state=df2$Ad_Freq_state^2
    ClickEfficacy1_tr2 <- lm((CTR*100)^.38 ~ AdWordCount + AdDuration + sq_Ad_Freq_state + ElectYear + `Alabama` + 
                              `Arkansas` + `Arizona` + `California` + `District of Columbia` + `Delaware` + `Florida` + 
                              `Georgia` + `Iowa` + `Idaho` + `Illinois` + `Kansas` + `Louisiana` + `Massachusetts` + `New York` + 
                              `Michigan` + `Minnesota` + `Missouri` + `Mississippi` + `North Carolina` + `New Jersey` + `New Mexico` + 
                              `Ohio` + `Oklahoma` + `Pennsylvania` + `South Carolina` + `Tennessee` + `Texas` + `Virginia` + 
                              `Vermont` + `Washington` + `Wisconsin` + `West Virginia`, data = df2) # Transformation significant!
    
    summary(ClickEfficacy1_tr2)
    #summary(ClickEfficacy1_tr1)
    plot(ClickEfficacy1_tr2)
    
    ols_plot_cooksd_bar(ClickEfficacy1_tr1)
    #ols_plot_dfbetas(ClickEfficacy1_tr1)
    ols_plot_dffits(ClickEfficacy1_tr1)
    ols_plot_resid_lev(ClickEfficacy1_tr1)  

    
### Add Interactions ###
    library(interactions)
    
    ClickEfficacy1_int <- lm((CTR*100)^.38 ~ AdWordCount + AdDuration + sq_Ad_Freq_state + Ad_Freq_state*ElectYear + `Alabama` + 
                               `Arkansas` + `Arizona` + `California` + `District of Columbia` + `Delaware` + `Florida` + 
                               `Georgia` + `Iowa` + `Idaho` + `Illinois` + `Kansas` + `Louisiana` + `Massachusetts` + `New York` + 
                               `Michigan` + `Minnesota` + `Missouri` + `Mississippi` + `North Carolina` + `New Jersey` + `New Mexico` + 
                               `Ohio` + `Oklahoma` + `Pennsylvania` + `South Carolina` + `Tennessee` + `Texas` + `Virginia` + 
                               `Vermont` + `Washington` + `Wisconsin` + `West Virginia`, data = df2) # Interaction significant
    
    interact_plot(ClickEfficacy1_int, pred = Ad_Freq_state, modx = ElectYear)
    
    summary(ClickEfficacy1_int)
    plot(ClickEfficacy1_int)
    
    bc=boxcox(ClickEfficacy1_int, lambda=seq(-2, 2))
    best.lam=bc$x[which(bc$y==max(bc$y))] # 0.383838383...
    
    
### Jacknife proceedure ###
    library(lmboot)
    
    # ATTENTION: Although the interaction between Ad_Freq_state and ElectYear was significant, the Jackknife procedure with
    # the interaction resulted in severe violations of normality, linearity, and homoscedasticity. The Jackknife model contains
    # no independent variable interactions but does contain the transformation of Ad_Freq_state squared. Additionally, alternate 
    # models were run without the y-transformation and only x-transformations and the Ad_Freq_state and ElectYear interaction without success. 
    # The model calculated below is the best-resulting model with out any severe violations in normality, linearity, and homoscedasticity
    
    JackObj <- jackknife(((CTR*100)^.38) ~ AdWordCount + AdDuration + sq_Ad_Freq_state + ElectYear, data = df2)
    
    plot(JackObj)
    
    #plot the sampling distribution of the slope coefficient
    hist(JackObj$bootEstParam[,2], main="Jackknife Sampling Distn.",
         xlab="Slope Estimate") 
    
    # jackknife 95% CI for slope parameter (percentile method) #
    quantile(JackObj$bootEstParam[,2], probs=c(.025, .975))
    
    ClickEfficacy1_jk <- lm((CTR*100)^.38 ~ JackObj$bootEstParam + 
                             `Arkansas` + `Arizona` + `California` + `District of Columbia` + `Delaware` + `Florida` + 
                             `Georgia` + `Iowa` + `Idaho` + `Illinois` + `Kansas` + `Louisiana` + `Massachusetts` + `New York` + 
                             `Michigan` + `Minnesota` + `Missouri` + `Mississippi` + `North Carolina` + `New Jersey` + `New Mexico` + 
                             `Ohio` + `Oklahoma` + `Pennsylvania` + `South Carolina` + `Tennessee` + `Texas` + `Virginia` + 
                             `Vermont` + `Washington` + `Wisconsin` + `West Virginia`, data = df2)
    
    # New Model w/ jackknife #
    
    summary(ClickEfficacy1_jk)
    ols_plot_cooksd_bar(ClickEfficacy1_jk)
    #ols_plot_dfbetas(ClickEfficacy1_jk)
    ols_plot_dffits(ClickEfficacy1_jk)
    ols_plot_resid_stud(ClickEfficacy1_jk)
    ols_plot_resid_lev(ClickEfficacy1_jk)  
    
    anova(ClickEfficacy1_jk)
    plot(ClickEfficacy1_jk)

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



### Extract Coefficients for Secondary model with demographics
    df2_1<-df2[,c("CTR","AdID")]
    cities<-read.csv("lessdummies.csv") #reading in the data
    cities<-cities[,-c(1,7,73)]
    df2_1<-merge(df2_1,cities,by.x="AdID",by.y="AdID",sort = TRUE)
    
    for ( col in 1:ncol(Race_Ethnicity)){
      colnames(Race_Ethnicity)[col] <-  sub("X", "", colnames(Race_Ethnicity)[col]) #Remove unnecessary words/characters in columns
    }
    
    Race_Ethnicity<-Race_Ethnicity[,-c(1)]
    
    demo15<-Race_Ethnicity[,c(1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49,52,55,58,61)]
    demo16<-Race_Ethnicity[,c(1,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60)]
    demo17<-Race_Ethnicity[,c(1,2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50,53,56,59)]
    # 1st Model #
    
    median(df2_1$CTR)
    reg1<-lm((CTR*100)~., df2_1[,-1]) #exclude AdIDs so we can add back later
    summary(reg1)
    
    bc=boxcox(reg1, lambda=seq(-2, 2))
    best.lam=bc$x[which(bc$y==max(bc$y))] # 0.343434...
  
    # 2nd Model #
    reg2<-lm((CTR*100)^.34~., df2_1[,-1]) #exclude AdIDs so we can add back later
    summary(reg2)  
  
    # 3rd Model: Austin, Texas#
    JackObj2 <- jackknife(((CTR*100)^.34) ~ AfAm + male + connectivity + rpm + tor + WordCount, data = df2_1) #perform the jackknife
    
    reg3 <- lm((CTR*100)^.34 ~ JackObj2$bootEstParam + Alabama + Atlanta.Georgia + Atlanta.Louisiana +
                Baton.Rouge.Louisiana + Buffalo.New.York + California + Camden.New.Jersey + Charlotte.North.Carolina +       
                Chester.Pennsylvania + Chicago.Illinois + Cleveland.Ohio + Connecticut + Des.Plaines.Illinois + Detroit.Michigan +     
                Ferguson.Missouri + Florida + Gainesville.Florida + Georgia + Houston.Texas +      
                Illinois + Lancaster.California + Long.Beach.California + Los.Angeles.California + Louisiana + Madison.Wisconsin +    
                Maryland + Massachusetts + Michigan + Milwaukee.Wisconsin + Minneapolis.Minnesota + Minnesota +  
                Mississippi + Naperville.Illinois + New.Jersey + New.Mexico + New.Orleans.Louisiana + New.York + 
                New.York.New.York + Newark.New.Jersey + North.Carolina + Oakland.California + Oakland.Maryland + Ohio +       
                Orlando.Florida + Palo.Alto.California + Pennsylvania + Philadelphia.Pennsylvania + Rochester.New.York + San.Francisco.California +       
                South.Carolina + St..Louis.Missouri + Staten.Island.New.York + Tampa.Florida + Tennessee + Vermont +    
                Virginia + Washington + Washington.District.of.Columbia + West.Virginia + Wheaton.Illinois, data = df2_1)
    summary(reg3)  
    

    vec = as.data.frame(ClickEfficacy1_jk[[1]])
    vec$names = rownames(vec)
    vec = vec[c(8,9,10,12,14,17,18,19,20,21,17,23,26,27,28,32,35,36),]
    vec$names[3] = 'District of Columbia'
    vec$names[9] = 'New York'
    vec$names[13] = 'New Jersey'
    vec$names[14] = 'New Mexico'
    
    as.factor(vec$names)
    
    statedf = merge(vec, Race_Ethnicity, all = F, by.x = 'names', by.y = 'State') 
    statedf = merge(statedf, presdfboth, all = F, by.x = 'names', by.y = 'state') 
    statedf = statedf[,c(1,2,5,8,11,41,44,50,59,62,77,82)]
    
    statedf$CTR_coeff=statedf$`ClickEfficacy1_jk[[1]]`
    
    for ( col in 1:ncol(statedf)){
      colnames(statedf)[col] <-  sub("X", "", colnames(statedf)[col]) #Remove unnecessary words/characters in columns
    }
    
    statedf
    
### Secondary model with demographics

    AdEfficacy <- lm(ctr ~ MedHHInc_16 , data = statedf)
    summary(AdEfficacy)
    plot(AdEfficacy)
    plot(statedf$`ClickEfficacy1_jk[[1]]`,statedf$`2016_Pop_Hispanic_Latino_`)

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
qplot(Ad_Freq_state, CTR*100, data = df2, facets = . ~ Texas)
boxplot(CTR*100 ~ Ad_Freq_state, data = df2, col = "blue")


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