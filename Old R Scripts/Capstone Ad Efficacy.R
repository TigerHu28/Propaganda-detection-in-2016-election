library(tidycensus)
library(stringr)
library(ggplot2)
library(dplyr)
library(readxl)
library(lubridate)

####GOAL: LINK EVERY FIPS CODE ASSOCIATED WITH AN AD TO THAT AD. This will make it easier to attach more variables to each ad.
# Look for US Dept of Elections? data

####Meeting

	#apply a log transformation to help the clustering in our rate variables. Me: what about standardizing? could use a scale() function.
	
	#presence/absence of words vs. number 
	
	#feedback analysis plan: be clearer that we moved away from the voter turnout predictor to other things like text and location categorical variables. 

	#Technical report: due on the 14th of April (Formalization of the notes we've taken), final report due the last day. Infographic aka exec summary aka memo for our audience due ___.

#Research goals:
 #Prevent future russian influence by
	#1. Identifying vulnerable populations
	#2. Uncover Russian objectives/goals
		#a. Create disagreement, disunity in U.S. 
		#b. Create dissatisfaction with gov (measured in election turnout)
		#c. try to get more favorable candidate elected

#Analysis plan: instruction sheet for someone who would want to repeat the method. Should be formal enough for holt or Rodu to know what is happening- not just Martinet. Potential objections to the methodology/ how you're answering the question with the data.  
#Submit draft before next monday or thursday

#How to address connectedness
	#clicks per person could give a measure of connectedness
	#any information on facebook ads that are not political
	#Potential objection: unclear what population of comparison is- if not all russian propoganda, then should probably compare to affectiveness of facebook ads at large.

#can cut some locations with few targets. Don't have to have a really important quantitative variable.

#use pop.s=get_estimates(geography='state', product="characteristics", year=2016) to fine caucasian proportions

# use an intersect() or an in1d() statement (python) to find whether a state is mentioned in a location vector
#FIX TYPO: 					ads4[ads4$AdID==2701,][1,c('Impressions')]=119800
ads = read.csv("/Users/Paul/Desktop/STAT 4996-Capstone/Capstone Project/FacebookAds.csv")

#fix the instances where clicks>impressions

ads[c(1545, 2175, 2706, 3349, 3445),4]=c(152, 8610, 545, 19510, 119800)
ads[c(1545),3]=c(100) #replace this with 100 according to the picture

ad0=subset(ads[,c(1,2, 3, 4, 23)], Impressions==0)
adc0=subset(ads[,c(1,2, 3, 4, 23)], Clicks==0 & Impressions!=0)

#clicks on ad 863, 3021 786 2807, 2502, 2274, 2425 1258,2671,2044,2012, 1266, 2670, 3254, 3249 & 2705, 2679 & 3244, do not match those in the data. need to check other ads with 0 clicks or imps
#Delete ad 763, 45, 2820



locs=as.matrix(ads$Location)
ads[,"Location"]=apply(locs, 1, as.character)
#drop irrelevant
adsall=ads
#ads=ads[,c('AdID', 'Clicks', 'Location')]
#only want rows with 'United States' or state names in location col:
ads=ads[str_which(locs, "United States"),]
#ads=ads[-as.numeric(str_which(locs, "Canada, United States")),]
ads=ads[-c(as.numeric(str_which(locs, "Canada"))),]
ads=ads[-c(as.numeric(str_which(locs, "Kingdom"))),]
ads=ads[-c(which(ads$Location=="United Kingdom, United States")),]
no.us=as.vector(ads$Location) #fix a strange case and typos
no.us[6]="United States: Baltimore, Maryland: Ferguson, Missouri; St. Louis, Missouri; Cleveland, Ohio"
no.us=str_replace(no.us, "Living In:", "")
no.us=str_replace(no.us, "Recently In:", "")
no.us=str_replace(no.us, "Astoria", "New York")
no.us=str_replace_all(no.us, 'Latitude .....', "")
no.us=str_replace_all(no.us, 'Longitude ......', '')
no.us=str_replace_all(no.us, " km", '')
no.us=str_replace_all(no.us, " mi", '')
no.us=str_replace_all(no.us, "[[:digit:]]", "")

#write.csv(lvls, file="no.us.csv")

no.us=str_replace_all(no.us, '[.]', '')
no.us=str_replace_all(no.us, '[-]', '')
no.us=str_replace_all(no.us, "[[+]]", "")
no.us=str_replace_all(no.us, " [(]", ",")
no.us=str_replace_all(no.us, "[)]", "")
no.us=str_replace_all(no.us, "[:]", "/")
no.us=str_replace_all(no.us, "[;]", "/") #also change "i," to "i/"
no.us=str_replace_all(no.us, "i,", "i/")
no.us=str_replace_all(no.us, "[,]", "")
no.us=str_replace_all(no.us, "  ", " ")
no.us=str_replace_all(no.us, "Ferguson St Louis", "St. Louis")
no.us=str_replace_all(no.us, "St Louis", "St. Louis")
no.us=str_replace_all(no.us, "Baton Rouge ", "Baton Rouge Louisiana/ ")
no.us=str_replace_all(no.us, "Camden Newark New Jersey", "Camden New Jersey/ Newark New Jersey/")
no.us=str_replace_all(no.us, "Baltimore Oakland Maryland", "Baltimore Maryland/ Oakland Maryland/")
no.us=str_trim(no.us, side='both')
no.us=str_replace(no.us, "United States/ Atlanta Louisiana/ Austin Houston Virginia", "United States/ Atlanta Louisiana/ Austin Texas/ Houston Texas/ Virginia")
no.us=str_replace(no.us, "United States/ Abilene Amarillo Austin Beaumont Brownwood Bryan Conroe Corpus Christi/ Dallas Fort Worth Houston Lubbock Odessa San Angelo San Antonio South Plains Temple Texas City Tyler Waco Texas", "United States/ Abilene Texas/ Amarillo Texas/ Austin Texas/ Beaumont Texas/ Brownwood Texas/ Bryan Texas/ Conroe Texas/ Corpus Christi Texas/ Dallas Texas/ Fort Worth Texas/ Houston Texas/ Lubbock Texas/ Odessa Texas/ San Angelo Texas/ San Antonio Texas/ South Plains Texas/ Temple Texas/ Texas City Texas/  Tyler Texas/ Waco Texas")
no.us=str_replace(no.us, "United States/ California/ Florida/ Illinois Michigan/ New Jersey/ New York/ Texas/ Virginia", "United States/ California/ Florida/ Illinois/ Michigan/ New Jersey/ New York/ Texas/ Virginia")
no.us=str_replace(no.us, "United States/ Alabama/ Georgia/ New Mexico/ Arizona Texas", "United States/ Alabama/ Georgia/ New Mexico/ Arizona/ Texas")
no.us=str_replace(no.us,"United States/ New Mexico/ Arizona Texas", "United States/ New Mexico/ Arizona/ Texas")
no.us=str_replace(no.us,"United States/ Philadelphia Pittsburgh Pennsylvania", "United States/ Philadelphia Pennsylvania/ Pittsburgh Pennsylvania")
no.us=str_replace(no.us, "United States/ Allentown Erie Harrisburg Philadelphia Pittsburgh Scranton Pennsylvania", "United States/ Allentown Pennsylvania/ Erie Pennsylvania/ Harrisburg Pennsylvania/ Philadelphia Pennsylvania/ Pittsburgh Pennsylvania/ Scranton Pennsylvania")
no.us=str_replace(no.us, "United States/ Illinois Virginia/ West Virginia", "United States/ Illinois/ Virginia/ West Virginia")
no.us=str_replace(no.us, "United States/ Lancaster San Francisco California/ Madison Wisconsin","United States/ Lancaster California/ San Francisco California/ Madison Wisconsin")
no.us=str_replace(no.us, "United States New York New York", "United States/ New York New York")
no.us=str_replace(no.us, "United States/ Connecticut/ Delaware/ Maine/ Maryland/ Massachusetts Ferguson Missouri/ Virginia City Nevada/ New Hampshire/ New Jersey New York New York Ohio/ Pennsylvania/ Rhode Island/ Vermont", "United States/ Connecticut/ Delaware/ Maine/ Maryland/ Massachusetts/ Ferguson Missouri/ Virginia City Nevada/ New Hampshire/ New Jersey/ New York New York/ Ohio/ Pennsylvania/ Rhode Island/ Vermont")
#no.us[str_which(no.us, "Chicago")]
no.us=str_replace(no.us, "United States/  Chicago Chicago Heights  Des Plaines  Naperville  Wheaton Illinois/ Ferguson Missouri", "United States/ Chicago Illinois/ Des Plaines Illinois/ Naperville Illinois/ Wheaton Illinois/ Ferguson Missouri")
no.us=str_replace(no.us, "United States/ Oakland California/ Atlanta Georgia/ Baton Rouge New Orleans Louisiana/ Baltimore Maryland/ St Louis Missouri/ Camden Newark New Jersey/ Cleveland Ohio/ Chester Pennsylvania", "United States/ Oakland California/ Atlanta Georgia/ Baton Rouge Louisiana/ New Orleans Louisiana/ Baltimore Maryland/ St. Louis Missouri/ Camden New Jersey/ Newark New Jersey/ Cleveland Ohio/ Chester Pennsylvania")
no.us=str_replace(no.us, "United States/ Atlanta Georgia/ New Orleans Louisiana/ Baltimore Oakland Maryland/ Ferguson St Louis Missouri/ Milwaukee Wisconsin", "United States/ Atlanta Georgia/ New Orleans Louisiana/ Baltimore Maryland/ Oakland Maryland/ Ferguson Missouri/ St. Louis Missouri/ Milwaukee Wisconsin")
no.us=str_replace(no.us, "Georgia United States/ Alabama/ Arizona/ California/ Florida/ Mississippi/ New Mexico/ New York/ Oklahoma/ Texas/ Washington", "United States/ Alabama/ Arizona/ California/ Florida/ Georgia/ Mississippi/ New Mexico/ New York/ Oklahoma/ Texas/ Washington")
no.us=str_replace(no.us, "United States Baltimore Maryland", "United States/ Baltimore Maryland")
no.us=str_replace(no.us, "United States/ California Illinois", "United States/ California/ Illinois")
no.us=str_replace(no.us,"United States Minneapolis Minnesota", "United States/ Minneapolis Minnesota")
no.us=str_replace(no.us,"United States/ Atlanta Georgia/ Maryland/ Ferguson St Louis Missouri/ Virginia", "United States/ Atlanta Georgia/ Maryland/ Ferguson Missouri/ St. Louis Missouri/ Virginia")

adsloc=cbind(ads, no.us) #use to make dummies

#splitting and rejoining
locs.split=str_split(no.us, "/", simplify = TRUE)
locs.split=apply(locs.split, 2, str_trim)
locs.split=apply(locs.split, 2, as.character)
ads=as.data.frame(ads)
locs.split=as.data.frame(locs.split)
ads=cbind(ads, locs.split, stringsAsFactors=FALSE)
ads$V1=as.character(ads$V1)
ads$V2=as.character(ads$V2)
#Try applying subset functions to the character vectors for each ad rather than the whole long location string
nonnat.ads=ads[-c(which(nchar(ads$V2)==0)),]
nat.ads=ads[c(which(nchar(ads$V2)==0)),]

#### FIPS
library(readxl)
cens.fip=read_xlsx("all-geocodes-v2016.xlsx")
data("fips_codes")
dat=force(fips_codes)

#Need to attach fips code
counties=dat$county
counties=str_replace(counties, " County", "")
paster =function(i){
	state_i=dat$state_name[i]
	count_i=counties[i]
	new.name=paste(count_i, state_i)
	return(new.name)
}
new.names=sapply(1:length(counties), function(x) paster(x))
dat=cbind(dat, new.names)

#same pasting process for state and county codes

paster2 =function(i){
	state_i=dat$state_code[i]
	count_i=dat$county_code[i]
	new.name=paste(state_i,count_i, sep="")
	return(new.name)
}
new.code=sapply(1:length(counties), function(x) paster2(x))
dat=cbind(dat, new.code)

codes=unique(dat$new.code)


less=as.matrix(nonnat.ads[,-c(2, 4:25)])

#stack so I can join to census data
	
less=unname(less)

two=less[,c(1, 2, 4)]
three=less[,c(1,2,5)]
four=less[,c(1,2,6)]
five=less[,c(1,2,7)]
six=less[,c(1,2,8)]
seven=less[,c(1,2,9)]
eight=less[,c(1,2,10)]
nine=less[,c(1,2,11)]
ten=less[,c(1,2,12)]
elev=less[,c(1,2,13)]
twel=less[,c(1,2,14)]
thirt=less[,c(1,2,15)]
fourt=less[,c(1,2,16)]
fift=less[,c(1,2,17)]
sixt=less[,c(1,2,18)]
sevent=less[,c(1,2,19)]
eighte=less[,c(1,2,20)]
ninet=less[,c(1,2,21)]

stacked=rbind(two, three, four, five, six, seven, eight, nine, ten, elev, twel, thirt, fourt, fift, sixt, sevent, eighte, ninet)
stacked=stacked[-c(which(stacked[,3]=="")),]
stacked=as.data.frame(stacked)

#Need to match these names to fips codes

cit.co=read.csv('/Users/Paul/Desktop/STAT 4996-Capstone/Capstone Project/uscities.csv')

#paste state names onto end of city names, then merge with stacked
cit.co=cit.co[,c(1,4,7,8)]
cit=as.character(cit.co$city)
st=as.character(cit.co$state_name)
#same for counties:
county=as.character(cit.co$county_name_all)

#need to paste the state names after the city and county names
library(stringr)
cit.state=str_c(cit, st, sep=" ")
co.state=str_c(county, st, sep=" ")
#need to return fips to their locations
fip=cit.co$county_fips_all
cit.fip=data.frame("Loc"=cit.state, "fips"=fip)
cit.fip=as.matrix(cit.fip)
count.fip=data.frame("Loc"=cit.state,"fips"=fip,"county"=co.state)

colnames(cit.fip)=c('cit.state','fips') #Floyd County Texas/South Plains Texas=48153
sp.tex=matrix(data=c("South Plains Texas", "48153"), nrow=1, ncol=2)
cit.fip = rbind(cit.fip, sp.tex)

#state level

pop.s=get_estimates(geography='state', product="population", year=2016)
#write.csv(pop.s, file='population.state.csv')
pop.s=unname(as.matrix(pop.s[,c(1,2)]))
cit.fip2=unname(as.matrix(cit.fip))
popscity=rbind(pop.s,cit.fip2)
#popscounty=rbind(pop.s, pop.co[,c(2,1)])

#join finally
names(stacked)=c("AdID", "Clicks", 'NAME') #need 

named=as.character(stacked[,3])
named=str_trim(named, side='both')
stacked[,3]=named

cities=popscity[,1]
cities=as.character(cities)
popscity[,1]=cities

colnames(popscity)=c("NAME", "fips")

popscity=as.data.frame(popscity)
names(stacked)=c('AdID', "Clicks", 'NAME')
data=merge(stacked, popscity, by="NAME") 
locfip=data[,c('NAME', 'fips')] #this has each instance of each location with its fips

data.distinct=distinct(data) 
#View(table(data.distinct$NAME)) #Shows target counts of all locations
data.distinct$fips=as.character(data.distinct$fips) #Why only 103 of these but 107 in unique(stacked$NAME)?

#need to put zeroes in front of fips codes
fip.more=data.distinct$fips
dat.less=data.distinct[c(which(nchar(fip.more)==4)),]

#put each of which(nchar(fip.more)==4) in for 491

	data.distinct[491,4]=str_c("0", data.distinct[491,4])
	less.ind=which(nchar(fip.more)==4)

	for (i in less.ind){
		data.distinct[i,4]=str_c("0", data.distinct[i,4])
	}
	
	data1=data.distinct #make a clean copy so can tell how many ads are dropped each time we merge on a new variable.
	
	data=data.distinct
	
#Give each unique location mentioned in ads its own fip:
	
	uni.locs=as.data.frame(unique(stacked$NAME))
	names(uni.locs)="NAME"
	loc.fip=merge(uni.locs, popscity, by="NAME") #want the result to have 107 rows (less if missing places)
	less.fip=distinct(loc.fip)

# Reference
#popscity[str_which(popscity$NAME, "Los Angeles"),] #SORT TABLES WITH THIS

	#### Click Through Rate
	
	ads4=as.data.frame(adsall)
	ads4 = subset(ads4, Impressions!=0)
	clicks=c(ads4$Clicks)
	imps=c(ads4$Impressions)
	ctr=(clicks/imps)*100
	ads4$ctr=ctr
	ctr.ad=data.frame('ctr'=ads4$ctr, 'AdID'=ads4$AdID)
	
	#merge with data:
	
	data = merge(data, ctr.ad, by= "AdID") #This has a row for each unique combo of location and AdID
	
	#delete alternative fips
	
	i.to.fix=which(nchar(as.character(data$fips))>5)

	for (i in i.to.fix){
		data[i,]=str_extract(data[i,], ".....")
	}
	
	#remove NAs in AdID that appeared somehow
	
	datctr=data[c(which(is.na(data$AdID)==FALSE)),] #USE THIS TO SEARCH FOR CORRELATIONS. first merge the variable with this on fips, then do groupby adid
	length(unique(datctr$fips))
	
	data=data[c(which(is.na(data$AdID)==FALSE)),] #USE THIS TO MERGE WITH VARIABLES TO RUN REG
	corfips=data$fips
	
	#df of all locations and their fips:
	datctr
	
#join with the national data
	
	ads5=read.csv("/Users/Paul/Desktop/STAT 4996-Capstone/Capstone Project/FacebookAds.csv")
	ads5=as.data.frame(ads5)
	natads=merge(nat.ads, ads5, by="AdID")
	natads=distinct(natads)
	natads=subset(natads, Impressions!=0)
	nctr=natads$Clicks.x/natads$Impressions
	natads$ctr=nctr
	names(natads)
	natads=natads[,c(1, 4, 49)]
	names(natads)[2]='fips'
	natads$fips=rep("0", dim(natads)[1])
	natads
	
	local=datctr[,c(1, 4, 5)]
	
	allads=rbind(natads, local)
	
	#length(unique(allads$AdID)) #number of useable ads
	
	allg=group_by(allads, fips)
	area_avg_ctrs=summarize(allg, avg=mean(as.double(ctr), na.rm = TRUE))
	
	ggplot(area_avg_ctrs, aes(avg))+geom_histogram()
	
	namefip=data.frame('fips'=datctr$fips, datctr$NAME)
	locfipctr=merge(namefip, area_avg_ctrs, by="fips")
		#View(locfipctr)
	
#### IVs
	
	
	#### Rubles per minute: supposed to predict Impressions but L shaped scatter
	
		#View(subset(adsall, rpm>5))
		#this outlier is an incorrect adspend/typo, so fix
		fixind=which(adsall$AdSpend==27500.00)
		adsall[fixind,21]=c(2500)
	
		#need duration first, with the final date as the end date if not listed
		create = mdy_hms(adsall$CreationDate)
		#figure out the last possible enddate
		endstr=as.character(adsall$EndDate)
		allends=ifelse(endstr=="", "08/15/17 10:47:53 PM PDT", endstr)
		end=mdy_hms(allends)
		adsall$end=end
		
		duration = end-create #this is in minutes
		adsall$Duration = abs(duration)
		times=adsall[,c(1, 26, 6, 7)]
		
		#calculate rubles per minute, substituting NA for 0
		rubs=as.character(adsall$AdSpend)
		rubs2=ifelse(is.na(rubs), "0", rubs)
		rubb=as.numeric(rubs2)
		
		rpm=rubb/abs(as.numeric(duration))
		rpm=ifelse(is.infinite(rpd), 0, rpm)
		
	#### Hour of the week variable
		
		ende=with_tz(end, tzone="EST")
		ender=round_date(ende, unit="hour")
		
		ce=with_tz(create, tzone="EST")
		cer=round_date(ce, unit="hour")
		wcer=wday(cer)
		adsall$day=wcer
		
		adsall$hour=hour(cer)
		
		adg=group_by(adsall, hour, day)
		hourcounts=count(adg)
		
		
		#join create_week and cer to adsall, then groupby both of those, and count the frequencies
		
		?lubridate
		

	### Variable for minutes spent running during U.S. off hours
		
		sort(adsall$end)
	
	#### Categorical variable for instagram vs. facebook
	
	#### Number of targets
		
		i.to.fix=which(nchar(as.character(data1$fips))>5)
		
		for (i in i.to.fix){
			data1[i,]=str_extract(data1[i,], ".....")
		}
		
		no.targets=data.frame(table(data1$fips))
		names(no.targets)=c('fips', 'targets')
		ctr.target=merge(locfipctr, no.targets, by='fips')
		ctr.target=distinct(ctr.target)
		
		#see how ctr relates to number of targets
		
		#med ctr vs number of targets
		ctrtar=ggplot(ctr.target, aes(avg, targets, color=datctr.NAME))+geom_point()
		ctrtar
		
		#color by state instead
		st=data.frame(rep(0, dim(ctr.target)[1]))
	
		for (pattern in state.name){
			st[str_which(ctr.target$datctr.NAME, pattern),]=pattern
		}
		these=st[,1]
		ctr.target$state=these
		
		names(ctr.target)[5]="st"
		
		#med ctr vs number of targets WITH state colors
		ctrtar2=ggplot(ctr.target, aes(avg, targets, color=st))+geom_point()
		ctrtar2
		
		#try avg ctr vs. number of registered voters
		
		
		#try recreating the ad theme vs. time plot without smoothing. When do they post and why?
		
		#View(ads4[str_which(as.character(ads4$Location), "Milwaukee"),])
	
		dates=data.frame(ads4$CreationDate)
		datechr=as.character(dates[,1])
		dates[,1]=str_sub(datechr, end=8)
		dates
		datesl=as.vector(dates)
		datesl
		#names(datesl)="date"
		library(lubridate)
	
		#try comparing the published theme plot to a location ts initial post plot
		
		tl=data.frame('loc'=ads4$Location, 'AdID'=ads4$AdID, 'datecode'=dated, 'datemdy'=datesl$ads4.CreationDate)
		locu=unique(stacked$NAME)
		
		#merge stacked with tl so I can compare cleaned locations and their dates
		tlst=merge(tl, stacked, by="AdID")
		
		#use faceting to compare multiple locations, geom_freqpoly
			#drop unnecessary columns
				tlst=tlst[,-c(2, 5)]
			tlst=arrange(tlst, NAME)
			#plot the first 122 rows, since Cleveland ends there
			
		
		
		#first
		date_loc=ggplot(tlst[c(1:146),], aes(datecode))+geom_freqpoly()+facet_grid(rows=vars(NAME),labeller = label_wrap_gen(width = 15))+theme(strip.text.y = element_text(size = 8, angle = 0))
		date_loc
		
		#second
		date_loc2=ggplot(tlst[c(147:259),], aes(datecode))+geom_freqpoly()+facet_grid(rows=vars(NAME),labeller = label_wrap_gen(width = 15))+theme(strip.text.y = element_text(size = 8, angle = 0))
		date_loc2
		
		date_loc3=ggplot(tlst[c(260:427),], aes(datecode))+geom_freqpoly()+facet_grid(rows=vars(NAME),labeller = label_wrap_gen(width = 15))+theme(strip.text.y = element_text(size = 8, angle = 0))
		date_loc3
		
		date_loc4=ggplot(tlst[c(428:703),], aes(datecode))+geom_freqpoly()+facet_grid(rows=vars(NAME),labeller = label_wrap_gen(width = 15))+theme(strip.text.y = element_text(size = 8, angle = 0))
		date_loc4
		
		#compare these to 'audience insights' proportions on facebook
		
		#the first wave seems to target areas with an unusually high proportion of african american residents
		
		#another wave includes Lancaster, Madison, and San Francisco
		
		
		
		#reference
		t + facet_grid(rows = vars(year))(
		
		
		#Look at when they posted ads with the highest ctrs, or # targets: Baltimore, St. Louis/ferguson, cleveland, NYC
		
		
		
		#should also plot effectiveness over time. This could illuminate whether ctr was really 
		#their goal, or whether they were succeeding. If a high ctr in one area is followed by a post 
		#in that same area, maybe that's their goal, rather than coverage.
		
		
		#interesting that these two are always opposed
		contentexclude=data.frame(ads4$ExcludedConnections, ads4$Interests)

		
			
	#### Median Household Income
		#turns out get_acs takes FIPS codes, so will be much faster to feed it fips instead of locations
		data("fips_codes")
		dat=force(fips_codes)
		stcorfips=corfips[nchar(corfips)<5]
		cocorfips=corfips[nchar(corfips)==5]
		#take the last 3 digits in these fips:
		ccfipsx=str_sub(cocorfips, -3)
			
		#remove.packages('tidycensus')
		#install.packages('tidycensus')
		
		#do it
			library(tidycensus)
			library(tidyverse)
			library(viridis)
			library(usmap)
			library(tigris)
			options(tigris_use_cache = TRUE)
			
			census_api_key("002042b0954dcb772d496634776f74eac9279a88", install=TRUE, overwrite = TRUE)
			readRenviron("~/.Renviron")
			Sys.getenv("CENSUS_API_KEY")
			
			#gotta trim the zeroes off the front for get_acs to take the fips codes.
			stcorfips[str_detect(stcorfips, "0.")] = str_sub(stcorfips[str_detect(stcorfips, "0.")], start=-1)
			
		bigframe=data.frame(NULL)
		scorfip=as.vector(unique(stcorfips))
		for (fip in scorfip){
			state_i=get_acs(geography='state', variables = "DP03_0062E",state=fip)
			bigframe=rbind(bigframe, state_i)
		}
		
		for (fip in ccfipsx){
			co_i=get_acs(geography='county', variables="DP03_0062E", county=fip)
			bigframe=rbind(bigframe, co_i)
		}
		
	
			head(bigframe)
			names(bigframe)[1]="fips"
			bigdata=merge(data, bigframe, by='fips', all.x=TRUE)
			bdd=distinct(bigdata)
			dim(data) #not sure why this had 447 rows
			length(unique(data$AdID)) #213 workable rows
			
			library(data.table)
			datam=as.data.table(bdd)
			dm.g=group_by(datam, AdID)
			amhhi.perad=summarise(dm.g, avg=mean(estimate))
			#merge that with a table of unique adids and ctr
			
			mhhi.ad=merge(datctr, amhhi.perad, by='AdID')
	
			
			
	#### AD Word Count
	
		#ads2=read.csv("/Users/Paul/Desktop/STAT 4996-Capstone/Capstone Project/FacebookAds.csv")
		#ads2=ads2[,c(1,2)]
	
		#text=as.character(ads2[,2])
		
		#counts=sapply(1:3516, function(x) str_count(text[x], " "))
		#counts=unlist(counts)
		#counts=counts+1
	
		#write.csv(counts, file='WordCounts.csv') 	#write.csv(yourData, file = "yourStataFile.csv")
	
		wc=read.csv("WordCounts.csv")
		counts=wc[,2]
		ads2=read.csv("/Users/Paul/Desktop/STAT 4996-Capstone/Capstone Project/FacebookAds.csv")
		ads2=ads2[,c(1)]
		ads3 = cbind(ads2, counts)
		ads3=as.data.frame(ads3)
		names(ads3) = c("AdID", "WordCount")
		#write.csv(ads3, file='WordCount.csv')
		
		data=merge(data, ads3, by="AdID") #obs still 921
		
		length(unique(data$AdID)) #225 unique ads
		
	
	#### Party Identification
		
		#install.packages("haven")
		library(haven)
		#read in a .dta file
		yourData = read_dta("/Users/Paul/Desktop/STAT 4996-Capstone/Capstone Project/cumulative_2006_2018.dta")
		
		less.party=subset(yourData, year >=2015)
	
		#write.csv(yourData, file = "yourStataFile.csv")
		
		#Need to make a subtable with every state, then calculate the three proportions of party id for each subtable. Then, we can give each party a column, and join to data.count.
	
		pid.fips = less.party[,c(1, 5, 13, 19)] #2=Repub, 3=Ind, 1=Dem
		
		#state level only
		
			state.fips=unique(pid.fips$state)
			st.subtabs=sapply(state.fips, function(x) subset(pid.fips, state==x)$pid3)
		
			dem.props=c()
			rep.props=c()
			ind.props=c()
			for (i in seq(1, 51, 1)){
				count.pid=as.data.frame(table(st.subtabs[i]))
				tot=sum(count.pid$Freq)
				prop.dem=count.pid[1,2]/tot
				prop.rep=count.pid[2,2]/tot
				prop.ind=count.pid[3,2]/tot
				dem.props=append(dem.props, prop.dem)
				rep.props=append(rep.props, prop.rep)
				ind.props=append(ind.props, prop.ind)
			}
		
		#city level
			
			cit.fips=unique(pid.fips$county_fips)
			cit.subtabs=sapply(cit.fips, function(x) subset(pid.fips, county_fips==x)$pid3)
			
			#names(cit.subtabs)#look up how to call many list names
			
			dem.props.cit=c()
			rep.props.cit=c()
			ind.props.cit=c()
			for (i in seq(1, length(cit.subtabs), 1)){
				count.pid=as.data.frame(table(cit.subtabs[i]))
				tot=sum(count.pid$Freq)
				prop.dem=count.pid[1,2]/tot
				prop.rep=count.pid[2,2]/tot
				prop.ind=count.pid[3,2]/tot
				dem.props.cit=append(dem.props.cit, prop.dem)
				rep.props.cit=append(rep.props.cit, prop.rep)
				ind.props.cit=append(ind.props.cit, prop.ind)
			}
			
		
		pid.tbl=data.frame(fips=c(state.fips, cit.fips), prop.dem=c(dem.props,dem.props.cit), prop.rep=c(rep.props,rep.props.cit), prop.ind=c(ind.props,ind.props.cit))
	
		# want to merge on data, but need a zero in front of the state fips in pid.tbl
			
			pid.fips=as.character(pid.tbl[,1])
			
			st.pid=pid.tbl[c(which(nchar(pid.fips)==1)),]
			pid.fips2 = as.vector(st.pid$fips)
			pid.pad=str_pad(pid.fips2, width=2, side="left", pad="0")
		
			pid.tbl=apply(pid.tbl, 2, as.character)
			pid.tbl[which(nchar(pid.fips)==1),1]=pid.pad
			pid.tbl = as.data.frame(pid.tbl)
			pid.tbl$fips = as.character(pid.tbl$fips)
			
			#merge with locfip df then group by location:
			
			pidadloc=merge(pid.tbl, locfip, by='fips')
			
				data5=merge(pid.tbl, data, by="fips") #x
				datapid=distinct(data5)
				head(datapid)
				length(unique(data$AdID)) #This has only 213 ads, whereas..
				length(unique(data1$AdID)) #... has 829. need affiliation data with more locations. Gonna proceed with the EDA anyway
				
		
			#nonnat.ads=ads[-c(which(nchar(ads$V2)==0)),]
			##popscity[str_which(popscity$NAME, "Los Angeles"),] #SORT TABLES WITH THIS
				
	#### Population
	
	#### Voter Turnout
				
				library(readxl)
				#go to https://l2political.com/products/printablereports/ and search for each of 
				
				
				#need to merge this with 2016 voter registration data. Section 1A has registered and eligible county level!!! Section F has number who participated. Participated/Registered&eligible=turnout rate
					regel=read_xls("/Users/Paul/Desktop/STAT 4996-Capstone/Capstone Project/EAVS 2016 Final Data for Public Release v.4.xls")
					regel2=read_xls("/Users/Paul/Desktop/STAT 4996-Capstone/Capstone Project/EAVS 2016 Final Data for Public Release v.4.xls", sheet = "SECTION F")
					regel2
					#get the county totals for wisconsin, madison, milwaukee, 
					WI=regel[str_which(regel$State, "WI"),]
					mad=WI[str_which(WI$JurisdictionName, "MADISON"),4]
					madreg=sum(as.numeric(mad$A1a))
					mil=WI[str_which(WI$JurisdictionName, "MILWAUKEE"),4]
					milreg=sum(as.numeric(mil$A1a))
					WIreg=sum(as.numeric(WI$A1a))
					regl=data.frame('fips'=regel$FIPSCode, 'regel'=regel$A1a, 'loc'=regel$JurisdictionName, 'st'=regel$State) #need columns 1 and 4. 
					#delete town-level info
					regl=regl[str_which(regl$loc, "TOWN", negate = TRUE),]
					regl$fips=as.character(regl$fips)	#select the first 5 fips
					regfip=str_sub(regl$fips, end=5)
					regl$fips=regfip
					regl[str_which(regl$st, "DC"),] #correct thus far
					regl=data.frame('fips'=regl$fips, 'tot'=regl$regel)
					#get state totals with a groupby
					#library(data.table)
					reg=as.data.table(regl[,-c(3,4)])
					reg[str_which(reg$fips, "09...")]
					stfips=str_sub(reg$fips, end=2) #get the state fips so can group by these instead of state abbreviations
					reg[, fips := stfips]
					reg[, tot := as.numeric(as.character(tot)),]
					regg=group_by(reg, fips)
					regst=summarise(regg, tot=sum(tot))
					#stack this baby with regl, give them same dtypes first
					regst=as.data.frame(regst)
					regl$tot=as.numeric(as.character(regl$tot))
			
					regv=rbind(regst, regl)
	
					#merge this with number who voted (F1a)
					
						#get the county totals for wisconsin, madison, milwaukee, 
						WI=regel2[str_which(regel2$State, "WI"),]
						mad=WI[str_which(WI$JurisdictionName, "MADISON"),4]
						madreg=sum(as.numeric(mad$F1a))
						mil=WI[str_which(WI$JurisdictionName, "MILWAUKEE"),4]
						milreg=sum(as.numeric(mil$F1a))
						WIreg=sum(as.numeric(WI$F1a))
						regl=data.frame('fips'=regel$FIPSCode, 'regel'=regel2$F1a, 'loc'=regel2$JurisdictionName, 'st'=regel2$State) #need columns 1 and 4. 
						#delete town-level info
						regl=regl[str_which(regl$loc, "TOWN", negate = TRUE),]
						regl$fips=as.character(regl$fips)	#select the first 5 fips
						regfip=str_sub(regl$fips, end=5)
						regl$fips=regfip
						#regl[str_which(regl$st, "DC"),] #correct thus far
						regl=data.frame('fips'=regl$fips, 'tot'=regl$regel)
						#get state totals with a groupby
						#library(data.table)
						reg=as.data.table(regl[,-c(3,4)])
						#reg
						#reg[str_which(reg$fips, "09...")]
						stfips=str_sub(reg$fips, end=2) #get the state fips so can group by these instead of state abbreviations
						reg[, fips := stfips]
						
						reg[, regel := as.numeric(as.character(tot)),]
						regg=group_by(reg, fips)
						regst=summarise(regg, tot=sum(regel))
						#stack this baby with regl, give them same dtypes first
						regst=as.data.frame(regst)
						regl$tot=as.numeric(as.character(regl$tot))
						
						regv2=rbind(regst, regl)
					
					turnr=regv2$tot/regv$tot
					
					turnfip=data.frame('fips'=regv$fips, 'turnout'=turnr)
					head(turnfip)
					
					#merge this to locfip
					
					turnloc=merge(turnfip, data, by='fips')
					
					#select just unique vect of AdIDs and Turnout rate
					
				
	#### More variables:
				
			#Would it be reasonable to use effectiveness of other ads to predict effectiveness of these ads? Then, could we consider the variance explained by other variables to be the unique features of the propoganda? 
	
	#### Before we can do EDA, gotta create a mechanism for calculating location-level estimate vectors
				
				#Solution 1: give each ad a vector of fips codes?
				
				library(data.table)
				
				locs.uni=unique(data.distinct$fips)
				locs.ind=seq(1, length(locs.uni))
				third=data.table("uni"=as.factor(locs.uni), 'ind'=as.factor(locs.ind))
				id.fip=data.table("AdID"=as.factor(data.distinct$AdID), "fips"=data.distinct$fips) #"fips2"=data.distinct$fips)
				new=id.fip[third, on=.(fips=uni)]
				#split off the "|" when multiple fips
				fipall=as.matrix(new$fips)
				fewer=sapply(fipall, str_split, pattern="[[|]]", simplify=TRUE, )
				few2=data.table(fewer)
				
				#data1=cbind(data.distinct, few2) #Compare this to data when merging on new variables to view the observation loss
				
				#data=cbind(data.distinct, few2) #This has 829 ads with fips codes. not too shabby.
				
				#Solution 2: write a function that will apply a scanner to each vector of codes,
				#and return one column of variable X1 for each location
				#write a function that will calculate a vect of row totals or row means for variable X1
				
					#PID: Need vector of one proportion per AdID: average the PIDs of locations targetted by ad A_i.
						
						data3=as.data.table(data)
						data3[, prop.dem := as.numeric(as.character(prop.dem))] #WARNING: can't convert factors straight to numeric.
						data3[, prop.rep := as.numeric(as.character(prop.rep))] 
						data3[, prop.ind := as.numeric(as.character(prop.ind))] 
	
						data.g=group_by(data3, AdID)
						ad.dem=summarise(data.g, avg = mean(prop.dem))
						ad.rep=summarise(data.g, avg = mean(prop.rep))
						ad.ind=summarise(data.g, avg = mean(prop.ind))
						
						#merge those three with a vector of unique adids
						data.pid=merge(ads, ad.dem, by="AdID")
						pid.ad = merge(data.pid, ad.rep, by="AdID")
						pid.ad = merge(pid.ad, ad.ind, by="AdID")
						names(pid.ad)[c(25, 26, 27)]=c("dem.id", "rep.id", "ind.id")
			
						
	#### Effective Words
						
		#Key words (dummy variables) Cross-reference Boyd and Dutt articles to ID words they both found highly effective.
	
						
	#### Proportion African American and Proportion conservative
						
				#compare ctr to prop AfAm for just the first ~1/2 of the data. Say 30 days after the election (corresponds to datecode 17113)
						
				#merge tl and allads in order to get as many ads as possible with their dates
						
						allad=merge(tl, allads, by="AdID")
						
						camp1=subset(allad, datecode<=17113)
						
						#DP05_0033PE is the variable for proportion of African Americans
						
						#detatch rows with fips=0 'United States', set them to the national proportion of pop
						natcamp1=subset(camp1, fips==0)
						natcamp1$pafam=.121
						#state level
						scamp1=camp1[nchar(camp1$fips)==2,]
						
						bigframe2=data.frame(NULL)
						sfip=as.vector(unique(scamp1$fips))
						for (fip in sfip){
							state_i=get_acs(geography='state', variables = "DP05_0033PE",state=fip)
							bigframe2=rbind(bigframe2, state_i)
						}
						
						#county level
						ccamp1=camp1[nchar(camp1$fips)==5,]
						#I seem to get the same set no matter what fips I run
						cprops=get_acs(geography='county', variables="DP05_0033PE", county="22071") #also try "22071"
						
						#stack the three:
						usprops=as.tbl(data.table('GEOID'=natcamp1$fips, 'NAME'=natcamp1$loc, 'estimate'=natcamp1$pafam))
						usprops
						camp1props=rbind(bigframe2[,-c(3, 5)], cprops[,-c(3, 5)], usprops)
						camp1props=as.data.frame(camp1props)
						names(camp1props)[1]='fips'
						
						#merge with ctr or number of targets using fips
						
						
						camp1ctrpr=merge(allads, camp1props, by='fips')
						campod=distinct(camp1ctrpr)
						#calculate median proportion afam among ad target areas
						campog=group_by(campod, AdID)
						camp1summ=summarize(campog, avg=mean(estimate))
						coctrprop=merge(allads[,-2], camp1summ, by='AdID')
						coctrpd=distinct(coctrprop)
						coctrpd$ctr=as.double(coctrpd$ctr)
						
						ads4[ads4$AdID==2701,][1,c('Impressions')]=119800
						
						
						propctr=ggplot(coctrpd, aes(avg, ctr, alpha=.5))+geom_point()
						propctr
						
						prophist=ggplot(coctrpd, aes(avg))+geom_histogram()
						prophist
						
						ctrhist=ggplot(coctrpd, aes(ctr))+geom_histogram()
						ctrhist
						
						#Both are pretty skewed. Could this make it a good predictor?
						lm()
						coctrpd$avg=coctrpd$avg^3
						amodel=lm(ctr~avg, coctrpd)
						anova(amodel)
						summary(amodel)
					
						
						subset(ads4, AdID=="1400")
						
						#compare to number of targets
						
						copt=merge(allads[,-3], camp1summ, by='AdID')
						fiptar=data.frame(table(copt$fips))
						names(fiptar)[1]='fips'
						coptl=data.frame(copt[,-1])
						ptar=merge(coptl, fiptar, by='fips')
						ptard=distinct(ptar)
						
						ptargets=ggplot(ptard, aes(avg, Freq))+geom_point()
						ptargets
					
						#remove the US
						ptnous=subset(ptard, Freq<2000)
						
						ptargets=ggplot(ptnous, aes(avg, Freq))+geom_point()
						ptargets
						
####Protest data
						
		protest=read.csv("protests.csv")
		summary(protest)
		
		View(table(protest[,c(1,2)]))
					
		View(protest[str_which(protest$Location, "Baltimore"),])
		
		View(ads4[str_which(ads4$Location, "Baltimore"),])
		
		View(ads4[str_which(ads4$Location, "Ferguson"),])
		
#### Propublica data
		
	propub=read.csv("fbpac-ads-en-US.csv", nrows=100, header=TRUE)

	View(propub)
	propub[1,]
	names()

		
		
#### EDA: Maybe shift to identifying vulnerable populations in terms of most targetted and most impressions and most money spent. Could combine these to make a vulnerability index, and predict with things like ad words, location, 
		#or make a different decision tree for each unique location, (subset to rows that mention that location) then 
						


	#spending vs. proportion of african americans
	
#Turnout vs. effectiveness
		
		#merge with ADID to calculate average turnout among people targetted-
				ctrturn=merge(datctr, turnfip, by='fips')
				ctrturnt=as.tbl(ctrturn)
				ctrturnt$AdID=as.double(as.character(ctrturnt$AdID))
				ctrturnt$AdID
				
				ctrtg=group_by(ctrturnt, AdID)
				turnad=summarise(ctrtg, med=median(turnout))
				turnad=as.data.frame(turnad)
				
				#reattach with ctr
				datctrless=distinct(data.frame('AdID'=datctr$AdID, 'ctr'=datctr$ctr))
				ctrtor=merge(datctrless, turnad, by='AdID')
				ctrtor
				ctrtor=distinct(ctrtor)
				
				#write.csv(ctrtor, file='AdIDTOR.csv')

				ctrctr=as.numeric(as.character(ctrtor$ctr))
				ctrtor$ctr=ctrctr/100
				
				ggplot(ctrtor, aes(ctr, med, alpha=.5))+geom_point()+coord_flip()
					
				
		#try with impressions
				
				imptor=data.frame('AdID'=ads4$AdID, 'imps'=ads4$Clicks)
				impto=merge(imptor, turnad, by='AdID')
				impto=distinct(impto)
				
				ggplot(impto, aes(imps, med, alpha=.5))+geom_bar()+coord_flip()
				
				ggplot(impto, aes(med))+geom_histogram()
				
				dim(impto)
				
				unique(datctr$AdID)
				
#### EDA spending or spending per minute vs. turnout rate
		
		sptor=merge(ctrtor, adsall, by="AdID")	
		sptor=distinct(sptor)
		ggplot(sptor, aes(AdSpend, med))+geom_point()
		
		#spend per minute
		sptor$rpm=sptor$AdSpend/as.numeric(sptor$Duration)
		ggplot(sptor, aes(rpm, med))+geom_point()
		
#### EDA impression per ruble vs. turnout rate
		
		ipr=sptor$Impressions/sptor$AdSpend
		sptor$ipr=ipr
		
		ggplot(sptor, aes(med, ipr))+geom_point()
		
#### EDA spending over time
		
		ggplot(data = sptor) + 
			geom_point(aes(x = CreationDate, y = 1:4, shape='a',color=AdID),size=2)  + 
			geom_point(aes(x = EndDate, y = 1:4,shape='b',color=AdID),size=2) + 
			geom_segment(aes(x = CreationDate, xend = EndDate, y=1:4,yend=1:4,color=AdID)) + 
			scale_y_reverse() + xlab('average CDRS') + ylab('Term') + 
			scale_shape_manual(name='',values=c('a'=19,'b'=17),labels=c('pre SMART','post SMART')) + 
			scale_color_discrete(name='AdID')
		
		
				
#### EDA rubles per minute vs impressions
				
			#merge
				adsall$rpm=rpm
				
				AdIDRPM=adsall[,c(1,30)]
				#write.csv(AdIDRPM, file='AdIDRPM.csv')

				rpm.imp=ggplot(adsall, aes(rpm, Impressions, alpha=.5))+geom_point()
				rpm.imp
				
				r.imp=ggplot(adsall, aes(AdSpend, Impressions, alpha=.5))+geom_point()
				r.imp
				
				#summarize ads with high spending (aka higher than the dark cloud near the origin)
				
				adsbigspend=subset(ads, AdSpend>50000) #100,000
				View(summary(adsbigspend))
				bigtext=adsbigspend$AdText
				bigtable=table(bigtext)
				bigtdf=as.data.frame(bigtable)
				names(bigtdf)
				
				bigtexts=subset(bigtdf, Freq>0)
				bigtexts
				
				#hist of rpm
				rpmh=ggplot(adsall, aes(rpm))+geom_histogram()
				rpmh
				
				#hist of impressions
				
				imph=ggplot(adsall, aes(Impressions))+geom_histogram()
				imph
				
				View(subset(adsall, Impressions>500000))
				
				#rpm vs. ctr
				
				rpmctr=merge(adsall, ctr.ad, by="AdID")
				rpmctr=distinct(rpmctr)
				
				rpmctrp=ggplot(rpmctr, aes(rpm, ctr, alpha=.15))+geom_point()
				rpmctrp
				
				
				#rpm vs. Impressions
				
				ri=ggplot(rpmctr, aes(rpm, Impressions, alpha=.25))+geom_point()
				ri
				
				#something else
				
				subset(rpmctr, ctr>45)
				rcless=rpmctr[-c(which(rpmctr$Location=="United States")),]
				rcless=rcless[-c(str_which(rcless$Location, "Living In: United States")),]

				ggplot(rcless, aes(ctr))+geom_histogram()
				#look up scatter plot of two ratios
				View(subset(rcless, ctr<2))
				
				ggplot(rcless, aes(Clicks, Impressions, alpha=.25))+geom_point()
				
				
				#rpm vs. clicks
				
				ggplot(rpmctr, aes(Clicks, rpm, alpha=.25))+geom_point()
				
				#duration (minutes) vs. impressions
				
				ggplot(adsall, aes(as.numeric(Duration), Impressions, alpha=.25))+geom_point()
				
				#duration vs ctr
				
				ggplot(rpmctr, aes(as.numeric(Duration), ctr, alpha=.25))+geom_point()
				
				#ctr over time, using end date on the x axis to see whether they improved. find the max and min for each date
				rpmctt=as.tbl(rpmctr)
				rctrg=group_by(rpmctt, end)
				ctrsum=summarize(rctrg, avg=mean(ctr))
				
				ctrtime=ggplot(ctrsum, aes(end, avg))+geom_line()
				ctrtime
				
				#ref 
				a <- ggplot(economics, aes(date, unemploy))
				a + geom_ribbon(aes(ymin=unemploy - 900, ymax=unemploy + 900))
				allg=group_by(allads, fips)
				area_avg_ctrs=summarize(allg, avg=mean(as.double(ctr), na.rm = TRUE))
				
#Denny's data
				
				denny=read.csv("Facebook.csv")
				hrt=as.data.frame(table(denny$Post.Weekday))
				head(denny)
				
				dg=group_by(denny, Post.Weekday, Post.Hour)
				dh.counts=count(dg) #n is the number of times that hour and week day combo appeared
				dh.counts$hournum=1:dim(dh.counts)[1]
				
				ggplot(dh.counts, aes(hournum, n))+geom_line() #This is rather cyclical. Could be useful to see how this compares to the ads post hour data.
				
				dh.counts[1:100,]

				area_avg_ctrs=summarize(allg, avg=mean(as.double(ctr), na.rm = TRUE))
				
				
#### Denny's data post hour vs. propaganda post hour. Do if time. Could be a good classifier of propaganda vs. not propaganda.
		
		#create=propaganda created date and time, so map them onto each other. Need a count of the number of times each unique hour and weekday combo appeared.
		
#### CTR vs protests
				
#### CTR vs. connectivity
				
				connect=read_xlsx("connectivity.xlsx")
				View(connect)
				#need to match counties to their states by taking everything under the state name and giving it a number.
				#test which values of connect$state, county... are in state.names
				conloc=connect$`State, County or County Equivalent`

				
				ifstate=conloc %in% state.name
				
				#each time it switches from FALSE to TRUE, we want to start assigning a new number
				
				stind=which(ifstate==TRUE)
				stind[1]
				
				stcode=c(NULL)
				reps=seq(1, length(stind)-1)
				
				stcoder=function(z, na.rm=TRUE){
					rep(conloc[stind[z]], stind[z+1]-stind[z])
				}
					
				statenam=sapply(reps, function(z) stcoder(z))
				statenv=unlist(statenam)
				statenv=c(statenv, rep(conloc[stind[50]], length(conloc)-stind[50]+1))	
				length(statenv)
				length(conloc)
									
				conl=data.frame("State, County or County Equivalent"=conloc, 'state'=statenv)	
				condf=as.data.frame(connect)
				names(condf)[1]="stateorcounty"
				names(conl)[1]='stateorcounty'
				con=merge(conl, condf, by="stateorcounty")
				names(con)[1]='county'
				connect.county=merge(dat, con, by='county'
				
				
				#then stack on states after merging states and fips
				stateconnect=con[con$county%in%state.name,]
				stfip=data.frame("state"=dat$state_name, 'fips'=dat$state_code, 'both'=dat$new.names)
				
				stateconfip=merge(stfip, stateconnect, by='state')
				
				#stack connect.county and stateconfip
				names(stateconfip)
				names(connect.county)
				
				
				conco=connect.county[,c(6, 7, 12)]
				names(conco)[1]=c('location')
				names(conco)[2]='fips'
				const=stateconfip[,c(1, 2, 8)]
				names(const)[1]='location'

				cons=rbind(const, conco)
				
				ctrcon=merge(datctr, cons, by='fips')
				ctrcd=distinct(ctrcon)
				

				#group by adid
				names(ctrcd)[7]='connectivity'
				ctrcg=group_by(ctrcd, AdID)
				connmed=summarise(ctrcg, med.connect=median(connectivity))
				#write.csv(connmed, file='AdIDCon.csv')
				
				head(connmed)
				ctrcd=ctrcd[,c(2, 3, 6, 5, 7)]
				
				conctr=merge(ctrcd, connmed, by='AdID')
				conctr=conctr[,-c(3, 5)]
				ccd=distinct(conctr)
				ccd$ctr=as.numeric(ccd$ctr)
				ccd$med.connect=as.numeric(ccd$med.connect)
				
				ggplot(ccd, aes(med.connect, as.numeric(ctr)))+geom_point()
				
				ggplot(ccd, aes(med.connect))+geom_histogram()
				
				#impressions vs. connectivity
				
				impad=merge(datctr, ads4[,c("Impressions", "AdID")], by='AdID')
				
				impcon=merge(impad, connmed, by="AdID")
				icon=impcon[,c(1, 3, 6, 7)]
				icd=distinct(icon)
				
				ggplot(icd, aes(med.connect, Impressions, alpha=.25))+geom_point()
				
				
#### EDA mhhi vs. effectiveness
					
					#color coded by location- need a vector of states to color code though.
					mhhi.ctr.plot=ggplot(mhhi.ad, aes(ctr, avg, alpha=.25, color=NAME))+geom_point()+labs(x="Click Through Rate (%)")
					mhhi.ctr.plot
					
					#color by state instead
					st=data.frame(rep(0, dim(mhhi.ad)[1]))
					
					for (pattern in state.name){
						st[str_which(mhhi.ad$NAME, pattern),]=pattern
					}
					these=st[,1]
					mhhi.ad$state=these
					
					names(mhhi.ad)[7]="st"
					
					mhhi.ctr.plot=ggplot(mhhi.ad, aes(ctr, avg, alpha=.25, color=st))+geom_point()+labs(x="Click Through Rate (%)")
					mhhi.ctr.plot
					
					
### EDA pid vs. effectiveness
	
	#Issue: should we average the pid props across all the locations targetted by an ad? That finds average party Identification among people targeted by that ad.
	
	adsn=read.csv("FacebookAds.csv")
	click=adsn$Clicks
	imp=adsn$Impressions
	
	ctrn=100*click/imp
	ads.ctr=data.frame("ctpct"=ctrn, "AdID"=adsn$AdID)
	
	eda.pid=merge(ads.ctr, pid.ad, by="AdID")
	
	ggplot(eda.pid, aes(dem.id, ctpct))+geom_point() #Dems
	ggplot(eda.pid, aes(rep.id, ctpct))+geom_point() #Republican
	ggplot(eda.pid, aes(ind.id, ctpct))+geom_point()
	
### EDA count vs. effectiveness. These two don't depend on location though, so should merge ads3 (count) with ctr.ad
	
		#Plotting

		ctrcount.plot=ggplot(data, aes(ctr, WordCount, alpha=.25))+geom_point()+labs(x="Click Through Rate (%)")
		ctrcount.plot
		
		#checking out the outliers
		
			out = subset(count.effect, ctr>=40)
			out.ids=c(out$AdID);out.ids
			out.data = subset(ads, AdID == 3350);out.data #ads seem to have been especially effective in New york, New York
		
			
	
	#Need to join the ctr for each ad with the long column of locations- W/O cities. Join with adid.
	
		#Take pop.ad from line 144 of 'Project Part III.Rmd'
		#names(pop.ad)=c("Name", "ID", "Clicks", 'Value')
		#int.ids=apply(as.matrix(pop.ad$ID), 1, as.integer)
		#pop.ad$ID=int.ids
		#loc.ctr=merge(pop.ad, ctr.ad.loc, by='ID')
		#loc.ctr=loc.ctr[,-c(3, 4, 6)]
	
	#Try on one state
				
		va.ctr=loc.ctr[grep("Virginia", Name, fixed=TRUE),]
	
		install.packages("usmap")
		library(usmap)	
		
		states=us_map(regions=c('states'))
		stnames=unique(states$full)
		
		sttabs=lapply(stnames, function(x) loc.ctr[grep(x, Name, fixed=TRUE),])
		
		gm_mean=function(x, na.rm=TRUE){
			exp(sum(log(x[x>0]), na.rm=na.rm)/length(x))
		}
	
		sttabs=lapply(seq(1, 51), function(x) gm_mean(apply(sttabs[[x]][,3], 2, as.double)))
		
		sttabs
		
	# Plotting
		
		t.c.less=subset(loc.ctr, People_Targeted<=10000000)
		ggplot(data=t.c.less, aes(x=People_Targeted, y=Clicks, alpha=.25))+geom_point() 
		
	#Joining CTR data with Word count + fips data
		
		ctr.wc=merge(ads.count, )
		
unique(data.distinct$fips)	

#joining the cleaned data: connmed, AdIDRPM, ctrtor, ads3

myvars=merge(connmed, AdIDRPM, by='AdID')
myvars1=merge(myvars, ctrtor, by='AdID')
myvars2=merge(myvars1, ads3, by='AdID')

names(myvars2)[2]='connectivity'
names(myvars2)[5]='tor'
#write.csv(myvars2, file='morevariables.csv')



#General EDA

summary(adsall)
summary(snapus)
summary(propub)

#EDA of Locations	- possible graphic packages are ggmap, ggplot2, or cartography. use locfip df
	#https://www.r-graph-gallery.com/bubble-map.html

library(tidycensus)
library(cartography)
library(sp)
#install.packages('leaflet')
library(leaflet)
library(maps)
library(ggplot2)
library(ggmap)
library(dplyr)
install.packages('satscanMapper')
library(satscanMapper)
data("st99_M_data")
head(st99_M_data)



map('county',col="grey", fill=TRUE, bg="white", lwd=0.05, mar=rep(1,4),border=0)
?map_data
usc=map_data("county") #merge to this data by combining the region and subregion column here, then finding their fips, and merging it to locfip
uss=map_data('state')
ussall=uss
names(uss)[5]='NAME'
usl=uss[,c(1, 2, 5)]
usl
library(data.table)
remove.packages('plyr')
detach(package:plyr)
library(dplyr)
#try taking the average coordinates of each state to get the center
usg=group_by(as.data.table(usl), NAME)
statec=summarize(usg, centerlong=mean(long), centerlat=mean(lat))

uss2=merge(usl, statec, by='NAME')
head(uss2)
uss2=distinct(uss2)

#paste county name in front of state name here
	paster3 =function(i){
		state_i=usc$region[i]
		count_i=usc$subregion[i]
		new.name=paste(count_i, state_i)
		return(new.name)
	}
	new.names2=sapply(1:length(usc$subregion), function(x) paster3(x))
	usc3=cbind(usc[,-c(5, 6)], new.names2)

count.fip$county=str_to_lower(count.fip$county)
names(usc3)[5]='county'

#join US county gps coordinates to locfip through count.fip:
count.usc=merge(usc3, count.fip, by='county')
names(count.usc)[6]='NAME' 
#row bind count.usc and uss
count.uscless=count.usc[,-c(1, 5, 7)]
uss2$code=rep("1", dim(uss2)[1]) #code=1 if it's a state, 0 otherwise.

#add county code
count.uscless$code=rep("0", dim(count.uscless)[1])
uss2=uss2[,-c(2, 3)]
names(uss2)=c('NAME','long', 'lat')

uss2$code=rep("1", dim(uss2)[1])
count.uscless=count.uscless[,-3]
count.uscless=count.uscless[,c(3, 1, 2, 4)]
uss2=uss2[,-4]
count.uss=rbind(count.uscless, uss2)

#convert NAME in both count.uss and loc.fip to lower 
count.uss$NAME=str_to_lower(count.uss$NAME)
#usc.locfip=merge(count.usc, loc.fip, by="NAME") #merging to fip data in case we want to use another exogenous variable in the future.
#usc.locfip

#find center points of counties

countg=group_by(count.uss, NAME)
countc=summarize(countg, long=mean(long), lat=mean(lat))
#rejoin codes

count.code=count.uss[,c(1, 4)]
countccode=merge(count.code, countc, by='NAME')
countc2=distinct(countccode)

#merge this to df with number of targets on city name
	num.targets=as.data.frame(table(stacked$NAME))
	names(num.targets)[1]="NAME"
	num.targets$NAME=str_to_lower(num.targets$NAME)

	
	
geo.targets=merge(countc2, num.targets, by='NAME')
geo.targets=distinct(geo.targets)

#take the average 

library(viridis)
#install.packages('mapproj')
library(mapproj)

# Center: reorder your dataset first! Big cities appear later = on top

themap= geo.targets %>%
	arrange(Freq) %>% 
	mutate(name=factor(NAME, unique(NAME))) %>% 
	ggplot() +
	geom_polygon(data = ussall, aes(x=long, y = lat, group = group), fill="darkgray", colour='grey60',alpha=0.3) +
	geom_point(data=geo.targets, aes(x=long, y=lat, size=Freq, color=code, text=NAME), alpha=0.5) +
	scale_size_continuous(range=c(0, 20)) +
	theme_void() + coord_map() + theme(legend.position="none")
	
geo.targets

install.packages('plotly')
library(plotly)

summary(themap)
intmap=ggplotly(themap, tooltip=c('NAME'))

htmlwidgets::saveWidget(intmap, 'index.html')

map('county',col="grey", fill=TRUE, bg="white", lwd=0.05, mar=rep(1,4),border=0)

#avoid overplotting- fix by eliminating redundant cases so we can see every bubble even tho overlap. also consider only plotting the boarder of the circles. just map state boundaries, don't need county boarders.
#if time, switch to a curved map boarder.

#ref
UK <- map_data("world") %>% filter(region=="UK")
data <- world.cities %>% filter(country.etc=="UK")


#try to color bubbles so states in one color, cities in another.

#ref
data <- world.cities %>% filter(country.etc=="UK")

# Rorder data + Add a new column with tooltip text
data <- data %>%
	arrange(pop) %>%
	mutate( name=factor(name, unique(name))) %>%
	mutate( mytext=paste(
		"City: ", name, "\n", 
		"Population: ", pop, sep="")
	)

# Make the map (static)
p <- data %>%
	ggplot() +
	geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
	geom_point(aes(x=long, y=lat, size=pop, color=pop, text=mytext, alpha=pop) ) +
	scale_size_continuous(range=c(1,15)) +
	scale_color_viridis(option="inferno", trans="log" ) +
	scale_alpha_continuous(trans="log") +
	theme_void() +
	ylim(50,59) +
	coord_map() +
	theme(legend.position = "none")
head(data)
p <- ggplotly(p, tooltip="text")
p
#group variable in UK appears to correspond to subregion, which is like a county
data %>%
	arrange(pop) %>% 
	mutate( name=factor(name, unique(name))) %>% 
	ggplot() +
	geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) + 
	geom_point( aes(x=long, y=lat, size=pop, color=pop), alpha=0.9) +
	scale_size_continuous(range=c(1,12)) +
	scale_color_viridis(trans="log") +
	theme_void() + ylim(50,59) + coord_map() + theme(legend.position="none")

if (require(maps)) {
	
	crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
	
	# Equivalent to crimes %>% tidyr::pivot_longer(Murder:Rape)
	vars <- lapply(names(crimes)[-1], function(j) {
		data.frame(state = crimes$state, variable = j, value = crimes[[j]])
	})
	crimes_long <- do.call("rbind", vars)
	
	states_map <- map_data("state")
	ggplot(crimes, aes(map_id = state)) +
		geom_map(aes(fill = Murder), map = states_map) +
		expand_limits(x = states_map$long, y = states_map$lat)
	
	last_plot() + coord_map()
	ggplot(crimes_long, aes(map_id = state)) +
		geom_map(aes(fill = value), map = states_map) +
		expand_limits(x = states_map$long, y = states_map$lat) +
		facet_wrap( ~ variable)
}



#Analysis

	##multiple comparisons
	library(multcomp)
	pairwise<-glht(reduced2, linfct = mcp(mfr= "Tukey"))
	summary(pairwise)
	
#regress on sets that don't repeat AdIDs- only those that have unique AdIDs so Clicks/CTR aren't repeated
	
#aux data proposal
	#Finally, in the event that we discover an auxiliary variable with a particularly strong proportional relationship, we could employ a synthetic or ratio estimation method to deduce a reasonable count of the number of clicks that occurred in each geographic location mentioned in our data. This has the potential to drastically increase our observation count, thereby improving the predictive capability of our model at the expense of unbiasedness. This notion is inspired by cluster sampling estimation, since we could consider the advertisements as our primary units, and their respective locations (New York, California, and Baltimore, MD for example) the secondary units. By this reasoning, if the size of the secondary units (measured in population) were proportional to the click count, we could estimate the number of clicks that appeared in each individual location (with some bias). We would then have to compare the sum of all these estimated clicks to the actual sum of clicks nationally to measure the magnitude of our bias. If successful, we would have a much better measure of which locations were most affected by the Russian advertisements, and could warn people accordingly. This would be a much more intuitive interpretation than that of our current model, which would only state that the difference between the expected CTR of Baltimore and the expected CTR of the rest of the country is β-hat, other things constant.

#make location dummies by comparing unique(stacked$NAME) to less. replace state.name with the former:
	
	# # Loop for state dummies

	# Loop for state dummies

	stackednamesu=unique(stacked_names)
	l = as.list(stackednamesu) #Create list for state dummies
	df3 = as.data.frame(matrix(,nrow = nrow(adsloc), ncol = length(l))) #Creates dataframe with the length of observations in df2
	names(df3) = l #dataframe 'l' is equal to df2; joins the state dummies into the df2 dataframe
	
	# The loop counts each time a state dummy coinsides with a "Location" that have the same characters
	for (i in 1:length(l)) {
		df3[,i] = ifelse(str_detect(adsloc$no.us, l[[i]]),1,0)
	}
	
	adslocid=adsloc$AdID
	df3$AdID=adslocid
	
#merge df1 and myvars2 to run reg:
	
	dfvars=merge(myvars2, df3, by='AdID') #some of the locations were dropped upon merging, so gotta delete their dummies:
	#drop an extra
	dfvars=dfvars[,-c(113)]
	#delete Temple Texas, texas city texas, scranton PA, south plains TX, richmond va, san angelo texas, san antonio, pittsburgh, 
	#odessa texas, memphis Tennessee, Lubbock TX, Lansing Michigan, Kansas, Jackson MI, Harrisburgh PA, 
	#glendale AZ, Gallup NM, Fort worth tX, Erie PA, Dallas TX, Corpus Cristi, Conroe, Chester AK, Charleston SC, Bryan TX, Brownwood TX, 
	#Beaumont TX, Amarillo TX, Abilene TX, Allentown PA.
	#names(dfvars)
	#dfvars=dfvars[,-c(9, 10, 32, 47, 57, 98, 101, 92, 94, 87, 89, 90, 85, 78, 60, 55, 51, 49, 48, 43, 42, 40, 38, 35, 31, 30, 29, 24, 22, 18, 17, 16, 8, 5, 7, 111, 105, 100, 86, 80, 68, 67)]
	

	#dfnames=names(dfvars) #drop "AdID", "AdText", "AdWordCount", "Clicks", "Impressions", "Location", "CreationDate", "EndDate", "AdSpend", "StatePopulation", "StateFips"
	
	#Make dummies for gender race and age group ASK if there's a best way to make variables for intervals.
	

		#gender:
		ifmale=ifelse(adsall$Gender=="Male", '1', '0')
		iffemale=ifelse(adsall$Gender=='Female', '1', '0')
		adsall$male=ifmale
		adsall$female=iffemale
		adsgender=adsall[,c(1, 31, 32)]
		
		dfvars=merge(dfvars, adsgender, by='AdID')
		
		toreplace=as.vector(rep(0, length(adsall$Behaviors)))
		ifafam=sapply(toreplace, as.character)
		ifafam[c(str_which(adsall$Behaviors, "African American"))]='1'
		
		adsall$AfAm=ifafam
		adsafam=adsall[,c(1, 33)]
		dfvars=merge(dfvars, adsafam, by='AdID')

		#convert all cvs to factor type:
		dfcats=dfvars[,c(7:115)]
		dffacts=data.frame(apply(dfcats, 2, as.factor))
		dfquants=dfvars[,c(2:6)]
		dfquants=apply(dfquants, 2, as.numeric)
		
		#subset to distinct combos of AdID and quant and AdID and cat
		#dfquants$AdID=dfvars$AdID
		#dffacts$AdID=dfvars$AdID
		#quantd=distinct(dfquants)
		#catd=distinct(dffacts)
		
		df=data.frame(dfquants, dffacts)
		df$AdID=dfvars$AdID
		#drop female, wilmington, and United States (for now)
		#df=df[,-c(70,72, 63)]
		df=na.exclude(df)
		ifremove=sapply(df, function(x) as.numeric(as.character(x)))
		
		ifz=apply(ifremove, 2, mean)
		lessdf=df[,c(ifz!=0)]

		df4=na.exclude(lessdf)
		summary(df4)
	
		df4[-c(16, 95, 111),-75]
		
		#fix an error
		str_which(adsall$AdID, '1036') #its rubles were actually 6000, so 
		adsall[1615,21]=6000
		itsduration=adsall[1615,27]
		class(itsduration)
		newrpm=adsall[1615,21]/4319.933
		#put good data in
		str_which(df4$AdID, '1036')
		names(df4)
		df4[1,2]=newrpm
		
		rownum=str_which(adsall$AdID, '1058') #its rubles were actually 1000, so 
		adsall[rownum,21]=1000
		itsduration=adsall[rownum,27]
		class(itsduration)
		newrpm=adsall[rownum,21]/4319.167
		#put good data in
		str_which(df4$AdID, '1058')
		names(df4)
		df4[3,2]=newrpm
		
		rownum=str_which(adsall$AdID, '1110') #its rubles were actually 1000, so 
		adsall[rownum,21]=4798.85
		itsduration=adsall[rownum,27]
		class(itsduration)
		newrpm=adsall[rownum,21]/11519.97
		#put good data in
		str_which(df4$AdID, '1110')
		names(df4)
		df4[8,2]=newrpm
rownames(df4)=df4$AdID
	reg1=lm(ctr~., df4[,-75]) #exclude AdIDs so we can add back later. Other boxcox tests showed .288 to be good
	summary(reg1)
	boxcox() #from MASS package
	# library(MASS)
	# boxcox(reg1)
	library(lmboot)
	
	?lmboot
	
#see whether ads with super low ctrs are associated with commands:
	
	

	JackObj <- jackknife(ctr^.288~ ., data = df4[,-75]) #perform the jackknife
	df[265,]
	#plot the sampling distribution of the slope coefficient
	hist(JackObj$bootEstParam[,2], main="Jackknife Sampling Distn.",
		  xlab="Slope Estimate") 

	plot(reg1) #issue is that so many obs in the center, so expect to see extreme obs above and below that cluster. Solutions: generate normally distributed data set of same size, use that as the response, and keep IVs, see how that graph looks. OR, permute the y values we have here (guarantees homoscedasticity) to see how the plot looks. 3. 
		
		#tons of leverage=1 points:
		#solutions: delete bc we didn't have enough data on them
		#censor them ex. by assigning them the same connectivity as a close city.
		
	regstep=step(reg1)
	summary(regstep) #many more locations are significant here interestingly.
	
	plot(regstep)
	
	#still a couple outliers:
	df4[c(1329, 1330),]
	
	lev1=df4[c(16, 91, 93:97, 99, 101, 102, 105, 111, 131, 136, 145, 148, 198),c(1:75)]
	View(lev1)

###NEXT: try a test set, ,look at points 16, 95, 111., maybe drop them and see what happens
	sort(table(stacked_names))
	
	#Rodu meeting April 20: explain relevance of eda more, maybe cut a few graphs and write what they showed.
	
	
##
	df4[c(16, 95, 111),]
	
	idstr=as.character(adsall$AdID)
	df4id=as.character(df4$AdID)
	
	badads=c()
	for (id in c('1036', '1039', '1058', '1069', '1080', '1085', '1110', '1116')){
		badadidx=str_which(idstr, id)
		badads=c(badads, badadidx)
	}
	
	View(adsall[badads,])
	median(adsall$Clicks/adsall$Impressions, na.rm = TRUE)
	worstads=adsall[badads,]
	erredctrs=worstads$Clicks/worstads$Impressions
	worstads$ctr=erredctrs
	View(worstads)
	#these all have high Impressions, all of their messages have a command, like Subscribe! or Follow!
	#what if we tried 5*(Clicks)+Impressions as the response? (might need to justify the '5' weight by finding typical ctr in stata. because these points were very effective in terms of number of impressions they gathered, but the low click count makes them 
	#all models are wrong but some are useful.
	
	#combine adid and residuals to see which had highest error:
	
	iderr=data.frame(AdID=df4$AdID, errs=abs(regstep$residuals))
View(iderr)	
			  

##### Classification model:

#Facebook Political Ads
getwd()
setwd("/Users/Paul/Desktop/STAT 4996-Capstone/Capstone Project/Political FB Ads/regions.csv")
reg=read.csv("regions.csv")

regname=reg$name
regid=reg[regname %in% state.name,]

#ohsnap=read_xls('snapshot.xls', sheet = 'end_dates')
#View(ohsnap) #doesn't have the years we need

snaps=read.csv('snapshots.csv') #has the years we need!!!
library(data.table)


#View(snaps$start_date)
library(lubridate)

#merge snapshot_region to regions on region_id and nyu_id respectively

sr=read.csv("snapshot_region.csv")
r=read.csv('regions.csv')	
names(r)[2]='region_id'

snapreg=merge(r, sr, by="region_id")
sreg=snapreg$name
uspol=snapreg[sreg %in% state.name,]
library(dplyr)
uspold=distinct(uspol) #This has 4m rows, with only u.s. appearing ads. Vars: 

#merge uspold's data with 'snapshots.csv' (called snaps here) on nyu_snapshot_id and nyu_id, respectively

names(snaps)[5]='nyu_snapshot_id'
ussnaps=merge(uspold, snaps, by='nyu_snapshot_id')

head(ussnaps) #hesitant to try to analyze  more snapshot data bc according to p.3-4, imps for unique snapshot IDs change depending on the date.

#merge demo_group and snapshot_demo by nyu_id and demo_id, respectively

dg=read.csv('demo_group.csv')
names(dg)[3]='demo_id'
sd=read.csv('snapshot_demo.csv')
snapdemo=merge(dg, sd, by='demo_id')
head(snapdemo)

#merge snapdemo and ussnaps by nyu_snapshot_id

snapus=merge(snapdemo, ussnaps, by='nyu_snapshot_id')
head(snapus)

#merge ads.csv, ads2, ads3 and categories.csv because both have an ID colum. Before that, need to merge page_categories to categories

pads1=read.csv('ads.csv')
padsnames=names(pads1)
pads2=read.csv('ads2.csv')
names(pads2)=padsnames
pads3=read.csv('ads3.csv')
names(pads3)=padsnames

pads=rbind(pads1, pads2, pads3)
pads$id=as.character(pads$id)

cats=read.csv('categories.csv')
names(cats)[1]='category_id'

pc=read.csv('page_categories.csv')

pcats=merge(pc, cats, by='category_id')

padscats=merge(pads, pcats, by=c('page_id'))

#merge cards and padscats on ad_archive_id

cards=read.csv("cards.csv")
names(cards)[1]='archive_id'

padscards=merge(padscats, cards, by='archive_id')

#merge this to snapshot.csv
ohsnap=read.csv('snapshot.csv')
names(ohsnap)[1]='archive_id'
padsdates=merge(padscards, ohsnap, by='archive_id')

sponsors=read.csv('ad_sponsors.csv')
names(sponsors)[1]='ad_sponsor_id'
polspons=merge(padsdates, sponsors, by='ad_sponsor_id')
View(polspons)

#compare snapus and padscards

#View(snapus)
#View(padscards)
padsnames=names(padscards)

intersect(padsnames, snapnames)

intersect(snapnames, padsnames)

names(snapus)[13]='archive_id'

polads=merge(snapus, padscards, by='archive_id')


intersect(snapus$archive_id, padscards$id)

#View(polads)

#next, need to subset to relevant time frame: 
#1. ads with start date after or equal to the first start date on propaganda. 
#2. also select polads with start date before or equal to the last end date in the propaganda
#the propaganda time data is called 'times' in the other script. find the min:
#use the %within% method to select polads after finding the extremes in propaganda
library(lubridate)
createday=round_date(create, unit='day')
create_day=ymd(createday)
times$createday=create_day
timesarr=arrange(times,createday) #This successfully ordered the ads chronologically
firsttime=timesarr$createday[1]
pick=length(timesarr$createday)
lasttime=na.omit(timesarr$createday)
pick=length(lasttime)[1]
finaltime=timesarr$createday[pick]

#order polads chronologically to make sure we have the relevant time period

polcreate=polads$start_date.x
pcreate_d=ymd(polcreate)
polads$createday=pcreate_d
pcreatearr=arrange(polads, createday)
#View(pcreatearr)


polads$start_date.x=ymd(polads$start_date.x)

polstarts=polads$start_date.x

ymd(polstarts) %within% interval(ymd(firsttime), ymd(finaltime))

#table(polstarts>=firsttime)

#ref

dates <- ymd(c("2014-12-20", "2014-12-30", "2015-01-01", "2015-01-03"))
blackouts<- c(interval(ymd("2014-12-30"), ymd("2014-12-31")),
				  interval(ymd("2014-12-30"), ymd("2015-01-03")))
dates %within% blackouts

### The snapus data has ads from 2015, but when merged with padscards, the data is only from 2018, so just analyze the snapus data for now.

snapstart=mdy(snapus$start_date)
snapschar=as.character(snapstart)
snap17=snapus[str_which(snapschar, "2017"),]
snap16=snapus[str_which(snapschar, "2016"),]
snap15=snapus[str_which(snapschar, "2015"),]

snapyears=rbind(snap15, snap16, snap17)
unique(snapyears$archive_id) #

#relpads=snapus[ymd(snapus$start_date) %within% interval(ymd(firsttime), ymd(finaltime)),] DIDNT WORK

snapus[str_which(snapus$start_date, "2017")]
#View(relpads)

names(relpads)[9]='location'
#View(table(relpads$location))

summary(relpads)

unique(relpads$nyu_snapshot_id) #this only has 7 ads, so try to cross the propublica data with the polads set to see whether any match. That way we'll have location data in the propub set

#another set

propublittle=read.csv('fbpac-ads-en-US.csv', nrows=100)
#View(propublittle)
names(propublittle)


#Location data in the 'targets' column!!! Also has some ads from 2017, so need to search for more, even earlier dates.

#Date range

propub=read.csv('fbpac-ads-en-US.csv', colClasses = c("character", "character", 'integer', 'integer', 'character', 'character', 'character', 'Date', 'Date', 'factor', 'character', 'integer', 'numeric', 'character', 'factor', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'numeric')) #specify columns,esp specify column type by just the first few hundred rows. Bash script to split the file into multiple files, 
#subset the propublica data to the relevant time frame
proc=as.character(propub$created_at)
class(proc)

library(stringr)

propub17=proc[str_which(proc, "2017")] #the earliest propublica ad started on	2017-07-31, and the propaganda ended "2017-08-14", so not too many relevant ads here.
propubtime=propub[proc<finaltime,] #only 3 political ads from the propub data set are in the relevant time frame


#according to the targeting

propub$created=proc
table(propub$created)
proc15=propub[str_which(proc, "2016"),]
proc15

#Predictiors of Propaganda/Not Propaganda

#compare gender, age, text, location, multicultural affinity (latter two are both in the 'targets' col).

#try to parse these ^ variables:

protarget=propub$targets #replace the commas with triple spaces
protarget=str_replace_all(protarget, ",", "   ")
protarget=str_replace_all(protarget, "[[:punct:]]", "")
protarget=str_replace_all(protarget, "target", "")
protarget=str_replace_all(protarget, "segment", "")

#subset to ads with the variables

ifvars=protarget!=""
propub$vars=protarget
provars=propub[ifvars,]
View(provars$vars)

#Gender Predictor

polv=str_to_lower(provars$vars)
polvc=str_remove_all(polv, 'segment')
ifgender=str_detect(polvc, 'men')
gendervars=polvc[ifgender]
nogirls=str_detect(gendervars, 'women', negate=TRUE)
ifgirls=str_detect(gendervars, 'women')
men=gendervars[nogirls]
nmen=sum(nogirls)
nwomen=sum(ifgirls)

propgen=adsall$Gender
summary(propgen)
ifmenprop=propgen=='Male'
ifwomenprop=propgen=='Female'
nmenprop=sum(ifmenprop)
nwomenprop=sum(ifwomenprop)

#the political ads targetted men
100*nmen/157117 #percent of the time, and women
100*nwomen/157117 #percent of the time, whereas the propaganda targetted men
100*nmenprop/dim(adsall)[1] #and women
100*nwomenprop/dim(adsall)[1]

#Violence Predictor

promessage=str_to_lower(provars$message)

propmessage=as.character(str_to_lower(adsall$AdText))

viowords=c('victim', 'abuse', ' death', ' dead', ' violent', 'kill', 'attack', 'murder', 'brutal', 'terror', 'fatal',' riot', 'blood', "gun", 'fight', 'shoot', 'shot', 'die', '2nd amendment', 'second amendment', 'black lives', ' war', 'firearm', 'selfdefense', 'livesmatter', 'lives matter', 'military', 'beat')

wordsdetect=function(words, text){
	texts=str_to_lower(text)
	texts=str_replace_all(texts, "[[:punct:]]", "")
	ifin=matrix(data=NA, nrow=length(text), ncol=1)
	for (word in words) {
		ifin[str_which(texts, word),]='1'
	}
	ifin[is.na(ifin),]='0'
	return(ifin)
}

ifvioprop=wordsdetect(viowords, propmessage)
propvio=table(ifvioprop)


ifviopol=wordsdetect(viowords, promessage)
polvio=table(ifviopol)

#the political ads used these propaganda-typical violent words
100*polvio[[2]]/sum(polvio) #percent of the time, whereas the propaganda used these words
100*propvio[[2]]/sum(propvio) #percent of the time

#Lone Exclamation point

#proportions of lone exclamation points

ifexprop=str_detect(propmessage, "! |!$")
ifexpol=str_detect(promessage, "! |!$")


propext=table(ifexprop)
polext=table(ifexpol)

#the political ads used one exclamation point
100*polext[[2]]/sum(polext) #percent of the time, whereas the propaganda used one exclamation point
100*propext[[2]]/sum(propext) #percent of the time



#Cop variable

copwords=c('police', ' cop', 'blue lives', 'arrest', 'sheriff')
ifcopprop=wordsdetect(copwords, propmessage)
ifcoppol=wordsdetect(copwords, promessage)
propcop=table(wordsdetect(copwords, propmessage))
polcop=table(wordsdetect(copwords, promessage))

100*propcop[[2]]/sum(propcop)
100*propcop[[2]]/sum(propcop)-100*polcop[[2]]/sum(polcop)



#Race predictor ***

racewords=c(' race', 'black', ' whites', 'hispanic', 'africa', ' raci', ' immigra')

ifracpol=wordsdetect(racewords, promessage)
polrace=table(ifracpol)

ifracprop=wordsdetect(racewords, propmessage)
proprace=table(ifracprop)


#the political ads refer to race 
100*polrace[[2]]/sum(polrace) #percent of the time, whereas the russians refer to race
100*proprace[[2]]/sum(proprace) #percent of the time!!!

100*proprace[[2]]/sum(proprace)-100*polrace[[2]]/sum(polrace)
#Fact variable

factwords=c('www.', 'source', '[:digit:]', '[(]', "[)]", "[:upper:](?=[:lower:])", '.com', ' link', 'according') #upper case letter followed by lower case letter shows presence of proper nouns

#make a new function since wordsdetect converts to lower case and doesn't count:

wordscount=function(words, text){
	texts=str_replace_all(text, "[[:punct:]]", "")
	ct=0
	for (word in words) {
		count_i=str_count(text, word)
		ct=ct+count_i
	}
	return(ct)
}
#new

factcountpol=wordscount(factwords, promessage)
nfactspol=sum(factcountpol)

factcountprop=wordscount(factwords, propmessage)
nfactsprop=sum(factcountprop)

#find facts per message, aka the typical number of facts in an ad.

nfactspol/length(promessage) #the typical political ad in the propublica set had 11.34 'facts'

nfactsprop/length(propmessage) #the typical propaganda ad has 2.69 'facts'

#old
iffactpol=wordsdetect(factwords, promessage)
polfact=table(iffactpol)

iffactprop=wordsdetect(factwords, propmessage)
propfact=table(iffactprop)


#the political ads refer to race 
100*polfact[[2]]/sum(polfact) #percent of the time, whereas the russians refer to race
100*propfact[[2]]/sum(propfact) #percent of the time!!!

#Voting/civil service predictor

vote=c('vote', 'elect', 'candidate', 'primary', 'caucus', 'republican', 'democrat')
ifvotepol=wordsdetect(vote, promessage)
ifvoteprop=wordsdetect(vote, propmessage)
polvote=table(ifvotepol)
propvote=table(ifvoteprop)
#the political ads refer to voting 
100*polvote[[2]]/sum(polvote) #percent of the time, whereas the russians refer to voting
100*propvote[[2]]/sum(propvote) #percent of the time!!!

#Baltimore/other highly targetted areas: St. Louis Missiouri, Cleveland Ohio, New York, Atlanta Georgia, Texas, California

ifloc=c("st louis|st. louis", "cleveland ", "new york", "atlanta", "texas", "california", 'united states')
iflocpol=wordsdetect(ifloc, str_to_lower(polv))
proploc=adsall$Location
iflocprop=wordsdetect(ifloc, str_to_lower(proploc))
polloc=table(iflocpol)
proploc=table(iflocprop)
#the political ads targetted these locations #Check later how often the propublica data targets any location
100*polloc[[2]]/sum(polloc) #percent of the time, whereas the russians targetted these locations
100*proploc[[2]]/sum(proploc) #percent of the time!!!

#Content type: video/not

polvars=provars
View(polvars)

#To be usage predictor

be=c(' be ', ' am ', ' is ', ' are | aren\'t', ' was ', ' were ', ' being ', ' been ')
ifvotepol=wordsdetect(vote, promessage)
ifvoteprop=wordsdetect(vote, propmessage)
polvote=table(ifvotepol)
propvote=table(ifvoteprop)
#the political ads refer to voting 
100*polvote[[2]]/sum(polvote) #percent of the time, whereas the russians refer to voting
100*propvote[[2]]/sum(propvote) #percent of the time!!!

#Duration predictor




#Other variable ideas:
#Forms of 'to be'
#They don't use the present tense or forms of to be.
#civil/social service ('vote' or 'help' or 'candidate)
#age == 18-65+
#location
#number of facts (measured in proper nouns, numbers, sources (presence of a link))
#anti-media
#southern pride/heritage
#civil/social disobedience (ex. crime, not voting, anti-election, racism)
#stopping immigration
#number of capitalized/yelled words
#post hour of week GMT

#Analysis

ifprop=c(rep(1, 3516), rep(0, 158117))
#ifmenprop, ifwomenprop, nogirls, ifgirls, ifvioprop, ifviopol, ifexprop, ifexpol, ifracpol, ifracprop, iffactpol, iffactprop, ifvotepol, ifvoteprop, 
# ifmen=c(ifmenprop, nogirls)
# ifwomen=c(ifwomenprop, ifgirls)
# ifvio=c(ifvioprop, ifviopol)
# ifexc=--c(ifexprop, ifexpol)
# ifrace=c(ifracprop, ifracpol)
# iffact=c(iffactprop, iffactpol)
# ifvote=c(ifvoteprop, ifvotepol)

propvars=data.frame(y=rep(1, 3516), ifvioprop, --ifexprop, ifracprop, ifvoteprop, ifcopprop, iflocprop, factcountprop)
polvars=data.frame(y=rep(0, 158117), ifviopol, --ifexpol, ifracpol, ifvotepol, ifcoppol, iflocpol, factcountpol)
library(dplyr)

nsamps=rep(1:2)
tprs=c()
for (i in nsamps) {
	polsamp=sample_n(as.tbl(polvars), 3516)
	names(polsamp)=c('y', 'ifvio', 'ifex', 'ifrace', 'ifvote', 'ifcop', 'iflocation', 'nfacts')
	names(propvars)=c('y', 'ifvio', 'ifex', 'ifrace', 'ifvote', 'ifcop', 'iflocation', 'nfacts') #Mr. Robbins favorite gin: something beef and blenderman?
	ifrusk=rbind(propvars, polsamp)
	ifruskq=ifrusk[,8]
	ifrusk=data.frame(apply(ifrusk[,c(1:7)], 2, as.factor)) #make variables 1:7 factors
	ifrusk$nfacts=ifruskq
	
	# ifrussia=data.frame(y=ifprop, ifexc, ifvio, ifrace, iffact, ifvote)
	# ifrussia=as.data.frame(apply(ifrussia, 2, factor))
	#proptree=tree(y~., ifrusk) #subset to most important predictors
	#plot(proptree);text(proptree, pretty=0) #these groups don't make much sense, so try looking at the proportions again
	
	#Train and Test
	
	#Split
	
	sample<-sample.int(nrow(ifrusk), floor(.50*nrow(ifrusk)), replace = F)
	train<-ifrusk[sample, ]
	test<-ifrusk[-sample, ]
	testx=test[,-1]
	testy=test[,1]
	
	#Random Forest
	
	trainy=train$y
	trainx=train[,-1]
	
	#I had difficulty selecting the tuning parameter via cross validation, so I opted to use the out of bag error method instead.
	oobers=c()
	dim(trainx)
	m=sqrt(7)
	for (i in seq(1, 20, 1)) {
		rf.fit = randomForest(trainx, trainy, ntree = 300, mtry = m, nodesize = i) #Check that nPerm=5 is the same as a five-fold cv.
		oober_i=rf.fit$err.rate[300,1]
		oobers=c(oobers, oober_i)
	}
	#write.csv(oobers, 'oobers.csv')
	bestsize=median(which(oobers==min(oobers)))
	
	#of the 20 tested node sizes, the node size=`bestsize`
	#had the smallest OOB error, so we will predict based on that setting:
	rf.fit = randomForest(trainx, as.factor(trainy), ntree = 500, mtry = m, nodesize = bestsize, importance = TRUE) #Check that nPerm=5 is the same as a five-fold cv.
	pred = predict(rf.fit, testx)
	
	#Results
	
	#Compare this to testY:
	confusionrf=table(testy, pred)
	overrf=mean(testy == pred)
	tprrf=confusionrf[2,2]/(confusionrf[2,2]+confusionrf[2,1]) #sensitivity
	tnrrf=confusionrf[1,1]/(confusionrf[1,1]+confusionrf[1,2]) #specificity
	
	#Overall Accuracy:
	overrf
	#Sensitivity:
	tprs=c(tprs, tprrf)
	#Specificity:
	tnrrf
}
overrf
tnrrf
tprrf

mean(tprs)
summary(tprs)


#Classification Tree
names(testx)
names(train)
traintree=tree(y~., train, split='gini')	

tree.pred=predict(traintree, testx, type='class')

ifcorrect=tree.pred==testy

confusionsvm=table(testy, tree.pred)
oversvm=mean(testy == tree.pred)
oversvmp=100*oversvm
tprsvm=confusionsvm[2,2]/(confusionsvm[2,2]+confusionsvm[2,1]) #sensitivity
tnrsvm=confusionsvm[1,1]/(confusionsvm[1,1]+confusionsvm[1,2]) #specificity
tppsvm=tprsvm*100
tnpsvm=tnrsvm*100

#Test accuracy:
`oversvm`
oversvmp #percent accurate overall.
#Sensitivity:
`tprsvm`
tppsvm #percent accurate when the ad was actually propaganda
#Specificity
`tnrsvm`
tnpsvm #percent accurate when the ad was actually just political

#next, look at the ones incorrectly predicted, see what they have in common.

library(randomForest)

mean(c(1.18, 1.66, 1.63))

#Nonlinear SVM

library(e1071)
fit2 <- svm(y ~ ., data = train, type='C-classification', kernel='linear',scale=FALSE)
#xgrid = expand.grid(X1 = px1, X2 = px2)
#func2 = predict(fit2, test, decision.values = TRUE)
#func2 = attributes(func2)$decision
ygrid2 = predict(fit2, testx)
# plot(testX, col = ifelse(ygrid == 'yes', "bisque", "cadetblue1"), pch = 20, cex = 0.2)
# points(trainX, col=ifelse(y=='yes', "darkorange", "deepskyblue"))
# 
# contour(px1, px2, matrix(func, 69, 99), level = 0, add = TRUE)
# contour(px1, px2, matrix(prob, 69, 99), level = 0.5, add = TRUE, col = "blue", lwd = 2)
confusionsvm2=table(testy, ygrid2)
oversvm2=mean(testy == ygrid2)
oversvm2p=100*oversvm2
tprsvm2=confusionsvm2[2,2]/(confusionsvm2[2,2]+confusionsvm2[2,1]) #sensitivity
tnrsvm2=confusionsvm2[1,1]/(confusionsvm2[1,1]+confusionsvm2[1,2]) #specificity
tppsvm2=100*tprsvm2
tnpsvm2=100*tnrsvm2
#Test accuracy
`oversvm2`
`oversvm2p` #percent accurate overall.
#Sensitivity:
`tprsvm2`
`tppsvm2` #percent accurate when predicting 'yes'.
#Specificity
`tnrsvm2`
`tnpsvm2` #percent accurate when predicting 'no'.

#binomial test will show whether the prediction rate is significantly higher than .5.







			  