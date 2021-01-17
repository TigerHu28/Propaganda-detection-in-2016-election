#Facebook Political Ads

#To Do:
	#address objections:
		#1. that the propublica data is a convenience sample. Solutions:
			#a. incorporate the NYU data scraped from the ad archive
			#b. research convenience sample data analysis improvement techniques
		#2. that the propub data and the propaganda does not cover the same time frame
			#a. show that the location dummies are still legitimate because the russians targetted the same locations on multiple occasions over time (see my location plots over time 'date_loc.png')
			#b. reduce reliance upon topical/thematic predictors like race, cops, etc.
	#improve prediction accuracy (prolly gonna need to be real high since propaganda is a small part of the pop of political ads
		#Check out the false positives- could they actually be propaganda?
		#2.5: remove prop ads with 'facemusic' in the text, or 'memopolis' posting page
		#Try pruning for variable selection.
		#3. make location dummies, then remove unimportant ones. use code from capstone ad efficacy.R. The 
		#4. make a creation date dummy- use an excel spreadsheet to see when ams typically post vs. when russians typically post. maybe assign a probability to each ad based on its creation hour of week
		#5. look at other predictor ideas listed at bottom of this
		#6. retry a bagged model
		#7. try a boosted model, others? does pruning improve prediction accuracy?

#To contact:
#1. U.S. Cyber command, https://www.cybercom.mil/Contact-Us/
#2. Facebook
#3. Propublica
#4. Ebrima N. Ceesay, PhD and CISSP, from https://iccs.fordham.edu/ai-a-trojan-attack-waiting-to-happen/
#5. 

#remove: memopolis page, 
# facemusic ad text

	#other ways to improve prediction accuracy:
		#a. remove some of the benign propaganda ads, like the ones about music. 
		#
		#c. 
	#other ways to expand upon research/improve legitimacy
		#a. run the twitter data in the facebook model
		#b. estimate how many ads on facebook are propaganda on a given day: consider the propaganda to be the population of them on facebook for that time frame, then look up how many other ads ran on facebook back then.


getwd()
#setwd("/Users/Paul/Desktop/STAT 4996-Capstone/Capstone Project/Political FB Ads/regions.csv")
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

	#head(ussnaps) #hesitant to try to analyze  more snapshot data bc according to p.3-4, imps for unique snapshot IDs change depending on the date.
	
	#merge demo_group and snapshot_demo by nyu_id and demo_id, respectively
	
	dg=read.csv('demo_group.csv')
	names(dg)[3]='demo_id'
	sd=read.csv('snapshot_demo.csv')
	snapdemo=merge(dg, sd, by='demo_id')
	#head(snapdemo)
	
	#merge snapdemo and ussnaps by nyu_snapshot_id
	
	snapus=merge(snapdemo, ussnaps, by='nyu_snapshot_id')
	#head(snapus)

#Start
	
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
	#write.csv(padscards, 'padscards.csv')
	
	#merge this to snapshot.csv
	ohsnap=read.csv('snapshot.csv')
	names(ohsnap)[1]='archive_id'
	padsdates=merge(padscards, ohsnap, by='archive_id')

	sponsors=read.csv('ad_sponsors.csv')
	names(sponsors)[1]='ad_sponsor_id'
	polspons=merge(padsdates, sponsors, by='ad_sponsor_id')
	
#compare snapus and padscards
	
	#View(snapus)
	#View(padscards)
	#padsnames=names(padscards)
	
	#names(snapus)[13]='archive_id'
	#names(snapus)[1]='nyu_id'
	
	#polads=merge(snapus, padscards, by='nyu_id') #couldn't merge these by (padscards=nyu_id,snapus=nyu_snapshot_id) or by archive ids.
	 
	#intersect(snapus$archive_id, padscards$id)

	#View(polads)
	
#next, need to subset to relevant time frame: 
		#1. ads with start date after or equal to the first start date on propaganda. 
		#2. also select polads with start date before or equal to the last end date in the propaganda
		#the propaganda time data is called 'times' in the other script. find the min:
		#use the %within% method to select polads after finding the extremes in propaganda
		# library(lubridate)
		# createday=round_date(create, unit='day')
		# create_day=ymd(createday)
		# times$createday=create_day
		# timesarr=arrange(times,createday) #This successfully ordered the ads chronologically
		# firsttime=timesarr$createday[1]
		# pick=length(timesarr$createday)
		# lasttime=na.omit(timesarr$createday)
		# pick=length(lasttime)[1]
		# finaltime=timesarr$createday[pick]
		
		#order polads chronologically to make sure we have the relevant time period
		
		# polcreate=polads$start_date.x
		# pcreate_d=ymd(polcreate)
		# polads$createday=pcreate_d
		# pcreatearr=arrange(polads, createday)
		# #View(pcreatearr)
		# 	
		
	# polads$start_date.x=ymd(polads$start_date.x)
	# 	
	# polstarts=polads$start_date.x
	# 
	# ymd(polstarts) %within% interval(ymd(firsttime), ymd(finaltime))
	# 
	# #table(polstarts>=firsttime)
	# 
	# #ref
	# 
	# dates <- ymd(c("2014-12-20", "2014-12-30", "2015-01-01", "2015-01-03"))
	# blackouts<- c(interval(ymd("2014-12-30"), ymd("2014-12-31")),
	# 				  interval(ymd("2014-12-30"), ymd("2015-01-03")))
	# dates %within% blackouts
	
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
	
	#names(relpads)[9]='location'
	#View(table(relpads$location))

	summary(relpads)
	
	unique(relpads$nyu_snapshot_id) #this only has 7 ads, so try to cross the propublica data with the polads set to see whether any match. That way we'll have location data in the propub set
	
#Propublica Data
	
	#Location data in the 'targets' column!!! Also has some ads from 2017, so need to search for more, even earlier dates.
	propublittle=read.csv('fbpac-ads-en-US.csv', nrows=100)
	#View(propublittle)
	propub=read.csv('fbpac-ads-en-US.csv', colClasses = c("character", "character", 'integer', 'integer', 'character', 'character', 'character', 'Date', 'Date', 'factor', 'character', 'integer', 'numeric', 'character', 'factor', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'numeric')) #specify columns,esp specify column type by just the first few hundred rows. Bash script to split the file into multiple files, 
	propub=subset(propub, political_probability>.5)
	propub2=read.csv('propubnew.csv', colClasses = c("character", "character", 'integer', 'integer', 'character', 'character', 'character', 'Date', 'Date', 'factor', 'character', 'integer', 'numeric', 'character', 'factor', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'numeric')) #specify columns,esp specify column type by just the first few hundred rows. Bash script to split the file into multiple files, 
	#propub2=subset(propub2, political_probability>.5)
	#are all the new ads really <.5 political probability? Subset to them with adid
	
		p1id=as.character(propub$id)
		p2id=as.character(propub2$id)
		
		ifsame=p1id%in%p2id
		sum(ifsame)
		propub[1,6]
		propub2[1,6]
		
	#Subset the propublica data to the relevant time frame
	
		#proc=as.character(propub$created_at)
		library(stringr)
		#propub17=proc[str_which(proc, "2017")] #the earliest propublica ad started on	2017-07-31, and the propaganda ended "2017-08-14", so not too many relevant ads here.
		#propubtime=propub[proc<finaltime,] #only 3 political ads from the propub data set are in the relevant time frame
		#propub$created=proc
		#table(propub$created)
		#proc15=propub[str_which(proc, "2016"),]
	
	#Remove propaganda by 'memeopolis', and ones with 'facemusic' in the text
	
		ifmeme=str_detect(str_to_lower(adsall$LandingPage), "memopolis")
		adsall=adsall[sapply(ifmeme, isFALSE),]
		ifmusic=str_detect(str_to_lower(adsall$AdText), "facemusic", negate = TRUE)
		adsall=adsall[ifmusic,]
	
#Row bind NYU padscards data to propublica data
	#since we lack location data for padscards, need to put 0 in all of the location dummies for them.
	
	nyu=padscards[,c('text.x', 'text.y', 'video_url.x')]
	#combind text.x and text.y
	textx=as.character(nyu$text.x)
	texty=as.character(nyu$text.y)
	newtext=str_c(texty, textx, sep=' ')
	nyu=data.frame('ifvideo'=ifelse(str_detect(as.character(nyu$video_url.x), '.'), TRUE, FALSE), 'message'=newtext)
	
	
#Clean
	
	#Propaganda 
		#remove URLS. subset to rows with urls, then split, and extract the urls.
		propmessage=as.character(adsall$AdText)
		propurls=str_extract(propmessage, " .*\\.\\w\\w\\w")
		
		propmc1=str_replace_all(propmessage, ".*Ad Text", "")#remove everything before 'Ad Text' and after 'Location:'
		propmc1=str_replace_all(propmc1, "Location.*.$", "")
		propmc1=str_trim(propmc1, side='both')		#remove first space padding
		
		#split, then remove the ones containing the pattern:
		patrm=function(text, pattern, end='[:blank:]'){
			testspl=str_split(text, end)
			ifmatch=sapply(testspl, function(x) str_detect(x, pattern, negate = TRUE))
			nreps=1:length(text)
			womatches=sapply(nreps, function(x) testspl[[x]][unlist(ifmatch[x])])
			ctext=sapply(nreps, function(x) str_c(womatches[[x]], sep=' ', collapse=' '))
			return(ctext)
		}
		#remove URLS:
		propmc=patrm(propmc1, '\\.\\w\\w\\w')
		propmc=patrm(propmc, 'bit.ly')
		propmc=patrm(propmc, 'goo.gl')
		propmc=patrm(propmc, 'ow.ly')
		propmc=patrm(propmc, '\\.\\w\\w')
		propmc=patrm(propmc, 'www\\.')
		
		#Check
			proptab=table(propmc) #only 36 are blank. check whether I deleted them
			#proptxt=data.frame(propmc, adsall$AdText)
			#View(proptxt[str_which(propmc, ".", negate = TRUE),]) #I did not delete them- must have just been images.
			
		
		#Propublica ads
		
			protarget=propub$targets #replace the commas with triple spaces
			protarget=str_replace_all(protarget, ",", "   ")
			protarget=str_replace_all(protarget, "[[:punct:]]", "")
			protarget=str_replace_all(protarget, "target", "")
			protarget=str_replace_all(protarget, "segment", "")
			propub$vars=protarget
			provars=propub
			promessage=c(provars$message)
			promu=unique(promessage) #only 70997 here, from 111860
			polurls=str_extract(promessage, "\\s.*\\.\\w\\w\\w.*\\s")
	
			pleasebclean=str_remove_all(promessage, '\\<(.*?)\\>') #STR1(.*?)STR2
		
			#remove URLS:
			poltxtc=patrm(pleasebclean, '\\.\\w\\w\\w', "[:blank:]")
			poltxtc=patrm(poltxtc, 'bit.ly', "[:blank:]")
			poltxtc=patrm(poltxtc, 'goo.gl', "[:blank:]")
			poltxtc=patrm(poltxtc, 'ow.ly', "[:blank:]")
			poltxtc=patrm(poltxtc, '\\.\\w\\w', "[:blank:]")
			poltxtc=patrm(poltxtc, 'www\\.', "[:blank:]")
		
			
			lastpolid=4000+length(promessage)-1
			polids=4000:lastpolid
			
			poltxtid=data.frame(polids, poltxtc, promessage)
			#check
			smallpol=poltxtid[str_count(promessage)<40,] #rejoin these to their original text to figure out what happened.
			View(smallpol)
			
			#looks pretty much clean. proceed
			#make sure not too many blank texts:
			#msgtab=table(poltxtc)
			#Very few blank ads
		
		#NYU ads
		
			nyum=as.character(nyu$message)
			#nyumu=unique(as.character(nyu$message)) #25000 ads originally, but only 3855 unique texts. 
			#clean instances of " &#123;&#123;product.brand&#125;&#125;"
			nyum=str_replace_all(nyum, '\\s[:punct:][:punct:]123.*[:punct:][:punct:][:punct:]125', '')
			nyum=str_replace_all(nyum, '\\n', '')
			nyum=iconv(nyum, "latin1", "ASCII", sub="") #convert to only latin characters.
			nyum=str_replace_all(nyum, '\\<br \\/\\>', '')
			nyum=str_replace_all(nyum, '\\&\\#039\\;', '\"')
			nyum=str_remove_all(nyum, '\\<(.*?)\\>')
			#remove urls
			nyum=patrm(nyum, '\\.\\w\\w\\w', "[:blank:]")
			nyum=patrm(nyum, 'bit.ly', "[:blank:]")
			nyum=patrm(nyum, 'goo.gl', "[:blank:]")
			nyum=patrm(nyum, 'ow.ly', "[:blank:]")
			nyum=patrm(nyum, '\\.\\w\\w', "[:blank:]")
			nyum=patrm(nyum, 'www\\.', "[:blank:]")
			nyum=str_remove(nyum, '\\;$')#remove that semicolon on the end, and end spaces
			nyum=str_trim(nyum, side='both') #the 25510 ads only have 2651 unique messages
			
			#check
			#nyumu=unique(nyum)
			#head(nyumu)
			nyutab=table(nyum)
		
	#Combine american ads:
		
		promessage=c(provars$message, as.character(nyu$message))
		promc=c(poltxtc, nyum)
	
		
#Predictiors of Propaganda/Not Propaganda
		
#Age targeting
		
		propage=as.character(adsall$Age)
		propage=str_replace_all(propage, '\\+', ' and older')
		propage=str_replace_all(propage, '\\-', ' to ')
		proage=propub$targets
		
		#attach snapus$age_range (from NYU) need to randomly sample length(nyum) ads from it.
		nyuage=sample_n(data.frame(snapus$age_range), length(nyum))
		nyuage=as.character(nyuage$snapus.age_range)
		#make uniform format
		nyuage=str_replace_all(nyuage, '\\+', ' and older')
		nyuage=str_replace_all(nyuage, '\\-', ' to ')
		proage=c(proage, nyuage)
		
		proages=proage[str_which(proage, 'Age')]
		pro18up=proage[str_which(proage, '18')]
		100*length(pro18up)/length(proage) #political ads target '18 and older' this percent of the time
		#whereas propaganda specifies this __ percent of the time.
		prop18up=propage[str_which(propage, "18")]
		100*length(prop18up)/length(propage)
		#propaganda targets specifically 18 year olds 31.26967% more than the typical ad. Might be helpful
		100*length(prop18up)/length(propage)-100*length(pro18up)/length(proage)
		
		if18prop=str_detect(propage, '18')
		if18pol=str_detect(proage, '18')
		
		#isolate political ad age targets
			#split on brackets, pick the ones containing 'Age'
			prospl=str_split(proage, '\\}\\, \\{')
			prospl2=unlist(prospl)
			proa=str_detect(prospl2, "\\\"Age")
			proagec=prospl2[proa]
			proagec=str_remove_all(proagec, ".*segment")
			proagec=str_remove_all(proagec, "[:punct:]")
			proagec=str_trim(proagec)
			View(table(proagec))
			View(table(propage)) #seems like the russians descriminate less by age as long as the targets are over 18.
		
	#compare text word frequencies- put matching words in the same row of a df, and put zero if not mentioned by the other set.
	polwords=unlist(str_split(str_to_lower(promc), " "))
	polifword=str_detect(polwords, ".")
	polwords=polwords[polifword];library(data.table)
	polwords=data.table(table(polwords))
	propwords=unlist(str_split(str_to_lower(propmc), " "))
	propwords=propwords[str_detect(propwords, ".")]
	propwords=data.table(table(propwords))
	names(polwords)[1]='word'
	names(propwords)[1]='word'
	
	wordcts=merge(polwords, propwords, by='word')
	names(wordcts)=c('word', 'political_count', 'propaganda_count') #might need to divide these counts by the number of ads in each set.
	#make a percent of the time each word appeared in the sets. aka '0' appeared in the propaganda 100*3/3255 percent of the time.
	wordcts$political_pctoccur=100*wordcts$political_count/sum(wordcts$political_count)
	wordcts$propaganda_pctoccur=100*wordcts$propaganda_count/sum(wordcts$propaganda_count)
	
	#find the biggest absolute difference in word appearance rates
	wordcts$absdiff=abs(wordcts$political_pctoccur-wordcts$propaganda_pctoccur)

#Word binaries for 'to', 'the', 'vote', 'black', 'police', 'our', and 'help', '
	
	#combine all the messages to quickly make predictors
	messages=c(propmc, promc)
	ifvote=str_count(messages, '(?<= |^)vote(?= |$)')
	ifblack=str_count(messages, '(?<= |^)black(?= |$)')
	ifpolice=str_count(messages, '(?<= |^)police(?= |$)')
	ifour=str_count(messages, '(?<= |^)our(?= |$)')
	ifhelp=str_count(messages, '(?<= |^)help(?= |$)')
	ifto=str_count(messages, '(?<= |^)to(?= |$)')
	nwords=str_count(messages, ' ')+1#find number of words in each ad for standardization
	wordvars=data.frame(ifprop=c(rep('1', 3255), rep('0', length(promc))), ifvote/nwords, ifblack/nwords, ifpolice/nwords, ifour/nwords, ifhelp/nwords, ifto/nwords)
	

	
	#compare gender, age, text, location, multicultural affinity (latter two are both in the 'targets' col).
	
	#try to parse these ^ variables:
	
		protarget=propub$targets #replace the commas with triple spaces
		protarget=str_replace_all(protarget, ",", "   ")
		protarget=str_replace_all(protarget, "[[:punct:]]", "")
		protarget=str_replace_all(protarget, "target", "")
		protarget=str_replace_all(protarget, "segment", "")
		propub$vars=protarget
		provars=propub
	
	#Gender Predictor
		
		 polv=str_to_lower(provars$vars)
		# polvc=str_remove_all(polv, 'segment')
		# ifgender=str_detect(polvc, 'men')
		# gendervars=polvc[ifgender]
		# nogirls=str_detect(gendervars, 'women', negate=TRUE)
		# ifgirls=str_detect(gendervars, 'women')
		# men=gendervars[nogirls]
		# nmen=sum(nogirls)
		# nwomen=sum(ifgirls)
		# 
		# propgen=adsall$Gender
		# summary(propgen)
		# ifmenprop=propgen=='Male'
		# ifwomenprop=propgen=='Female'
		# nmenprop=sum(ifmenprop)
		# nwomenprop=sum(ifwomenprop)
		# 
		# #the political ads targetted men
		# 100*nmen/157117 #percent of the time, and women
		# 100*nwomen/157117 #percent of the time, whereas the propaganda targetted men
		# 100*nmenprop/dim(adsall)[1] #and women
		# 100*nwomenprop/dim(adsall)[1]
	
	#Violence Predictor
	
		viowords=c('bomb', ' victim', ' death', ' dead', ' violent', ' kill', ' murder', ' brutal', ' terror', ' fatal',' riot', ' blood', ' shoot', ' shot', ' die', 'black lives', ' war$', ' war ', 'livesmatter', 'lives matter', 'dylann roof', 'brandon claxton', 'sarah reed')
		
		wordsdetect=function(words, text){
			texts=str_to_lower(text)
			texts=str_replace_all(texts, "[[:punct:]]", "")
			ifin=matrix(data=NA, nrow=length(text), ncol=1)
			for (word in words) {
				ifin[str_which(texts, word),]=TRUE
			}
			ifin[is.na(ifin),]=FALSE
			return(ifin)
		}
		
		wordscount=function(words, text){
			ct=0
			for (word in words) {
				count_i=str_count(text, word)
				ct=ct+count_i
			}
			return(ct)
		}
		#URLs might mess this up, so fix later.
		
		#Binary
		
			ifvioprop=wordsdetect(viowords, str_to_lower(propmc))
			ifviopol=wordsdetect(viowords, str_to_lower(promc))
			# 
			# propvio=table(ifvioprop)
			# polvio=table(ifviopol)
			# 
			# ifnonvioprop=apply(ifvioprop, 1, isFALSE)
			# 
			# #the political ads used a violent word point
			# 100*polvio[[2]]/sum(polvio) #percent of the time, whereas the propaganda used one exclamation point
			# 100*propvio[[2]]/sum(propvio) #percent of the time
			
		#count
		
			# propvioct=wordscount(viowords, str_to_lower(propmc))
			# polvioct=wordscount(viowords, str_to_lower(promc))
			# 
			# propnsentences=str_count(str_to_lower(propmc), "\\. |\\? |\\! |\\.$|\\?$|\\!$")  #divide each count by the number of sentences to get a measure of violent word density.
			# propnsentences[which(propnsentences==0)]<-1
			# propviodens=propvioct/nwords[1:3255]
			# polnsentences=str_count(str_to_lower(promessage), "\\. |\\? |\\! |\\.$|\\?$|\\!$")  #divide each count by the number of sentences to get a measure of violent word density.
			# polnsentences[which(polnsentences==0)]<-1
			# polviodens=polvioct/nwords[3256:length(nwords)]
			# 
			# viocts=data.frame(ifprop=c(rep('1', 3255), rep('0', length(promessage))), viodensity=c(propviodens, polviodens))
			# 
			# ggplot(viocts, aes(log(viodensity+.0009), ifprop))+geom_boxplot() #not much difference, so this must not be a good predictor as is.
			# 
			# ggplot(data.frame(prop=propvioct), aes(prop))+geom_histogram() #looks like propaganda generally have more violent words than the political ads.
			# ggplot(data.frame(pol=polvioct), aes(pol))+geom_histogram()
			# 
			# sum(propvioct>0)/length(propmessage)
			# sum(polvioct>0)/length(promessage)
			
#Lone Exclamation point
		
		#proportions of lone exclamation points
		
		ifexprop=str_detect(propmessage, "! |!$")
		ifexpol=str_detect(promessage, "! |!$")
	
		
		propext=table(ifexprop)
		polext=table(ifexpol)
		
		#the political ads used one exclamation point
		100*polext[[2]]/sum(polext) #percent of the time, whereas the propaganda used one exclamation point
		100*propext[[2]]/sum(propext) #percent of the time
		
		#Count of them
		
		expropct=str_count(propmessage, "! |!$")
		expolct=str_count(promessage, "! |!$")
		nexprop=sum(expropct)
		nexpol=sum(expolct)
		
		
	#Cop variable
		
		copwords=c('police',  'cop', 'blue lives', 'arrest', 'sheriff', 'officer')
		ifcopprop=wordsdetect(copwords, str_to_lower(propmc))
		ifcoppol=wordsdetect(copwords, str_to_lower(promc))
		propcop=table(ifcopprop)
		polcop=table(ifcoppol)
		
		100*propcop[[2]]/sum(propcop)
		100*propcop[[2]]/sum(propcop)-100*polcop[[2]]/sum(polcop)
		#The propaganda discusses police about 16% more than american ads
		
		#copwords=c('police', ' cop', 'blue lives', 'arrest', 'sheriff', 'law enforcement', 'officer')
		
	#Race/ethnicity predictor ***
		
		racewords=c(' race', 'black', ' whites', 'hispanic', 'africa', ' raci', ' immigra', 'muslim', 'jew', 'islam', 'brown')
		
		ifracpol=wordsdetect(racewords, str_to_lower(promc))
		polrace=table(ifracpol)
		
		ifracprop=wordsdetect(racewords, str_to_lower(propmc))
		proprace=table(ifracprop)
		
	
		#the political ads refer to race 
		100*polrace[[2]]/sum(polrace) #percent of the time, whereas the russians refer to race
		100*proprace[[2]]/sum(proprace) #percent of the time!!!
	
		100*proprace[[2]]/sum(proprace)-100*polrace[[2]]/sum(polrace)
		
#URL variable: whether or not it contains a url to an outside/non-social media site.
		
		ifurlprop=str_detect(propmessage, " .*\\.\\w\\w\\w")
		ifurlpol=str_detect(promessage, " .*\\.\\w\\w\\w")
		
		100*sum(ifurlprop)/length(propmessage) #percent of propaganda contains a url
		100*sum(ifurlpol)/length(promessage) #percent of propaganda contains a url

		
		
#Fact variable
		
		propurls=str_extract(propmessage, " .*\\.\\w\\w\\w")


		factwords=c('source', '[:digit:]', '.gov','.org', '.edu', 'according', ' poll', ' study', ' dollar', " thousand", 'hundred', 'million', 'research') #upper case letter followed by lower case letter shows presence of proper nouns

		#new
		
			polfactct=wordscount(factwords, str_to_lower(promc))
			
			
			propfactct=wordscount(factwords, str_to_lower(propmc))
			
		#add proper nouns and parenthases
			morefacts=c('[(]', "[)]", "[^[:punct:]] [[:upper:]][[:lower:]]", '\\"') #need to exclude beginnings of sentences
			polfactct2=wordscount(morefacts, promc)
			propfactct2=wordscount(morefacts, propmc)
			#element-wise add counts
			polfactcts=(polfactct+polfactct2)/nwords[3256:length(nwords)]
			propfactcts=(propfactct+propfactct2)/nwords[1:3255]
			
			nfactspol=sum(polfactcts)
			nfactsprop=sum(propfactcts)
			
			#find facts per message, aka the typical number of facts in an ad.
			
			nfactspol/length(promessage) #the typical political ad in the propublica set had 11.34 'facts'
			summary(polfactcts)
			nfactsprop/length(propmessage) #the typical propaganda ad has 2.69 'facts'
			summary(propfactcts)
			
			factcts=data.frame(ifprop=c(rep('1', 3255), rep('0', length(promessage))), nfacts=c(propfactcts, polfactcts))
			
			#boxplot to compare the distributions
			
			ggplot(factcts, aes(nfacts, ifprop))+geom_boxplot()
			#figure out why so many outliers. actually, they seem legit.
			#use polfactcts and propfactcts in the models.

	#Voting/civil service/prosocial
		
		vote=c('green', ' supporting', 'vote|voting', 'elect', 'ballot', 'candidate', 'primary', 'caucus', 'republican', 'democrat', 'help', 'donat', 'sign', 'chip in', 'contribut', 'fundrais', 'climate change', ' epa ', ' clean', ' renewable', 'school', 'environment')
		ifvotepol=wordsdetect(vote, str_to_lower(promc))
		ifvoteprop=wordsdetect(vote, str_to_lower(propmessage))
		polvote=table(ifvotepol)
		propvote=table(ifvoteprop)
		#the political ads refer to voting 
		100*polvote[[2]]/sum(polvote) #percent of the time, whereas the russians refer to voting
		100*propvote[[2]]/sum(propvote) #percent of the time!!!
		#old pct different:
		100*polvote[[2]]/sum(polvote)-100*propvote[[2]]/sum(propvote) #42.48581
		
		votepolct=wordscount(vote, promessage)
		votepropct=wordscount(vote, propmessage)
		
		summary(votepolct)
		summary(votepropct)
		
#Location: Baltimore/other highly targetted areas: St. Louis Missiouri, Cleveland Ohio, New York, Atlanta Georgia, Texas, California
		
		manytargets=c('baltimore', "st louis", "cleveland", "atlanta", "texas", 'illinois', 'new orleans', 'milwaukee', 'maryland', 'charlotte', 'detroit', 'richmond', 'minneapolis', 'san francisco', 'houston', 'lancaster', 'madison', 'oakland', 'ferguson')
		#make a dummy variable for each of these.
		proploc=adsall$Location
		polv=str_to_lower(provars$vars)
		polv2=polv[polifword]#remove the polv rows without corresponding text data
		proppolloc=str_to_lower(c(proploc, polv)) #propaganda first, political second

			ifbalt=str_detect(proppolloc, pattern='baltimore')
			ifstlou=str_detect(proppolloc, pattern='st louis')
			ifclev=str_detect(proppolloc, pattern='cleveland')
			ifatl=str_detect(proppolloc, pattern='atlanta')
			iftex=str_detect(proppolloc, pattern='texas')
			ifilli=str_detect(proppolloc, pattern='illinois')
			ifnewo=str_detect(proppolloc, pattern='new orleans')
			ifus=str_detect(proppolloc, pattern='united states')
			
			locdums=cbind(ifbalt, ifstlou, ifclev, ifatl, iftex, ifilli, ifnewo, ifus)
																
			#row bind a (25510)x8 df of zeroes since we don't know the locations for said data. later, maybe try to get from the other nyu data.
			#later, randomly sample location targets of 25510 other nyu ads, and test them above.
			nyudums=as.data.frame(matrix(data=rep(0, 25510*8), nrow=25510, ncol = 8))
			nyudums=data.frame(nyudums==1)
			names(nyudums)=c('ifbalt', 'ifstlou', 'ifclev', 'ifatl', 'iftex', 'ifilli', 'ifnewo', 'ifus')
			locdums=rbind(locdums, nyudums)
			
#Content type: video/not
			
			#0001450, 0001452, 
		
		# polvars=provars
		# #View(polvars)
		# 
		# #figure out how many of padscards$video_url.y include .mp4
		# 
		# ifmp4=str_detect(padscards$video_url.y, 'mp4')
		# sum(ifmp4)
		# vidurlytab=table(padscards$video_url.y)
		# 
		# #do the same for the propaganda urls
		# 
		# adsall$LandingPage
		# propsourcezip=data.frame(SourceFile=adsall$SourceFile, SourceZip=adsall$SourceZip)
		#save the adsall source file and source zip to excel to manually input v/not video
		#write.csv(propsourcezip, file='sourcezip.csv')
		
#To be usage predictor
		
		# be=c(' be ', ' am ', ' is ', ' are | aren\'t', ' was ', ' were ', ' being ', ' been ')
		# ifvotepol=wordsdetect(vote, promessage)
		# ifvoteprop=wordsdetect(vote, propmessage)
		# polvote=table(ifvotepol)
		# propvote=table(ifvoteprop)
		# #the political ads refer to voting 
		# 100*polvote[[2]]/sum(polvote) #percent of the time, whereas the russians refer to voting
		# 100*propvote[[2]]/sum(propvote) #percent of the time!!!
	
#Sentiment analysis
		
		#install.packages("tidytext")
		#install.packages('textdata')
		library(tidytext)
		
		wordsents=get_sentiments('nrc')
		
		#match 'word' column to each word in each ad's text.
		
#Video
		
	vids=read.csv('sourcezip2.csv')
	vid=vids$X
	dim(vids)

#Word lengths; find the average number of characters in each word
	
	#remove links and hashtags: delete everything between www. and .com and http and .com
	# propmc=str_replace_all(str_to_lower(propmessage), "www\\..*\\.com", "")
	# propmc=str_replace_all(propmc, "#.* ", "")
	# propmc=str_replace_all(propmc, "\\<p\\>", "") #remove strange <p> values
	# propmc=str_replace_all(propmc, "href.*", "")#get rid of everything between href and <p>
	# propmc=str_replace_all(propmc, "class\\=.* ", "")##remove all between class= and the next space
	# propmc=str_replace_all(propmc, "class\\=.*$", "")
	# propmc=str_replace_all(propmc, "[[:punct:]]a.*[[:punct:]]p", "")
	# propmc=str_replace_all(propmc, "http.* ", "")#get rid of everything between http and the next space
	# propmc=str_replace_all(propmc, "http.*us", "")
	# propmc=str_replace_all(propmc, "http.*com", "")
	# propmc=str_replace_all(propmc, "http.* ", "") #remove everything between https and a space, and replace hyphens
	# propmc=str_remove_all(propmc, "https") #finally, remove the https's
	# propmc=str_replace_all(propmc, "http.*.$", "")
	# propmc=str_replace_all(propmc, "[^[:alnum:] \\.com\\/\\-\\-]", "")
	# propmc=str_replace_all(propmc, '\\.\\w\\w\\w.*.$', ".com") #delete everything after the .com/.gov/.net and before the next space or end. replace with .com
	# propmc=str_replace_all(propmc, "5afz.*", "") #remove everything after 5afz
	# propmc=str_replace_all(propmc, ' .*\\.\\w\\w\\w', "") #remove everything between ' ' and .gov, .net, .edu
	# propmc=propmc=str_replace_all(propmc, 'hovercard.*', "")#delete everything after hovercard, 
	# propmc=str_replace_all(propmc, 'span idu|div .*', "")#remove all after 'span ' and 'div '
	# propmc=str_replace_all(propmc, '\\/', " ")
	# propmc=str_replace_all(propmc, ' .*\\.us|\\.tv', "")#remove all before .us
	# propmc=str_replace_all(propmc, 'span iqw.*', "")#remove all after span iqw
	# propmc=str_replace_all(propmc, '\\.', "")#remove left over periods
	# propmc=str_replace_all(propmc, '-', ' ')
	# propmc=str_replace_all(propmc, '-', ' ')
	# propmspl=str_split(propmc, " ")
	# #repeat for the political ads
	# promc=str_replace_all(str_to_lower(promessage), "www\\..*\\.com", "")
	# promc=str_replace_all(promc, "#.* ", "")
	# promc=str_replace_all(promc, "\\<p\\>", "") #remove strange <p> values
	# promc=str_replace_all(promc, "href.*", "")#get rid of everything between href and <p>
	# promc=str_replace_all(promc, "class\\=.* ", "")##remove all between class= and the next space
	# promc=str_replace_all(promc, "class\\=.*$", "")
	# promc=str_replace_all(promc, "[[:punct:]]a.*[[:punct:]]p", "")
	# promc=str_replace_all(promc, "http.* ", "")#get rid of everything between http and the next space
	# promc=str_replace_all(promc, "http.*us", "")
	# promc=str_replace_all(promc, "http.*com", "")
	# promc=str_replace_all(promc, "http.* ", "") #remove everything between https and a space, and replace hyphens
	# promc=str_remove_all(promc, "https") #finally, remove the https's
	# promc=str_replace_all(promc, "http.*.$", "")
	# promc=str_replace_all(promc, "[^[:alnum:] \\.com\\/\\-\\-]", "")
	# promc=str_replace_all(promc, '\\.\\w\\w\\w.*.$', ".com") #delete everything after the .com/.gov/.net and before the next space or end. replace with .com
	# promc=str_replace_all(promc, "5afz.*", "") #remove everything after 5afz
	# promc=str_replace_all(promc, ' .*\\.\\w\\w\\w', "") #remove everything between ' ' and .gov, .net, .edu
	# promc=promc=str_replace_all(promc, 'hovercard.*', "")#delete everything after hovercard, 
	# promc=promc=str_replace_all(promc, 'span idu|div .*', "")#remove all after 'span ' and 'div '
	# promc=str_replace_all(promc, '\\/', " ")
	# promc=str_replace_all(promc, ' .*\\.us|\\.tv', "")#remove all before .us
	# promc=str_replace_all(promc, 'span iqw.*', "")#remove all after span iqw
	# promc=str_replace_all(promc, '\\.', "")#remove left over periods
	# promc=str_replace_all(promc, '-', ' ')
	# promc=str_replace_all(promc, '-', ' ')
	# typos=promc[which(polmax>18)] #fix case by case
	# promc=str_replace_all(promc, 'makersrepublic', 'makers republic')
	# promc=str_replace_all(promc, 'onerepublic', 'one republic')
	# promc=str_replace_all(promc, 'lessdemon', 'less demon')
	# promc=str_replace_all(promc, 'makersrepublic', 'makers republic')
	# promc=str_replace_all(promc, 'tiveserik', 'tives erik')
	# promc=str_replace_all(promc, 'makersrepublic', 'makers republic')
	# promc=str_replace_all(promc, 'careendanger', 'care endanger')
	# 
	# promspl=str_split(promc, " ")
	# 
	# 
	# #find the length of the biggest word in each ad
	# propmax=sapply(1:3255, function(x) max(str_length(propmspl[[x]]))) #characters per word for each ad
	# 					
	# polmax=sapply(1:length(promspl), function(x) max(str_length(promspl[[x]]))) #characters per word for each ad

	#hist of characters per word for political/propub ads
		#library(ggplot2)
		#ggplot(sample_n(data.frame(polmax), 3255), aes(polmax))+geom_histogram()
		
	#hist of characters per word for propaganda
		#ggplot(data.frame(propmax), aes(propmax))+geom_histogram()
	
		#the distributions look very similar, so drop this as a predictor.

#Repost binary
	
	propre=str_detect(str_to_lower(propmessage), 'repost')
	polre=str_detect(str_to_lower(promessage), 'repost')
	
	#proportion of propaganda with 'repost':
	sum(propre)/length(propmessage)
	
#Pronoun usage predictor
	
	#count the number of times pronouns are used per sentence 
	
		# pronouns=c('(?<= |^)i(?= |$)', '(?<= |^)you(?= |$)', '(?<= |^)he(?= |$)', '(?<= |^)she(?= |$)', '(?<= |^)it(?= |$)', '(?<= |^)we ', '(?<= |^)they(?= |$)', '(?<= |^)me(?= |$)', '(?<= |^)him(?= |$)', '(?<= |^)her(?= |$)', '(?<= |^)us(?= |$)','(?<= |^)them(?= |$)')
		# propnounct=wordscount(pronouns, str_to_lower(propmc))
		# polnounct=wordscount(pronouns, str_to_lower(promc))
		# 
		# propnsentences=str_count(str_to_lower(propmessage), "\\. |\\? |\\! |\\.$|\\?$|\\!$")  #divide each count by the number of sentences to get a measure of violent word density.
		# propnwords=str_count(str_to_lower(propmessage), " ")+1
		# propnsentences[which(propnsentences==0)]<-1
		# propnoundens=propnounct/propnsentences
		# summary(propnoundens)
		# summary(propnounct)
		# 
		# polnsentences=str_count(str_to_lower(promessage), "\\. |\\? |\\! |\\.$|\\?$|\\!$")  #divide each count by the number of sentences to get a measure of violent word density.
		# polnsentences[which(polnsentences==0)]<-1
		# polnoundens=polnounct/polnsentences
		# 
		# noundens=data.frame(ifprop=c(rep('1', 3255), rep('0', length(promessage))), noundensity=c(propnoundens, polnoundens))
		# 
		# ggplot(noundens, aes(noundensity, ifprop))+geom_boxplot() #not much difference, so this must not be a good predictor as is.
	#a little bit of separation, as the political ads sometimes have more than 7 pronouns per sentence, unlike the propaganda.
	
	#check outliers
	
#LIWC text analysis
	
	#Make csvs of just ad text from propaganda and american ads:
	#write.csv(propmc, file='proptext.csv')
	#write.csv(promc, file='poltext.csv')
	
	#Add symbols for segmentation: attach semicolons to propaganda text, then rowbind all the text.
	
	proppol=c(propmc, promc)
	write.csv(proppol, file='proppoltext.csv')
	
#pick up here after Thursday- gotta rerun the LIWC software.
	
	#See what happened
	liwc=read.csv(file='LIWC2015 Results (proppoltext.csv).csv')
	#liwc=liwc[,-c(84:95)] #drop unused punctuation columns
	#summary(liwc) #got a ton of data, so just look at group separation of authenticity score for now.
	#end=dim(locdums)[1]
	#authentic=data.frame(authenticity=liwc$Authentic, ifprop=c(rep(1, 3255), rep(0, end-3255)))
	
	end=dim(locdums)[1]
	liwc$ifprop=c(rep('Russian', 3255), rep('American', end-3255))
	lastid=4000+length(promessage)-1
	polids=4000:lastid #this should be 137370 long
	#index for liwc needs to be the length of 3255+
	liwc$AdID=c(adsall$AdID, polids)
	

	#ggplot(liwc, aes(ifprop, Analytic))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5) #propaganda looks more analytic generally
	
	#sample so it's easier to tell
	library(dplyr)
	ggplot(authentic, aes(authenticity, factor(ifprop)))+geom_boxplot() #doesn't look like much separation, contrary to what Boyd said, so check out the other data:
	polsample=sample_n(liwc[c(3256:dim(liwc)[1]),], 3255, replace = TRUE)
	liwcsamp=rbind(liwc[c(1:3255),], polsample)
	ggplot(liwcsamp, aes(ifprop, Analytic))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)	#looks like the propaganda has more ads on the lower half of the analytic score, especially many near zero.maybe a cutoff above or equal to 25 would make a good binary predictor.
	ggplot(liwcsamp, aes(ifprop, Tone))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5) #no idea what this means
	ggplot(liwcsamp, aes(ifprop, Sixltr))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, Dic))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, function.))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, pronoun))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, ppron))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, i))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, we))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5) #maybe a cutoff above or equal to 25 would make a good binary predictor.
	ggplot(liwcsamp, aes(ifprop, you))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, shehe))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, they))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, ipron))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, article))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, prep))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5) #could be some separation here
	#!!!
	ggplot(liwcsamp, aes(ifprop, auxverb))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, adverb))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, conj))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, negate))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, verb))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, adj))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, compare))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, interrog))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, number))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5) #pretty big cluster here in american ads- might want to look at their text.
	ggplot(liwcsamp, aes(ifprop, quant))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	
	#Affect Words
	ggplot(liwcsamp, aes(ifprop, posemo))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)   #maybe a cutoff above or equal to 25 would make a good binary predictor.
	ggplot(liwcsamp, aes(ifprop, negemo))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, anx))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, anger))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, sad))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	
	#Social Words
	ggplot(liwcsamp, aes(ifprop, social))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, family))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, friend))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, female))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, male))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	
	#Cognitive Processes
	ggplot(liwcsamp, aes(ifprop, insight))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, cause))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, discrep))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, tentat))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, certain))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, differ))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	
	#Perceptual Processes
	ggplot(liwcsamp, aes(ifprop, percept))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5) 
	ggplot(liwcsamp, aes(ifprop, see))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5) #maybe a cutoff above or equal to 13.5 would make a good binary predictor.
	ggplot(liwcsamp, aes(ifprop, hear))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, feel))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	
	#Biological Processes
	ggplot(liwcsamp, aes(ifprop, bio))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, health))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	
	#Core Drives and Needs
	ggplot(liwcsamp, aes(ifprop, drives))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, affiliation))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, achieve))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, power))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, reward))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, risk))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	
	#Time orientation
	ggplot(liwcsamp, aes(ifprop, focuspast))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, focuspresent))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, focusfuture))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	
	#Relativity
	ggplot(liwcsamp, aes(ifprop, relativ))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, motion))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, space))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, time))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	
	#Personal Concerns
	ggplot(liwcsamp, aes(ifprop, work))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, leisure))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, home))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, money))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, relig))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, death))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	
	#Informal speech
	ggplot(liwcsamp, aes(ifprop, informal))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, swear))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, netspeak))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, assent))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)
	ggplot(liwcsamp, aes(ifprop, nonflu))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5) #maybe a cutoff above or equal to 7.5 would make a good binary predictor.
	ggplot(liwcsamp, aes(ifprop, filler))+geom_boxplot()+geom_jitter(color="grey", size=0.7, alpha=0.5)

	#keep Analytic, we, number, posemo., see, and nonflu
	
		#check why the cluster of american ads on the 'number' variable. 
		amnum=subset(liwcsamp, number>75)
		#View(amnum)
		
		#looks like there are a bunch of american ads with nothing but numbers in the text. figure out why.
			#join these ads to their original, uncleaned text. need to give the political ads ids that don't overlap with the propaganda AdIDs
			AdID=as.numeric(adsall$AdID)
			summary(AdID) #looks like the political ads can have ids 4000:4000+length(promessage):
			poltexts=data.frame(AdID=as.character(seq(4000, 4000+length(promessage)-1)), cleantext=promc, text=promessage)
			
			#toobig=merge(amnum, poltexts, by='AdID')
			#View(toobig)
			#looks like I accidentally deleted the text of the first two ads here, maybe bc they came after <p>. restore. The rest just have numbers for their adtext, which is super wierd.
			#could be throwing off the fact predictor, so need to figure out where they came from.
			#in the mean time, don't include 'number' as a variable.
			
			#Try Analytic, we, posemo, see, and nonflu
			
#Other variable ideas:
		#ad extremeness (could just use political probability from the Propublica data)
		#a variable measuring passive/active verbs (not voice) could help reduce the false positive rate, since many of these actually advocate participating in democracy.
		#a variable indicating environment (not just generally prosocial behavior) may help reduce the fp rate since many fps were environmental with an exclamation point.
		#a variable indicating reproductive rights topic
		# whether the target age is exactly 18 and up.
		#from boyd: 
			#1. emoji usage, 
			#personal pronouns more common among IRA, 
			#authenticity index
		#word sophistication index, like whether the ad used an SAT word.
		#Forms of 'to be'
		#NLTK library sentiment score
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
		#word length (aka whether russians have more limited vocab)
		## variable for the word 'repost'
		# variable for the word 'Black' or 'Brown', see whether political ads use 'black' or Black or in the page name
		# variable for syria
		# add bomb to violence variable
		# power indicator variable
		# add interests to text strings
		# prison binary variable
		# command variable: search text for 'like', 'comment', 'subscribe', 'join', share  
		# add brown to the race variable
		# add education/school to the prosocial variable
		# look at frequencies of words across political/prop text
		# self-defense' variable
			
		liwcnames=names(liwc)
			str_which(names(liwc), 'Analytic|we|posemo|see|nonflu') #drop the 5th and 6th indices
		
propvars=data.frame(y=rep(1, 3255), ifvioprop, ifexprop, ifracprop, 
						  ifcopprop, propfactcts, locdums[1:3255,], ifurlprop, ifvoteprop, if18prop, wordvars[1:3255,-1], apply(liwc[1:3255,-c(1,2, dim(liwc)-1, dim(liwc))], 2, as.numeric), AdID=liwc[1:3255,"AdID"]) #drop ifprop,, then Drop A and B cols from liwc since idk what they are.

#all of these should have length 140625
polwordvars=wordvars[3256:end,-1]
polvars=data.frame(y=rep(0, end-3255),ifviopol, ifexpol,
						 ifracpol, ifcoppol, polfactcts, locdums[3256:end,], ifurlpol, ifvotepol, if18pol, polwordvars, apply(liwc[3256:dim(liwc)[1],-c(1,2, dim(liwc)-1, dim(liwc))], 2, as.numeric), AdID=liwc[3256:dim(liwc)[1],"AdID"])
#Try to Free up some RAM
	# names(propvars)=names(polvars1)
	# proppolcsv=rbind(propvars, polvars1)
	# write.csv(proppolcsv, file='proppolcsv.csv')
	# #Clear objects from the workspace

#polvars=read.csv('proppolcsv.csv')

	#propvars=polvars[1:3255,-1]
	#polvars=polvars[-c(1:3255),-1]

#Analysis

	#Try pruning for variable selection.

		##use CV
		polsamp=sample_n(as.tbl(polvars), 3255)
		names(propvars)=names(polsamp) #Mr. Robbins favorite gin: something beef and blenderman?
		ifrusk=rbind(propvars, polsamp)
		ifruskq=ifrusk[,c('polfactcts', names(ifrusk)[str_which(names(ifrusk), 'ifvote\\.nwords'):dim(ifrusk)[2]])]
		ifruskq=apply(ifruskq, 2, as.numeric)
		ifrusk=data.frame(apply(ifrusk[,c(1:5, 7:17)], 2, as.factor)) #make variables 1:7 factors
		ifrusk=data.frame(ifrusk, ifruskq)
		#ifrusk2=ifrusk[,c('y', moreimpa)]
		#Train and Test
		sample<-sample.int(nrow(ifrusk), floor(.50*nrow(ifrusk)), replace = F)
		train<-ifrusk[sample, ]
		test<-ifrusk[-sample, ]
		testx=test[,-str_which(names(test), 'y')]
		testy=test[,c('y')]
		trainy=train$y
		tree.class.train<-tree(y~., data=train[,-116])#remove AdID
		tree.pred=predict(tree.class.train, test, type='class')
		cv.class<-cv.tree(tree.class.train, K=10, FUN=prune.misclass) ##FUN=prune.misclass so error rate is used to guide CV and pruning, rather than the deviance which is the default (and should not be used in classification).
		cv.class
		
		##plot of dev against size
		plot(cv.class$size, cv.class$dev,type='b')
		
		##size of tree chosen by pruning
		trees.num.class<-cv.class$size[which.min(cv.class$dev)]
		trees.num.class ##4 terminal nodes. A lot smaller than recursive binary splitting
		
		##fit tree with size chosen by pruning
		prune.class<-prune.misclass(tree.class.train, best=11)
		prune.class
		
		##plot pruned tree
		plot(prune.class)
		text(prune.class, cex=0.75, pretty=0)
		
		##prediction based on pruned tree for test data
		treeprunepreds<-predict(prune.class, newdata=test[,c(-1, -116)], type="class")
		##confusion matrix
		confprune=table(testy, treeprunepreds)
		conftree=table(testy, tree.pred)
		sum(treeprunepreds==tree.pred)
		#tpr
		confprune[2,2]/(confprune[2,2]+confprune[2,1]) #0.9082569
		conftree[2,2]/(conftree[2,2]+conftree[2,1])
		##overall accuracy
		mean(treeprunepreds==testy) ##0.849. small improvement over recursive binary splitting
		#with 13 nodes, 0.8890937
		
		#keep these best predictors, and retry with gini splits
			vars2keep=tree.class.train$frame['var']
			vars2keep=vars2keep$var[str_detect(vars2keep$var, 'leaf', negate=TRUE)]
			tree.class.train<-tree(y~., data=train[,c(1, vars2keep)])#remove AdID
			tree.pred=predict(tree.class.train, test[,c(1, vars2keep)], type='class')
			conftree=table(testy, tree.pred)
			conftree[2,2]/(conftree[2,2]+conftree[2,1])
			#worse.
		
			
	#Bagged model for Importance
			
		#CAUTION: takes an hour to run

		#Find variable importance, for both decrease in gini and decrease in accuracy, 
		#run multiple times to get typical importance for resamples of propub
		library(randomForest)
		imports=data.frame('0'=character(), '1'=character(),MeanDecreaseAccuracy=character(), MeanDecreaseGini=character())
		names(imports)[c(1, 2)]=c('0','1')
		nsamps=rep(1:15)
		for (i in nsamps) {
			polsamp=sample_n(as.tbl(polvars), 3255)
			names(propvars)=names(polsamp) #Mr. Robbins favorite gin: something beef and blenderman?
			ifrusk=rbind(propvars, polsamp)
			ifruskq=ifrusk[,c('polfactcts', names(ifrusk)[str_which(names(ifrusk), 'ifvote\\.nwords'):dim(ifrusk)[2]])]
			ifruskq=apply(ifruskq, 2, as.numeric)
			ifrusk=data.frame(apply(ifrusk[,c(1:5, 7:17)], 2, as.factor)) #make variables 1:7 factors
			ifrusk=data.frame(ifrusk, ifruskq)
			ifrusk=ifrusk[,-c(7, 116)]
			rf.fit = randomForest(ifrusk[,-c(1)], ifrusk[,1], mtry = dim(ifrusk[,-1])[2], importance=TRUE) #Check that nPerm=5 is the same as a five-fold cv.
			import_i=rf.fit$importance
			imports=rbind(imports, import_i)
		}
		
		#make rownames their own 'variable' column
		imports$Variable=rownames(imports)
		impvars=as.character(imports$Variable)
		
		varsless=lapply(impvars, function(x) str_remove_all(x, pattern="[:digit:]"))
		imports$Variable=unlist(varsless)
		
		#use groupby to get variable averages
		impg=group_by(imports, Variable)
		impmeans=summarise(impg, avgdgini=mean(MeanDecreaseGini), avgdaccuracy=mean(MeanDecreaseAccuracy)) #for some reason didn't include decrease accuracy this time.
		View(impmeans)
		View(impmeans[,c(1,2)])
		
		#write.csv(impmeans, 'VariableImportance.csv')

		#Find the more important variables to keep:
		
			moreimpgini=subset(impmeans, avgdgini>11.32401)
			moreimpacc=subset(impmeans, avgdaccuracy>.001309677)
			moreimpg=moreimpgini$Variable
			moreimpa=moreimpacc$Variable
	
	############
	##Boosting##
	############
	
	library(gbm)
	nsamps=rep(1:40)
	overboosts=c()
	tppboosts=c()
	tnpboosts=c()
	precisionsboost=c()
	#subset to new variables:
	for (i in nsamps) {
		polsamp=sample_n(as.tbl(polvars), 3255)
		names(propvars)=names(polsamp) #Mr. Robbins favorite gin: something beef and blenderman?
		ifrusk=rbind(propvars, polsamp)
		ifruskq=ifrusk[,c('polfactcts', names(ifrusk)[str_which(names(ifrusk), 'ifvote\\.nwords'):dim(ifrusk)[2]])]
		ifruskq=apply(ifruskq, 2, as.numeric)
		ifrusk=data.frame(apply(ifrusk[,c(1:5, 7:17)], 2, as.factor)) #make variables 1:7 factors
		ifrusk=data.frame(ifrusk, ifruskq)
		#ifrusk2=ifrusk[,c('y', moreimpa)]
		#Train and Test
		sample<-sample.int(nrow(ifrusk), floor(.50*nrow(ifrusk)), replace = F)
		train<-ifrusk[sample, ]
		test<-ifrusk[-sample, ]
		#testx=test[,-str_which(names(test), 'y')]
		testy=test[,c('y')]
		trainy=train$y
		#trainx=train[,-str_which(names(test), 'y')]
		#convert y to numeric:
		train$y=as.numeric(as.character(trainy))
		#rm ifstlou and AdID
		boost.class<-gbm(y~., data=train[,-c(7, str_which(names(train), 'AdID'))], distribution="bernoulli", n.trees=500)
		#summary(boost.class)
		#plot(boost.class,i="rm")
		#plot(boost.class,i="lstat")
		##this gives predicted probabilities, not predicted class. This is because the response is using 0/1 dummy codes with gbm()
		testyn=as.numeric(as.character(testy))
		testy=data.frame(y=testyn)
		test$y=testy
		pred.boost<-predict(boost.class, newdata=test, n.trees=500, type = "response", shrinkage=.01)
		##confusion matrix
		boost.tab<-table(testyn, ifelse(pred.boost>0.5, '1', '0'))
		overboostp=100*(boost.tab[1,1]+boost.tab[2,2])/3255
		tprboost=boost.tab[2,2]/(boost.tab[2,2]+boost.tab[2,1]) #sensitivity
		tnrboost=boost.tab[1,1]/(boost.tab[1,1]+boost.tab[1,2]) #specificity
		tppboost=100*tprboost
		tnpboost=100*tnrboost
		precisionboost=boost.tab[2,2]/(boost.tab[2,2]+boost.tab[1,2])
		precisionsboost=c(precisionsboost, precisionboost)
		#Test accuracy
		overboosts=c(overboosts, overboostp) #percent accurate overall.
		#Sensitivity:
		tppboosts=c(tppboosts, tppboost) #percent accurate given propaganda
		#Specificity
		tnpboosts=c(tnpboosts, tnpboost) #percent accurate given non-propaganda.
	}
	boostpsum=summary(precisionsboost)
	boostoversum=summary(overboosts)
	boosttppsum=summary(tppboosts)
	boosttnpsum=summary(tnpboosts)
	
	#looks like boost>bag
	
	
	#in the boosted model the confusion matrix structure is 
	#TN, FP
	#FN, TP
	

#Random Forest
	
	#Caution: takes >1hr to run. Changed from 40 resamples and 20 iterations to find bestsize, to 40 resamples with bestsize=12 all 40 times (one iteration to find bestsize=12)
		
			library(dplyr)
			
		#Get accuracy rates
		
			 rf=function() {
				polsamp=sample_n(as.tbl(polvars), 3255)
				names(propvars)=names(polsamp) #Mr. Robbins favorite gin: something beef and blenderman?
				ifrusk=rbind(propvars, polsamp)
				ifruskq=ifrusk[,c('polfactcts', names(ifrusk)[str_which(names(ifrusk), 'ifvote\\.nwords'):dim(ifrusk)[2]])]
				ifruskq=apply(ifruskq, 2, as.numeric)
				ifrusk=data.frame(apply(ifrusk[,c(1:5, 7:17)], 2, as.factor)) #make variables 1:7 factors
				ifrusk=data.frame(ifrusk, ifruskq)
				#ifrusk2=ifrusk[,c('y', moreimpa)]
				#Train and Test
				sample<-sample.int(nrow(ifrusk), floor(.50*nrow(ifrusk)), replace = F)
				train<-ifrusk[sample, ]
				test<-ifrusk[-sample, ]
				ify=names(test)=='y'
				testx=test[,ify==FALSE]
				testx=testx[,-c(dim(testx)[2], 6)]
				testy=test[,c('y')]
				trainy=train$y
				trainx=train[,-c(1, 7, dim(train)[2])] #remove y, ifstlou, and AdID.
				#convert y to numeric:
				train$y=as.numeric(as.character(trainy))
				m=sqrt(dim(trainx)[2])
				#use the out of bag error method to get best tree size.
				oobers=c()
				for (i in seq(1, 20, 1)) {
					rf.fit = randomForest(trainx, trainy, ntree = 300, mtry = m, nodesize = i) #Check that nPerm=5 is the same as a five-fold cv.
					oober_i=rf.fit$err.rate[300,1]
					oobers=c(oobers, oober_i)
				}
				#write.csv(oobers, 'oobers.csv')
				#of the 20 tested node sizes, the node size=`bestsize`
				#had the smallest OOB error, so we will predict based on that setting:
				bestsize=median(which(oobers==min(oobers)))
				rf.fit = randomForest(trainx, as.factor(trainy), ntree = 300, mtry = m, nodesize = bestsize) #Check that nPerm=5 is the same as a five-fold cv.
				pred = predict(rf.fit, testx)
				#Results
				#Compare this to testY:
				confusionrf=table(testy, pred)
				overrf=mean(testy == pred)
				precisionrf=confusionrf[2,2]/(confusionrf[2,2]+confusionrf[1,2])
				tprrf=confusionrf[2,2]/(confusionrf[2,2]+confusionrf[2,1]) #sensitivity
				tnrrf=confusionrf[1,1]/(confusionrf[1,1]+confusionrf[1,2]) #specificity
				return(data.frame('overall'=overrf, 'precision'=precisionrf, 'tpr'=tprrf, 'tnr'=tnrrf))
			}
		
			 rfaccuracies=replicate(40, rf())
			 rfas=unlist(rfaccuracies) #make groups 1:4, use groupby to get avg, median, and maxes.
			 rfas=data.frame('group'=rep(1:4, 40), 'data'=rfas)
			 rfovers=subset(rfas, group=='1')
			 rfpres=subset(rfas, group=='2')
			 rftprs=subset(rfas, group=='3')
			 rftnrs=subset(rfas, group=='4')
			 rfoversum=summary(rfovers$data)
			 rfprecisionsum=summary(rfpres$data)
			 rftprsum=summary(rftprs$data)
			 rftnrsum=summary(rftnrs$data)
			 
	#Find variable importance, for both decrease in gini and decrease in accuracy, 
				#run multiple times to get typical importance for resamples of propub
			
			imports=data.frame('0'=character(), '1'=character(),MeanDecreaseAccuracy=character(), MeanDecreaseGini=character())
			names(imports)[c(1, 2)]=c('0','1')
			for (i in nsamps) {
				polsamp=sample_n(as.tbl(polvars), 3255)
				names(polsamp)=c('y', 'ifvio', 'ifex', 'ifrace','ifcop', 'factdensity', 'ifbaltimore', 'ifstlou', 'ifclev', 'ifatl', 'iftex',
									  'ifilli', 'ifnewo', 'ifus', 'ifurl', 'ifvote', 'ifvote.nwords', 'ifblack.nwords', 'ifpolice.nwords', 
									  'ifour.nwords', 'ifhelp.nwords', 'ifto.nwords', 'Analytic', 'we', 'posemo', 'see', 'nonflu')
				names(propvars)=names(polsamp) #Mr. Robbins favorite gin: something beef and blenderman?
				ifrusk=rbind(propvars, polsamp)
				ifruskq=ifrusk[,c('factdensity', names(ifrusk)[str_which(names(ifrusk), 'ifvote\\.nwords'):dim(ifrusk)[2]])]
				ifruskq=apply(ifruskq, 2, as.numeric)
				ifrusk=data.frame(apply(ifrusk[,c(1:5, 7:17)], 2, as.factor)) #make variables 1:7 factors
				ifrusk=data.frame(ifrusk, ifruskq)
				m=sqrt(dim(ifrusk[2])-1)
				oobers=c()
				for (i in seq(1, 20, 1)) {
					rf.fit = randomForest(ifrusk[,-1], ifrusk[,1], ntree = 300, mtry = m, nodesize = i) #Check that nPerm=5 is the same as a five-fold cv.
					oober_i=rf.fit$err.rate[300,1]
					oobers=c(oobers, oober_i)
				}
				bestsize=median(which(oobers==min(oobers)))
				rf.fit = randomForest(trainx, as.factor(trainy), ntree = 500, mtry = m, nodesize = bestsize, importance = TRUE) #Check that nPerm=5 is the same as a five-fold cv.
				imports=rbind(imports, rf.fit$importance)
			}
			
			#make rownames their own 'variable' column
			imports$Variable=rownames(imports)
			impvars=as.character(imports$Variable)
			
			varsless=lapply(impvars, function(x) str_remove_all(x, pattern="[:digit:]"))
			imports$Variable=unlist(varsless)
			
			#use groupby to get variable averages
			impg=group_by(imports, Variable)
			impmeans=summarise(impg, avgdgini=mean(MeanDecreaseGini)) #avgdaccuracy=mean(MeanDecreaseAccuracy) #for some reason didn't include decrease accuracy this time.
			View(impmeans)
			
		
 #Classification Tree
			
	library(tree)
		#Get accuracy rates
		#Cut some of the unimportant variables so tree won't reach maximum depth:
		varimps=read.csv('VariableImportance.csv')
		lessimp=varimps[varimps$avgdgini<6.75,] #chose to cut everything worse than 'tentat'
		limpvars=lessimp$Variable
		ifunimp=names(polvars)%in%limpvars
		polvarsless=polvars[,ifunimp==FALSE] #down to 83 variables.
		propvarsless=propvars[,ifunimp==FALSE]
		
		nsamps=rep(1:40)
		overtree=c()
		tpptree=c()
		tnptree=c()
		precisions=c()
		for (i in nsamps) {
			polsamp=sample_n(as.tbl(polvarsless), 3255)
			names(propvarsless)=names(polsamp) #Mr. Robbins favorite gin: something beef and blenderman?
			ifrusk=rbind(propvarsless, polsamp)
			ifruskq=ifrusk[,c('polfactcts', names(ifrusk)[9:dim(ifrusk)[2]])]
			ifruskq=apply(ifruskq, 2, as.numeric)
			ifrusk=data.frame(apply(ifrusk[,c(1:4, 6:8)], 2, as.factor)) #make variables 1:7 factors
			ifrusk=data.frame(ifrusk, ifruskq)
			#ifrusk2=ifrusk[,c('y', moreimpa)]
			#Train and Test
			sample<-sample.int(nrow(ifrusk), floor(.50*nrow(ifrusk)), replace = F)
			train<-ifrusk[sample, ]
			test<-ifrusk[-sample, ]
			#testx=test[,-str_which(names(test), 'y')]
			testy=test[,c('y')]
			trainy=train$y
			trainx=train[,-c(1, 84)] #remove y and AdID.
			#convert y to numeric:
			train$y=as.numeric(as.character(trainy))
			traintree=tree(y~., train[,-c(84)], split='gini')
			#predict
			tree.pred=predict(traintree, testx, type='class')
			ifcorrect=tree.pred==testy
			confusionsvm=table(testy, tree.pred)
			oversvm=mean(testy == tree.pred)
			oversvmp=100*oversvm
			tprsvm=confusionsvm[2,2]/(confusionsvm[2,2]+confusionsvm[2,1]) #sensitivity
			tnrsvm=confusionsvm[1,1]/(confusionsvm[1,1]+confusionsvm[1,2]) #specificity
			precision=confusionsvm[2,2]/(confusionsvm[2,2]+confusionsvm[1,2])
			tppsvm=tprsvm*100
			tnpsvm=tnrsvm*100
			overtree=c(overtree, oversvmp)
			tpptree=c(tpptree, tppsvm)
			tnptree=c(tnptree, tnpsvm)
			precisions=c(precisions, precision)
		}
		treeprecisionsum=summary(precisions)
		treeoversum=summary(overtree)
		treetppsum=summary(tpptree)
		treetnpsum=summary(tnptree)
		
#Linear SVM
		
		library(e1071)
		polsamp=sample_n(as.tbl(polvars), 3255)
		names(polsamp)=names(propvars)
		ifrusk=rbind(propvars, polsamp)
		ifruskq=ifrusk[,str_which(names(ifrusk), 'ct')]
		ifruskc=ifrusk[,-c(str_which(names(ifrusk), 'ct'))]
		ifruskc <- as.data.frame(lapply(ifruskc, unlist))
		ifrusk=data.frame(apply(ifruskc, 2, as.factor)) #make variables 1:7 factors
		ifrusk2=data.frame(ifruskq, ifrusk)
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
		
#Bagging
		
		library(randomForest)
		#Get accuracy rates
		nsamps=rep(1:40)
		overbags=c()
		precisionsbag=c()
		tppbags=c()
		tnpbags=c()
		for (i in nsamps) {
			polsamp=sample_n(as.tbl(polvars), 3255)
			names(propvars)=names(polsamp) 
			ifrusk=rbind(propvars, polsamp)
			ifruskq=ifrusk[,c('polfactcts', names(ifrusk)[str_which(names(ifrusk), 'ifvote\\.nwords'):dim(ifrusk)[2]])]
			ifruskq=apply(ifruskq, 2, as.numeric)
			ifrusk=data.frame(apply(ifrusk[,c(1:5, 7:17)], 2, as.factor)) #make variables 1:7 factors
			ifrusk=data.frame(ifrusk, ifruskq)
			#ifrusk2=ifrusk[,c('y', moreimpa)]
			#Train and Test
			sample<-sample.int(nrow(ifrusk), floor(.50*nrow(ifrusk)), replace = F)
			train<-ifrusk[sample, ]
			test<-ifrusk[-sample, ]
			#testx=test[,-str_which(names(test), 'y')]
			testy=test[,c('y')]
			trainy=train$y
			#trainx=train[,-str_which(names(test), 'y')]
			train$y=as.factor(trainy) #convert y to numeric:
			testyn=as.factor(testy)
			testy=data.frame(y=testyn)
			test$y=testy
			train=train[,-c(7, dim(train)[2])]
			m=dim(train)[2]-1
			#rm ifstlou and AdID
			bag.fit = randomForest(y~., data=train, mtry = m, importance=FALSE) #Check that nPerm=5 is the same as a five-fold cv.
			#importance(bag.fit)		#varImpPlot(bag.fit)
			pred.bag<-predict(bag.fit, newdata=test)
			confusionbag=table(testyn, pred.bag)
			overbag=mean(testyn == pred.bag)
			overbagp=100*overbag
			precisionbag=confusionbag[2,2]/(confusionbag[2,2]+confusionbag[1,2])
			tprbag=confusionbag[2,2]/(confusionbag[2,2]+confusionbag[2,1]) #sensitivity
			tnrbag=confusionbag[1,1]/(confusionbag[1,1]+confusionbag[1,2]) #specificity
			tppbag=100*tprbag
			tnpbag=100*tnrbag
			overbags=c(overbags, overbagp)
			precisionsbag=c(precisionsbag, precisionbag)
			tppbags=c(tppbags, tppbag)
			tnpbags=c(tnpbags, tnpbag)
		}
		bagoversum=summary(overbags)
		bagprecisionsum=summary(precisionsbag)
		bagtppsum=summary(tppbags)
		bagtnpsum=summary(tnpbags)
		

	#Naive vs. Boost
		
		##confusion matrix
	
		naivey=as.vector(rep(1,length(testyn)))
		predboost2=ifelse(pred.boost>0.5, 1, 0)
		boost.tab=table(naivey, predboost2)
		tprnaive=boost.tab[2]/sum(boost.tab)
		
		##test accuracy with boosting
		(boost.tab[1,1]+boost.tab[2,2])/sum(boost.tab) ##0.905. Same as RF. 
		
		tprboost=boost.tab[2,2]/(boost.tab[2,2]+boost.tab[2,1]) #sensitivity
		tnrboost=boost.tab[1,1]/(boost.tab[1,1]+boost.tab[1,2]) #specificity
		tppboost=100*tprboost
		tnpboost=100*tnrboost
		#Test accuracy
		`overboost`
		`overboostp` #percent accurate overall.
		#Sensitivity:
		`tprboost`
		`tppboost` #percent accurate when predicting 'yes'.
		#Specificity
		`tnrboost`
		`tnpboost` #percent accurate when predicting 'no'.
		
#Choose best model
		
		#Summarize accuracies:
		results=data.frame('Min'=c(), '1st Qu.'=c(), 'Median'=c(), 'Mean'=c(), '3rd Qu.'=c(), 'Max.'=c())
		selecter=function(summary){
			dat_i=lapply(1:6, function(x) summary[[x]])
			datdf=data.frame(dat_i)
			names(datdf)=c('Min', '1st Qu.', 'Median', 'Mean', '3rd Qu.', 'Max.')
			return(datdf)
		}
		alldat=list(bagoversum/100, bagprecisionsum, bagtppsum/100, bagtnpsum/100, rfoversum, rfprecisionsum, rftprsum, rftnrsum, boostoversum/100, boostpsum, boosttppsum/100, boosttnpsum/100)
		results=sapply(1:12, function(x) rbind(results, selecter(alldat[[x]])))
		results=as.data.frame(t(results))
		
		results$Model=c(rep('Bagged', 4), rep('Random Forest',4), rep('Boosted',4))#rep('RBS Tree', 4), rep('Pruned RBS Tree', 4), 
		results$Measure=c(rep(c('Overall Accuracy', 'Precision', 'True Positive Rate', 'True Negative Rate'), 3))

		resultl=results[,c('Model', 'Measure', 'Mean', 'Median', 'Max.')]
		resultl$Mean=unlist(resultl$Mean)
		resultl$Median=unlist(resultl$Median)
		resultl$Max.=unlist(resultl$Max.)
		View(resultl)
	#binomial test will show whether the prediction rate is significantly higher than .5.
		
	
		#install.packages("pROC")
		library(pROC)
		
		plot(roc(test_set$bad_widget, glm_response_scores, direction="<"),
			  col="yellow", lwd=3, main="The turtle finds its way")
		
		
		ifcorrect=tree.pred==testy
		
		roc(testy, tree.pred)
		
#Detection analysis
		
	#Is it possible that some of these false positives are actually propaganda? take a look.
		library(dplyr)
		library(gbm)
		polsamp=sample_n(as.tbl(polvars), 3255)
		names(propvars)=names(polsamp) #Mr. Robbins favorite gin: something beef and blenderman?
		ifrusk=rbind(propvars, polsamp)
		ifruskq=ifrusk[,c('polfactcts', names(ifrusk)[str_which(names(ifrusk), 'ifvote\\.nwords'):dim(ifrusk)[2]])]
		ifruskq=ifruskq[,-c(dim(ifruskq)[2])]
		ifruskq=apply(ifruskq, 2, as.numeric)
		ifrusk=data.frame(apply(ifrusk[,c(1:5, 7:17, 116)], 2, as.factor)) #make variables 1:7 factors
		ifrusk=data.frame(ifrusk, ifruskq)
		#Train and Test
		sample<-sample.int(nrow(ifrusk), floor(.50*nrow(ifrusk)), replace = F)
		train<-ifrusk[sample, ]
		test<-ifrusk[-sample, ]
		testy=test[,c('y')]
		trainy=train$y
		#convert y to numeric:
		train$y=as.numeric(as.character(trainy))
		boost.class<-gbm(y~., data=train[,-c(str_which(names(train), 'AdID|ifstlou'))], distribution="bernoulli", n.trees=500)
		#summary(boost.class)
		#plot(boost.class,i="rm")
		#plot(boost.class,i="lstat")
		##this gives predicted probabilities, not predicted class. This is because the response is using 0/1 dummy codes with gbm()
		testyn=as.numeric(as.character(testy))
		testy=data.frame(y=testyn)
		test$y=testy
		pred.boost<-predict(boost.class, newdata=test[,-str_which(names(train), 'AdID')], n.trees=500, type = "response")
		##confusion matrix
		predicts=ifelse(pred.boost>0.5, '1', '0')
		boost.tab<-table(testyn, predicts)
		
		#in the boosted model the confusion matrix structure is 
		#TN, FP
		#FN, TP
		
		#Access the false positives:
		test$prediction=predicts
		iffalse=test$prediction!=test$y
		#subset to the errors:
		testerrors=test[iffalse,]
		#subset to positive predictions
		fp=subset(testerrors, prediction=='1')
		fp#this should have 146 rows.
		fpids=fp$AdID
		
		#Get their text to see whether those ads are obviously not legit.
		fptext=liwc[liwc$AdID%in%fpids,]
		#View(fptext)
		
		#explore whether a suspicious ad in the propub is actually propaganda
		#join ad ids to the raw original data
		provars$AdID=poltxtid$polids
		provars$messagec=promc[1:111860]
		badad=provars[provars$AdID%in%fptext$AdID,]
	
		badadl=badad[,c('AdID', 'messagec', 'title', 'paid_for_by', 'page')]
		View(badadl)
		
		#Later, look up how to interpret the probabilities returned by the boosted model.
		
		#Random Forest model, since it had the highest precision.
			#Train it on all of the 3255 propaganda ads, and a rs of 3255 'domestic' ads. Later: make sure no suspicious 'domestic' ads are selected for this.
			#save the indices or AdIDs of the 3255 'domestic' ads selected, then make predictions with the remaining 'domestic' ads.
		
		domsampints<-sample.int(nrow(polvars), floor(0.02369513*nrow(polvars)), replace = F)
		domsample=polvars[domsampints,]
		domtest=polvars[-domsampints,]
		#polsamp=sample_n(as.tbl(polvars), 3255)
		#names(propvars)=names(polsamp) 
		ifrusk=rbind(propvars, domsample)
		ifruskq=ifrusk[,c('polfactcts', names(ifrusk)[str_which(names(ifrusk), 'ifvote\\.nwords'):dim(ifrusk)[2]])]
		ifruskq=ifruskq[,-c(dim(ifruskq)[2])]
		ifruskq=apply(ifruskq, 2, as.numeric)
		ifrusk=data.frame(apply(ifrusk[,c(1:5, 7:17, 116)], 2, as.factor)) #make variables 1:7 factors
		ifrusk=data.frame(ifrusk, ifruskq)
			#convert y to factor:
			ifrusk$y=as.factor(as.numeric(as.character(ifrusk$y)))
			ifrusky=ifrusk$y
			ifruskx=ifrusk[,-c(1, 7, 16)]
			m=sqrt(dim(ifruskx)[2])
			#use the out of bag error method to get best tree size.
			oobers=c()
			for (i in seq(1, 20, 1)) {
				rf.fit = randomForest(ifruskx, ifrusky, ntree = 300, mtry = m, nodesize = i) #Check that nPerm=5 is the same as a five-fold cv.
				oober_i=rf.fit$err.rate[300,1]
				oobers=c(oobers, oober_i)
			}
			#write.csv(oobers, 'oobers.csv')
			#of the 20 tested node sizes, the node size=`bestsize`
			#had the smallest OOB error, so we will predict based on that setting:
			bestsize=median(which(oobers==min(oobers)))
			rf.fit = randomForest(ifruskx, as.factor(ifrusky), ntree = 500, mtry = m, nodesize = bestsize) #Check that nPerm=5 is the same as a five-fold cv.
				domtestq=domtest[,c('polfactcts', names(domtest)[str_which(names(ifrusk), 'ifvote\\.nwords'):dim(domtest)[2]])]
				domtestq=domtestq[,-c(dim(domtestq)[2])]
				domtestq=apply(domtestq, 2, as.numeric)
				domtest=data.frame(apply(domtest[,c(1:5, 7:17, 116)], 2, as.factor)) #make variables 1:7 factors
				domtest=data.frame(domtest, domtestq)
				domtestx=domtest[,-c(1, 7, 16)]
				domtesty=domtest[,'y']
			pred = predict(rf.fit, domtestx)
			#Results
			#Compare this to testY:
			confusionrf=table(domtesty, pred)
			confusionrf
			#6423 Propublica ads were classified as 'propaganda', meaning 
			confusionrf[1,2]/c(confusionrf[1,2]+confusionrf[1,1]) #4.78% are 'incorrectly' classified propaganda (aka 95.209% were classified domestic). look at them:
			
			results=data.frame('actual'=domtesty, 'predicted'=pred, 'AdID'=domtest$AdID)
			resultsus=subset(results, predicted=='1') #6423 ads were classified 'propaganda'. 
			badad=provars[provars$AdID%in%resultsus$AdID,] #Later: also compare results to the nyu ad pages.
			
			badadl=badad[,c('AdID', 'messagec', 'title', 'paid_for_by', 'page')]
			View(badadl)
			
			#Remove verified or clearly erroneous ones:
			View(table(badadl$title))
			badadl$title=str_remove_all(badadl$title, '[:punct:]')
			#excluded 'importantnotimportant', HIAS, Better Medicare Alliance 'BallotReady', Phone2Action, Energy Citizens, PolicyEd, Kialo, Trade For America, Boredom Therapy bc benign, tho not verified.
			legit=c('importantnotimportant', 'AFSCME', 'Bloomberg', 'The New York Times', 'Pew Research Center', 'House Majority PAC', 'HIAS', 'FCTRY', 'Eric Bolling', 'Defenders of Wildlife', 'The Intercept', 'Mother Jones', 'Environmental Defense Fund', 'Donald J. Trump','Congressman Peter King', 'CARE Action', 'Better Medicare Alliance', 'BallotReady', 'Antonio Delgado', 'UltraViolet', 'Phone2Action', 'Energy Citizens', 'Vera Institute of Justice', 'Sherrod Brown', 'National Institute for Reproductive Health', 'Boredom Therapy', 'Trade For America', 'Kialo', 'Human Rights Watch', 'Earthjustice', 'Ben  Jerrys', 'Thrasher Coffee', 'Erase The Hate', 'AARP', 'Washington Post', 'Senator Ron Wyden', 'Plan International USA', 'African Wildlife Foundation', 'Tom Steyer', 'Cathy Myers', 'PolicyEd', 'Greenpeace USA', 'FWD.us', 'ExxonMobil','Everytown for Gun Safety','USO', 'I Love You America', 'The Wilderness Society', 'Randy Bryce', 'MJ for Texas','March for Science','Equal Citizens', 'Democratic Attorneys General Association','The Christian Science Monitor', 'Coalition to Protect Americas Health Care', 'Jeff Merkley','Dinesh DSouza', 'AAAS  The American Association for the Advancement of Science','Yale Program on Climate Change Communication','Joe Kennedy III','Independent Voter', 'CREDO Mobile', 'Southern Poverty Law Center', 'Ocean Conservancy', 'I Am An Immigrant', 'Ben Shapiro', 'Shaun King', 'MoveOnorg', 'Beto ORourke', 'Sierra Club', 'UNICEF USA', 'Penzeys Spices', 'OZY', 'Amnesty International USA', 'ADL  AntiDefamation League', 'MoveOn', 
					  'NARAL ProChoice America', 'Care2', 'PragerU', 'ACLU', 'International Rescue Committee', 
					  'Color Of Change', 'Feeding America', 'Wounded Warrior Project', 'Truthout', 'Color of Change', 'Care Net', 'Define American', 'Americans for Prosperity', 'National Domestic Workers Alliance', 'The American Prospect', 'Authenticity 50', 'Grist', 'AFSCME Retirees', 'Turning Point USA', 'SpeakEasy Political', 'SEIU MN State Council', 'No Labels', 'OZY Future', 'World Wildlife Fund', 'USA for UNHCR', 'The Young Turks','Joe Biden', 'Planned Parenthood', 'Women for Trump', 'Barron', 'The Nature Conservancy')
			ifgood=badadl$title%in%legit
			badonly=badadl[ifgood==FALSE,]
		
			
#Pick Up here!!			
			#Remove pages containing 'carefans', theLIBREinitiative, GLAAD, AllOutOrg, NibComics, 'CSPAN', 'humanrightscampaign', 'NationalImmigrationLawCenter', 'ShriForMI', 'Upworthy', 'youngamericasfoundation'
				#'AFTunion', americanjewishworldservice, FAIRImmigration, jacobinmag
				
			#subset to unique combos of page and message
			pm=badonly[,c('messagec', 'page')]
			pmd=distinct(pm)
			
			
			View(table(pmd$page))
			
			#Subset to those without a sponsor
			iftext=str_detect(badonly$paid_for_by, '.')
			nospons=badonly[iftext==FALSE,]
			View(table(nospons$page))
			
			#look for 'black defense foundation' ads
			
				blf=propub2[,c('advertiser', 'page')]
				View(blf)
			
			#May 24 2018, facebook started requiring sponsor info in order to post pol ads
			
			#Suspicious pages:
sus=c("https://www.facebook.com/proudrightwinger/", 
		"https://www.facebook.com/AmericanConsequences/",
		"https://www.facebook.com/Ill-go-ahead-and-keep-my-guns-Thanks-791543660926838/",
		"https://www.facebook.com/theaafnation/",
		"https://www.facebook.com/WolfPAChq/",
		"https://www.facebook.com/antiseditionist/",
		"https://www.facebook.com/TheFightStartsNow/",
		"https://www.facebook.com/countable.us/",
		"https://www.facebook.com/USConstitution1789/",
		"https://www.facebook.com/peoplestrumpet/",
		"https://www.facebook.com/RantNationBlazeTV/",
		"https://www.facebook.com/Family-Survival-246751399183894/", 
		"https://www.facebook.com/TheLoneLiberalRepublican/",
		"https://www.facebook.com/ClarionProject/",
		"https://www.facebook.com/realnewsandnews/",
		"https://www.facebook.com/todaysnation08/",
		"https://www.facebook.com/ifamericansknew/",
		"https://www.facebook.com/trustedconservative/",
		"https://www.facebook.com/bluematters/",
		"https://www.facebook.com/Conservative-Gear-1194865807192273/",
		"https://www.facebook.com/Exploiting-America-2019371548083878/",
		"https://www.facebook.com/helloILove/",
		"https://www.facebook.com/gopworld/",
		"https://www.facebook.com/Our-Flag-Our-Country-239916320035689/",
		"https://www.facebook.com/pg/joinifyouarerepublicanandproud/",
		"https://www.facebook.com/reconnectAmerica/",
		"https://www.facebook.com/American-Consequences-307530139699465/",
		"https://www.facebook.com/WakeUpWorldAndSmellTheCoffee/",
		"https://www.facebook.com/Americans-united-332021670251294/",
		"https://www.facebook.com/blacklivesmatter1/",
		"https://www.facebook.com/conservativerepublic2020/",
		"https://www.facebook.com/CtrlAltRightDel/",
		"https://www.facebook.com/FightForChangeToday/",
		"https://www.facebook.com/FoundingFathersDaily/",
		"https://www.facebook.com/futureinamerica/",
		"https://www.facebook.com/usahope/",
		"https://www.facebook.com/I-Care-108145336048412/",
		"https://www.facebook.com/ILMFOrg/",
		"https://www.facebook.com/impeachtrumpasap/",
		"https://www.facebook.com/keepnbear/",
		"https://www.facebook.com/Knock-The-Vote-253432098699912/",
		"https://www.facebook.com/Liberty-Alerts-2128037827458314/",
		"https://www.facebook.com/pg/libertydigest/",
		"https://www.facebook.com/NomadLivingFree/",
		"https://www.facebook.com/Nasty-Women-1085024131613003/",
		"https://www.facebook.com/OULeft.org/",
		"https://www.facebook.com/persecutioninamerica/",
		"https://www.facebook.com/Police-Unions-Exposed-326409467814034/",
		"https://www.facebook.com/Proud-To-Be-A-Democrat-549283205447657/",
		"https://www.facebook.com/PursuitHQ/",
		"https://www.facebook.com/Redbluedivide/",
		"https://www.facebook.com/RedSymposium/",
		"https://www.facebook.com/Republican-Merchandise-705969119786669/",
		"https://www.facebook.com/StoppingTheDonald/",
		"https://www.facebook.com/Take-Congress-Back-310038016453657/",
		"https://www.facebook.com/texascitizenscoalition/",
		"https://www.facebook.com/TheLeftCanGoRight/",
		"https://www.facebook.com/groups/1471379036207837/",
		"https://www.facebook.com/ThePledgeOfAllegianceUSA/",
		"https://www.facebook.com/theresistanceprays/",
		"https://www.facebook.com/trueconservativenews/",
		"https://www.facebook.com/trumpreleasetaxes/",
		"https://www.facebook.com/TrumpTrainNews/",
		"https://www.facebook.com/unfitforus/",
		"https://www.facebook.com/reallyamerican/",
		"https://www.facebook.com/unitedforvalues/",
		"https://www.facebook.com/USdems/",
		"https://www.facebook.com/wearegreatagainpac/",
		"https://www.facebook.com/WeBuildTheWall/",
		"https://www.facebook.com/wedemandjusticenow/",
		"https://www.facebook.com/weresistnow/",
		"https://www.facebook.com/wetraindems/",
		"https://www.facebook.com/ShopTrump/",
		"https://www.facebook.com/ILMFOrg/")



#Remove these from the population of Propublica+NYU ads (polvars)

	#extract the ad IDs associated with these pages
		susids=provars[provars$page%in%sus, 'AdID'] #looks like 853 ads are associated with these pages.
	#remove ads with those ids from polvars
	
		ifsus=polvars$AdID%in%susids
		cleandom=polvars[sapply(ifsus, isFALSE),]
		#should sample from this, not polvars
	
	#Look at only the suspicious ads:
		susads=provars[provars$page%in%sus,]
		susads=susads[,c('AdID', 'vars', 'paid_for_by', 'page', 'advertiser', 'targets', 'message', 'title')]
		View(susads)
		
#Apply Term Frequency-Inverse Document Frequency statistics to each ad. Could use it to identify other important thematic predictors by looking at the difference between domestic importance and foreign importance for each word. The words with large differences may indicate which words are important to the russians, which are important to us.

#Run ad text through the Prta system: https://www.tanbih.org/propagandasubmit
	#This could show whether this russian propaganda exhibits features typical of other propaganda. If so, could lead to some good predictors.

		#pick a random propaganda ad to plug into the program
		library(dplyr)
		aprop=sample_n(propvars, 1)
		anid=aprop$AdID
		itstext=subset(adsall, AdID==anid)
		itstext$AdText
		
		#Do the same with a non-propaganda ad:
		adom=sample_n(cleandom, 1)
		anid2=adom$AdID
		domtext=subset(poltxtid, polids==anid2)
		domtext$poltxtc
		
libinfo=c("https://www.facebook.com/proudrightwinger/","philippines based", "https://www.facebook.com/Conservative-Gear-1194865807192273/","philippines based", "https://www.facebook.com/Ill-go-ahead-and-keep-my-guns-Thanks-791543660926838/","philippines based", "https://www.facebook.com/AmericanConsequences/","u.s. based", "https://www.facebook.com/theaafnation/","phillipines based", "https://www.facebook.com/WolfPAChq/","u.s. based", "https://www.facebook.com/reallyamerican/","u.s. based", "https://www.facebook.com/antiseditionist/","u.s. based", "https://www.facebook.com/countable.us/","argentina based", "https://www.facebook.com/USConstitution1789/","u.s. based, ads taken down", "https://www.facebook.com/peoplestrumpet/","unknown", "https://www.facebook.com/RantNationBlazeTV/","mexico based", "https://www.facebook.com/Family-Survival-246751399183894/","u.s. based, but many ads taken down", "https://www.facebook.com/TheLoneLiberalRepublican/","u.s. based", "https://www.facebook.com/realnewsandnews/","unknown", "https://www.facebook.com/ifamericansknew/","u.s. based", "https://www.facebook.com/trustedconservative/","u.s. based, some posts taken down", "https://www.facebook.com/ads/library/?active_status=all&ad_type=political_and_issue_ads&country=US&impression_search_field=has_impressions_lifetime&view_all_page_id=2019371548083878&sort_data[direction]=desc&sort_data[mode]=relevancy_monthly_grouped","unknown", "https://www.facebook.com/gopworld/","india based", "https://www.facebook.com/americaourflagourcountry/","unknown", "https://www.facebook.com/reconnectAmerica/","page unavailable on library", "https://www.facebook.com/WakeUpWorldAndSmellTheCoffee/","u.s. based but see ads", "https://www.facebook.com/Americans-united-332021670251294/","ads deleted from library", "https://www.facebook.com/blacklivesmatter1/","page unavailable", "https://www.facebook.com/CtrlAltRightDel/","page deleted", "https://www.facebook.com/FightForChangeToday/","u.s. based, but ads deleted from library", "https://www.facebook.com/FoundingFathersDaily/","u.s. based", "https://www.facebook.com/futureinamerica/","u.s. based", "https://www.facebook.com/I-Care-108145336048412/","could not find ads in the library", "https://www.facebook.com/ILMFOrg/","vietnam based", "https://www.facebook.com/impeachtrumpasap/","u.s. based", "https://www.facebook.com/keepnbear/","u.s. based", "https://www.facebook.com/Knock-The-Vote-253432098699912/","unknown", "https://www.facebook.com/pg/libertydigest/","u.s. based", "https://www.facebook.com/allnastywomen/","vietnam based", "https://www.facebook.com/OULeft.org/","u.s. based but suspicious content", "https://www.facebook.com/persecutioninamerica/","u.s. based", "https://www.facebook.com/Police-Unions-Exposed-326409467814034/","unknown", "https://www.facebook.com/ProudDemocrats/","pakistan based", "https://www.facebook.com/Redbluedivide/","u.s. based", "https://www.facebook.com/RedSymposium/","u.s. based", "https://www.facebook.com/Republican-Merchandise-705969119786669/", "page deleted", "https://www.facebook.com/StoppingTheDonald/", "u.s. based", "https://www.facebook.com/Take-Congress-Back-310038016453657/", "page deleted", "https://www.facebook.com/texascitizenscoalition/", "u.s. based", "https://www.facebook.com/TheLeftCanGoRight/", "u.s. based", "https://www.facebook.com/ThePledgeOfAllegianceUSA/", "u.s. based", "https://www.facebook.com/theresistanceprays/", "u.s. based", "https://www.facebook.com/trueconservativenews/", "page deleted", "https://www.facebook.com/trumpreleasetaxes/", "u.s. based", "https://www.facebook.com/TrumpTrainNews/", "u.s. based", "https://www.facebook.com/unfitforus/", "unknown", "https://www.facebook.com/unitedforvalues/", "u.s. based", "https://www.facebook.com/USdems/", "new zeland based", "https://www.facebook.com/wearegreatagainpac/", "u.s. based", "https://www.facebook.com/weresistnow/", "spain and mexico based", "https://www.facebook.com/ShopTrump/", "u.s. based and ads deleted from library")
#join with provars to get ads, advertiser, page, and library info on the spreadsheet.

susall=badadl[libinfo%in%badadl$page,]
x=c(0, 1)
susdf=data.frame(libinfo, 'code'=rep(x, 57, each=1))
info=subset(susdf, code==1)
pages=subset(susdf, code==0)
info$page=pages$libinfo
susmore=merge(susall, info, by.y='page')
info=info[,c(1, 3)]
write.csv(info, file='FlaggedPages.csv')

#

#which should we include? a. foreign based pages (according to the library), b. solicitors, c. unable to find on fb or the library
			
				#2018 is Our Year. Join Us. (no page, just title)
				#"https://www.facebook.com/pg/WorkingCaliforniansAgainstCorruption/about/?ref=page_internal
				#"https://www.facebook.com/ads/library/?active_status=all&ad_type=all&country=US&impression_search_field=has_impressions_lifetime&view_all_page_id=1340186812659728&sort_data[direction]=desc&sort_data[mode]=relevancy_monthly_grouped
				#"https://www.facebook.com/ads/library/?active_status=all&ad_type=all&country=US&impression_search_field=has_impressions_lifetime&view_all_page_id=344115172635025&sort_data[direction]=desc&sort_data[mode]=relevancy_monthly_grouped

			
			#limbo pages
				#Patriot Depot (Has an actual address tho)
				#CARE (aka Christian Action Research and Education) is about abortion, so maybe off topic, but unverified
				#Coalition to Protect America's Health Care (probly not propaganda but not verified)
				#ShareBlue Media (title), may have been missclass bc kind of a watchdog criticizing the gov
				#MedPage Today, prob not prop cuz off topic
				#PolicyEd, benign
				#Open Doors USA
				#Conversations with Bill Kristol
				#TheChristianLeft
				#Blue Wave 2018
				#Likewise Media (https://www.facebook.com/likewisestory/)
				#https://www.facebook.com/reallyamerican/
				#https://www.facebook.com/texaspridefans/ (missing)
				#4aPeoplesParty https://www.facebook.com/4aPeoplesParty/
				#https://www.facebook.com/AccountabilityinGov/
				#https://www.facebook.com/ConservativeCorpsCauses/
				#https://www.facebook.com/eightlessonsofwar/
				#https://www.facebook.com/LeeCampTruth/ (removed)
				#https://www.facebook.com/WeBuiltThisHouseYoureLivingIn/
				#https://www.facebook.com/UnitedforPrivacy/
				#PragerU (references Young Turks, whose coleader Ana Kasparin is Armenian), 
				#MoveOn (recruits ppl to work from home online, supported bernie, founder is russian: https://en.wikipedia.org/wiki/Ilya_Sheyman) 
				
			#Legit looking pages (actual errors),
				#Truthout, Color of Change, Care Net, Define American, Americans for Prosperity, penzy's spices, National Domestic Workers Alliance, The American Prospect
				#Authenticity 50, Grist, AFSCME Retirees, Turning Point USA, SpeakEasy Political, SEIU MN State Council (Another Union)
				#No Labels
				#OZY Future
				#National Democratic Redistricting Committee
				#Yale Climate Connections, off topic
				#HIAS
				#Institute for Southern Studies (page is 'facingsouth') https://www.southernstudies.org/staff
				#American Friends Service Committee
				#muslimadvocates
				#@nrcat (may have been misclass for opposing u.s. prison system)
				#RepublicansRuleofLaw
				#https://www.facebook.com/PursuitHQ/
				#https://www.facebook.com/WeBuildTheWall/
				#"https://www.facebook.com/wedemandjusticenow/"
				#https://www.facebook.com/NomadLivingFree/
				#https://www.facebook.com/bluematters/
				
			
	#check out Shareblue Media
			library('stringr')
			View(badonly[str_which(badonly$page, 'blacklives'),])
			provars[str_which(provars$page, 'Flag'),'page']
	#'BLM News',  was a page recently taken down, so see if it's in the set
		ifblm=str_detect(provars$page, 'BLM') #Nope.
	
	#see whether any pages with 'black' titles are ghana based.
	
	#Look up these pages: https://about.fb.com/news/2019/10/removing-more-coordinated-inauthentic-behavior-from-iran-and-russia/ in the ad archive.
		
	#Detected 'The BL', the foreign propaganda page run by the Vietnamese.
	
	#maybe an actually good source: https://www.thirdpartyfilms.com/films/ournewpresident
	
#pull up data on all suspicious pages:
		#ifsus=badadl$page%in%sus
		susp=badadl[ifsus,]
		View(susp)
		
#Russian state sponsored/IRA linked websites (according to Graphika's report):
		#nahnews.org
		#Sputnik
		#news-front.info
		#PolitRussia
		#Federal News Agency
		#RIAFAN
		
#5000 followers get transparency. 
		#domaintools.com
		#builtwith.com - google analytics codes 
		#issues with clickbait IRA copycats
	
#Medium.com/@ushadrons
		