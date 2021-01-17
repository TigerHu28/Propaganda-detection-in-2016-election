setwd("C:/Users/JanPatrick/Documents/UVA/UVA Spring 2020/STAT 4996")
library(tidyverse)
library(stringr)
library(splitstackshape)
library(tidycensus)
library(stringi)
dffacebook<-read.csv("FacebookAds.csv")
df1<-dffacebook[c(FALSE,TRUE),]
df1<-df1[!(is.na(df1$Impressions)),]
df1<-df1[!(is.na(df1$Clicks)),]
df1<-subset(df1,df1$Impressions!=0)
df1<-subset(df1,df1$Location!="")
df1$AdWordCount<-str_count(df1$AdText,"\\w+")
df1<-subset(df1,df1$AdWordCount!=0)
df1<-df1[,c("AdID","Clicks","Impressions","Location","AdWordCount")]
df1<-subset(df1,df1$Location!="Living in: United States")
df1<-subset(df1,df1$Location!="Living In: United States")
df1<-subset(df1,df1$Location!="United States")
df1<-df1[str_detect(df1$Location,"Canada")==FALSE,]
df1<-df1[str_detect(df1$Location,"Germnay")==FALSE,]
df1<-df1[str_detect(df1$Location,"United Kingdom")==FALSE,]
df1<-df1[str_detect(df1$Location,"Russia")==FALSE,]
removewords<-c("Living In:","United States","United States:","Latitude .....",
               "Longitude .....","[()]","km","mi","[+]")
df1<-as.data.frame(sapply(df1,function(x)gsub(paste(removewords,collapse="|"),"",x)))
df1$Location<-str_replace_all(df1$Location,"[[:digit:]]", "")
df1$Location<-str_replace_all(df1$Location,":",";")
df1<-separate_rows(df1,Location,sep=";")
df1[325,'Location']<-c('Ferguson, St. Louis Missouri; Cleveland Ohio')
df1[543,'Location']<-c('Austin, Houston Texas')
df1[569,'Location']<-c('Massachusetts; Ferguson Missouri')
df1[572,'Location']<-c('New Jersey; New York New York; Ohio')
df1[729,'Location']<-c('Illinois; Virginia')
df1[739,'Location']<-c('Georgia; Alabama')
df1[c(935,942),'Location']<-c('Illinois; Michigan')
df1[c(910,926),'Location']<-c('Arizona; Texas')
df1<-separate_rows(df1,Location,sep=";")
df1$StateFips<-fips_codes[match(stri_extract_last_words(df1$Location),
                                stri_extract_last_words(fips_codes$state_name)),2]
df1[735,]$StateFips<-54
df1[c(683,767,769,770,835,838,872,910,968),]$StateFips<-45
dfpop<-as.data.frame(get_estimates(geography="state",product="population",year=2016,keep_geo_vars=TRUE))
dfpop$GEOID<-as.numeric(dfpop$GEOID)
dfpop$GEOID<-str_pad(dfpop$GEOID,2,pad="0")
df1$StatePopulation<-dfpop[match(df1$StateFips,dfpop$GEOID),4]
df1$StateName<-fips_codes[match(df1$StateFips,fips_codes$state_code),3]