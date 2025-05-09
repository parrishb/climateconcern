# Preamble #### 
rm(list=ls())
library(reshape2)
library(countrycode)
library(ggplot2)
library(magrittr)
library(tidyverse)
library(ggrepel)
# source("globalmrp_functions.R")

if(Sys.info()["user"] == "pberg"){
  setwd("~/Documents/GitHub/climateconcern")
}

if (Sys.info()["user"] =="clara"){
  setwd("~/Documents/climateconcern")
}


# load data #### 

# set model name
modelname<-"country_walk_region_walk_fxdstart" ## new model 240715
timecollapse<-"2yr"
qrestrict<-"concernhuman"


# datafilter<-"none"
datafilter<-paste(timecollapse,qrestrict,sep="_")

betas_sequence <- F

#load model result
if(exists("datafilter")){
load(paste0("outputs_stan/stan_summ_",modelname,"_",datafilter,".Rdata"))
}else{
load(paste0("outputs_stan/stan_summ_",modelname,".Rdata"))
}
summ_stan$parameter <- sub("\\[.+\\]", "", summ_stan$variable)

## load input object to model (for recodes) 
if(exists("datafilter")){
  load(paste0("data/d_rstan_region_",datafilter,".Rda"))
}else{
load("data/d_rstan_region.Rda")
}

#separate indices from parameter names
summ_stan$index<-summ_stan$variable
unique(summ_stan$parameter)
summ_stan<-summ_stan%>%
  mutate(index=case_when(parameter=="country_alpha"~gsub("country_alpha\\[","",index),
                         parameter=="region_alpha"~gsub("region_alpha\\[","",index),
                         parameter=="country_poststrat"~gsub("country_poststrat\\[","",index),
                         parameter=="region_effect"~gsub("region_effect\\[","",index),
                         parameter=="beta"~gsub("beta\\[","",index), ## beta is diff (intercept)
                         parameter=="gamma"~gsub("gamma\\[","",index)), ## gamma is disc (slope)
         index=gsub("\\]","",index))


# extract parameters (by type) and convert indices to names ####


# region alphas: go from indices to years, regions and countries
regions<-d%>%select(regioncode_int,mergekey)%>%distinct()
d.years<-d%>%select(year2,year_int)%>%distinct()
dnat.years<-d.nat%>%select(year2,year_int)%>%distinct()
years<-full_join(d.years,dnat.years,by=c("year2","year_int"))
d.countries<-d%>%select(iso_3166,mergekey,iso_3166_int)%>%distinct()
dnat.countries<-d.nat%>%select(iso_3166,mergekey,iso_3166_int)%>%distinct()
countries<-full_join(d.countries,dnat.countries,by=c("iso_3166","mergekey","iso_3166_int"))

alphas<-summ_stan%>%
  filter(parameter=="region_alpha")%>%
  separate(index,into=c("geog_int","year_int"),sep=",")%>% 
  mutate(geog_int=as.integer(geog_int),
         year_int=as.integer(year_int))
region.alphas<-alphas%>%
  left_join(regions,by=c("geog_int"="regioncode_int"))%>%
  left_join(years,by="year_int")%>%
  left_join(countries,by="mergekey")

# country poststratified values: go from indices to years and countries
countries<-countries%>%select(iso_3166,iso_3166_int)%>%distinct()

country_alphas<-summ_stan%>%
  filter(parameter=="country_alpha")%>%
  separate(index,into=c("geog_int","year_int"),sep=",")%>%
  mutate(geog_int=as.integer(geog_int),
         year_int=as.integer(year_int))%>%
  left_join(countries,by=c("geog_int"="iso_3166_int"))%>%
  left_join(years,by="year_int")%>%
  distinct()

country.poststrat<-summ_stan%>%filter(parameter=="country_poststrat")%>%
  separate(index,into=c("geog_int","year_int"),sep=",")%>%
  mutate(geog_int=as.integer(geog_int),
         year_int=as.integer(year_int))%>%
  left_join(countries,by=c("geog_int"="iso_3166_int"))%>%
  left_join(years,by="year_int")%>%
  distinct()
setdiff(years$year_int,country.poststrat$year_int) # should be zero

# gammas: go from indices to questions (discrimination parameters)
d.questions<-d%>%select(question,question_int)%>%distinct()%>%
  filter(!is.na(question)) 
dnat.questions<-d.nat%>%select(question,question_int)%>%distinct()%>%
  filter(!is.na(question))
questions<-full_join(d.questions,dnat.questions,by=c("question","question_int"))
discrimination<-summ_stan%>%filter(parameter=="gamma")%>%
  select(index,mean,q95,q5,parameter)%>%
  mutate(index=as.integer(index))%>%
  left_join(questions,by=c("index"="question_int"))
setdiff(questions$question,discrimination$question) ## zero=no missing questions. good. 

# betas: isolate cutpoints (difficulties of answer options)
difficulties<-filter(summ_stan, parameter=="beta")

if(betas_sequence){
  
  # count number of thresholds per question
  no_of_thres<-sapply(sort(unique(d$question_int)),
                      function(x) max(d$option[d$question_int==x])-1)
  
  # add question and option numbers
  difficulties$option<-unlist(sapply(no_of_thres,function(x) seq(1:x)))
  difficulties$question_int<-rep(unique(d$question_int),no_of_thres)
  
} else{
  
  # add question and option numbers
  betaindices <- sapply(difficulties$index, strsplit, ",")
  betaindices <- do.call("rbind", betaindices)
  colnames(betaindices) <- c("question_int","option")
  difficulties <- cbind(betaindices, difficulties)
  difficulties$question_int <- as.numeric(difficulties$question_int)
  difficulties$option <- as.numeric(difficulties$option)
  
  #only keep used cutpoints
  no_of_opt <- tapply(d$option, d$question_int, max)
  difficulties <- difficulties[difficulties$option < no_of_opt[difficulties$question_int],]
  
}




#binary for whether we have observations in each region-year
region.obs<-d%>%
  select(-c(option,yes))%>%
  distinct()%>%
  mutate(sample=ifelse(size>0,1,0))

region.obs<-region.obs%>%
  group_by(year_int,regioncode_int)%>%
  summarise(obs=sum(sample))%>%
  ungroup()

#add to region.alphas
region.alphas<-left_join(region.alphas,region.obs,by=c("year_int","geog_int"="regioncode_int"))
region.alphas<-region.alphas%>%mutate(obs=ifelse(is.na(obs),0,obs))

#binary variable for whether we have observations in each country-year
d.country.obs<-d%>%
  select(iso_3166_int,year_int,size)%>%
  group_by(iso_3166_int,year_int)%>%
  summarise(size=sum(size))%>%
  ungroup()%>%
  mutate(sample=ifelse(size>0,1,0))%>%
  group_by(year_int,iso_3166_int)%>%
  summarise(obs=sum(sample))%>%
  ungroup()
dnat.country.obs<-d.nat%>%
  select(iso_3166_int,year_int,size)%>%
  group_by(iso_3166_int,year_int)%>%
  summarise(size=sum(size))%>%
  ungroup()%>%
  mutate(sample=ifelse(size>0,1,0))%>%
  group_by(year_int,iso_3166_int)%>%
  summarise(obs=sum(sample))%>%
  ungroup()
country.obs<-full_join(d.country.obs,dnat.country.obs,by=c("year_int","iso_3166_int","obs"))


#add to country.poststrat
country.poststrat<-left_join(country.poststrat,country.obs,by=c("year_int","geog_int"="iso_3166_int"))
country.poststrat<-country.poststrat%>%mutate(obs=ifelse(is.na(obs),0,obs))

#number of observations per question
d.question.obs<-d%>%
  select(question_int,size,year_int,regioncode_int,iso_3166_int)%>%
  distinct()%>%
  mutate(sample=ifelse(size>0,1,0))%>%
  group_by(question_int)%>%
  summarise(obs=sum(sample))%>%
  ungroup()%>%
  left_join(questions,by="question_int")

dnat.question.obs<-d.nat%>%
  select(question_int,size,year_int,iso_3166_int)%>%
  distinct()%>%
  mutate(sample=ifelse(size>0,1,0))%>%
  group_by(question_int)%>%
  summarise(obs=sum(sample))%>%
  ungroup()%>%
  left_join(questions,by="question_int")

question.obs<-rbind(d.question.obs,dnat.question.obs)%>%
  group_by(question_int,question)%>%
  summarise(obs=sum(obs))%>%
  ungroup()

#add to difficulties and discrimination
difficulties<-left_join(difficulties,question.obs,by="question_int")
setdiff(questions$question,difficulties$question) ##  missing questions. 230120 
discrimination<-left_join(discrimination,question.obs,by="question")

# merge in continent names and then country names #### 

# add continent names
d.continents<-d%>%select(iso_3166,Continent_Name,continent_int)%>%distinct()
dnat.continents<-d.nat%>%select(iso_3166,Continent_Name,continent_int)%>%distinct()
continents<-full_join(d.continents,dnat.continents,by=c("iso_3166","Continent_Name","continent_int"))
country.poststrat<-left_join(country.poststrat,continents,by=c("iso_3166"))
country.poststrat[is.na(country.poststrat$Continent_Name),] ## 0 here

# load country names 
countrynames<-read_csv("basedata/Covariates/region_covariates.csv")%>%
  select(NAME_0_gadm,iso_3166)%>%distinct()
country.poststrat<-left_join(country.poststrat,countrynames,by="iso_3166")

# examine country names for issues and correct:
sort(unique(country.poststrat$NAME_0_gadm)) 
country.poststrat$NAME_0_gadm[country.poststrat$iso_3166=="CI"]<-"Cote d'Ivoire"
nrow(country.poststrat[is.na(country.poststrat$NAME_0_gadm),]) ## should be zero. if not, check for naming errors. 

# store output objects: alphas, gammas, and betas ####

## read in region_key_disaggregated and merge in, for website ##
regionkey<-read_csv("basedata/region_key_disaggregated.csv")
countrynames<-regionkey%>%
  mutate(NAME_0_gadm=ifelse(mergekey=="CY-NC"|mergekey=="CY-06","Northern Cyprus",NAME_0_gadm))%>%
  select(iso3166_country,NAME_0_gadm,mergekey)%>%
  distinct()
region.alphas<-left_join(region.alphas,countrynames,by=c("iso_3166"="iso3166_country","mergekey"))
## one-to-many join here because of two regions in Cyprus and one in Morocco. 

# save outputs #### 
# rename questions to be consistent with our discussion in text 
#### change survey item names to be consistent with how we discuss in text and resave: 
## replace "concern" with "worry" and "human" or "human_caused" with "attribution" 
d%<>%
  mutate(question=gsub("concern","worry",question),
         question=gsub("worry_worry","worry",question),
         # question=gsub("worry_worried_","worry_",question),
         question=gsub("human_caused","attribution",question),
         question=gsub("human_cause","attribution",question),
         question=gsub("human","attribution",question))
d.nat%<>%
  mutate(question=gsub("concern","worry",question),
         question=gsub("worry_worry","worry",question),
         # question=gsub("worry_worried_","worry_",question),
         question=gsub("human_caused","attribution",question),
         question=gsub("human_cause","attribution",question),
         question=gsub("human","attribution",question))
discrimination%<>%
  mutate(question=gsub("concern","worry",question),
         question=gsub("worry_worry","worry",question),
         # question=gsub("worry_worried_","worry_",question),
         question=gsub("human_caused","attribution",question),
         question=gsub("human_cause","attribution",question),
         question=gsub("human","attribution",question))
difficulties%<>%
  mutate(question=gsub("concern","worry",question),
         question=gsub("worry_worry","worry",question),
         # question=gsub("worry_worried_","worry_",question),
         question=gsub("human_caused","attribution",question),
         question=gsub("human_cause","attribution",question),
         question=gsub("human","attribution",question))


continents<-country.poststrat%>%
  select(Continent_Name,iso_3166)%>%
  distinct()


## analyze extent of problem with region-level estimates changing too much between last year of data and 2020 #### 
est.reg<-region.alphas%>%
  rename(year=year2)%>%
  left_join(continents,by="iso_3166")
est.nat<-country.poststrat%>%
  rename(year=year2)

reg.lastyear<-est.reg%>%
  ## find last year with observations for each region, save climate concern in that year
  filter(obs>0)%>%
  group_by(mergekey)%>%
  arrange(year)%>%
  slice_tail(n=1)%>%
  select(iso_3166,mergekey,mean,year)%>%
  filter(year!="2022-23")%>% ## eliminate data from 2022-23 b/c we're not worried about places where we have data in the final year 
  rename(reg.mean=mean)%>%
  left_join(est.nat,by=c("iso_3166","year"))%>% ## merge with national estimates in that year 
  select(iso_3166,mergekey,reg.mean,year,mean)%>%
  rename(nat.mean=mean)
reg.22<-est.reg%>%filter(year=="2022-23")%>%select(mergekey,mean)%>%rename(reg.mean22=mean)
nat.22<-est.nat%>%filter(year=="2022-23")%>%select(iso_3166,mean)%>%rename(nat.mean22=mean)
reg.lastyear%<>%
  left_join(reg.22,by="mergekey")%>%
  left_join(nat.22,by="iso_3166")%>%
  mutate(delta.nat=nat.mean22-nat.mean,
         delta.reg=reg.mean22-reg.mean,
         diff.deltas=delta.reg-delta.nat,
## next lines: # find regions for which the change in regional estimates (estimate_2020 - estimate_last year of data collection) 
# is different in sign from the change in national estimates between the same years, AND the absolute value of the difference (delta region - delta nation) 
# in these changes is greater than 0.1, OR
# The change in regional and national estimates are both positive and the difference in their changes is greater than 0.1 
# (ie, region increased by 0.1 units more than the country), OR
# The change in regional and national estimates are both negative and the difference in their changes (delta region - delta nation) is less than -0.1 
# (ie, region decreased in concern by 0.1 units more than the country)
         toobig=ifelse((abs(diff.deltas)>0.1 & sign(delta.reg)!=sign(delta.nat))| 
                         (delta.reg>0 & delta.nat>0 & diff.deltas>0.1)|
                         (delta.reg<0 & delta.nat<0 & diff.deltas< -0.1)
                       ,1,0))%>%
  arrange(desc(toobig),mergekey)
table(reg.lastyear$toobig)
# For all other regions, either
# the region's concern went down less than the country's (smaller, negative shift, when the country also shifted negatively)
# The region's concern went up less than the country's (smaller, positive shift, when the country also shifted positively)
# The region's concern changed in a different direction, but the magnitude of the difference was small
# View(reg.lastyear%>%arrange(desc(toobig),desc(diff.deltas)))

interp<-reg.lastyear%>%
  filter(toobig==1)%>%
  mutate(year=substr(year,1,4),
         mean22=reg.mean+delta.nat)%>%
  select(mergekey,reg.mean,year,mean22)%>%
  pivot_longer(cols=c(reg.mean,mean22),names_to="mean")%>%
  mutate(year=ifelse(mean=="mean22",2022,year))## change year so it's just the 1st 4 digits, then create vector between each interval so that there's 2-year spaces 
## add the delta.nat amount to reg. estimate to get the 2020 estimate and then interpolate between the two 
interp2<-list()
for(i in 1:length(unique(interp$mergekey))){
  dsub<-interp%>%filter(mergekey==unique(interp$mergekey)[i])
  means<-approx(dsub$year,dsub$value,xout=seq(dsub$year[1],dsub$year[2],2)) ## this doesn't work if concern decreased...
  # means<-approx(dsub$year,dsub$value,xout=seq(min(dsub$year),max(dsub$year),2))
  interp2[[i]]<-data.frame(mergekey=unique(interp$mergekey)[i],
                           year=means$x,mean.interp=means$y)
}
interp2<-do.call(rbind,interp2)%>%
  mutate(year2=case_when(#as.numeric(year)<=1999~"1998-99",
      #as.numeric(year)>1999&as.numeric(year)<=2001~"2000-01",
      as.numeric(year)>2001&as.numeric(year)<=2003~"2002-03",
      # as.numeric(year)<=2003~"1998-2003",
      as.numeric(year)>2003&as.numeric(year)<=2005~"2004-05",
      as.numeric(year)>2005&as.numeric(year)<=2007~"2006-07",
      as.numeric(year)>2007&as.numeric(year)<=2009~"2008-09",
      as.numeric(year)>2009&as.numeric(year)<=2011~"2010-11",
      as.numeric(year)>2011&as.numeric(year)<=2013~"2012-13",
      as.numeric(year)>2013&as.numeric(year)<=2015~"2014-15",
      as.numeric(year)>2015&as.numeric(year)<=2017~"2016-17",
      as.numeric(year)>2017&as.numeric(year)<=2019~"2018-19",
      as.numeric(year)>2019&as.numeric(year)<=2021~"2020-21",
      as.numeric(year)>2021&as.numeric(year)<=2023~"2022-23")
  )%>%
  select(-year)

## merge interpolated values into region.alphas
region.alphas<-left_join(region.alphas,interp2,by=c('mergekey',"year2"))%>%
  mutate(interpolated=ifelse(!is.na(mean.interp)&mean!=mean.interp,1,0), ## create a flag for interpolated values
         mean=ifelse(interpolated==1,mean.interp,mean)) ## replace mean with interpolated mean if it's been interpolated

# rescale estimates on 0-->1 scale, for visualizations #### 
est.reg<-region.alphas%>%
  rename(year=year2)%>%
  left_join(continents,by="iso_3166")

est.nat$mean.std<-scale(est.nat$mean,center=TRUE,scale=TRUE)[,1]
est.nat$mean.std.bin<-cut(est.nat$mean.std,breaks=c(-3,-2,-1.5,-1,-0.5,0.5,1,1.5,2,4),
                          labels=c("-2","-1.5","-1","-0.5","0","0.5","1","1.5","2+"))
est.nat$mean.scl<-scales::rescale(est.nat$mean,to=c(0,1),from=range(est.nat$mean,na.rm=TRUE,finite=TRUE))
est.nat$mean.scl.bin<-cut(est.nat$mean.scl,breaks=c(-1,.1,.2,.3,.4,.5,.6,.7,.8,.9,1),
                          labels=c("0-0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5","0.5-0.6",
                                   "0.6-0.7","0.7-0.8","0.8-0.9","0.9-1"))


est.reg$mean.std<-scale(est.reg$mean,center=TRUE,scale=TRUE)[,1]
est.reg$mean.std.bin<-cut(est.reg$mean.std,breaks=c(-3,-2,-1.5,-1,-0.5,0.5,1,1.5,2,4),
                          labels=c("-2","-1.5","-1","-0.5","0","0.5","1","1.5","2+"))
est.reg$mean.scl<-scales::rescale(est.reg$mean,to=c(0,1),from=range(est.reg$mean,na.rm=TRUE,finite=TRUE))
est.reg$mean.scl.bin<-cut(est.reg$mean.scl,breaks=c(-1,.1,.2,.3,.4,.5,.6,.7,.8,.9,1),
                          labels=c("0-0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5","0.5-0.6",
                                   "0.6-0.7","0.7-0.8","0.8-0.9","0.9-1"))

## load survey data
load(paste0("inputs/megapoll_globalmrp_ordinal_replication.Rda"))

if(exists("datafilter")){
  save(region.alphas,country.poststrat,difficulties,discrimination,d,d.nat,survey.data,est.nat,est.reg,
       file=paste0("outputs_stan/stan_clean_",modelname,"_",datafilter,".Rdata"))
}else{
save(region.alphas, country.poststrat, difficulties,discrimination, d,d.nat,survey.data,est.nat,est.reg,
     discrimination, file=paste0("outputs_stan/stan_clean_",modelname,".Rdata"))
}


## save csv file with country and region outputs subsetted to 2010-11 and 2022-23
est.reg.sub<-est.reg%>%filter(year=="2010-11"|year=="2022-23")
est.nat.sub<-est.nat%>%filter(year=="2010-11"|year=="2022-23")
if(exists("datafilter")){
  write.csv(est.reg.sub,file=paste0("analyzed_outputs/estimates_regional_2010-22","_",modelname,".csv"),row.names=FALSE)
  write.csv(est.nat.sub,file=paste0("analyzed_outputs/estimates_2010-22","_",modelname,".csv"),row.names=FALSE)
}else{
  write.csv(est.reg.sub,file=paste0("analyzed_outputs/estimates_regional_2010-22",modelname,".csv"),row.names=FALSE)
  write.csv(est.nat.sub,file=paste0("analyzed_outputs/estimates_2010-22",modelname,".csv"),row.names=FALSE)
}
