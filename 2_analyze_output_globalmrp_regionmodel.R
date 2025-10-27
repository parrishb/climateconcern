# Preamble #### 
rm(list=ls())
library(reshape2)
library(countrycode)
library(ggplot2)
library(ggrepel)
library(broom)
library(ggforce)
library(lfe)
library(modelsummary)
library(sensemakr)
library(xtable)
library(magrittr)
library(tidyverse)
library(grid)

onUser<-function(x){
  user<-Sys.info()["user"]
  onD<-grepl(x,user,ignore.case=TRUE)
  return(onD)
}

# set your working directory and figure output folders here #### 
if(onUser("pberg")){
  setwd("~/Documents/GitHub/climateconcern")
  figfolder<-"~/Documents/GitHub/climateconcern/figures/"
  # dropbox<-"~/Dropbox (Personal)/global_mrp/"
}
## Clara set up your folders here
if(onUser("clara")){
  setwd("~/Documents/globalmrp/globalmrp_hpc")
  figfolder<-"~/Documents/climateconcern/figures"
}

source("globalmrp_functions.R")

#load and prepare data ####
modelname<-"country_walk_region_walk_fxdstart" ## new model 240715
timecollapse<-"2yr"
qrestrict<-"concernhuman"
datafilter<-paste(timecollapse,qrestrict,sep="_")

# geogfilter<-"oecd"
if(exists("geogfilter")){datafilter<-paste(datafilter,geogfilter,sep="_")}


if(exists("datafilter")){
  load(paste0("outputs_stan/stan_clean_",modelname,"_",datafilter,".Rdata"))
  # load(paste0("data/d_rstan_region_",datafilter,".Rda"))
}else{
  load(paste0("outputs_stan/stan_clean_",modelname,".Rdata"))
  # load("data/d_rstan_region.Rda")
}

## aggregated data objects from data that are not publicly shareable
load("inputs/surveys_nonpublic.Rda")

# Table S2: create table showing thickness of data across countries #### 
surveydesc<-d%>%
  left_join(select(country.poststrat,NAME_0_gadm,iso_3166)%>%distinct(),by="iso_3166")%>%
  select(iso_3166,NAME_0_gadm,question,year2)%>%
  distinct()%>%
  mutate(qyr=paste(question,year2))%>%
  group_by(NAME_0_gadm,iso_3166)%>%
  summarise(`question-years`=n_distinct(qyr),
            years=n_distinct(year2),
            questions=n_distinct(question))%>%
  arrange(desc(`question-years`))%>%
  mutate(NAME_0_gadm=ifelse(iso_3166=="ST","Sao Tome and Principe",NAME_0_gadm))
write_csv(surveydesc,file=paste0(figfolder,"country_data_summaries.csv"),col_names=FALSE)

## find distribution of country-years, years, and questions in the data 
glimpse(surveydesc)
quantile(surveydesc$questions) ## 75th percentile is 16
quantile(surveydesc$`question-years`) ## 75th percentile is 25.5 (26)

qs_in_model<-unique(d$question) 
length(unique(survey.data$source2)) ## 97 sources (this number will not be right in replication because of non-public sources)
length(unique(d$iso_3166)) ## 166 countries
length(unique(d$mergekey)) ## 2188 regions
length(unique(d$question)) ## 78 questions

## find number of survey responses 
d.sum<-d%>%
  select(question,mergekey,size,year2)%>%
  group_by(question,mergekey,year2)%>%
  distinct()%>%
  summarise(size=sum(size))
sum(d.sum$size)
dnat.sum<-d.nat%>%
  select(question,iso_3166,size,year2)%>%
  group_by(question,iso_3166,year2)%>%
  distinct()%>%
  summarise(size=sum(size))
sum(d.sum$size,dnat.sum$size) ## 3.9 million 

if(exists("d.nat")){
  # d.nat<-d.nat%>%filter(!is.na(question)) ## remove after debugging dataprep 
  qs_in_model_nat<-unique(d.nat$question)
  qs_in_model<-unique(c(qs_in_model,qs_in_model_nat))
}

# validation 1: examine correlations with raw data from several regional/ global surveys #### 

## find question x source combos with the most countries in them ## 
## count all non-NA values for each country-year-question
d.samples<-survey.data%>%
  select(all_of(qs_in_model),year2,iso_3166,source2)%>% ## use source2 so that multi-year sources get collapsed for comparisons and sample sizes. 
  mutate_at(qs_in_model,as.numeric)%>%
  group_by(source2,iso_3166,year2)%>%
  # summarise(across(all_of(qs_in_model)),sum(.,na.rm=TRUE))%>%
  # summarise(across(qs_in_model),sum(!is.na(.)))%>%
  summarise_at(qs_in_model,list(~sum(!is.na(.))))%>%
  ungroup()
d.samples<-d.samples%>%
  pivot_longer(cols=4:ncol(d.samples),names_to="question",values_to="size")#%>%
  # mutate(question=substr(question,1,nchar(question)-2))
n.qns<-d.samples%>%
  group_by(question,source2)%>%
  filter(size!=0)%>%
  summarize(ctry.n=n_distinct(iso_3166))%>%
  arrange(desc(ctry.n))%>%
  mutate(qsource=paste(question,source2,sep="-"))
sort(unique(n.qns$source2))

## how many sources and questions in the model? ####
length(unique(n.qns$source2)) ## 77 sources these numbers will be wrong in replication code because of non-public data
length(unique(n.qns$question)) ## 81 questions

## for reviewers memo, look at correlation with raw data from the Dynata survey
dynata<-d.samples%>%
  filter(source2=="mildenbergerdynata",
         size!=0)
dynata.yes<-survey.data%>%
  select(all_of(unique(dynata$question)),year2,iso_3166,source2)%>%
  filter(source2=="mildenbergerdynata")%>%
  mutate_at(unique(dynata$question),as.numeric)%>%
  mutate_at(unique(dynata$question),~ifelse(.<0,NA,.))%>%
  group_by(iso_3166,year2)%>%
  summarise_at(unique(dynata$question),list(~mean((.),na.rm=TRUE)))%>%
  ungroup()%>%
  pivot_longer(unique(dynata$question),names_to="question",values_to="avg.resp")
d.dynata<-full_join(dynata.yes,dynata,by=c("iso_3166","question","year2"))

## merge in estimates 
d.dynata%<>%left_join(country.poststrat%>%select(mean,iso_3166,year2),by=c("iso_3166","year2")) %>%arrange(iso_3166,year2)
d.dynata%<>%
  group_by(question)%>%
  mutate(prop.scl=scale(avg.resp)[,1], ## scale values within each question-year
         corr=round(cor(prop.scl,mean,use="everything"),2))%>%
  ungroup()
## table of correlations with dynata data: 
print.xtable(xtable(d.dynata%>%
  select(question,corr)%>%
  distinct()%>%
    rename(correlation=corr)),include.rownames = FALSE)
# ggplot(d.dynata,aes(x=prop.scl,y=mean,label=iso_3166))+
#   geom_text()+
#   geom_text(aes(label=corr,x=2,y=2),color="blue")+
#   # geom_abline(intercept=0,slope=1,lty="dotted")+
#   # scale_x_continuous(limits=c(0,3))+
#   # scale_y_continuous(limits=c(0,3))+
#   facet_wrap(~question)

## For figure S5, filter to questions with more than 30 countries included
qlist<-filter(n.qns,ctry.n>=30)%>%
  bind_rows(qlist.np)

## collapse across sources 
d.samples%<>%
  group_by(question,iso_3166,year2,source2)%>%
  summarise(size=sum(size,na.rm=TRUE))%>%
  ungroup()
unique(d.samples$source2)

## aggregate responses across sources by question-year
d.yesresponses<-survey.data%>%
  select(all_of(qlist$question),year2,iso_3166,source2)%>%#
  mutate_at(qlist$question,as.numeric)%>%
  mutate_at(qlist$question,~ifelse(.<0,NA,.))%>% ## eliminate dk responses
  group_by(source2,iso_3166,year2)%>%
  summarise_at(qlist$question,list(~mean((.),na.rm=TRUE)))%>%
  # summarise_at(item.names,funs(mean((.),na.rm=TRUE)))%>%
  ungroup()


## combine into single dataset and get average responses
d.yesresponses<-d.yesresponses%>%
  pivot_longer(cols=qlist$question,names_to='question',values_to='avg.resp')
d.val<-full_join(d.yesresponses,d.samples,by=c("source2","iso_3166","question","year2")) #,"qsource"
class(d.val$year2)
class(country.poststrat$year2) 
d.val<-d.val%>%
  filter(size!=0)
d.val%<>%bind_rows(d.val.np1)

## find discrepancies between survey data and rstan output
setdiff(unique(d.val$iso_3166),unique(country.poststrat$iso_3166)) ## uncomment lines below if either of these turns anything up 
setdiff(unique(country.poststrat$iso_3166),unique(d.val$iso_3166)) 
## we don't have scores from Kosovo or Angola

d.val<-d.val%>%left_join(country.poststrat,by=c("iso_3166","year2")) %>%arrange(iso_3166,year2)

## merge in discrimination parameters 
discrimination2<-discrimination%>%
  select(question,mean,question_int)%>%
  rename(discrimination=mean#,Rhat_question=Rhat
         )
d.val<-left_join(d.val,discrimination2,by="question")%>%
  filter(question%in%qlist$question)


## look at correlations between model estimates and raw data 
d.val<-d.val%>%mutate(prop=avg.resp) ## create a new variable for avg. response, so i can use consistent code for ordinal and binary data
cordata<-d.val%>%
  filter(!is.na(mean))%>%## comment this line out once I've sorted things so that all countries match between output and input (XK only remains)
  group_by(question,year2)%>%
  reframe(ncountries=n_distinct(iso_3166),
            prop.scl=scale(prop)[,1], ## scale values within each question-year
            corr=round(cor(prop.scl,mean,use="everything"),2))%>%
  ungroup()%>%
  select(-prop.scl)%>%
  distinct()%>%
  # filter(!is.na(discrimination))%>% 
  # group_by(question,year2)%>% #source2,
  # summarize(ncountries=n_distinct(iso_3166),
  #           corr=round(cor(prop,mean,use="everything"),2))%>% ### changed this from "complete.obs" to "everything" when changed to do this on full dataset
  arrange(desc(corr))%>%
  filter(ncountries>1)
nrow(d.val[d.val$question%in%cordata$question[is.na(cordata$corr)],]) ## nothing. good. 

d.val2<-d.val%>%
  group_by(question,year2,source2,discrimination,Continent_Name)%>%
  reframe(prop.scl=scale(prop)[,1],
          iso_3166=iso_3166,mean=mean)%>%
  ungroup()

d.val2<-left_join(d.val2,cordata,by=c("question","year2"))%>%#"source2",
  filter(ncountries>1)
### examine correlation between discrimination and correlation with latent construct ## 
nrow(d.val2[is.na(d.val2$corr),]) ## 0--good
nrow(d.val2[is.na(d.val2$Continent_Name),]) ## 10--where?
unique(d.val[is.na(d.val$Continent_Name),"iso_3166"]) # AO, XK--these are the countries for which we don't estimate scores
d.val%<>%mutate(Continent_Name=case_when(iso_3166=="AO"~"Africa",
                                         iso_3166=="XK"~"Europe",
                                         TRUE~Continent_Name))


## what is the mean discrimination parameter? #### 
mean(discrimination2$discrimination) ## mean is 1.2
exp(mean(log(discrimination2$discrimination))) ## geometric mean is 1.00166
quantile(discrimination2$discrimination) ## median is 1.08
print(d.val2%>%select(discrimination)%>%distinct()%>%arrange(discrimination),n=Inf)
## create an object that only contains items within 0.1 of average discrimination parameter. 
d.val.avgdisc<-d.val2%>%filter(discrimination>=round(mean(discrimination2$discrimination),1)-.11&
                                discrimination<=round(mean(discrimination2$discrimination),1)+0.1)
d.val.avgdisc<-d.val2%>%filter(discrimination>=round(exp(mean(log(discrimination2$discrimination))),1)-.1&
                                 discrimination<=round(exp(mean(log(discrimination2$discrimination))),1)+0.1)

## figure S5: Correlation of latent climate concern estaimtes with survey questions from input dataset
ytitle<-"Raw data: Mean response (scaled)"

d.val2<-d.val2%>%filter(!is.na(Continent_Name)) 
unique(d.val2$Continent_Name)

### cross-sectional correlation plot: single page #### 
corplotdata<-d.val2%>%
  mutate(qsource=paste(question,source2,sep="-"))%>%
  filter(qsource%in%qlist$qsource,
         !is.na(discrimination))%>% ## filter out questions that aren't in our model (comment this out once everything is fully merged)
  mutate(discrimination=round(discrimination,2),
         question=fct_reorder(question,corr))
corplot<-corplotdata%>%
         # question=fct_reorder(question,desc(discrimination))%>%
  ggplot(aes(x=mean,y=prop.scl,color=Continent_Name,label=iso_3166))+
  geom_point(size=.5)+
  geom_text(aes(x=2,y=2,label=corr),color="blue",size=2.5)+
  geom_text(aes(x=0,y=2,label=discrimination),color="black",size=2.5)+
  # facet_wrap(~question~sourceyr,nrow=10,ncol=10)+
  facet_wrap(~question~year2,nrow=10,ncol=5, labeller = label_wrap_gen(multi_line=FALSE))+
  labs(x="Latent climate concern estimate", #\n(Discrimination shown in black, correlation in blue)
       y=ytitle,
       color="Continent")+
  theme_bw()+
  theme(axis.text=element_text(size=6))+
  theme(strip.text.x=element_text(size=6),legend.position="bottom")
quartz(18,18)
# corplot
grid.draw(shift_legend(corplot))

### correlation plot showing correlations for questions with average discrimination parameters #### 
corplot.avgdisc<-d.val.avgdisc%>%
  mutate(qsource=paste(question,source2,sep="-"))%>%
  filter(qsource%in%qlist$qsource,
         !is.na(discrimination))%>% ## filter out questions that aren't in our model (comment this out once everything is fully merged)
  mutate(discrimination=round(discrimination,2),
         question=fct_reorder(question,discrimination))%>%
  ggplot(aes(x=mean,y=prop.scl#,color=Continent_Name
             ))+
  geom_point(size=.5)+
  geom_smooth(aes(x=mean,y=prop.scl),method=lm,se=FALSE)+
  geom_text(aes(x=2,y=-2.6,label=corr),color="blue",size=2.5)+
  geom_text(aes(x=1,y=-2.6,label=discrimination),color="black",size=2.5)+
  facet_wrap(~question~year2,nrow=10,ncol=5, labeller = label_wrap_gen(multi_line=FALSE))+
  labs(x="Latent climate concern estimate", 
       y=ytitle,
       color="Continent")+
  theme_bw()+
  theme(axis.text=element_text(size=6))+
  theme(strip.text.x=element_text(size=6),legend.position="bottom")
corplot.avgdisc

## save plots 
if(exists("datafilter")){
  ggsave(file=paste0(figfolder,"validation_correlations_avgdisc-",modelname,"_",datafilter,".pdf"),
         width=6.5,height=4,corplot.avgdisc)
}else{
  ggsave(file=paste0(figfolder,"validation_correlations_avgdisc-",modelname,".pdf"),
         width=6.5,height=2,corplot.avgdisc)
}


if(exists("datafilter")){
  ggsave(file=paste0(figfolder,"validation_correlations-",modelname,"_",datafilter,".pdf"),
         width=6.5,height=9,corplot)
}else{
ggsave(file=paste0(figfolder,"validation_correlations-",modelname,".pdf"),
       width=6.5,height=9,corplot)
}



# validation2: compare estimates with Bergquist and Warshaw US dataset ####
## national time series comparison #### 
bw.nat<-readstata13::read.dta13("predictors/bergquistwarshaw_national.dta")%>%
  filter(year>=2002)%>%
  mutate(year2=case_when(
    year>2001&year<=2003~"2002-03",
    year>2003&year<=2005~"2004-05",
    year>2005&year<=2007~"2006-07",
    year>2007&year<=2009~"2008-09",
    year>2009&year<=2011~"2010-11",
    year>2011&year<=2013~"2012-13",
    year>2013&year<=2015~"2014-15",
    year>2015&year<=2017~"2016-17",
    year>2017&year<=2019~"2018-19",
    year>2019&year<=2021~"2020-21",
    year>2021&year<=2023~"2022-23"))%>%
  group_by(year2)%>%
  summarise(climate_concern_median=quantile(mass_climate_concern,probs=0.5,na.rm=TRUE))
us.nat<-est.nat%>%
  filter(iso_3166=="US")
table(us.nat$year)
us.nat%<>%left_join(bw.nat,by=c("year"="year2"))
cor(us.nat$mean,us.nat$climate_concern_median,use="complete.obs") ## 0.76

### Figure S7: time-series plot with national data ####
usnatplot<-ggplot(us.nat,aes(x=mean,y=climate_concern_median,label=year))+
  geom_text()+
  annotate(geom="text",x=1.1,y=-.76,label=round(cor(us.nat$mean,us.nat$climate_concern_median,use="complete.obs"),2),
           color="blue")+
  labs(x="Global climate concern (US)",y="US climate concern\n(Bergquist and Warshaw 2019)")+
  geom_smooth(method="lm",se=FALSE)+
  scale_x_continuous(limits=c(0.9,1.2))+
  theme_bw()
usnatplot
ggsave(filename=paste0(figfolder,"validation_correlations_nationaltimeseries-",modelname,datafilter,".pdf"),
       width=6.5,height=3.5,usnatplot)


## sub-national correlations with Bergquist and Warshaw data ####
us<-est.reg%>%
  filter(iso_3166=="US")%>%
  separate(mergekey,into=c("us","abb"),sep="-")
# us<-us%>%
#   separate(mergekey,into=c("us","abb"),sep="-")
bw<-read_csv("predictors/bergquistwarshaw_climate.csv")
table(bw$period)
table(us$year)
## for time-collapsed data
if(exists("timecollapse")){
  if(timecollapse=="5yr"){
    bw<-bw%>%mutate(
      year2=case_when(period<=2004~"1998-2004",
                      period>2004&period<=2009~"2005-2009",
                      period>2009&period<=2014~"2010-2014",
                      period>2014&period<=2021~"2015-2021"))%>%
      group_by(year2,abb)%>%
      summarise(climate=mean(climate,na.rm=TRUE))%>%
      ungroup()
  }else if(timecollapse=="2yr"){
    bw<-bw%>%mutate(
      year2=case_when(#period<=1999~"1998-99",
                            #period>1999&period<=2001~"2000-01",
        # period<=2003~"1998-2003",
                            period>2001&period<=2003~"2002-03",
                            period>2003&period<=2005~"2004-05",
                            period>2005&period<=2007~"2006-07",
                            period>2007&period<=2009~"2008-09",
                            period>2009&period<=2011~"2010-11",
                            period>2011&period<=2013~"2012-13",
                            period>2013&period<=2015~"2014-15",
                            period>2015&period<=2017~"2016-17",
                            period>2017&period<=2019~"2018-19",
                            period>2019&period<=2021~"2020-21",
                      period>2021&period<=2023~"2022-23"))%>%
      group_by(year2,abb)%>%
      summarise(climate=quantile(climate,probs=0.5,na.rm=TRUE))%>%
      ungroup()
  }
}

us<-left_join(us,bw,by=c("abb","year"="year2"))

## regress state-level global estimates on PBCW estimates, with state FEs #### 
library(fixest)

statemod<-lm(mean.scl~climate+factor(abb),data=us)
partial_r2(statemod,covariates="climate") ## 0.17

cordataus<-us%>%
  filter(!is.na(climate))%>%
  group_by(year)%>% ## year2? 220813
  summarize(corr=round(cor(mean,climate,use="complete.obs"),2))%>%
  arrange(desc(corr))
us<-left_join(us,cordataus,by="year") ## year2? 220813

### figure S7: Convergent validation (cross-sectional) with Bergquist and Warshaw state-level data #### 
uscorplot<-us%>%
  filter(!is.na(climate))%>%
  ggplot(aes(x=mean,y=climate,label=abb))+
  geom_text(size=2)+
  geom_text(data=us[!is.na(us$climate),],aes(x=1,y=4,label=corr),color="blue")+
  facet_wrap(~year,nrow=6,ncol=4)+ 
  labs(x="Global climate concern",y="Bergquist and Warshaw\nclimate concern")+
  theme_bw()+
  theme(strip.text.x=element_text(size=8),legend.position="bottom")
uscorplot #
if(exists("datafilter")){
  ggsave(file=paste0(figfolder,"validation_correlations_statelevel-",modelname,"_",datafilter,"_",".pdf"),
         width=6.5,height=3.5,uscorplot)
}else{
ggsave(file=paste0(figfolder,"validation_correlations_statelevel-",modelname,"_",".pdf"),
       width=6.5,height=3.5,uscorplot)
}


# validation 3: time series #### 
## find questions with longest time series 
## table of questions asked in 30+ countries, arranged by source
timeseries1<-d.val%>%
  select(source2,year2,iso_3166,question)%>%
  distinct()%>%
  select(source2,year2,question)%>%
  distinct()%>%
  arrange(source2,year2,question)%>%
  rename("Source"=source2,"Year"=year2,"Question"=question)
## restrict to best-known and longest-running sources 
timeseries1%<>%filter(grepl(c("eurobarometer|pew|gallup"),Source)==TRUE)
print.xtable(xtable(timeseries1),include.rownames=FALSE) 
## paste this in the paper as concrete illustration of data sparsity --but first merge in full questions

timeseries<-d.val%>% ## note that this is a subset of the data that only includes questions asked in 30 or more countries 
  select(source2,year2,iso_3166,question)%>%
  distinct()%>%
  mutate(yearquestion=paste(year2,question,sep="-"))%>%
  select(question,yearquestion)%>%
  distinct()%>%
  group_by(question)%>%
  summarise(n=n())%>%
  arrange(desc(n))
quantile(timeseries$n)

## select questions that are asked in 3+ years
sel<-timeseries[timeseries$n>=3,"question"]
d%<>%
  filter(question%in%sel$question)


d.yesresponses<-survey.data%>%
  select(all_of(sel$question),year2,iso_3166)%>%
  mutate_at(sel$question,as.numeric)%>%
  group_by(iso_3166,year2)%>%
  summarise_at(sel$question,~mean((.),na.rm=TRUE))%>%
  ungroup()

## count all non-NA values for each country-year-question
d.samples<-survey.data%>%
  select(all_of(sel$question),year2,iso_3166)%>%
  mutate_at(sel$question,as.numeric)%>%
  group_by(iso_3166,year2)%>%
  summarise_at(vars(sel$question),funs(n=sum(!is.na(.))))%>%
  ungroup()
d.samples<-d.samples%>%
  pivot_longer(cols=3:ncol(d.samples),names_to="question",values_to="size")%>%
  mutate(question=substr(question,1,nchar(question)-2))

## combine into single dataset and get proportion of yes responses
d.yesresponses<-d.yesresponses%>%
  pivot_longer(cols=sel$question,names_to='question',values_to='yes')
d.val<-full_join(d.yesresponses,d.samples,by=c("iso_3166","question","year2"))
d.val<-d.val%>%
  filter(size!=0)%>%
  bind_rows(d.val.np2) ## merge in prepared data from aggregated gallup data 
country.est<-country.poststrat%>%select(iso_3166,year2,mean)## year2? 220813
d.val<-d.val%>%
  left_join(country.est,by=c("iso_3166","year2")) ## year2? 220813

d.val<-d.val%>%left_join(timeseries,by="question")

dwide<-d.val%>%
  pivot_wider(names_from="question",values_from="yes")%>%
  mutate_at(c(6:ncol(.)),~scale(.))%>% ## standardize these columns
  pivot_longer(cols=c(6:ncol(.)),names_to="question",values_to="yes.scl")
d.val<-d.val%>%left_join(dwide,by=intersect(names(d.val),names(dwide)))

## correlation across time series #### 
dwide<-d.val%>%
  select(iso_3166,year2,question,yes.scl)%>%
  pivot_wider(names_from="question",values_from="yes.scl")
## pairwise combinations: 
combos<-combn(names(dwide)[3:ncol(dwide)],2)
r2s<-list()
for(i in 1:ncol(combos)){
  mod.data<-dwide%>% ## country-years for which we have data from both questions
    select(iso_3166,year2,combos[1,i],combos[2,i])%>%
    na.omit()
  # print(nrow(mod.data))
  if(nrow(mod.data)>length(unique(mod.data$iso_3166))){ ## filter to combos with enough degrees of freedom to estimate regression
    print(paste("i=",i))
    # print(combos[,i])
    # print(paste("country year combos=",max(table(mod.data$iso_3166))))
  mod<-lm(unlist(mod.data[,3])~unlist(mod.data[,4])+factor(mod.data$iso_3166))
  r2s[[i]]<-data.frame(x=combos[2,i],y=combos[1,i],partial.R2=partial_r2(mod,covariates="unlist(mod.data[, 4])"),df=summary(mod)$df[2])%>%
    mutate(`N years`=max(table(mod.data$iso_3166)))
  print(r2s[[i]])
  print(paste("nrow mod.data=",nrow(mod.data)))
  }
}
r2s<-do.call(rbind,r2s)%>%
  filter(df>10) ## filter to regressions with at least 10 degrees of freedom
xtable(r2s)

### table S5: predictive ability across input survey questions #### 
print.xtable(xtable(r2s,label="tab:timeseries_inputs",
                    caption="{\\bf Predictive ability across input survey questions:} The table shows the coefficient of determination (Partial R$^2$) 
                    for regressions between responses to each combination of two questions that are asked in three or more years. 
                    Column 3 shows the coefficient of determination after removing country 
                  fixed effects. Column 4 shows the degrees of freedom in the regression, and column 5 shows the number of years for which we have 
                    data from both time series in some overlapping set of countries."),
             include.rownames=FALSE,file=paste0(figfolder,"timeseries_inputs",datafilter,"_",modelname,".tex"))


### table S4: predictive ability of our climate concern estimates on standardized, mean responses to question series asked in 3+ years #### 
ts.pred<-d.val%>%
  nest_by(question)%>%
  mutate(mods1=list(lm(yes.scl~mean+factor(iso_3166),data=data)),
         partial.r2=partial_r2(mods1,covariates="mean"))%>%
  pull(partial.r2,name=question)
ts.pred<-data.frame(Question=names(ts.pred),`Partial R2`=ts.pred)%>% ## some very bad results here for noqfilter_oecd
  left_join(discrimination2,by=c("Question"="question"))%>%
  left_join(timeseries,by=c("Question"="question"))%>%
  select(Question,n,Partial.R2,discrimination)%>%
  rename(`Years asked`="n")%>%
  arrange(desc(discrimination))
if(exists("datafilter")){
  print.xtable(xtable(ts.pred,label="tab:timeseries",
                      caption="{\\bf Predictive ability of our climate concern estimates on standardized, mean responses to question series asked in 3 or more years: } Column 3 shows the coefficient of determination after removing country fixed effects. Column 4 shows the discrimination parameter estimate, in our climate concern model, for each item."),
               include.rownames=FALSE,file=paste0(figfolder,"timeseries_",datafilter,"_",modelname,".tex"))
}else{
  print.xtable(xtable(ts.pred,label="tab:timeseries",
                      caption="{\\bf Predictive ability of our climate concern estimates on standardized, mean responses to question series asked in 3 or more years: } Column 3 shows the coefficient of determination after removing country fixed effects. Column 4 shows the discrimination parameter estimate, in our climate concern model, for each item."),
               include.rownames=FALSE,file=paste0(figfolder,"timeseries",datafilter,"_",modelname,".tex"))
}

# ### predictive ability of our climate concern estimates on european ideology (DELETE IF DON'T USE) ####
# ## Dunlap and Mccright have a 2015 paper showing stronger correlation in western vs. post-communist countries. maybe we show that? 
# ideo<-haven::read_dta("~/Documents/GitHub/globalmrp/globalmrp_hpc/inputs/europe_ideology.dta")%>%
#   mutate(biennium=as_factor(biennium))%>%
#   mutate(year=substr(as.character(biennium),6,nchar(as.character(biennium)))) ## use 2nd year of biennium here 
# 
# country.poststrat2<-country.poststrat%>%
#   mutate(year2 = str_c(str_sub(year2, 1, 5),"20",str_sub(year2, 6, str_length(year2))))%>%
#   mutate(year=substr(year2,1,4)) ## use 1st year of biennium here. 
# ## So we're merging, eg, 2003-2004 ideology into 2004-2005 climate concern 
# 
# country.poststrat2%<>%
#   left_join(ideo%>%select(year,country,econ_abs_post_mean,soc_post_mean),
#             by=c("NAME_0_gadm"="country","year"))
# nrow(country.poststrat2[!is.na(country.poststrat2$econ_abs_post_mean),]) ## 200 values here
# unique(country.poststrat2$NAME_0_gadm[!is.na(country.poststrat2$econ_abs_post_mean)])
# 
# 
# ## find correlation in each year
# cordata_europe<-country.poststrat2%>%
#   filter(!is.na(econ_abs_post_mean))%>%
#   group_by(year)%>% ## year2? 220813
#   summarize(ideocorr=round(cor(mean,econ_abs_post_mean,use="complete.obs"),2))%>%
#   arrange(desc(ideocorr))
# country.poststrat2%<>%
#   left_join(cordata_europe,by="year") 
# 
# ## cross-sectional correlations: 
# ideocorplot<-country.poststrat2%>%
#   filter(!is.na(econ_abs_post_mean))%>%
#   ggplot(aes(x=mean,y=econ_abs_post_mean,label=NAME_0_gadm))+
#   geom_text(size=2)+
#   geom_text(data=country.poststrat2[!is.na(country.poststrat2$econ_abs_post_mean),],aes(x=1,y=4,label=ideocorr),color="blue")+
#   facet_wrap(~year,nrow=6,ncol=4)+ 
#   labs(x="Global climate concern",y="Economic conservatism\n(Caughey, O'Grady, & Warshaw)")+
#   theme_bw()+
#   theme(strip.text.x=element_text(size=8),legend.position="bottom")
# ideocorplot#
# if(exists("datafilter")){
#   ggsave(file=paste0(figfolder,"validation_correlations_europeideology-",modelname,"_",datafilter,"_",".pdf"),
#          width=6.5,height=3.5,ideocorplot)
# }else{
#   ggsave(file=paste0(figfolder,"validation_correlations_europeideology-",modelname,"_",".pdf"),
#          width=6.5,height=3.5,ideocorplot)
# }
# 
# ## create indicator for post-communism
# europe<-unique(country.poststrat2[!is.na(country.poststrat2$econ_abs_post_mean),"NAME_0_gadm"])
# print(europe,n=Inf)
# country.poststrat2%<>%
#   mutate(postcom=ifelse(NAME_0_gadm%in%c("Bulgaria","Czech Republic","Estonia","Hungary","Lithuania","Latvia","Slovenia","Slovakia")==TRUE,"postcom","western"))
# 
# cordata_europe2<-country.poststrat2%>%
#   filter(!is.na(econ_abs_post_mean))%>%
#   group_by(year,postcom)%>% ## year2? 220813
#   summarize(ideocorr2=round(cor(mean,econ_abs_post_mean,use="complete.obs"),2))%>%
#   arrange(desc(ideocorr2))
# country.poststrat2%<>%
#   left_join(cordata_europe2,by=c("year","postcom"))
# 
# ideocorplot2<-country.poststrat2%>%
#   filter(!is.na(econ_abs_post_mean))%>%
#   ggplot(aes(x=mean,y=econ_abs_post_mean,label=NAME_0_gadm))+
#   geom_text(size=2)+
#   geom_text(data=country.poststrat2[!is.na(country.poststrat2$econ_abs_post_mean),],aes(x=1,y=4,label=ideocorr2),color="blue")+
#   facet_wrap(~year~postcom,nrow=6,ncol=4)+ 
#   labs(x="Global climate concern",y="Economic conservatism\n(Caughey, O'Grady, & Warshaw)")+
#   theme_bw()+
#   theme(strip.text.x=element_text(size=8),legend.position="bottom")
# ideocorplot2 ## we see a mostly stronger relationship in western countries, but not in every year (and 2008/ 2010 are notable and problematic exceptions)
# 
# if(exists("datafilter")){
#   ggsave(file=paste0(figfolder,"validation_correlations_europeideology_postcom-",modelname,"_",datafilter,"_",".pdf"),
#          width=6.5,height=9,ideocorplot2)
# }else{
#   ggsave(file=paste0(figfolder,"validation_correlations_europeideology_postcom-",modelname,"_",".pdf"),
#          width=6.5,height=9,ideocorplot2)
# }

### validation of 2 time point comparison. use pew worry questions. delete if don't use. #### 
q.vals<-c("worry_serious_pew","worry_climatethreat_pew")
d.yesresponses.val<-survey.data%>%
  select(all_of(q.vals),year2,iso_3166)%>%
  mutate_at(q.vals,as.numeric)%>%
  group_by(iso_3166,year2)%>%
  summarise_at(q.vals,~mean((.),na.rm=TRUE))%>%
  ungroup()
d.samples.val<-survey.data%>%
  select(all_of(q.vals),year2,iso_3166)%>%
  mutate_at(q.vals,as.numeric)%>%
  group_by(iso_3166,year2)%>%
  summarise_at(q.vals,list(n=~sum(!is.na(.))))%>%
  ungroup()
d.samples.val%<>%
  pivot_longer(cols=3:ncol(d.samples.val),names_to="question",values_to="size")%>%
  mutate(question=substr(question,1,nchar(question)-2))

## combine into single dataset and get proportion of yes responses
d.yesresponses.val%<>%
  pivot_longer(cols=3:ncol(d.yesresponses.val),names_to='question',values_to='yes')
d.val<-full_join(d.yesresponses.val,d.samples.val,by=c("iso_3166","question","year2"))%>%
  filter(size!=0)%>%
  select(-size)

country.est<-country.poststrat%>%select(iso_3166,year2,mean)## year2? 220813
d.val<-d.val%>%
  left_join(country.est,by=c("iso_3166","year2")) ## year2? 220813
table(d.val$question,d.val$year2) ### worry_climatethreat_pew is a good candidate here...asked in 2012/13-2022/23

# ## see what this looks like with worry_serious_pew-- delete if don't use
# d.val%<>%filter(question=="worry_serious_pew",year2%in%c("2006-07","2014-15")==TRUE)
# delta.sd<-d.val%>%
#   filter(year2=="2006-07")%>%
#   group_by(year2)%>%
#   summarise_at(c("yes","mean"),list(sd=~sd((.),na.rm=TRUE)))%>%
#   select(-year2)%>%
#   # rename("yes"="yes_sd","mean"="mean_sd")%>%
#   pivot_longer(cols=everything(),names_to="var",values_to="sdev")
# d.val%<>%pivot_wider(names_from=year2,values_from=c(yes,mean))%>%
#   mutate(meandiff=`mean_2014-15`-`mean_2006-07`,
#          yesdiff=`yes_2014-15`-`yes_2006-07`)%>%
#   mutate(meandiff.std=meandiff/delta.sd$sdev[delta.sd$var=="mean_sd"],
#          yesdiff.std=yesdiff/delta.sd$sdev[delta.sd$var=="yes_sd"])
# quartz(12,12)
# pewplot<-
#   ggplot(d.val,aes(x=meandiff.std,y=yesdiff.std,label=iso_3166))+
#   geom_text()+
#   scale_y_continuous(limits=c(-1,2))+
#   scale_x_continuous(limits=c(-1,2))+
#   geom_abline(slope=1,intercept=0)+
#   geom_hline(yintercept=0,lty="dotted",color="red")+
#   geom_vline(xintercept=0,lty="dotted",color="red")+
#   theme_bw()+
#   labs(x="Change in climate concern\n(2006-07 standard deviations)",
#        y="Change in country-aggregated survey responses\n(2006-07 standard deviations)",
#        title="Change in worry_climatethreat_pew and climate concern estimates,\n2006-07 --2014-15")
# pewplot
# ggsave(filename=paste0(figfolder,"validation_2006-2014.pdf"),width=6.5,height=6.5,pewplot)


## now see what it looks like with worry_climatethreat_pew
d.val<-full_join(d.yesresponses.val,d.samples.val,by=c("iso_3166","question","year2"))%>%
  filter(size!=0)%>%
  select(-size)

country.est<-country.poststrat%>%select(iso_3166,year2,mean)## year2? 220813
d.val<-d.val%>%
  left_join(country.est,by=c("iso_3166","year2"))
d.val%<>%filter(question=="worry_climatethreat_pew",year2%in%c("2012-13","2022-23")==TRUE) ## we lose a lot here because the worry question wasn't asked in all places in both years.
delta.sd<-d.val%>%
  filter(year2=="2012-13")%>%
  group_by(year2)%>%
  summarise_at(c("yes","mean"),list(sd=~sd((.),na.rm=TRUE)))%>%
  select(-year2)%>%
  # rename("yes"="yes_sd","mean"="mean_sd")%>%
  pivot_longer(cols=everything(),names_to="var",values_to="sdev")

d.val%<>%pivot_wider(names_from=year2,values_from=c(yes,mean))%>%
  mutate(meandiff=`mean_2022-23`-`mean_2012-13`,
         yesdiff=`yes_2022-23`-`yes_2012-13`)%>%
  mutate(meandiff.std=meandiff/delta.sd$sdev[delta.sd$var=="mean_sd"],
         yesdiff.std=yesdiff/delta.sd$sdev[delta.sd$var=="yes_sd"])
valcor<-round(cor(d.val$meandiff.std,d.val$yesdiff.std,use="complete.obs"),2) ## 0.77 correlation

pewplot<-
  ggplot(d.val,aes(x=meandiff.std,y=yesdiff.std,label=iso_3166))+
  geom_text()+
  scale_y_continuous(limits=c(-1,2))+
  scale_x_continuous(limits=c(-1,2))+
  geom_abline(slope=1,intercept=0,lty="dashed")+  
  geom_text(aes(x=1.75,y=0,label=paste0("r = ",valcor)),color="blue")+
  # geom_hline(yintercept=0,lty="dotted",color="red")+
  # geom_vline(xintercept=0,lty="dotted",color="red")+
  theme_bw()+
  labs(x="Change in climate concern\n(2012-13 standard deviations)",
       y="Change in country-aggregated survey responses\n(2012-13 standard deviations)"#,
       # title="Change in worry_climatethreat_pew and climate concern estimates,\n2012-13 -- 2022-23"
       )
pewplot
ggsave(filename=paste0(figfolder,"validation_2012-2022.pdf"),width=6.5,height=6.5,pewplot)







# ### validation of comparison between 2010-11 and 2022-23 estimates--DELETE IF DON'T USE ####
# d.yesresponses<-survey.data%>%
#   select(all_of(qs_in_model),year2,iso_3166)%>%
#   mutate_at(qs_in_model,as.numeric)%>%
#   filter(year2%in%c("2010-11","2022-23")==TRUE)%>%
#   group_by(iso_3166,year2)%>%
#   summarise_at(qs_in_model,~mean((.),na.rm=TRUE))%>%
#   ungroup()
# 
# d.samples<-survey.data%>%
#   select(all_of(qs_in_model),year2,iso_3166)%>%
#   mutate_at(qs_in_model,as.numeric)%>%
#   filter(year2%in%c("2010-11","2022-23")==TRUE)%>%
#   group_by(iso_3166,year2)%>%
#   summarise_at(vars(qs_in_model),list(n=~sum(!is.na(.))))%>%
#   ungroup()
# d.samples<-d.samples%>%
#   pivot_longer(cols=3:ncol(d.samples),names_to="question",values_to="size")%>%
#   mutate(question=substr(question,1,nchar(question)-2))
# 
# ## combine into single dataset and get proportion of yes responses
# d.yesresponses<-d.yesresponses%>%
#   pivot_longer(cols=3:ncol(d.yesresponses),names_to='question',values_to='yes')
# d.val<-full_join(d.yesresponses,d.samples,by=c("iso_3166","question","year2"))%>%
#   filter(size!=0)
# d.val%<>%
#   bind_rows(d.val.np2) ## merge in prepared data from aggregated gallup data
# 
# country.est<-country.poststrat%>%select(iso_3166,year2,mean)## year2? 220813
# d.val<-d.val%>%
#   left_join(country.est,by=c("iso_3166","year2")) ## year2? 220813
# 
# ## limit the above to questions asked in both 2010 and 2022...
# qs.2010_2022<-d.val%>%
#   select(year2,iso_3166,question)%>%
#   filter(year2%in%c("2010-11","2022-23"))%>%
#   mutate(yearquestion=paste(year2,question,sep="-"))%>%
#   select(question,yearquestion)%>%
#   distinct()%>%
#   group_by(question)%>%
#   summarise(n=n())%>%
#   filter(n>=2)
# sel<-unique(qs.2010_2022$question)
# 
# d.val%<>%filter(question%in%sel)
# 
# dwide<-d.val%>%
#   pivot_wider(names_from="question",values_from="yes")%>%
#   mutate_at(c(6:ncol(.)),~scale(.))%>% ## standardize these columns
#   pivot_longer(cols=c(6:ncol(.)),names_to="question",values_to="yes.scl")
# d.val<-d.val%>%left_join(dwide,by=intersect(names(d.val),names(dwide)))




