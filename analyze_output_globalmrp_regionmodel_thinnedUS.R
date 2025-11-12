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
library(ggpubr)

onUser<-function(x){
  user<-Sys.info()["user"]
  onD<-grepl(x,user,ignore.case=TRUE)
  return(onD)
}

# set your working directory and figure output folders here #### 
if(onUser("pberg")){
  setwd("~/Documents/GitHub/globalmrp/globalmrp_hpc/")
  figfolder<-"~/Documents/GitHub/globalmrp/figures/"
  replication<-"~/Documents/GitHub/climateconcern/"
  # dropbox<-"~/Dropbox (Personal)/global_mrp/"
}
## Clara set up your folders here
if(onUser("clara")){
  setwd("~/Documents/globalmrp/globalmrp_hpc")
  figfolder<-"~/Documents/climateconcern/figures"
}

source("globalmrp_functions.R")

#load and prepare data ####
modelname<-"country_walk_region_walk_fxdstart_thinned" ## new model 240715
timecollapse<-"2yr"
qrestrict<-"concernhuman"
iter<-"50b"
datafilter<-paste0(timecollapse,"_",qrestrict,iter)

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

qs_in_model<-unique(d$question) 
length(unique(survey.data$source2)) ## 97 sources (this number will not be right in replication because of non-public sources)
length(unique(d$iso_3166)) ## 166 countries
length(unique(d$mergekey)) ## 2188 regions
length(unique(d$question)) ## 78 questions

## find number of survey responses 
# d.sum<-d%>%
#   select(question,mergekey,size,year2)%>%
#   group_by(question,mergekey,year2)%>%
#   distinct()%>%
#   summarise(size=sum(size))
# sum(d.sum$size)
# dnat.sum<-d.nat%>%
#   select(question,iso_3166,size,year2)%>%
#   group_by(question,iso_3166,year2)%>%
#   distinct()%>%
#   summarise(size=sum(size))
# sum(d.sum$size,dnat.sum$size) ## 3.9 million 






# validation2: compare estimates with Bergquist and Warshaw US dataset ####
## national time series comparison #### 
bw.nat<-readstata13::read.dta13(paste0(replication,"predictors/bergquistwarshaw_national.dta"))%>%
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
us.nat%<>%left_join(bw.nat,by=c("year"="year2"))%>%
  filter(!is.na(climate_concern_median))
cor(us.nat$climate_concern_median,us.nat$mean,use="complete.obs") ## note that if we exclude 2004-05 from these correlations, they improve quite a bit.
cor(us.nat$climate_concern_median[us.nat$year!="2004-05"],us.nat$mean[us.nat$year!="2004-05"],use="complete.obs")

### faceted time series with B&W vs. our data, showing trends #### 
usnatplot.pairedscatter<-us.nat%>%
  pivot_longer(cols=c(climate_concern_median,mean.scl),values_to="est",names_to="measure")%>%
  mutate(measure=ifelse(measure=="mean.scl","Global climate concern","Bergquist & Warshaw climate concern"))%>%
  ggplot(aes(x=year,y=est))+
  geom_point()+
  geom_smooth(method="lowess",se=FALSE)+
  facet_wrap(~measure,ncol=1,nrow=2,scales="free")+
  labs(x="year",y="Estimated US climate concern")+
  theme_bw()
usnatplot.pairedscatter
ggsave(filename=paste0(figfolder,"validation_correlations_nationaltimeseries_pairedscatter-",modelname,datafilter,".pdf"),
       width=6.5,height=3.5,usnatplot.pairedscatter)

### Figure S7: time-series plot with national data ####
usnatplot<-ggplot(us.nat,aes(x=mean,y=climate_concern_median,label=year))+
  geom_text(size=2.5)+
  annotate(geom="text",x=1.05,y=-1,label=round(cor(us.nat$mean,us.nat$climate_concern_median,use="complete.obs"),2),
           color="blue")+
  labs(x="",y="")+
  geom_smooth(method="lm",se=FALSE)+
  # scale_x_continuous(limits=c(0.9,1.2))+
  # scale_x_continuous(limits=c(0.5,1.75))+
  # scale_y_continuous(limits=c(-1.2,0))+
  theme_bw()
usnatplot

usnatplot.no0405<-ggplot(us.nat[us.nat$year!="2004-05",],aes(x=mean,y=climate_concern_median,label=year))+
  geom_text(size=2.5)+
  annotate(geom="text",x=1.05,y=-1,label=round(cor(us.nat$mean[us.nat$year!="2004-05"],us.nat$climate_concern_median[us.nat$year!="2004-05"],use="complete.obs"),2),
           color="blue")+
  labs(x="",y="")+
  geom_smooth(method="lm",se=FALSE)+
  # scale_x_continuous(limits=c(0.9,1.2))+
  # scale_x_continuous(limits=c(0.5,1.75))+
  # scale_y_continuous(limits=c(-1.2,0))+
  theme_bw()
usnatplot.no0405
usnatplot2<-ggarrange(usnatplot,usnatplot.no0405,ncol=1,nrow=2)
ggsave(paste0(figfolder,"validation_correlations_nationaltimeseries-",modelname,datafilter,".pdf"),
       width=6.5,height=4.5,
       annotate_figure(usnatplot2,bottom=text_grob("Global climate concern (US)"),
                                            left=text_grob("US climate concern\n(Bergquist and Warshaw 2019)",rot=90)))
# ggsave(filename=paste0(figfolder,"validation_correlations_nationaltimeseries-",modelname,datafilter,".pdf"),
#        width=6.5,height=3.5,usnatplot)


## sub-national correlations with Bergquist and Warshaw data ####
us<-est.reg%>%
  filter(iso_3166=="US")%>%
  separate(mergekey,into=c("us","abb"),sep="-")
# us<-us%>%
#   separate(mergekey,into=c("us","abb"),sep="-")
bw<-read_csv(paste0(replication,"predictors/bergquistwarshaw_climate.csv"))
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
partial_r2(statemod,covariates="climate") 

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
ggsave(file=paste0(figfolder,"validation_correlations_statelevel-",modelname,"_",datafilter,".pdf"),
         width=6.5,height=3.5,uscorplot)




