# Preamble #### 
rm(list=ls())
library(reshape2)
library(countrycode)
library(ggplot2)
library(ggrepel)
library(broom)
library(ggforce)
library(ggpubr)
library(corrplot)
# library(ggsflabel)
library(rmapshaper)
library(sp)
# devtools::install_github("ianmoran11/mmtable2")
library(mmtable2)
library(xtable)
library(magrittr)
library(gt)
library(RColorBrewer)
library(colorspace)
library(nFactors)
library(estimatr)
library(readxl)
library(sf)
library(fixest)
library(grid) ## for function "unit" in maps
library(ggspatial) ## for north arrow function in maps
library(scico)
library(tidyverse)
# source("globalmrp_functions.R")

onUser<-function(x){
  user<-Sys.info()["user"]
  onD<-grepl(x,user,ignore.case=TRUE)
  return(onD)
}
if(onUser("pberg")){
  setwd("~/Documents/GitHub/climateconcern")
  figfolder<-"~/Documents/GitHub/climateconcern/figures/"
}
## Clara set up your folders here
if(onUser("clara")){
  setwd("~/Documents/climateconcern")
  figfolder<-"~/Documents/climateconcern/figures/"
}

#load cleaned model outputs
modelname<-"country_walk_region_walk_fxdstart" ## new model 240715
timecollapse<-"2yr"
qrestrict<-"concernhuman"
datafilter<-paste(timecollapse,qrestrict,sep="_")


if(exists("datafilter")){
  load(paste0("outputs_stan/stan_clean_",modelname,"_",datafilter,".Rdata"))
}else{
load(paste0("outputs_stan/stan_clean_",modelname,".Rdata"))
}

## aggregated data objects from data that are not publicly shareable
load("inputs/surveys_nonpublic.Rda")

## number of questions in our final model: 
length(unique(d$question)) #78
length(unique(d.nat$question)) # 64
## total sample
sum(d$yes) ## 3.6 million
sum(d.nat$yes) ## 301578

qs<-data.frame(question=c(unique(d$question),unique(d.nat$question)))%>%distinct() 
nrow(qs) ##81 

## how many *respondents* are in the dataset. 
respondents<-survey.data%>%select(all_of(qs$question))%>%
  filter_all(any_vars(!is.na(.))) ## This will be an under-count in replication code because of non-shareable data; but add to nrow(constructed to get full count)
nrow(nonpublic.constructed) + nrow(respondents)
## number of countries and regions in input: 
length(unique(d$iso_3166)) ## 166
length(unique(d$mergekey[grepl(".nat",d$mergekey)==FALSE])) ## 2168


## number of countries where concern increased 
# increase in global climate concern 
est.nat%<>%filter(year%in%c("2012-13","2022-23")) ## CHANGE THESE TO MATCH MAPS
est.reg%<>%filter(year%in%c("2012-13","2022-23"))
est.nat%>%
  select(iso_3166,mean.scl,year)%>%
  pivot_wider(names_from=year,values_from=mean.scl)%>%
  mutate(delta=`2022-23`-`2012-13`,
         # increase=ifelse(delta>=0,1,0))%>%
         change=case_when(delta==0~"stable",
                            delta>0~"increase",
                            delta<0~"decrease"))%>%
  group_by(change)%>%
  count() ## 107 increase, 56 decrease, 2 stable

# fig of top increasers and decreasers #### 
summ<-est.nat%>%
  select(NAME_0_gadm,mean.scl,year)%>%
  pivot_wider(names_from=year,values_from=mean.scl)%>%
  mutate(delta=`2022-23`-`2012-13`,
         increase=factor(ifelse(delta>0,1,0)))


summtop<-summ%>%
  arrange(desc(delta))%>%
  slice_head(n=10)
summbottom<-summ%>%
  arrange(delta)%>%
  slice_head(n=10)
# mypal<-brewer.pal(10,"RdBu")[c(3,8)]
mypal <- scico(10, palette = "roma")[c(3, 8)]  


change<-rbind(summtop,summbottom)%>%
  mutate(NAME_0_gadm=factor(NAME_0_gadm),
         NAME_0_gadm=fct_reorder(NAME_0_gadm,delta))%>%
  arrange(NAME_0_gadm)
changefig<-change%>%
  ggplot()+
  geom_segment(aes(x=`2012-13`,y=NAME_0_gadm,xend=`2022-23`,yend=NAME_0_gadm,color=increase),
                            arrow=arrow(length=unit(0.2,"cm")))+
  geom_point(aes(x=`2012-13`,y=NAME_0_gadm,color=increase))+
  scale_color_manual(values=rev(mypal))+
  theme_bw()+
  labs(x="Climate concern",y="")+
  theme(legend.position="none")

quartz(12,12)
changefig
ggsave(paste0(figfolder,"countrychanges_2012-22_",modelname,".pdf"),width=3.5,height=6.5,changefig)

# maps #### 
## load country-level polygons
regions <- st_read("basedata/simplified geometries/g1.agg_wmissingcountries_simplifedgeometry.gpkg")
setdiff(est.reg$mergekey,regions$Group.1)
regions <- ms_simplify(regions)


# countries<-st_read("basedata/gadm_410-levels.gpkg",layer="ADM_0")
countries<-st_read("basedata/simplified geometries/gadm_410-level0_simplifiedgeometry.gpkg")

setdiff(est.nat$NAME_0_gadm,countries$COUNTRY) ## rename these in countries shapefile so that names match those in est.2020
countries%<>%
  mutate(COUNTRY=case_when(GID_0=="MEX"~"Mexico",
                           GID_0=="COG"~"Republic of Congo",
                           GID_0=="CIV"~"Cote d'Ivoire",
                           GID_0=="CPV"~"Cape Verde",
                           GID_0=="CZE"~"Czech Republic",
                           GID_0=="PSE"~"Palestina",
                           GID_0=="STP"~"Sao Tome and Principe",
                           .default=COUNTRY))
est.nat%<>%mutate(NAME_0_gadm=ifelse(iso_3166=="ST","Sao Tome and Principe",NAME_0_gadm)) ## take out special characters in both shapefiles and estimates for this country 
setdiff(est.nat$NAME_0_gadm,countries$COUNTRY) 
countries<-ms_simplify(countries)
countries<-left_join(countries,est.nat,by=c("COUNTRY"="NAME_0_gadm"))
regions<-regions%>%left_join(est.reg,by=c("Group.1"="mergekey"))
countrychange<-left_join(countries,summ,by=c("COUNTRY"="NAME_0_gadm"))

## save outputs for website #### 
regionkey<-read_csv("individual polls/regioncodes/region_key_disaggregated_EDIT_THIS_ONE.csv")
regionkey%<>%left_join(countries%>%select(year,mean.scl,iso_3166),
                       by=c("iso3166_country"="iso_3166"))%>%
  rename(mean.scl.national=mean.scl)#%>%
# select(-geom)
regionkey%<>%left_join(regions%>%select(year,mean.scl,regioncode),by=c("regioncode","year"))%>%
  rename(mean.scl.regional=mean.scl)#%>%
# select(-geom)
regionkey%<>%
  # filter(!is.na(year))%>% ## filter out na values --but need to keep these so that the mappers have all the geographic units
  pivot_wider(names_from=year,values_from=c("mean.scl.regional","mean.scl.national"))%>%
  select(-mean.scl.national_NA,-mean.scl.regional_NA)

regionkey%<>%select(-geom.x,-geom.y)%>%distinct()

save(regionkey,file=paste0("analyzed_outputs/webdata/",modelname,"_formapping.Rda"))
write_csv(regionkey,file=paste0("analyzed_outputs/webdata/",modelname,".csv"))
rm(regionkey)

## Fig. 1: world map #### 
## world map #### 
countryest<-countries%>%filter(!is.na(mean.std))
## save this file 
countrynoest1<-countries%>%filter(is.na(mean.std))%>%
  mutate(year="2012-13") ## changed this from 2010-11 to match our validation 
countrynoest2<-countries%>%filter(is.na(mean.std))%>%
  mutate(year="2022-23")
countrynoest<-rbind(countrynoest1,countrynoest2)
rm(list=c("countrynoest1","countrynoest2"))
# countryest<-as_Spatial(countryest)
# countries<-as_Spatial(countries)
quartz(12,12)

## World map in equal area projection
countryest_proj <- st_transform(countryest, "+proj=moll")
countrynoest_proj <- st_transform(countrynoest, "+proj=moll")

countries_proj1<-st_transform(countries, "+proj=moll")

## deltas 2010-2020
countrychange%<>%filter(!is.na(delta))
countrydelta_proj<-st_transform(countrychange,"+proj=moll")

worldplot<-countrynoest_proj%>%
  ggplot()+
  theme_bw()+
  geom_sf(aes(geometry=geom),fill="lightgray",lwd=.1,color="darkgray")+ ## original
  # geom_sf(aes(geometry=geom),fill="white",lwd=.1,color="darkgray")+ ## with white fill, for use with continuous color scale
  geom_sf(data=countryest_proj,aes(geometry=geom,fill=mean.scl),lwd=.1,color="darkgray")+ ## continuous variable
  scale_fill_scico(palette = "roma", direction = -1)+
  # scale_fill_gradientn(colours = custom_redblue,guide = guide_colorbar(reverse = TRUE)) +
  # scale_fill_distiller(palette = "RdBu", direction = -1,guide = guide_colorbar(reverse = TRUE))+
  # geom_sf(data=countryest_proj,aes(geometry=geom,fill=mean.scl.bin),lwd=.1,color="darkgray")+ ##  binned variable 
  # scale_fill_brewer(palette="RdBu",direction=-1,drop=FALSE,guide=guide_legend(reverse=TRUE))+
  labs(fill="Climate\nconcern")+
  theme_classic()+
  theme(axis.text=element_blank(),
        axis.title= element_blank(),
        axis.ticks = element_blank(),
        panel.grid=element_blank(),
        axis.line = element_blank()
  )+
  annotation_north_arrow(height=unit(.5,"cm"),width=unit(.35,"cm"),
                         style=north_arrow_orienteering(text_size=4)) +  
  coord_sf()+
  facet_wrap(~year,nrow=2,ncol=1)

worldplot
ggsave(file=paste0(figfolder,"map_projected_paired_continuous_",modelname,"_",datafilter,".pdf"),height=6.5,width=6.5,
       worldplot)
# ggsave(file=paste0(figfolder,"map_projected_paired_",modelname,"_",datafilter,".pdf"),height=6.5,width=6.5,
#        worldplot)

## fig. S9: Deltas
worldplot_delta<-countrynoest_proj%>%
  ggplot()+
  theme_bw()+
  geom_sf(aes(geometry=geom),fill="lightgray",lwd=.1,color="darkgray")+
  geom_sf(data=countrydelta_proj,aes(geometry=geom,fill=delta),lwd=.1,color="darkgray")+
  # scale_fill_distiller(palette="RdBu")+
  scale_fill_scico(palette = "roma", direction = -1)+
  # scale_fill_brewer(palette="RdBu",direction=-1,drop=FALSE,guide=guide_legend(reverse=TRUE))+
  labs(fill="Change in \nclimate\nconcern")+
  theme_classic()+
  theme(axis.text=element_blank(),
        axis.title= element_blank(),
        axis.ticks = element_blank(),
        panel.grid=element_blank(),
        axis.line = element_blank()
  )+
  annotation_north_arrow(height=unit(.5,"cm"),width=unit(.35,"cm"),
                         style=north_arrow_orienteering(text_size=4)) +  
  coord_sf()
worldplot_delta
ggsave(file=paste0(figfolder,"map_delta_",modelname,"_",datafilter,".pdf"),height=6.5,width=6.5,
       worldplot_delta)

## subnational maps of Europe #### 
regions[is.na(regions$year)==TRUE,]$year <- "2022-23"
europe <- st_transform(regions, "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs ")
# countries_proj <- st_transform(countrynoest, "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs ")
countries_proj<-st_transform(countries_proj1, "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs ")
europe2022 <- europe[europe$year=="2022-23",]
quartz(12,12)

europlot<-europe2022%>%
  ggplot()+
  theme_bw()+
 # geom_sf(data=countries_proj,aes(geometry=geom),fill="lightgray")+
  geom_sf(aes(geometry=geom,fill=mean.scl),lwd=.1,color="darkgray")+
  # scale_fill_brewer(palette="PRGn",direction=1,drop=FALSE,guide=guide_legend(reverse=TRUE))+
  # scale_fill_brewer(palette="RdBu",direction=-1,drop=FALSE,guide=guide_legend(reverse=TRUE),na.value="gray")+
  scale_fill_scico(palette = "roma", direction = -1,na.value="gray",limits=c(0,1),breaks=c(0,0.25,0.5,0.75,1))+
  # labs(fill="Climate concern\n(st.dev from global mean)")+
  labs(fill="Climate\nconcern")+
  geom_sf(data=countries_proj,aes(geometry=geom),fill=NA,lwd=.2,color="darkgray")+
  # geom_sf(data=europecountries,aes(geometry=geom),lwd=.2,color="darkgray",fill=NA)+
  theme(axis.text=element_blank(),
        axis.title= element_blank(),
        axis.ticks = element_blank(),
        panel.grid=element_blank(),
        axis.line = element_blank()
  )+
  annotation_north_arrow(height=unit(.5,"cm"),width=unit(.35,"cm"),
                         style=north_arrow_orienteering(text_size=4)) +  
  coord_sf()+
  coord_sf(xlim = c(-2000000, 2000000), ylim = c(0, 5000000))
#  facet_wrap(~year)
europlot


## subnational maps of east asia #### 
eastasia <- st_transform(regions, "+proj=aea +lat_1=27 +lat_2=45 +lat_0=35 +lon_0=105 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ")
countries_proj <- st_transform(countries_proj1, "+proj=aea +lat_1=27 +lat_2=45 +lat_0=35 +lon_0=105 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ")
eastasia <- eastasia[eastasia$year=="2022-23",]
quartz(12,12)

eastasiaplot<-eastasia%>%
  ggplot()+
  theme_bw()+
  # geom_sf(data=countries_proj,aes(geometry=geom),fill="lightgray")+
  geom_sf(aes(geometry=geom,fill=mean.scl),lwd=.1,color="darkgray")+
  # scale_fill_brewer(palette="PRGn",direction=1,drop=FALSE,guide=guide_legend(reverse=TRUE))+
  # scale_fill_brewer(palette="RdBu",direction=-1,drop=FALSE,guide=guide_legend(reverse=TRUE),na.value="gray")+
  scale_fill_scico(palette = "roma", direction = -1,na.value="gray",limits=c(0,1),breaks=c(0,0.25,0.5,0.75,1))+
  # labs(fill="Climate concern\n(st.dev from global mean)")+
  geom_sf(data=countries_proj,aes(geometry=geom),fill=NA,lwd=.2,color="darkgray")+
  labs(fill="Climate\nconcern")+
  theme(axis.text=element_blank(),
        axis.title= element_blank(),
        axis.ticks = element_blank(),
        panel.grid=element_blank(),
        axis.line = element_blank()
  )+
  annotation_north_arrow(height=unit(.5,"cm"),width=unit(.35,"cm"),
                         style=north_arrow_orienteering(text_size=4)) +  
  coord_sf(xlim = c(-2500000, 3500000), ylim = c(-3000000, 4000000))
#  facet_wrap(~year)
eastasiaplot
ggsave(file=paste0(figfolder,"map_concern_eastasia_",modelname,".pdf"),width=6.5,height=6.5,eastasiaplot)



## Fig. S12: subnational maps of south asia and middle east #### 
southasia <- st_transform(regions, "+proj=aea +lat_1=28 +lat_2=12 +lat_0=20 +lon_0=78 +x_0=2000000 +y_0=2000000 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ")
countries_proj <- st_transform(countries_proj1, "+proj=aea +lat_1=28 +lat_2=12 +lat_0=20 +lon_0=78 +x_0=2000000 +y_0=2000000 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ")
southasia <- southasia[southasia$year=="2022-23",]
quartz(12,12)

southasiaplot<-southasia%>%
  ggplot()+
  theme_bw()+
  # geom_sf(data=countries_proj,aes(geometry=geom),fill="lightgray")+
  geom_sf(aes(geometry=geom,fill=mean.scl),lwd=.1,color="darkgray")+
  # scale_fill_brewer(palette="PRGn",direction=1,drop=FALSE,guide=guide_legend(reverse=TRUE))+
  # scale_fill_brewer(palette="RdBu",direction=-1,drop=FALSE,guide=guide_legend(reverse=TRUE),na.value="gray")+
  scale_fill_scico(palette = "roma", direction = -1,na.value="gray",limits=c(0,1),breaks=c(0,0.25,0.5,0.75,1))+
  # labs(fill="Climate concern\n(st.dev from global mean)")+
  labs(fill="Climate\nconcern")+
  geom_sf(data=countries_proj,aes(geometry=geom),fill=NA,lwd=.2,color="darkgray")+
  theme(axis.text=element_blank(),
        axis.title= element_blank(),
        axis.ticks = element_blank(),
        panel.grid=element_blank(),
        axis.line = element_blank()
  )+
  annotation_north_arrow(height=unit(.5,"cm"),width=unit(.35,"cm"),
                         style=north_arrow_orienteering(text_size=4)) +  
  coord_sf(xlim = c(-2500000, 4000000), ylim = c(-0000000, 5000000))
#  facet_wrap(~year)
southasiaplot
ggsave(file=paste0(figfolder,"map_concern_southasia_",modelname,".pdf"),width=6.5,height=6.5,southasiaplot)


## Figure 3: combined figure with south asia and europe/ eurasia #### 
ggsave(file=paste0(figfolder,"map_concern_europe_southasia_",modelname,".pdf"),width=9,height=6.5,
       ggarrange(europlot,southasiaplot,nrow=1,ncol=2))
  

## Fig. S13: subnational maps of Africa #### 
regions[is.na(regions$Continent_Name)==TRUE,]$Continent_Name <- "Missing"
africa <- st_transform(regions, "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ")
africa<-africa%>%filter(Continent_Name %in% c("Africa","Europe","Asia","Missing"))
countries_proj <- st_transform(countries_proj1, "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ")
africa<-africa%>%filter(year=="2022-23")
quartz(12,12)

africaplot<-africa%>%
  ggplot()+
  theme_bw()+
  # geom_sf(data=countries_proj,aes(geometry=geom),fill="lightgray")+
  geom_sf(aes(geometry=geom,fill=mean.scl),lwd=.1,color="darkgray")+
  # scale_fill_brewer(palette="PRGn",direction=1,drop=FALSE,guide=guide_legend(reverse=TRUE))+
  scale_fill_scico(palette = "roma", direction = -1,na.value="gray",limits=c(0,1),breaks=c(0,0.25,0.5,0.75,1))+
  # scale_fill_brewer(palette="RdBu",direction=-1,drop=FALSE,guide=guide_legend(reverse=TRUE),na.value="gray")+
  # labs(fill="Climate concern\n(st.dev from global mean)")+
  labs(fill="Climate\nconcern")+
  geom_sf(data=countries_proj,aes(geometry=geom),fill=NA,lwd=.2,color="darkgray")+ ## this line is now creating lines across the map. why? 
  theme(axis.text=element_blank(),
        axis.title= element_blank(),
        axis.ticks = element_blank(),
        panel.grid=element_blank(),
        axis.line = element_blank()
  )+
  annotation_north_arrow(height=unit(.5,"cm"),width=unit(.35,"cm"),
                         style=north_arrow_orienteering(text_size=4)) + 
  coord_sf(xlim = c(-4500000, 3500000), ylim = c(-4000000, 4000000))
#  facet_wrap(~year)
africaplot
ggsave(file=paste0(figfolder,"map_concern_africa_",modelname,".pdf"),width=6.5,height=6.5,africaplot)

## Fig. S11: subnational maps of South America #### 
southamerica <- st_transform(regions, "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs ")
southamerica<-southamerica%>%filter(Continent_Name %in% c("South America","North America","Missing"))
countries_proj <- st_transform(countries_proj1, "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs ")
southamerica <- southamerica[southamerica$year=="2022-23",]
quartz(12,12)

southamericaplot<-southamerica%>%
  ggplot()+
  theme_bw()+
  # geom_sf(data=countries_proj,aes(geometry=geom),fill="lightgray")+
  geom_sf(aes(geometry=geom,fill=mean.scl),lwd=.1,color="darkgray")+
  # scale_fill_brewer(palette="PRGn",direction=1,drop=FALSE,guide=guide_legend(reverse=TRUE))+
  # scale_fill_brewer(palette="RdBu",direction=-1,drop=FALSE,guide=guide_legend(reverse=TRUE),na.value="gray")+
  scale_fill_scico(palette = "roma", direction = -1,na.value="gray",limits=c(0,1),breaks=c(0,0.25,0.5,0.75,1))+
  # labs(fill="Climate concern\n(st.dev from global mean)")+
  labs(fill="Climate\nconcern")+
  geom_sf(data=countries_proj,aes(geometry=geom),fill=NA,lwd=.2,color="darkgray")+
  theme(axis.text=element_blank(),
        axis.title= element_blank(),
        axis.ticks = element_blank(),
        panel.grid=element_blank(),
        axis.line = element_blank()
  )+
  annotation_north_arrow(height=unit(.5,"cm"),width=unit(.35,"cm"),
                         style=north_arrow_orienteering(text_size=4)) + 
  coord_sf(xlim = c(-3000000, 3000000), ylim = c(-2500000, 5000000))
#  facet_wrap(~year)
southamericaplot
ggsave(file=paste0(figfolder,"map_concern_southamerica_",modelname,".pdf"),width=6.5,height=6.5,southamericaplot)

## subnational maps of North America & Caribbean #### 
northamerica <- st_transform(regions, "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")
northamerica<-northamerica%>%filter(Continent_Name %in% c("South America","North America","Missing"))
countries_proj <- st_transform(countries_proj1, "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")
northamerica <- northamerica[northamerica$year=="2022-23",]
quartz(12,12)

northamericaplot<-northamerica%>%
  ggplot()+
  theme_bw()+
  # geom_sf(data=countries_proj,aes(geometry=geom),fill="lightgray")+
  geom_sf(aes(geometry=geom,fill=mean.scl),lwd=.1,color="darkgray")+
  # scale_fill_brewer(palette="PRGn",direction=1,drop=FALSE,guide=guide_legend(reverse=TRUE))+
  # scale_fill_brewer(palette="RdBu",direction=-1,drop=FALSE,guide=guide_legend(reverse=TRUE),na.value="gray")+
  scale_fill_scico(palette = "roma", direction = -1,na.value="gray",limits=c(0,1),breaks=c(0,0.25,0.5,0.75,1))+
  # labs(fill="Climate concern\n(st.dev from global mean)")+
  labs(fill="Climate\nconcern")+
  geom_sf(data=countries_proj,aes(geometry=geom),fill=NA,lwd=.2,color="darkgray")+
  theme(axis.text=element_blank(),
        axis.title= element_blank(),
        axis.ticks = element_blank(),
        panel.grid=element_blank(),
        axis.line = element_blank()
  )+
  annotation_north_arrow(height=unit(.5,"cm"),width=unit(.35,"cm"),
                         style=north_arrow_orienteering(text_size=4)) + 
  coord_sf(xlim = c(-3500000, 3000000), ylim = c(-3500000, 3500000))
#  facet_wrap(~year)
northamericaplot
ggsave(file=paste0(figfolder,"map_concern_northamerica_",modelname,".pdf"),width=6.5,height=6.5,northamericaplot)

## subnational maps of Oceania #### 
oceania <- st_transform(regions, "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs  ")
oceania<-oceania%>%filter(Continent_Name %in% c("Asia","Oceania","Eurasia", "Missing"))
countries_proj <- st_transform(countries_proj1, "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs ")
oceania <- oceania[oceania$year=="2022-23",]
quartz(12,12)

oceaniaplot<-oceania%>%
  ggplot()+
  theme_bw()+
  # geom_sf(data=countries_proj,aes(geometry=geom),fill="lightgray")+
  geom_sf(aes(geometry=geom,fill=mean.scl),lwd=.1,color="darkgray")+
  # scale_fill_brewer(palette="PRGn",direction=1,drop=FALSE,guide=guide_legend(reverse=TRUE))+
  # scale_fill_brewer(palette="RdBu",direction=-1,drop=FALSE,guide=guide_legend(reverse=TRUE),na.value="gray")+
  scale_fill_scico(palette = "roma", direction = -1,na.value="gray",limits=c(0,1),breaks=c(0,0.25,0.5,0.75,1))+
  # labs(fill="Climate concern\n(st.dev from global mean)")+
  labs(fill="Climate\nconcern")+
  geom_sf(data=countries_proj,aes(geometry=geom),fill=NA,lwd=.2,color="darkgray")+
  theme(axis.text=element_blank(),
        axis.title= element_blank(),
        axis.ticks = element_blank(),
        panel.grid=element_blank(),
        axis.line = element_blank()
  )+
  annotation_north_arrow(height=unit(.5,"cm"),width=unit(.35,"cm"),
                         style=north_arrow_orienteering(text_size=4)) + 
  coord_sf(xlim = c(-2500000, 5000000), ylim = c(-5500000, 500000))
#  facet_wrap(~year)
oceaniaplot
ggsave(file=paste0(figfolder,"map_concern_oceania_",modelname,".pdf"),width=4.5,height=6.5,oceaniaplot)


# Figure S16: countries with biggest increases and decreases #### 

# tables of regional estimates #### 
top20<-data.frame(regions)%>%
  filter(year=="2022-23")%>%
  select(regionname_ISOenglish,mergekey,NAME_0,Continent_Name,mean,q95,q5)%>%
  distinct()%>%
  filter(!is.na(mean))%>%
  arrange(mean)%>%
  slice_tail(n=20)%>%
  arrange(regionname_ISOenglish)

bottom20<-data.frame(regions)%>%
  filter(year=="2022-23")%>%
  select(regionname_ISOenglish,mergekey,NAME_0,Continent_Name,mean,q95,q5)%>%
  distinct()%>%
  filter(!is.na(mean))%>%
  arrange(mean)%>%
  slice_head(n=20)%>%
  arrange(regionname_ISOenglish)
ciplot.world<-rbind(top20,bottom20)%>%
  mutate(fullname=paste(regionname_ISOenglish,NAME_0,sep=", "),
         fullname=fct_reorder(fullname,mean))%>%
  ggplot(aes(x=mean,y=fullname,color=Continent_Name))+
  geom_point()+
  geom_linerange(aes(y=fullname,xmin=q5,xmax=q95))+
  labs(y="Region",x="Climate concern",color="Continent")+
  theme_bw()
ciplot.world  
ggsave(file=paste0(figfolder,"ciplot_worldregions_",modelname,".png"),width=6.5,height=6.5,ciplot.world) ## have to save these as png files or come up with a way for the pdf output to correctly save non-English characters

# Fig. S17: highest and lowest climate concerned regions by continent #### 
top5<-data.frame(regions)%>%
  filter(year=="2022-23")%>%
  select(regionname_ISOenglish,mergekey,NAME_0,Continent_Name,mean,q95,q5)%>%
  distinct()%>%
  mutate(Continent_Name=case_when(Continent_Name=="North America"|Continent_Name=="South America"~"Americas",
                                  Continent_Name=="Asia"|Continent_Name=="Oceania"~"Asia, Oceania",
                                  Continent_Name=="Europe"|Continent_Name=="Eurasia"~"Europe, Eurasia",
                                  Continent_Name=="Africa"~"Africa",
                                  .default=Continent_Name))%>%
  filter(!is.na(mean))%>%
  group_by(Continent_Name)%>%
  arrange(mean)%>%
  slice_tail(n=5)%>%
  ungroup()%>%
  select(-mergekey)
bottom5<-data.frame(regions)%>%
  filter(year=="2022-23")%>%
  select(regionname_ISOenglish,mergekey,NAME_0,Continent_Name,mean,q95,q5)%>%
  mutate(Continent_Name=case_when(Continent_Name=="North America"|Continent_Name=="South America"~"Americas",
                                  Continent_Name=="Asia"|Continent_Name=="Oceania"~"Asia, Oceania",
                                  Continent_Name=="Europe"|Continent_Name=="Eurasia"~"Europe, Eurasia",
                                  Continent_Name=="Africa"~"Africa",
                                  .default=Continent_Name))%>%
  filter(!is.na(mean))%>%
  group_by(Continent_Name)%>%
  arrange(mean)%>%
  slice_head(n=5)%>%
  ungroup()%>%
  select(-mergekey)
top.bottom5fig<-rbind(top5,bottom5)%>%
  mutate(fullname=paste(regionname_ISOenglish,NAME_0,sep=",\n"),
         fullname=fct_reorder(fullname,mean))%>%
  ggplot(aes(x=mean,y=fullname))+
  geom_pointrange(aes(y=fullname,xmin=q5,xmax=q95),size=.1)+
  # geom_linerange(aes(y=fullname,xmin=q5,xmax=q95))+
  labs(y="Region",x="Climate concern",color="Continent")+
  theme_bw()+
  guides(color="none")+
  facet_wrap(~Continent_Name,scales="free_y")
top.bottom5fig
ggsave(file=paste0(figfolder,"ciplot_continentregions_",modelname,".png"),width=6.5,height=6.5,top.bottom5fig)


# Fig. S15: ci plot with 2022-23 values ####
## rearrange by order of climate concern
est.2022<-est.nat%>%filter(year=="2022-23")
est.2022$NAME_0_gadm<-reorder(est.2022$NAME_0_gadm,est.2022$mean)
est.2022<-est.2022%>%
  arrange(NAME_0_gadm)
est.2022$group<-c(rep(1,round(nrow(est.2022)/2,0)),
                  rep(2,nrow(est.2022)-round(nrow(est.2022)/2,0)))
head(levels(est.2022$NAME_0_gadm))

## plot
quartz(12,12)
estplot<-ggplot(est.2022,aes(x=mean,y=NAME_0_gadm,color=Continent_Name))+
  geom_point()+
  geom_linerange(aes(y=NAME_0_gadm,xmin=q5,xmax=q95))+
  facet_wrap(~group,scales="free_y",strip.position="top")+
  labs(y="",x="Climate concern index",color="Continent")+
  theme_bw()+
  theme(strip.background=element_blank(),strip.text=element_blank(),
        text=element_text(size=8))
estplot

if(exists("datafilter")){
  ggsave(file=paste0(figfolder,"estimates_",modelname,"_",datafilter,Sys.Date(),".pdf"),
         width=6.5,height=9,units="in",estplot)
}else{
  ggsave(file=paste0(figfolder,"estimates_",modelname,"_",Sys.Date(),".pdf"),
       width=6.5,height=9,units="in",estplot)
}
# Fig. S8:  barplot with discrimination parameters ####
## separate out category
discrimination2<-discrimination%>%
  mutate(question=ifelse(question=="attribution2_eurobarometer","attribution_eurobarometer2",question), ## this should be fixed in next merge 
         question2=question)%>%
  separate(question,c("Item category","question_short"),extra="merge",sep="_")%>%
  mutate(question2=fct_reorder(question2,mean))
## quantiles: 
quantile(discrimination2$mean)

discplot<-ggplot(discrimination2,aes(x=mean,y=question2))+
  geom_bar(stat="identity",aes(fill=`Item category`))+
  # geom_pointrange(aes(y=question2,x=mean,xmin=q5,xmax=q95,color=`Item category`))+
  labs(x="Discrimination parameter",y="Survey item")+
  theme_classic()+
  guides(fill=guide_legend(reverse=TRUE))+
  theme(text=element_text(size=8))
discplot
if(exists("datafilter")){
  ggsave(file=paste0(figfolder,"discrimination_all_",datafilter,"_",modelname,".pdf"),width=6.5,height=9)
}else{
ggsave(file=paste0(figfolder,"discrimination_all.pdf"),width=6.5,height=9)
}

# Figure S1: time coverage across countries by continent #### 
dsum<-d%>%
  select(-c(regioncode_int,size,yes))%>%
  distinct()%>%
  group_by(year2,iso_3166,Continent_Name)%>%tally()
# summarise(qs=count(question_int))
head(dsum) 
range(dsum$n)

# quartz(18,18)

questions.plot<-ggplot(dsum,aes(x=year2,y=iso_3166,alpha=n,color=Continent_Name))+
  geom_point()+
  facet_wrap(~Continent_Name,nrow=7,ncol=1,scales="free_y")+
  theme_classic()+
  theme(axis.text.y=element_blank(),axis.ticks.y = element_blank(),
        legend.position = "none")+
  labs(y="",x="")

questions.plot
if(exists("datafilter")){
  ggsave(filename=paste0(figfolder,"questions_plot_",datafilter,".pdf"),width=8,height=10,plot=questions.plot)
}else{
ggsave(filename=paste0(figfolder,"questions_plot.pdf"),width=8,height=10,plot=questions.plot)
}

# Fig. S19: country and region-level trends with CIs ####
country.est<-country.poststrat%>%select(iso_3166,year2,mean,q5,q95)## year2? 220813
countrynames<-country.poststrat%>%select(iso_3166,NAME_0_gadm)%>%distinct()%>%
  mutate(NAME_0_gadm=ifelse(iso_3166=="ST","Sao Tome and Principe",NAME_0_gadm))
country.est<-left_join(country.est,countrynames,by="iso_3166")
##country alphas; one graph per country #### 
country.alphas.fig<-
  ggplot(country.est, aes(year2, mean, group = 1)) +
  geom_point() +
  geom_line() +
  geom_ribbon(data=country.est, aes(ymin=q5,ymax=q95, alpha=0.3)) +  ## this is 90% CIs
  facet_wrap(~ NAME_0_gadm) +
  theme(legend.position = "none")+
  scale_x_discrete(limits=sort(unique(country.est$year2)),breaks=c("2000-01","2005-06","2010-11","2015-16","2020-21","2022-23"))+
  theme(axis.text.x=element_text(angle=45))
pdf(paste0(figfolder,"countrytrends-",modelname,"_",".pdf"), 15, 20)
country.alphas.fig
dev.off()


# Fig. 4: Climate concern and vulnerability #### 
## merge in exposure data, to analyze change in quadrants
d<-read_csv("predictors/ND-GAIN2022_exposure.csv")
est.nat<-left_join(est.nat,d,by=c("iso_3166","NAME_0_gadm"))
est.nat$exp.std<-scale(est.nat$exposure,center=TRUE,scale=TRUE)

## quadrants plot without 2010 values #### 
reg1<-lm_robust(mean.std~exp.std,data=subset(est.nat,year=="2022-23"))

## table S6: climate risk exposure and climate concern 
texreg::texreg(reg1,custom.coef.names=c("(Intercept)","Exposure (st.dev.)"),
       custom.model.names="Climate concern",
       include.ci=FALSE,label="tab:regressionsimple",
       caption="{ \\bf Effect of climate risk exposure on climate concern:} The table shows the results from an OLS regression of climate concern (standardized) on climate risk exposure (standardized), at the national level, in 2022. The model was estimated using OLS regression with heteroskedasticity-robust standard errors.",
       file=paste0(figfolder,"regression_results_simple.tex"),float.pos="!htpb")

est.nat22<-est.nat%>%filter(year=="2022-23")
est.nat22$mean_2022.stdwithin<-scale(est.nat22$mean,center=TRUE,scale=TRUE)
countrylabs<-est.nat22%>%
  filter((mean_2022.stdwithin>quantile(est.nat22$mean_2022.stdwithin,na.rm=TRUE,probs=0.75)&exp.std<quantile(est.nat22$exp.std,na.rm=TRUE,prob=.25))|## high concern, low exposure
           (mean_2022.stdwithin<quantile(est.nat22$mean_2022.stdwithin,na.rm=TRUE,probs=.25)&exp.std>quantile(est.nat22$exp.std,na.rm=TRUE,prob=.75))| # low concern, high exposure
           (mean_2022.stdwithin<quantile(est.nat22$mean_2022.stdwithin,na.rm=TRUE,probs=.25)&exp.std<quantile(est.nat22$exp.std,na.rm=TRUE,prob=.25))| # low concern, exposure concern
           (mean_2022.stdwithin>quantile(est.nat22$mean_2022.stdwithin,na.rm=TRUE,probs=.75)&exp.std>quantile(est.nat22$exp.std,na.rm=TRUE,prob=.75))| # high concern, high exposure
           NAME_0_gadm%in%c("United States","China","Brazil","India","Russia","Japan","Germany"))%>%
  mutate(type=ifelse(NAME_0_gadm%in%c("United States","China","Brazil","India","Russia","Japan","Germany"),"emitter","outlier"))
countrylabs%<>%mutate(NAME_0_gadm=ifelse(iso_3166=="CD","DRC",NAME_0_gadm))

quadrants<-est.nat22%>%
  ggplot(aes(x=exp.std,y=mean_2022.stdwithin,label=iso_3166))+
  geom_vline(aes(xintercept=0))+
  geom_hline(aes(yintercept=0))+
  geom_smooth(method="lm",se=FALSE,lty="dashed",lwd=.5)+
  geom_text(data=subset(est.nat22,iso_3166%in%countrylabs$iso_3166==FALSE),inherit.aes=TRUE,color="lightgray",size=1.5)+
  geom_point(data=countrylabs,inherit.aes =TRUE)+
  geom_text_repel(data=countrylabs,
                  aes(x=exp.std,y=mean_2022.stdwithin,label=NAME_0_gadm,color=type),
                  nudge_x=0.2,min.segment.length=.3)+
  scale_color_manual(values=c("emitter"="orange","outlier"="black"))+
  guides(color="none")+
  theme_classic()+
  theme(axis.line=element_blank(),
        axis.ticks=element_blank())+
  annotate(geom="text",x=2.5,y=-3,label="High exposure,\nlow concern",color="blue")+
  annotate(geom="text",x=2.5,y=3,label="High exposure,\nhigh concern",color="blue")+
  annotate(geom="text",x=-2.5,y=-3,label="Low exposure,\nlow concern",color="blue")+
  annotate(geom="text",x=-2.5,y=3,label="Low exposure,\nhigh concern",color="blue")+ 
  annotate("text",x=3,y=.5,label=paste("beta == ",round(reg1$coefficients[2],2)),color="blue",parse=TRUE)+
  annotate("text",x=3,y=.3,label=paste("p == ",round(reg1$p.value[2],2)),color="blue",parse=TRUE)+
  labs(x="Climate exposure (st.dev)",y="Climate concern (st.dev)")
quadrants
ggsave(file=paste0(figfolder,"quadrants_exposure_concern_",modelname,".pdf"),width=6.5,height=6.5,quadrants)

# Figure 6: sub-national emissions regression, scatterplot #### 
emissions<-read_excel("predictors/EDGAR_GHG_NUTS2_by_sector_1990_2021.xlsx",sheet=1)
emissions%<>%
  filter(sector=="ENE",
         grepl("FRY",NUTS_ID)==FALSE)%>% # filter to energy production sector 
  # select(-`2021`)%>%
  # pivot_longer(`1990`:`2020`,values_to="emissions",names_to="year")%>%
  select(NUTS_ID,CNTR_CODE,NAME_LATN,sector,`2021`)%>%
  group_by(NUTS_ID,CNTR_CODE,NAME_LATN)%>%
  summarise(emissions.cumul=sum(`2021`,na.rm=TRUE),
            emissions.cumul=emissions.cumul*1000) ## convert to tons rather than kilotons
crosswalk<-read_excel("predictors/EU_crosswalk.xlsx",sheet=1)
emissions<-left_join(emissions,crosswalk,by=c("NUTS_ID"="eucodes")) ## aggregate across EU regions with same code in our dataset
emissions<-emissions%>%
  group_by(CNTR_CODE,ourcodes)%>%
  summarise(emissions.cumul=sum(emissions.cumul))
pop<-read_csv("basedata/Covariates/region_covariates.csv")%>%
  select(mergekey,region_pop)
emissions<-left_join(emissions,pop,by=c("ourcodes"="mergekey"))%>%
  mutate(emissions.percap=emissions.cumul/region_pop)
emissions$emissions.percap.bin<-cut(emissions$emissions.percap,breaks=c(-1,1,2,3,4,5,10,20,100),
                                    labels=c("0-1","1-2","2-3","3-4","4-5","5-10","10-20","20+"))

regions=left_join(regions,emissions,by=c("mergekey"="ourcodes"))
europe<-regions%>%filter(Continent_Name=="Europe")%>%filter(year=="2022-23") ## filter to Europe and only 2022-23
europe$mean.std<-scale(europe$mean,center=TRUE,scale=TRUE) ## this regression standardizes estimates *within* Europe for interpretation. 
europe$emissions.log.std<-scale(log(europe$emissions.percap),center=TRUE,scale=TRUE)
reg2<-lm_robust(mean.std~emissions.log.std+factor(iso_3166),data=europe) ##-.26. the linear-log model returns a coefficient of -.15, so much smaller.
summary(reg2)
reg2<-feols(mean.std~emissions.log.std,vcov=~iso_3166,data=europe)
summary(reg2)
reg2<-coeftable(reg2)
rownames(reg2)[2]<-"Emissions (logged, standardized)"

## table S7: energy sector emissions and climate concern 
print.xtable(xtable(reg2,label="tab:regression.emissions.concern",caption="{\\bf Effect of energy-sector CO$_2$ emissions on climate concern:} The figure table shows the results from an OLS regression of climate concern (standardized, in 2022-23) on per-capita emissions from the energy sector (standardized, 2021) at the sub-national region level in the European Union. Standard errors are clustered by country."),
             caption.placement="bottom",file=paste0(figfolder,"regression_results_emissions.tex"))


emissionsplot<-ggplot(data=europe,aes(x=emissions.log.std,y=mean.std,label=iso_3166))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE,lty="dashed")+
  annotate("text",x=-3,y=-1.5,label=paste("beta == ",round(reg2[2,1],2)),
                                          # round(reg2$coefficients[2],2)),
           color="blue",parse=TRUE)+
  annotate("text",x=-3,y=-1.8,label=paste("p == ",round(reg2[2,4],2)),
                                        # round(reg2$p.value[2],10)),
           color="blue",parse=TRUE)+
  scale_y_continuous(limits=c(-2,3))+
  labs(x="Energy-sector carbon dioxide emissions\n(tons per capita, logged, st.dev, 2021)",y="Climate concern (st.dev), 2022-23")+
  theme_bw()
emissionsplot
ggsave(file=paste0(figfolder,"scatter_emissions_concern_europeregions_",modelname,".pdf"),width=6.5,height=3.5,emissionsplot)



# Fig. S3: correlation matrix showing dimensionality with country-year groups: only questions with 20+ countries #### 
## average each question within group-biennia, 
## center these averages within year (to eliminate time effects), 
## and then average across biennia within groups.

## load survey data and collapse time periods (have to do this anew here b/c survey.data saved in data input only contains questions used...no action or awareness questions) #### 
load(paste0("inputs/megapoll_globalmrp_ordinal_replication.Rda"))

survey.data%<>%filter(as.numeric(year)>=2002)


names(survey.data)<-gsub("concern","worry",names(survey.data))
names(survey.data)<-gsub("worry_worry","worry",names(survey.data))
names(survey.data)<-gsub("human_caused","attribution",names(survey.data))
names(survey.data)<-gsub("human_cause","attribution",names(survey.data))
names(survey.data)<-gsub("human","attribution",names(survey.data))
names(survey.data)<-gsub("happening_aware_","aware_",names(survey.data))
names(survey.data)<-gsub("happening_aware","aware",names(survey.data))

survey.data2<-survey.data%>%
  select(contains("attribution"),contains("aware"),contains("worry"),mergekey)%>%
  filter_all(any_vars(!is.na(.)))

## identify number of countries per question: ####
qs<-survey.data%>%
  select(contains("attribution"),contains("aware"),contains("worry_")#,contains("action_"))
  )
qs<-names(qs)

d.samples<-survey.data%>%
  select(qs,iso_3166)%>% 
  # mutate_at(contains("concern_"),contains("happening_"),contains("human_"),as.numeric)%>%
  group_by(iso_3166)%>%
  summarise_at(all_of(qs),funs(n=sum(!is.na(.))))%>%
  ungroup()
d.samples<-d.samples%>%
  pivot_longer(cols=2:ncol(d.samples),names_to="question",values_to="size")%>%
  mutate(question=substr(question,1,nchar(question)-2))
n.qns<-d.samples%>%
  group_by(question)%>%
  filter(size!=0)%>%
  summarize(ctry.n=n_distinct(iso_3166))%>%
  arrange(desc(ctry.n))

qlist<-filter(n.qns,ctry.n>=20)
qlist<-unique(qlist$question)

country.yr.means<-list()

for(i in 1:length(qlist)){
  print(i)
  print(qlist[i])
  d<-survey.data%>%
    select(iso_3166,year2,qlist[i])%>%
    drop_na()%>%
    # mutate(qlist[i]=as.numeric(qlist[i]))%>%
    group_by(iso_3166,year2)%>%
    summarise(val=mean(get(qlist[i])))%>% ## take mean within country-year for this question
    # d<-d%>%
    group_by(year2)%>% ## group by year and scale
    mutate(val=scale(val))%>%
    group_by(iso_3166)%>% ## take within-country mean of these scaled values
    summarise(val=mean(val,na.rm=TRUE))%>%
    mutate(question=qlist[i])
  country.yr.means[[i]]<-d
}
country.yr.means<-do.call(rbind,country.yr.means)

## load non-public data 
load(paste0("inputs/surveys_nonpublic.Rda"))


country.yr.means%<>%bind_rows(country.yr.means.np) ## merge with the non-public data. This is only 4 questions
unique(country.yr.means.np$question)
country.yr.means.wide<-pivot_wider(country.yr.means,id_cols=c(iso_3166),names_from="question",values_from="val")
cormat.yr<-cor(country.yr.means.wide[,2:ncol(country.yr.means.wide)],use="pairwise.complete.obs")

## find average correlation for each variable 
cormat.df<-data.frame(cormat.yr)
avgcor<-apply(cormat.df,MARGIN = 2,mean,na.rm=TRUE)
avgcor<-data.frame(var=names(cormat.df),cor=round(avgcor,2))%>%
  mutate(cor=paste(var,cor,sep=": "))
rownames(cormat.yr)<-avgcor$cor
colnames(cormat.yr)<-avgcor$cor

pdf(paste0(figfolder,"corplot_country_year_sub.pdf"))
corrplot(cormat.yr,order="alphabet",
         #order="FPC",## order by first principal component
         tl.pos="lt",
         tl.cex=.5,number.cex=0.5)
dev.off()

# correlation matrix divided by global north vs. global south ####
## limit to questions with 30+ countries, merge in continent names so I can look at these correlations 
## by Global North vs. Global South (very roughly approximated by Europe + North America vs. rest of world)

## if want to further restrict the questions entering here
# qlist<-filter(n.qns,ctry.n>=30)
# qlist<-unique(qlist$question)
# qlist<-qlist[-grep("aware",qlist)]
country.yr.means%<>%filter(question%in%qlist==TRUE)
country.yr.means.wide%<>%select(iso_3166,all_of(qlist))
# country.yr.means.wide%<>%select(iso_3166)
continents<-est.nat%>%select(iso_3166,Continent_Name)%>%distinct()
country.yr.means%<>%left_join(continents)%>%
  mutate(europeus=ifelse(Continent_Name%in%c("Europe","North America")==TRUE,"yes","no"))
country.yr.means.wide%<>%left_join(continents)
country.yr.means.wide%<>%
  mutate(europeus=ifelse(Continent_Name%in%c("Europe","North America")==TRUE,"yes","no"))
avgcorlist<-list()
for(i in 1:length(c("yes","no"))){
  df<-country.yr.means.wide%>%
    filter(europeus==c("yes","no")[i])%>%
    select(-Continent_Name,-europeus)
  ## find number of countries for which we have enough data to calculate a correlation --set minimum to 10
  ns<-df%>%summarise(across(-iso_3166,~sum(!is.na(.x))))%>%
    pivot_longer(cols=everything(),values_to="n",names_to="question")%>%
    filter(n>15)
  df%<>%select(iso_3166,all_of(ns$question))
  cormat.yr<-cor(df[,2:ncol(df)],use="pairwise.complete.obs")
  cormat.df<-data.frame(cormat.yr) 
  ## remove full rows and columns of NA values 
  nas<-apply(cormat.df,1,function(x) all(is.na(x)))
  if(length(which(nas==TRUE))>0){
  cormat.df<-cormat.df[-which(nas==TRUE),-which(nas==TRUE)] 
  }
  avgcor<-apply(cormat.df,MARGIN = 2,mean,na.rm=TRUE)
  avgcor<-data.frame(var=names(cormat.df),cor=round(avgcor,2))
  avgcornames<-avgcor%>%
    mutate(cor=paste(var,cor,sep=": "))
  if(length(which(nas==TRUE))>0){
    cormat.yr<-cormat.yr[-which(nas==TRUE),-which(nas==TRUE)]
  }
  rownames(cormat.yr)<-avgcornames$cor
  colnames(cormat.yr)<-avgcornames$cor
  pdf(paste0(figfolder,"corplot_country_year_sub_","europeus=",c("yes","no")[i],".pdf"))
  corrplot(cormat.yr,order="alphabet",
           #order="FPC",## order by first principal component
           tl.pos="lt",
           tl.cex=.5,number.cex=0.5)
  dev.off()
  avgcorlist[[i]]<-avgcor
  names(avgcorlist[[i]])[2]=paste0("cor=",c("yes","no")[i])
  }
avgcor=full_join(avgcorlist[[1]],avgcorlist[[2]],by="var")
## merge with average values and stdev from the two regions 
# country.yr.means%<>%
#   group_by(europeus,question)%>%
#   summarise(mean = mean(val,na.rm=TRUE),
#                stdev = sd(val,na.rm=TRUE))
# country.yr.means%<>%
#   pivot_wider(names_from=europeus,values_from=c(mean,stdev),names_sep=":")
# avgcor%<>%left_join(country.yr.means,by=c("var"="question"))
# avgcor%<>%mutate(across(where(is.numeric),~round(.x,2)))

## set limits 
c(max(avgcor$`cor=yes`,na.rm=TRUE),max(avgcor$`cor=no`,na.rm=TRUE),min(avgcor$`cor=yes`,na.rm=TRUE),min(avgcor$`cor=no`,na.rm=TRUE))
corplot_northsouth<-ggplot(avgcor,aes(x=`cor=yes`,y=`cor=no`,label=var))+
  geom_text(size=2.5)+
  # geom_point()+
  scale_x_continuous(limits=c(0,1))+ ## ensure that these limits include maxes and mins above
  scale_y_continuous(limits=c(0,1))+
  geom_abline(slope=1,intercept=0,lty="dashed")+
  labs(x="Average pairwise correlation:\nEurope and North America",
       y="Average pairwise correlation:\nAsia,Africa,Oceania,Eurasia")+
  theme_bw()
corplot_northsouth
ggsave(filename=paste0(figfolder,"pairwisecorrelations_scatter.pdf"),width=6.5,height=3.5,corplot_northsouth)
# ## merge in discrimination parameters to see if tight correlations here are correlated with strong discrimination parameters
# avgcor%<>%left_join(discrimination2%>%select(question2,mean)%>%distinct(),by=c("var"="question2"))
# avgcor%<>%rename(discrimination=mean)
# ggplot(avgcor,aes(x=`cor=yes`,y=`cor=no`,label=var,color=discrimination))+
#   geom_point()+
#   scale_color_scico(palette="roma")+
#   scale_x_continuous(limits=c(-.5,1))+
#   scale_y_continuous(limits=c(-.5,1))+
#   geom_abline(slope=1,intercept=0)
# 
# ggplot(avgcor,aes(x=`stdev:yes`,y=`stdev:no`,label=var))+
#   geom_text(size=2)+
#   scale_x_continuous(limits=c(0,1.5))+
#   scale_y_continuous(limits=c(0,1.5))

# dimensionality for surveys for which we have multiple questions ####
sources<-c("worldbank_2010","mildenbergerdynata_2021")
dsub<-survey.data%>%
  filter(source%in%sources)%>%
  select(-year,-year2,-source2,-iso_3166,-nonrepresentative,
         -regioncode,-constructed,-party,
         -mergekey,-`...1`,-partyfactcode,-partyfactid,-ideowiki,-ideowiki2,-partyid#,-age,
         #-respondent_gender
         )

## Figure S2: side-by-side screeplots for Mildenberger and World Bank surveys #### 
## (the only 2 in our data set that asked multiple questions in multiple countries)
dsub.m<-dsub%>%filter(source=="mildenbergerdynata_2021")
qs.m<-dsub.m%>%
  select(-source)%>%
  pivot_longer(everything(),names_to="question",values_to="response")%>%
  filter(!is.na(response))%>%
  group_by(question)%>%
  summarise(total=sum(response))%>%
  filter(total>0)
qs.m<-unique(qs.m$question)
dsub.m<-dsub.m%>%select(all_of(qs.m))%>%
  drop_na()
## scale responses
dsub.m2<-apply(dsub.m,MARGIN=2,FUN=function(x){scale(x)})

ev.m<-eigen(cor(dsub.m2))
ap.m<-parallel(subject=nrow(dsub.m2),var=ncol(dsub.m2),
             rep=100,cent=0.05)
nS.m<-nScree(x=ev.m$values,ap=ap.m$eigen$qevpea) 
pdf(height=3.5,width=3,pointsize=8,
    file=paste0(figfolder,"screeplot_mildenberger_2021.pdf"))
plotnScree(nS.m,main="Scree plot for 2021 Validation Survey")
dev.off()

## examine the screeplot with only the 5 remaining items after removing the awareness item 
dsub.m%<>%select(-happening_ccam)
qs.m<-dsub.m%>%
  pivot_longer(everything(),names_to="question",values_to="response")%>%
  filter(!is.na(response))%>%
  group_by(question)%>%
  summarise(total=sum(response))%>%
  filter(total>0)
qs.m<-unique(qs.m$question)
dsub.m<-dsub.m%>%select(all_of(qs.m))%>%
  drop_na()
## scale responses
dsub.m2<-apply(dsub.m,MARGIN=2,FUN=function(x){scale(x)})

ev.m<-eigen(cor(dsub.m2))
ap.m<-parallel(subject=nrow(dsub.m2),var=ncol(dsub.m2),
               rep=100,cent=0.05)
nS.m<-nScree(x=ev.m$values,ap=ap.m$eigen$qevpea) 
pdf(height=3.5,width=6,pointsize=8,
    file=paste0(figfolder,"screeplot_mildenberger_2021_nohappening.pdf"))
plotnScree(nS.m,main="Scree plot for 2021 Validation Survey, without awareness item")
dev.off()




dsub.wb<-dsub%>%filter(source=="worldbank_2010")
qs.wb<-dsub.wb%>%
  select(-source)%>%
  pivot_longer(everything(),names_to="question",values_to="response")%>%
  filter(!is.na(response))%>%
  group_by(question)%>%
  summarise(total=sum(response))%>%
  filter(total>0)
qs.wb<-unique(qs.wb$question)
dsub.wb<-dsub.wb%>%select(all_of(qs.wb))%>%
  drop_na()
## scale responses
dsub.wb2<-apply(dsub.wb,MARGIN=2,FUN=function(x){scale(x)})

ev.wb<-eigen(cor(dsub.wb2))
ap.wb<-parallel(subject=nrow(dsub.wb2),var=ncol(dsub.wb2),
               rep=100,cent=0.05)
nS.wb<-nScree(x=ev.wb$values,ap=ap.wb$eigen$qevpea) 
pdf(height=3.5,width=3,pointsize=8,
    file=paste0(figfolder,"screeplot_worldbank_2010.pdf"))
plotnScree(nS.wb,main="Scree plot for 2010 World Bank Survey")
dev.off()

# table of respondents for which region codes have been constructed #### 
table(survey.data$constructed)
table(nonpublic.constructed$constructed)
unique(survey.data$source[survey.data$constructed==2])
sum(table(survey.data$constructed))
nrow(survey.data)
unique(survey.data[survey.data$constructed=="NA","source"])


