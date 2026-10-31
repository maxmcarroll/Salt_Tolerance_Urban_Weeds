library(tidyverse)
library(ggplot2)
library(cowplot)

#note to reader -- initially we used "location" to refer to roadside collection environment or park collection environment. "street" was also used initially for the roadside collection environment and revised for clarity later.
#code order: greenhouse experiment, soil salinity, supplementary information, germination experiment

#greenhouse analysis
#set correct folder
setwd("~/JHU Stuff/research/Avolio lab/salt tolerance 2024 (senior thesis)/data")

#read in seed collection information
collectiondata<-read.csv("final collection measurements.csv") %>% 
  select(Line.ID, Site, Location)

#read in greenhouse measurements
measurements<-read.csv("final greenhouse measurements.csv") %>%
  mutate(Line.ID=paste(Species, Lines.ID..expanded., sep="."))

#clean measurement data -- remove "empty" pots i.e. those that were not alive at the beginning of the study, fix typos for plants that were dead at the end of the study, and select only columns that are needed -- columns that aren't used in this analysis are removed
meas1<-measurements %>% 
  filter(dead.!='empty') %>% 
  mutate(dead.=ifelse(dead.=='daed', 'dead', dead.)) %>% 
  select(Species, Lines.ID..expanded., Treatment, Replicate, dead., X8.21:X11.15, Line.ID) 

#selects only Veronica arvensis (VA) plants from the experimental data, removes the last date (all VA were harvested before then), makes a new row for each weekly measurement of each plant, turns calendar date into days since beginning of experiment, cleans condition (i.e. plant health measured by visual tissue damage), and removes date and uncleaned condition
va<-meas1 %>% 
  filter(Species=='VA') %>% 
  select(-X11.15) %>% 
  pivot_longer(X8.21:X11.8, names_to = 'date', values_to = 'cond') %>% 
  mutate(cond2=ifelse(cond %in% c('heaalthy','healthty', 'healthy', 'healthy '), 3, 
                      ifelse(cond %in% c('dead', 'cleared', ""), 0,
                             ifelse(cond %in% c('heavvy wither','heavy wither'), 1,
                                    ifelse(cond %in% c('light wither','slightly wilted'), 2, 999))))) %>% 
  mutate(day=case_when(
    date=='X8.21' ~ 1
    ,date=="X8.26" ~ 5
    ,date=="X8.31"  ~ 10
    ,date=="X9.06" ~ 16
    ,date=="X9.13" ~ 23
    ,date=="X9.2"  ~30
    ,date=="X9.27" ~37
    ,date=="X10.04" ~ 44
    ,date=="X10.11" ~ 51
    ,date=="X10.16" ~ 56
    ,date=="X10.25" ~ 65
    ,date=="X11.2" ~ 73
    ,date=="X11.8" ~ 79
    ,.default=999
  )) %>% 
  select(-date, -cond)

#same procedure as above object "va" but for the other species, Lamium purpureum (LP). LP was in the greenhouse for less time than LP (see manuscript) which is why the dates are different
lp<-meas1 %>% 
  filter(Species=='LP') %>% 
  select(-X8.21, -X8.26, -X8.31, -X9.06, -X9.13, -X9.2, -X9.27, -X10.04) %>% 
  pivot_longer(X10.11:X11.15, names_to = 'date', values_to = 'cond') %>% 
  mutate(cond2=ifelse(cond %in% c('healthy','healthy -- flower'), 3, 
                      ifelse(cond %in% c('dead', 'cleared', ''), 0,
                             ifelse(cond %in% c('heavy wither'), 1,
                                    ifelse(cond %in% c('light wither','light wither -- flower', 'wilty'), 2, 999))))) %>% 
  mutate(day=case_when(
    date=="X10.11" ~ 1
    ,date=="X10.16" ~ 6
    ,date=="X10.25" ~ 15
    ,date=="X11.2"  ~ 23
    ,date=="X11.8" ~ 29
    ,date=="X11.15" ~ 36
    ,.default = 999)) %>% 
  select(-date, -cond)

#merges the two objects containing each species' data into one object
alldat<-lp %>% 
  bind_rows(va) %>% 
  left_join(collectiondata)

#writes an excel .csv file with all the measurements, so we can use SAS to analyze plant condition. see SAS code
write.csv(alldat,"~/JHU Stuff/research/Avolio lab/salt tolerance 2024 (senior thesis)/data/allgrowthdatamarch25.csv",row.names=FALSE)

#collection data file was joined after making the data long. this joins the unformatted datasets together. "measForChi" was used for analysis in SAS and making survival rate graphs
measForChi<-meas1 %>%
  left_join(collectiondata)

#write CSV for survival rate analysis in SAS, see SAS code
write.csv(measForChi,"~/JHU Stuff/research/Avolio lab/salt tolerance 2024 (senior thesis)/data/MaxSaltToleranceGreenhouseExperiment.csv",row.names=FALSE)

#takes average for plant condition on each day, with separate averages for each species, treatment, and location.
means<-alldat %>%
  group_by(Species, Treatment, Location, day) %>% 
  summarise(mcond=mean(cond2), se=sd(cond2)/sqrt(n()))

#takes only VA averages from all the averages across time. significance groups added for labeling from SAS results
VAmeans<-means %>%
  filter(Species == "VA")%>%
  ungroup()%>%
  mutate(siggroup=c(NA,NA,NA,NA,NA,"c","c","c","c",NA,NA,NA,NA,
                    NA,NA,"b","b","b","b","b","b","b","b","b","b","b",
                    NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"c",NA,NA,
                    "a","a","a","a","a","a","a","a","a","a","a","a","a"),
         yposition=case_when(
           Location=="Park"~mcond-se-.2,
           Location=="Street"~mcond+se+.2
         ))

#graphs VA condition across time. different lines for each of the four factorial groups
VAgraph <- ggplot(VAmeans, aes(x=day,y=mcond, 
  color = factor(interaction(Treatment, Location),
                 levels = c("W.Street", "W.Park", "S.Street", "S.Park")),
  shape = factor(interaction(Treatment, Location),
                 levels = c("W.Street", "W.Park", "S.Street", "S.Park")),
  linetype = factor(interaction(Treatment, Location),
                 levels = c("W.Street", "W.Park", "S.Street", "S.Park"))))+
  geom_point(size=3)+
  geom_line(show.legend=FALSE)+
  geom_errorbar(aes(ymin=mcond-se,ymax=mcond+se),width=2, linetype="solid")+
  geom_text(aes(label=siggroup,y=yposition),size=3,color="black")+
  xlab("Days into Experiment")+
  ylab("Average Condition")+
  scale_color_manual(name="Treatment,\nCollection\nEnvironment",
    values=c("S.Park"="#66CC66","W.Park"="#66CC66",
             "S.Street"="#333333","W.Street"="#333333"),
    labels = c("S.Park"="Salt, Park","W.Park"="Water, Park",
               "S.Street"="Salt, Roadside","W.Street"="Water, Roadside"),)+
  ggtitle(expression(paste("(b)")))+
  scale_shape_manual(name="Treatment, Location",
    values = c("S.Park"=17,"W.Park"=24,"S.Street"=16,"W.Street"=21))+
  scale_linetype_manual(name="Treatment, Location",
    values = c("S.Park"=1,"W.Park"=2,"S.Street"=1,"W.Street"=2))+
  guides(color=guide_legend(override.aes=list(shape=
                          c(21, 24, 16, 17))),shape="none",linetype="none")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.title = element_text(size=8),
        axis.text = element_text(size=8),
        plot.title = element_text(size=8))

print(VAgraph)

#survival rates by salt treatment group. statistical analysis was done in SAS
VA1011surviveratesbysalt<-alldat %>%
  filter(Species=="VA" & day==51) %>%
  group_by(Treatment, day) %>%
  mutate(cond2=ifelse(cond2>0,1,0)) %>%
  summarise(dead=sum(cond2==0,na.rm=TRUE),alive=sum(cond2!=0),total=n(),rate=alive/total,
            se=sqrt(rate*(1-rate)/total),.groups="drop")

#survival rates for four experimental groups
VA1011surviverates<-alldat %>%
  filter(Species=="VA" & day==51) %>%
  group_by(Treatment, Location, day) %>%
  mutate(cond2=ifelse(cond2>0,1,0)) %>%
  summarise(dead=sum(cond2==0,na.rm=TRUE),alive=sum(cond2!=0),total=n(),rate=alive/total,
            se=sqrt(rate*(1-rate)/total),.groups="drop") %>%
  mutate(siggroup=(c("b","b","a","a")))

#survival rate graph on 10.11 (after 51 days of salt treatment)
VA1011survivalrategraph<-ggplot(VA1011surviverates, aes(x=factor(Treatment, levels=c("W","S")), y=rate, fill=Location))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_errorbar(aes(ymin=rate-se,ymax=rate+se),
                color="black",width=.4,position=position_dodge(0.9),
                linewidth=0.6)+
  geom_text(aes(label=siggroup,y=rate+.14),size=3,color="black",position=position_dodge(0.9))+
  xlab("Treatment")+
  ylab("Survival Rate")+
  theme_minimal(base_size=14)+
  labs(fill="Collection\nEnvironment")+
  scale_x_discrete(labels=c("S"="Salt","W"="Control"))+
  scale_fill_manual(values=c("Park"="#66CC66","Street"="#333333"),
                    labels=c("Park"="Park","Street"="Roadside"))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  guides(color=guide_legend(reverse=TRUE),
         shape=guide_legend(reverse=TRUE))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  theme(legend.justification=c(0.99,0.99),
        legend.position=c(0.99,0.99),
        legend.title = element_text(size = 8), 
        legend.text = element_text(size = 8),
        legend.key = element_blank(),
        legend.key.size = unit(0.6, 'lines'),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.title = element_text(size=8),
        axis.text = element_text(size=8),
        plot.title = element_text(size=8))+
  ylim(0,1)+         
  ggtitle(expression(paste("(a)")))

print(VA1011survivalrategraph)

#combined survival rate and experimental condition figure for VA
VAbothgraphs<-plot_grid(VA1011survivalrategraph,VAgraph,rel_widths=c(2,4),ncol=2)
print(VAbothgraphs)
ggsave(filename="C:/Users/maxmc/OneDrive/Documents/JHU Stuff/research/Avolio lab/salt tolerance 2024 (senior thesis)/manuscripts/figures/VAdoublegraph.tiff",plot=VAbothgraphs,width=8,height=4,dpi=600)

#takes LP averages from all the averages across time
LPmeans<-means%>%
  filter(Species == "LP")

#LP condition means for roadside and park. significance groups added from SAS results added for labeling. 
#we used one graph divided by collection environment and one graph divided by treatment for figure clarity, since our observations are less clear in the four-group graph
LPlocmeans<-alldat %>% 
  filter(Species == "LP") %>%
  group_by(Location, day) %>% 
  summarise(mcond=mean(cond2), se=sd(cond2)/sqrt(n())) %>%
  ungroup() %>%
  mutate(siggroup=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"*",NA),
         yposition=case_when(
           siggroup=="*"~mcond+se+0.2,
           TRUE~NA_real_))

#graph of LP condition across time by collection environment group
LPlocgraph<-ggplot(LPlocmeans, aes(
  x=day,y=mcond,color=Location,shape=Location))+
  geom_point(size=3)+
  geom_line()+
  geom_errorbar(aes(ymin=mcond-se,ymax=mcond+se), width=2)+
  geom_text(aes(label=siggroup,y=yposition),size=5,color="black")+
  xlab("Days into Experiment")+
  ylab("Average Condition")+
  theme_minimal(base_size=14)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(name="Collection\nEnvironment",
                     values=c("Park"="#66CC66","Street"="#333333"),
                     labels=c("Park"="Park","Street"="Roadside"))+
  scale_shape_manual(name="Collection\nEnvironment",
                     values=c("Park"=17,"Street"=16),
                     labels=c("Park"="Park","Street"="Roadside"))+
  guides(color=guide_legend(reverse=TRUE),
         shape=guide_legend(reverse=TRUE))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.title = element_text(size=8),
        axis.title = element_text(size=8))+
  ggtitle(expression(paste("(c)")))

print(LPlocgraph)

#LP condition means for salt and water. significance groups from SAS results added for labeling
LPtrtmeans<-alldat %>% 
  filter(Species == "LP") %>%
  group_by(Treatment, day) %>% 
  summarise(mcond=mean(cond2), se=sd(cond2)/sqrt(n())) %>%
  ungroup() %>%
  mutate(siggroup=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,"*","*",NA),
         yposition=case_when(
    siggroup=="*"~mcond+se+0.2,
    TRUE~NA_real_))

#graph of LP condition across time by treatment group
LPtrtgraph<-ggplot(data=LPtrtmeans, aes(
  x=day,y=mcond,color=Treatment,shape=Treatment,linetype=Treatment))+
  geom_point(size=3)+
  geom_line()+
  geom_errorbar(aes(ymin=mcond-se,ymax=mcond+se), width=2,linetype=1)+
  geom_text(aes(label=siggroup,y=yposition),size=5,color="black")+
  xlab("Days into Experiment")+
  ylab("Average Condition")+
  labs(color="Treatment")+
  theme_minimal(base_size=14)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values=c("S"="#333333","W"="#333333"),
                     labels=c("S"="Salt","W"="Water"))+
  scale_shape_manual(values=c("S"=16,"W"=21),
                     labels=c("S"="Salt","W"="Water"))+
  scale_linetype_manual(values=c("S"=1,"W"=2))+
  guides(color=guide_legend(reverse=TRUE),
         shape=guide_legend(reverse=TRUE),
         linetype="none")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.title = element_text(size=8),
        axis.title = element_text(size=8))+
  ggtitle(expression(paste("(b)")))

print(LPtrtgraph)

#LP survival rates on day 29
LP1108surviverates<-alldat %>%
  filter(Species=="LP" & day==29) %>%
  group_by(Treatment, Location, day) %>%
  mutate(cond2=ifelse(cond2>0,1,0)) %>%
  summarise(dead=sum(cond2==0,na.rm=TRUE),alive=sum(cond2!=0),total=n(),rate=alive/total,
            se=sqrt(rate*(1-rate)/total),.groups="drop")

#graph for day 29 (11.08)
LP1108survivalrategraph<-ggplot(LP1108surviverates, aes(x=factor(Treatment, levels=c("W","S")), y=rate, fill=Location))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_errorbar(aes(ymin=rate-se,ymax=rate+se),
                color="black",width=.4,position=position_dodge(0.9),
                linewidth=0.6)+
  xlab("Treatment")+
  ylab("Survival Rate")+
  theme_minimal(base_size=14)+
  labs(fill="Collection\nEnvironment")+
  scale_x_discrete(labels=c("S"="Salt","W"="Control"))+
  scale_fill_manual(values=c("Park"="#66CC66","Street"="#333333"),
                    labels=c("Park"="Park","Street"="Roadside"))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  guides(color=guide_legend(reverse=TRUE),
         shape=guide_legend(reverse=TRUE))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  theme(legend.justification=c(0.99,0.99),
        legend.position=c(0.99,0.99),
        legend.title = element_text(size = 8), 
        legend.text = element_text(size = 8),
        legend.key = element_blank(),
        legend.key.size = unit(0.6, 'lines'),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.title = element_text(size=8),
        axis.text = element_text(size=8),
        plot.title = element_text(size=8))+
  ylim(0,1)+         
  ggtitle(expression(paste("(a)")))

print(LP1108survivalrategraph)

#survival, condition by treatment, and condition by collection environment graphs for LP
LPthreegraphs<-plot_grid(LP1108survivalrategraph,LPtrtgraph,LPlocgraph,ncol=3)
print(LPthreegraphs)
ggsave(filename="C:/Users/maxmc/OneDrive/Documents/JHU Stuff/research/Avolio lab/salt tolerance 2024 (senior thesis)/manuscripts/figures/LPtriplegraph.tiff",plot=LPthreegraphs,width=8,height=4,dpi=600)

#counts for table S2 (greenhouse experiment replicates)
#joined measurements with collection data, filtered only for those with an index number (which meant they were alive when the experiment started), and counted how many lines and replicates were alive at the start of the experiment for each species
countdat<-measurements%>%
  left_join(collectiondata)%>%
  filter(!is.na(Index.Number))

repLineCount<-countdat%>%
  group_by(Site, Location, Species)%>%
  summarize(nlines=n_distinct(Line.ID),count=n())

repLineCountByLoc<-countdat%>%
  group_by(Species, Location)%>%
  summarize(nlines=n_distinct(Line.ID),count=n())

repLineCountLocTrt<-countdat%>%
  group_by(Species, Location, Treatment)%>%
  summarize(nlines=n_distinct(Line.ID),count=n())

#soil salinity analysis
#reading soil salinity data. removed two samples where excess rock content meant there was not enough soil (<10g) to measure EC
soilsalt<-read.csv("soil EC remeasure.csv")%>%
  filter(EC..microSiemens.!="sample too small")
soilsalt$EC..microSiemens. <- as.numeric(soilsalt$EC..microSiemens.)

#takes averages by roadside/park across collection sites
locationsaltavg<-soilsalt%>%
  group_by(Location)%>%
  summarize(avgEC=mean(EC..microSiemens.),SE=sd(EC..microSiemens.,na.rm=T)/sqrt(n()),n=n())

#averages by roadside/park and collection site used to get sample counts for table S3
granularsaltavgs<-soilsalt%>%
  group_by(Site, Location)%>%
  summarize(avgEC=mean(EC..microSiemens.),SE=sd(EC..microSiemens.,na.rm=T)/sqrt(n()),n=n())

#ANOVA to check for differences by site and location*site interaction
locsitesaltaov<-aov(EC..microSiemens.~Location*Site,data=soilsalt)
summary(locsitesaltaov)
TukeyHSD(locsitesaltaov)

#graph of averaged EC between roadside and street
saltgraph<-ggplot(locationsaltavg, aes(x=Location, y=avgEC,fill=Location))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_errorbar(aes(ymin=avgEC-SE,ymax=avgEC+SE),
                color="black",width=.4,linewidth=1.2,position=position_dodge(0.9))+
  ylab("Average EC (microSiemens)")+
  theme_minimal(base_size=14)+
  theme(plot.title=element_text(hjust=.5))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  scale_x_discrete(labels=c("PK"="Park","ST"="Roadside"))+
  scale_fill_manual(name="Collection\nEnvironment",
                    values=c("PK"="#66CC66","ST"="#333333"),
                    labels=c("PK"="Park","ST"="Roadside"))+
  theme(legend.justification=c(0.99,0.99),
        legend.position=c(0.99,0.99),
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12),
        legend.key = element_blank(),
        legend.key.size = unit(0.6, 'lines'),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size=12),
        plot.title = element_text(size=12))

print(saltgraph)
ggsave(filename="C:/Users/maxmc/OneDrive/Documents/JHU Stuff/research/Avolio lab/salt tolerance 2024 (senior thesis)/manuscripts/figures/narrowsaltgraph.tiff",plot=saltgraph,width=6,height=6.5,dpi=600)

#germination analysis
library(ggpubr)
theme_set(theme_bw(12))

setwd("~/JHU Stuff/research/Avolio lab/salt tolerance 2024 (senior thesis)/Jenny's files/Pilot Study/data")

#clean germination dataset. here, "V" refers to roadside (V from road verge) and "P" refers to "park"
dat<-read.csv("salt_germination_pilot_data.csv") %>% 
  filter(species!='TO'&species!='LS') %>% 
  filter(maternal_line %in% c('control', 'ultra-low'))
dat$salt_treatment<-recode_factor(dat$salt_treatment, `11.688` = "medium", 
                                  `4.4` = "low", `1.469` = "ultra-low", `0` = "control")
dat$management_type<-recode_factor(dat$management_type, "v" = "V")

#counts of maternal lines for table S1
summary<-dat %>% 
  select(site, management_type, species, ID) %>% 
  unique() %>% 
  group_by(site, management_type, species) %>% 
  summarise(n=length(ID))

#LP germination rates
lamium<-dat %>% 
  filter(species=="LAPU") %>% 
  mutate(park=ifelse(management_type=="P", "Park", "Verge")) %>% 
  group_by(park, salt_treatment) %>% 
  summarise(germ=mean(germ.rate)) %>% 
  filter(salt_treatment!="low"&salt_treatment!="medium")

#LP control group germination rates
control<-lamium %>% 
  filter(salt_treatment=="control") %>% 
  rename(cgerm=germ) %>% 
  select(-salt_treatment)

#how many LP germinated in the salt group divided by how many germinated in the control group
pdiff<-lamium %>% 
  left_join(control) %>% 
  mutate(germdiff=(germ/cgerm)) %>% 
  filter(salt_treatment!="control") %>% 
  mutate(Species="Lamium purpureum")

#germination rates for Stellaria media (SM)
stellaria<-dat %>% 
  filter(species=="STME") %>% 
  mutate(park=ifelse(management_type=="P", "Park", "Verge")) %>% 
  group_by(park, salt_treatment) %>% 
  summarise(germ=mean(germ.rate)) %>% 
  filter(salt_treatment!="low"&salt_treatment!="medium")

#SM control group germination rates
Scontrol<-stellaria %>% 
  filter(salt_treatment=="control") %>% 
  rename(cgerm=germ) %>% 
  select(-salt_treatment)

#how many SM germinated in the salt group divided by how many germinated in the control group
spdiff<-stellaria %>% 
  left_join(Scontrol) %>% 
  mutate(germdiff=(germ)/cgerm) %>% 
  filter(salt_treatment!="control") %>% 
  mutate(Species="Stellaria media")

#percent reductions for both species. the percentages we noted as trends in the results section are 1 - "germdiff" from this table
pdiffbothspecies<-pdiff %>% 
  bind_rows(spdiff)

plot<-dat %>% 
  group_by(management_type, salt_treatment, species) %>% 
  summarize(mg=mean(germ.rate), sd=sd(germ.rate), n=length(germ.rate)) %>% 
  mutate(se=sd/sqrt(n)) %>% 
  mutate(Species=ifelse(species=='STME', "Stellaria media", 'Lamium purpureum'))

#reorder data so scale_fill_manual works correctly
plot$salt_treatment <- factor(plot$salt_treatment, levels = c("control", "ultra-low"))
plot$management_type <- factor(plot$management_type, levels = c("P", "V"))

#germination rates figure
a<-ggplot(data=plot, aes(x=salt_treatment, y=mg, fill=management_type))+
  geom_bar(stat='identity', position=position_dodge(0.9))+
  scale_fill_manual(name="Collection\nEnvironment", labels=c("Park", 'Roadside'),  values = c("#66CC66", "#333333"))+
  geom_errorbar(aes(ymin=mg-se, ymax=mg+se), position = position_dodge(0.9), width=0.3)+
  theme_bw(base_size=8)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        strip.text=element_text(face="italic"))+
  scale_x_discrete(labels=c('Control', 'Salt'))+
  ylab("Germination Rate")+
  facet_wrap(~Species, labeller=labeller(Species = 
                                           c("Lamium purpureum" = "L. purpureum","Stellaria media" = "S. media")))+
  ggtitle('(a)')
a

tograph<-pdiff %>% 
  bind_rows(spdiff)

#comparative germination rates between treatments figure
b<-ggplot(data=tograph, aes(x=park, y=germdiff, fill=park))+
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_manual(name="", labels=c("Park", 'Road Verge'), values = c("#66CC66", "#333333"))+
  scale_x_discrete(labels=c('Park', 'Road Verge'))+
  xlab('')+
  ylab("Proportion that Germinated\nin Salt Relative to Control")+
  theme_bw(base_size=8)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        legend.position = "none",
        strip.text = element_text(face = "italic"))+
  facet_wrap(~Species, labeller=labeller(Species = c("Lamium purpureum" = "L. purpureum","Stellaria media" = "S. media")))+
  ggtitle('(b)')
b

#merge and print germination figure (both species)
germfig<-ggarrange(a,b, common.legend = T, ncol = 1)
germfig
ggsave(filename="C:/Users/maxmc/OneDrive/Documents/JHU Stuff/research/Avolio lab/salt tolerance 2024 (senior thesis)/manuscripts/figures/abgermfig.tiff",plot=germfig,width=5,height=4,units="in",dpi=600)
