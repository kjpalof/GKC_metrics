#K.Palof      3-1-2017
# GKC logbook data - examine ideas for in season metrics for closures / GHLs

rm(list = ls()) # clear workspace 

## Load packages ---------------------------------------------------
library(tidyverse)
library(reshape2)
library(scales)
library(gridExtra)
options(scipen=9999) # remove scientific notation
theme_set(theme_bw()+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))

## Load data -------------------------------------------------------
dat <- read.csv("./data/gkc_logbook_all_030117.csv")
head(dat)

dat %>% mutate(date = as.Date(EFFORT_DATE, format='%m/%d/%Y'), cpue = TARGET_SPECIES_RETAINED/NUMBER_POTS_LIFTED, 
               dayofyear1 = format(date, "%j"), dayofyear = as.numeric(dayofyear1)) %>% 
  select(year = YEAR, Area_code = I_FISHERY_CODE, Area = I_FISHERY, ADFG_NO, TICKET_NO, EFFORT_NO, 
         date, pots = NUMBER_POTS_LIFTED, crab = TARGET_SPECIES_RETAINED, cpue, dayofyear) -> data
# dayofyear is the Juleian day for each year.

#CPUE by year, Area for first month ----------------------------
month1 <- c(041:079)
data %>% filter(dayofyear %in% month1) ->month1data
head(month1data)
month1data %>% filter(!is.na(cpue)) %>% group_by(Area, year) %>% 
  summarise(meanC = mean(cpue)) ->cpue_month1

cpue_month1 %>% group_by(Area) %>% summarise(CPUE_allyears = mean(meanC)) %>% 
  mutate(mean20 = CPUE_allyears*.20) -> mean_allyears
cpue_month1 %>% left_join(mean_allyears) -> cpue_month1

ggplot(cpue_month1, aes(year, meanC)) +geom_point(aes(colour = Area)) +geom_line(aes(colour = Area))


ggplot(cpue_month1, aes(year, meanC)) +geom_point() +geom_line()+facet_wrap(~Area, scales = "free_y") +
  geom_line(aes(y = CPUE_allyears)) + geom_line(aes(y = mean20), color ="red") + ggtitle("First month of fishery")+
  ylab("Average CPUE") -> fig7
png(file='./figures/firstmonth_CPUE.png', res=200, width=7, height=5, units ="in")  
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,1)))
vplayout<-function(x,y) viewport (layout.pos.row=x, layout.pos.col=y)
print(fig7,vp=vplayout(1,1:1))
dev.off()

month1data %>% 
  group_by(year,Area_code) %>% 
  ggplot(aes(year,cpue))+
stat_summary(fun.data = mean_cl_boot, geom = "smooth") + facet_wrap(~Area) +
stat_summary(fun.y = mean, geom = "point")

# CPUE by year/ Area for all data ------------------------------------
data %>% filter(!is.na(cpue)) %>% filter(Area != "") %>% group_by(Area, year) %>% 
  summarise(meanC = mean(cpue)) ->cpue_data

cpue_data %>% group_by(Area) %>% summarise(CPUE_allyears = mean(meanC)) %>% 
  mutate(mean20 = CPUE_allyears*.20) -> All_mean_allyears
cpue_data %>% left_join(All_mean_allyears) -> cpue_data1

ggplot(cpue_data1, aes(year, meanC)) +geom_point(aes(colour = Area)) +geom_line(aes(colour = Area))
ggplot(cpue_data1, aes(year, meanC)) +geom_point() +geom_line()+facet_wrap(~Area, scales = "free_y") +
  geom_line(aes(y = CPUE_allyears)) + geom_line(aes(y = mean20), color ="red") + ggtitle("Entire season")


#CPUE by year, Area for first two weeks ----------------------------
weeks2 <- c(41:63)
data %>% filter(dayofyear %in% weeks2) ->weeks2data
head(weeks2data)
weeks2data %>% filter(!is.na(cpue)) %>% group_by(Area, year) %>% 
  summarise(meanC = mean(cpue)) ->cpue_week2

cpue_week2 %>% group_by(Area) %>% summarise(CPUE_allyears = mean(meanC)) %>% 
  mutate(mean20 = CPUE_allyears*.20) -> week2_allyears
cpue_week2 %>% left_join(week2_allyears) -> cpue_week2

ggplot(cpue_week2, aes(year, meanC)) +geom_point(aes(colour = Area)) +geom_line(aes(colour = Area))
ggplot(cpue_week2, aes(year, meanC)) +geom_point() +geom_line()+facet_wrap(~Area) +
  geom_line(aes(y = CPUE_allyears)) + geom_line(aes(y = mean20), color ="red") + ggtitle("First two weeks of fishery")


# Other ideas:
# lbs/boat/day
