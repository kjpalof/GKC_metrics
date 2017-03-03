#K.Palof      3-1-2017
# GKC logbook data - examine ideas for in season metrics for closures / GHLs

#rm(list = ls()) # clear workspace 

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
ggplot(cpue_month1, aes(year, meanC)) +geom_point() +geom_line()+facet_wrap(~Area) +
  geom_line(aes(y = CPUE_allyears)) + geom_line(aes(y = mean20), color ="red") + ggtitle("First month of fishery")


# CPUE by year/ Area for all data ------------------------------------
data %>% filter(!is.na(cpue)) %>% filter(Area != "") %>% group_by(Area, year) %>% 
  summarise(meanC = mean(cpue)) ->cpue_data

cpue_data %>% group_by(Area) %>% summarise(CPUE_allyears = mean(meanC)) %>% 
  mutate(mean20 = CPUE_allyears*.20) -> All_mean_allyears
cpue_data %>% left_join(All_mean_allyears) -> cpue_data1

ggplot(cpue_data1, aes(year, meanC)) +geom_point(aes(colour = Area)) +geom_line(aes(colour = Area))
ggplot(cpue_data1, aes(year, meanC)) +geom_point() +geom_line()+facet_wrap(~Area) +
  geom_line(aes(y = CPUE_allyears)) + geom_line(aes(y = mean20), color ="red") + ggtitle("Entire season")
