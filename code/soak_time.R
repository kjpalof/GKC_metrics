#K.Palof
# 3-13-17

# attempt to determine soak time in first few days of fishery. 

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
season <- read.csv("./data/season_dates.csv")

dat %>% mutate(date = as.Date(EFFORT_DATE, format='%m/%d/%Y'), cpue = TARGET_SPECIES_RETAINED/NUMBER_POTS_LIFTED, 
               dayofyear1 = format(date, "%j"), dayofyear = as.numeric(dayofyear1)) %>% 
  select(year = YEAR, Area_code = I_FISHERY_CODE, Area = I_FISHERY, ADFG_NO, TICKET_NO, EFFORT_NO, 
         date, pots = NUMBER_POTS_LIFTED, crab = TARGET_SPECIES_RETAINED, cpue, dayofyear) -> data
# dayofyear is the Juleian day for each year.

season %>% mutate(start = as.Date(start_date, format = '%m/%d/%Y'), end = as.Date(closure, format = '%m/%d/%Y'), 
                  start_day1 = format(start, "%j"), end_day1 = format(end, "%j"), 
                  start_day = as.numeric(start_day1), end_day = as.numeric(end_day1)) %>% 
  select(year, Area, start, end, start_day, end_day) ->season_dates

# merge data and season_dates
data %>% left_join(season_dates) ->data2
  
data2 %>% mutate(fishery_day = dayofyear - start_day) -> data2

data2 %>% group_by(year, Area, fishery_day) %>% 
  summarise(cpue_m = mean(cpue)) -> day_CPUE_sum

# figures for fishery day and CPUE
day_CPUE_sum %>% 
  filter(fishery_day <7) %>% 
  ggplot(aes(fishery_day, cpue_m)) +geom_point() +facet_wrap(~Area, scales = "free_y")


day_CPUE_sum %>% 
  filter(fishery_day < 14) %>% 
  ggplot(aes(year,cpue_m))+
  stat_summary(fun.data = mean_cl_boot, geom = "smooth") + facet_wrap(~Area) +
  stat_summary(fun.y = mean, geom = "point")

