#K.Palof      3-1-2017
# GKC fish ticket data - examine ideas for in season metrics for closures / GHLs
# Fish ticket data

#rm(list = ls()) # clear workspace 

## Load packages ---------------------------------------------------
library(tidyverse)
library(reshape2)
library(scales)
library(gridExtra)
library(broom)
options(scipen=9999) # remove scientific notation
theme_set(theme_bw()+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))

## Load data -------------------------------------------------------
fshtct <- read.csv("./data/gkc_fishticket_all_17.csv")
head(fshtct)

fshtct %>% filter(YEAR >= 1980) %>% mutate(year = YEAR, Area = I_FISHERY, catch.date = as.Date(CATCH_DATE, format = '%m/%d/%Y'), 
                                           sell.date = as.Date(SELL_DATE, format = '%m/%d/%Y'), 
                                           catch.day = as.numeric(format(catch.date, "%j")),
                                           sell.day =as.numeric(format(sell.date, "%j"))) %>% 
  select(year, TICKET_NO, ADFG_NO, VESSEL_NAME, catch.date, catch.day, STAT_WEEK, sell.date, sell.day,
                  SEASON, Area, ITEM_NO, STAT_AREA, SPECIES_CODE, NUMBERS, POUNDS, POTS, DELIVERY) ->fshtkt

fshtkt %>% filter(year >= 2000) -> fshtkt00
                  
# need day of fishery catch occured, add season start and end dates.
season %>% mutate(start = as.Date(start_date, format = '%m/%d/%Y'), end = as.Date(closure, format = '%m/%d/%Y'), 
                  start_day1 = format(start, "%j"), end_day1 = format(end, "%j"), 
                  start_day = as.numeric(start_day1), end_day = as.numeric(end_day1)) %>% 
  select(year, Area, start, end, start_day, end_day) ->season_dates


# merge data and season_dates
fshtkt00 %>% left_join(season_dates) -> fshtkt00_d

fshtkt00_d %>% mutate(fishery_day = catch.day - start_day) -> fshtkt00_d

## East Central
fshtkt00_d %>% filter (Area == "East Central GKC") %>% select(year, Area, fishery_day, POUNDS, POTS, NUMBERS) %>%
  group_by(year, Area, fishery_day) %>% summarise(pounds = sum(POUNDS)) ->eastC

## cumulative pounds by fishery day by year
eastC %>% mutate(cumu = cumsum(pounds)) %>% filter(fishery_day < 20) %>% filter(year > 2008) -> eastC_1
eastC_1%>% 
  ggplot(aes(fishery_day, cumu, colour = year, group = year)) +geom_point() + geom_smooth(method = "lm", fill =NA)+
  scale_color_gradient(low = "blue", high ="red")

# regression for first 20 days
eastC_1 %>% # 
  group_by(year) %>%
  do(fit = lm(cumu ~ fishery_day, data =.)) -> step1

step1 %>%
  tidy(fit) -> step1_slope

step1 %>%
  glance(fit) ->step1_out

# need to summarise these for each year - need slope, p-value, ? R -squared



## Graph of fishery day by pounds
fshtkt00_d %>% 
    filter (Area == "East Central GKC") %>% 
    ggplot(aes(fishery_day, POUNDS)) +geom_point() +facet_wrap(~year)


