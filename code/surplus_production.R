#K.Palof
# 3-13-17/ 6-8-17
# Attempt at surplus production models in R using TropFishR package and prod_mod (data, plot = FALSE)
# https://www.rdocumentation.org/packages/TropFishR/versions/1.0.0/topics/prod_mod

# need data set with year, catch (lbs), pot lifts, CPUE
# use data set Andrew compiled that's in excel currently - file name:
# add recent data from fish tickets or log books???

## Load packages ---------------------------------------------------
library(tidyverse)
library(reshape2)
library(TropFishR)

### load data ---------
gkc <- read.csv("./data/gkc_fishticket_all_17.csv")
head(gkc)

gkc %>% select(Season = SEASON, Area = I_FISHERY, numbers = NUMBERS, pounds = POUNDS, 
               pots = POTS, Cfec_no = CFEC_NO) -> gkc1

head(gkc1)

### catch by season with effort -----------
gkc1 %>% group_by(Area, Season) %>% 
  summarise(number = sum(numbers), biomass = sum(pounds), pot_effort = sum(pots),
              permits = length(unique(Cfec_no)))
  

