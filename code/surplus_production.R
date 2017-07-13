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
#library(TropFishR)

### load data ---------
gkc <- read.csv("./data/gkc_fishticket_all_17.csv")
head(gkc)

gkc %>% select(Season = SEASON, Area = I_FISHERY, numbers = NUMBERS, pounds = POUNDS, 
               pots = POTS, Cfec_no = CFEC_NO) -> gkc1

head(gkc1)

### fish ticket catch by season -----------
gkc1 %>% group_by(Area, Season) %>% 
  summarise(number = sum(numbers), biomass = sum(pounds), pot_effort = sum(pots),
              permits = length(unique(Cfec_no))) -> gkc2

as.data.frame(gkc2 %>% filter(Area == "East Central GKC")) # checks out with what I have in excel surplus production file from 2013

### Area models ----------------
# each area has it's own model - one with fish ticket data and # permits
#           - one with logbook data and # pot lifts.

