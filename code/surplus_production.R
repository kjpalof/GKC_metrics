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
#library(TropFishR) # this overwrites some of the tidyverse commands.  need to load it first? or later?


### load data ---------
gkc <- read.csv("./data/gkc_fishticket_all_17.csv")
gkc_log <- read.csv("./data/gkc_logbook_all_030117.csv")
head(gkc)
head(gkc_log)

### select variables -------------
gkc %>% select(Season = SEASON, Area = I_FISHERY, numbers = NUMBERS, pounds = POUNDS, 
               pots = POTS, Cfec_no = CFEC_NO) -> gkc1
head(gkc1)

# need to add season here to match fish tickets
gkc_log %>% select(Year = YEAR, Area = I_FISHERY, no_crab = TARGET_SPECIES_RETAINED, 
                   pots = NUMBER_POTS_LIFTED, Cfec_no = CFEC_NO) -> gkc1_log
gkc1_log %>% mutate(Season = ifelse (Year == 2000, "Oct1999 - Sep00", 
                              ifelse(Year == 2001,"Oct2000 - Sep01", 
                               ifelse(Year == 2002, "Oct2001 - Sep02", 
                                ifelse(Year == 2003, "Oct2002 - Sep03", 
                                 ifelse(Year == 2004, "Oct2003 - Sep04", 
                                  ifelse(Year == 2005, "Oct2004 - Sep05", 
                                    ifelse(Year == 2006, "Oct2005 - Sep06", 
                                      ifelse(Year == 2007, "Oct2006 - Sep07", 
                                       ifelse(Year == 2008, "Oct2007 - Sep08", 
                                        ifelse(Year == 2009, "Oct2008 - Sep09", 
                                         ifelse (Year == 2010, "Oct2009 - Sep10", 
                                          ifelse(Year == 2011, "Oct2010 - Sep11", 
                                           ifelse(Year == 2012, "Oct2011 - Sep12", 
                                            ifelse(Year == 2013, "Oct2012 - Sep13", 
                                             ifelse(Year == 2014, "Oct2013 - Sep14", 
                                              ifelse(Year == 2015, "Oct2014 - Sep15", 
                                               ifelse(Year == 2016, "Oct2015 - Sep16", 
       ifelse(Year == 2017, "Oct2016 - Sep17", ""))))))))))))))))))) -> gkc1_log
head(gkc1_log)
### logbook catch by season
gkc1_log %>% group_by(Area, Season, Year) %>% 
  summarise(no_crabs = sum(no_crab, na.rm = TRUE), pot_effort = sum(pots, na.rm = TRUE),
            permits = length(unique(Cfec_no))) -> gkc2_log


### fish ticket catch by season -----------
gkc1 %>% group_by(Area, Season) %>% 
  summarise(number = sum(numbers, na.rm = TRUE), biomass = sum(pounds, na.rm = TRUE), ft_pots = sum(pots, na.rm=TRUE),
              ft_permits = length(unique(Cfec_no))) -> gkc2

as.data.frame(gkc2 %>% filter(Area == "East Central GKC")) # checks out with what I have in excel surplus production file from 2013
gkc2 %>% filter(Season == "Oct2006 - Sep07")

### Area models ----------------
# each area has it's own model - one with fish ticket data and # permits
#           - one with logbook data and # pot lifts.
# merge gkc2 and gkc2_log so have catch in numbers, no_crab, and pounds with pots from both and permits.
gkc2 %>% left_join(gkc2_log) -> gkc3
head(gkc3)

unique(gkc3$Area)
# Areas: ""                           "East Central GKC"           "Icy Strait GKC"            
# "Lower Chatham Strait GKC"   "Mid-Chatham Strait GKC"     "Misc. Golden King Crab"    
# "North Stephens Passage GKC" "Northern GKC"               "Southern GKC"    

### East Central -----
gkc3 %>% filter(Area == "East Central GKC") -> gkc3_east
# need one pot column - prior to 2000 (ft_pots) 2000 and after (pot_effort)
gkc3_east %>% mutate(pots = ifelse(Year >= 2000, pot_effort, 
                                   ifelse(is.na(Year), ft_pots, 0))) -> gkc3_east
