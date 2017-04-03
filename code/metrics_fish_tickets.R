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
library(extrafont)
library(grid)
options(scipen=9999) # remove scientific notation
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12,base_family='Times New Roman')
          +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))

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

## total harvest by year -------------
fshtkt %>% filter(!is.na(POUNDS))%>% group_by(year) %>% summarise(harvest = sum(POUNDS), effort = sum(POTS)) ->harvest_all
ggplot(harvest_all, aes(year, harvest)) +geom_bar(stat = "identity")+ylab("Harvest, lbs")+
  ggtitle("Southeast Harvest")
ggsave("./figures/regional_harvest.png", width = 4.44, height = 2.77)
# width = 4, height = 2.5, units = "in"
# total harvest by area and year
fshtkt %>% filter(!is.na(POUNDS), !is.na(POTS), Area != "") %>% group_by(Area, year) %>% 
  summarise(harvest = sum(POUNDS)) ->harvest_area
reg_har_area <-ggplot(harvest_area, aes(year, harvest, fill = Area))+geom_bar(stat = "identity") +
  scale_fill_brewer( palette = "Paired") + ggtitle("Harvest by area") +ylab("Harvest, lbs")
#ggsave("./figures/regional_harvest_byarea.png")
# save manuually 800 by 500 
png(file='./figures/regional_harvest_area.png', res=200, width=8, height=6, units ="in")  
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,1)))
vplayout<-function(x,y) viewport (layout.pos.row=x, layout.pos.col=y)
print(reg_har_area,vp=vplayout(1,1:1))
dev.off()




# didn't need this to make graph ------------
spread(harvest_area, Area, harvest) -> harvest_area1
harvest_area1 %>% select(-`Misc. Golden King Crab`) %>% replace(is.na(.), 0) %>% 
  mutate(total = rowSums(.[2:7]))-> harvest_area2
#----------------

### Attempt to calculate pounds per day per boat - All of southeast 
fshtkt00_d %>% filter(!is.na(POUNDS), Area != "") %>% 
  group_by(year, fishery_day) %>% summarise( pounds = sum(POUNDS), boats = n_distinct(ADFG_NO))-> pounds_day

pounds_day %>% mutate(lb_perboat = pounds/boats) ->pounds_day
pounds_day %>% group_by(year) %>% summarise (meanlb = mean(lb_perboat)) -> lb_boat_day1

ggplot(lb_boat_day1, aes(year, meanlb))+geom_point()

## By Area 
fshtkt00_d %>% filter(!is.na(POUNDS), Area != "") %>% 
  group_by(Area, year, fishery_day) %>% summarise( pounds = sum(POUNDS), boats = n_distinct(ADFG_NO)) %>% 
  mutate(lb_perboat = pounds/boats) -> pounds_dayA
pounds_dayA %>% group_by(Area, year) %>% summarise (meanlb = mean(lb_perboat)) -> lb_boat_day1A

ggplot(lb_boat_day1A, aes(year, meanlb))+geom_line() + facet_wrap(~Area, scales = "free_y")+
  ggtitle("Average pounds per boat per day over season")


### pounds per boat day - Adam's method
fshtkt00_d %>% filter(!is.na(POUNDS), Area != "") %>% mutate(lengthS = end_day - start_day) %>% 
  group_by(year) %>% summarise(pounds = sum(POUNDS), boats = n_distinct(ADFG_NO), maxS = max(lengthS)) %>% 
  mutate(lb_perboatday = (pounds/boats)/maxS) -> lb_boat_day2
ggplot(lb_boat_day2, aes(year, lb_perboatday))+geom_point()

## By Area
fshtkt00_d %>% filter(!is.na(POUNDS), Area != "") %>% mutate(lengthS = end_day - start_day) %>% 
  group_by(Area, year) %>% summarise(pounds = sum(POUNDS), boats = n_distinct((ADFG_NO)), 
                                     lengthS = max(lengthS)) %>% 
  mutate(lb_perboat = pounds/boats/lengthS) -> lb_boat_day2B
ggplot(lb_boat_day2B, aes(year, lb_perboat))+ geom_point() + geom_line() +facet_wrap(~Area, scales = "free_y")+
  ggtitle("Pounds per boat per day for the entire season")

# look at 2010 east central data
fshtkt00_d %>% filter (Area == "East Central GKC", year == 2010) ->exam1

### All Southeast -------------------------
fshtkt00_d %>% select(year, Area, fishery_day, POUNDS, POTS, NUMBERS) %>%
  group_by(year, fishery_day) %>% summarise(pounds = sum(POUNDS)) ->allSE

allSE %>% mutate(cumu = cumsum(pounds), Year = as.character(year)) %>% 
  filter(fishery_day < 22) %>% filter(year > 2008) -> allSE_09
allSE_09%>% 
  ggplot(aes(fishery_day, cumu, colour = Year, group = year)) +geom_point() + geom_smooth(method = "lm", fill =NA)+
  ggtitle("All Southeast cumulative harvest")+ylab("Cumulative Harvest, lbs")

# regression for first 21 days - by year - for ALL SOUTHEAST
allSE_09 %>% # 
  group_by(year) %>%
  do(fit = lm(cumu ~ fishery_day, data =.)) -> one

one %>%
  tidy(fit) -> slope_SE

one %>%
  glance(fit) ->out_SE

out_SE %>% 
  select(year, r.squared, p.value) -> out_SE_1

slope_SE %>% filter(term == 'fishery_day') %>% 
  select(year, estimate, std.error) %>% 
  right_join(out_SE_1) -> SE_end  # estimate here is slope from regression

## East Central---------------------------
fshtkt00_d %>% filter (Area == "East Central GKC") %>% select(year, Area, fishery_day, POUNDS, POTS, NUMBERS) %>%
  group_by(year, Area, fishery_day) %>% summarise(pounds = sum(POUNDS)) ->eastC

## cumulative pounds by fishery day by year
eastC %>% mutate(cumu = cumsum(pounds), Year = as.character(year)) %>% filter(fishery_day < 20) %>% filter(year > 2008) -> eastC_1
eastC_1%>% 
  ggplot(aes(fishery_day, cumu, colour = Year, group = year)) +geom_point() + geom_smooth(method = "lm", fill =NA)+
  ggtitle("East Central cumulative harvest")+ylab("Cumulative Harvest, lbs")
  
#scale_color_gradient(low = "blue", high ="red")

# regression for first 20 days - by year
eastC_1 %>% # 
  group_by(year) %>%
  do(fit = lm(cumu ~ fishery_day, data =.)) -> step1

step1 %>%
  tidy(fit) -> step1_slope

step1 %>%
  glance(fit) ->step1_out

step1_out %>% 
  select(year, r.squared, p.value) -> step1_out2

step1_slope %>% filter(term == 'fishery_day') %>% 
  select(year, estimate, std.error) %>% 
  right_join(step1_out2) -> step1_end  # estimate here is slope from regression

# need to summarise these for each year - need slope, p-value, ? R -squared
# Regression for first 20 days with year as variable
fit2 = aov(cumu ~ fishery_day + year, data =eastC_1)
fit3 = aov(cumu ~ fishery_day*year, data = eastC_1)

anova(fit2, fit3) # interaction between fishery_day and year IS SIGNIFICANT






## Graph of fishery day by pounds - THIS IS NOT cumulative pounds - not useful right now
fshtkt00_d %>% 
    filter (Area == "East Central GKC") %>% 
    ggplot(aes(fishery_day, POUNDS)) +geom_point() +facet_wrap(~year)


