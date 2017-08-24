#K.Palof
# 3-13-17/ 6-8-17
# Attempt at surplus production models in R using TropFishR package and prod_mod (data, plot = FALSE)
# https://www.rdocumentation.org/packages/TropFishR/versions/1.0.0/topics/prod_mod

# need data set with year, catch (lbs), pot lifts, CPUE
# use data set Andrew compiled that's in excel currently - file name:
# add recent data from fish tickets or log books???

## Load packages ---------------------------------------------------
library(TropFishR) # this overwrites some of the tidyverse commands.  need to load it first? or later?
library(tidyverse)
library(reshape2)
library(extrafont)
library(gridExtra)
library(grid)
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12,base_family='Times New Roman')+
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank()))
#library(TropFishR) # this overwrites some of the tidyverse commands.  need to load it first? or later?


### load data ---------
gkc <- read.csv("./data/gkc_fishticket_all_17.csv")
gkc_log <- read.csv("./data/gkc_logbook_all_030117.csv")
season_year <- read.csv("./data/season_year.csv")
head(gkc)
head(gkc_log)

### select variables -------------
# fish ticket data by year - this is more consistent since seasons
#     are NOT correct in recent (since 2000) years
gkc %>% select(Year = YEAR, Area = I_FISHERY, numbers = NUMBERS, pounds = POUNDS, 
               pots = POTS, Cfec_no = CFEC_NO) -> gkc1
# by season - DON'T USE - use year instead
#gkc %>% select(Season = SEASON, Area = I_FISHERY, numbers = NUMBERS, pounds = POUNDS, 
#               pots = POTS, Cfec_no = CFEC_NO) -> gkc1
head(gkc1)

# need to add season here to match fish tickets
gkc_log %>% select(Year = YEAR, Area = I_FISHERY, no_crab = TARGET_SPECIES_RETAINED, 
                   pots = NUMBER_POTS_LIFTED, Cfec_no = CFEC_NO) -> gkc1_log
#gkc1_log %>% mutate(Season = ifelse (Year == 2000, "Oct1999 - Sep00", 
#                              ifelse(Year == 2001,"Oct2000 - Sep01", 
#                               ifelse(Year == 2002, "Oct2001 - Sep02", 
#                                ifelse(Year == 2003, "Oct2002 - Sep03", 
#                                 ifelse(Year == 2004, "Oct2003 - Sep04", 
#                                  ifelse(Year == 2005, "Oct2004 - Sep05", 
#                                    ifelse(Year == 2006, "Oct2005 - Sep06", 
#                                      ifelse(Year == 2007, "Oct2006 - Sep07", 
#                                       ifelse(Year == 2008, "Oct2007 - Sep08", 
#                                        ifelse(Year == 2009, "Oct2008 - Sep09", 
#                                         ifelse (Year == 2010, "Oct2009 - Sep10", 
#                                          ifelse(Year == 2011, "Oct2010 - Sep11", 
#                                           ifelse(Year == 2012, "Oct2011 - Sep12", 
#                                            ifelse(Year == 2013, "Oct2012 - Sep13", 
#                                             ifelse(Year == 2014, "Oct2013 - Sep14", 
#                                              ifelse(Year == 2015, "Oct2014 - Sep15", 
#                                               ifelse(Year == 2016, "Oct2015 - Sep16", 
#       ifelse(Year == 2017, "Oct2016 - Sep17", ""))))))))))))))))))) -> gkc1_log
head(gkc1_log)
### logbook catch by year -----
gkc1_log %>% group_by(Area, Year) %>% 
  summarise(no_crabs = sum(no_crab, na.rm = TRUE), 
          pot_effort = sum(pots, na.rm = TRUE),
            permits = length(unique(Cfec_no))) -> gkc2_log
# this was done by season.  Don't use. 
#gkc1_log %>% group_by(Area, Season, Year) %>% 
#  summarise(no_crabs = sum(no_crab, na.rm = TRUE), pot_effort = sum(pots, na.rm = TRUE),
#            permits = length(unique(Cfec_no))) -> gkc2_log


### fish ticket catch by year -----------
gkc1 %>% group_by(Area, Year) %>% 
  summarise(number = sum(numbers, na.rm = TRUE), biomass = sum(pounds, na.rm = TRUE), ft_pots = sum(pots, na.rm=TRUE),
              ft_permits = length(unique(Cfec_no))) -> gkc2

#gkc1 %>% group_by(Area, Season) %>% 
#  summarise(number = sum(numbers, na.rm = TRUE), biomass = sum(pounds, na.rm = TRUE), ft_pots = sum(pots, na.rm=TRUE),
#            ft_permits = length(unique(Cfec_no))) -> gkc2
#gkc2 %>% left_join(season_year) -> gkc2

as.data.frame(gkc2 %>% filter(Area == "East Central GKC")) # checks out with what I have in excel surplus production file from 2013

### Area models ----------------
# each area has it's own model - one with fish ticket data and # permits
#           - one with logbook data and # pot lifts.
# merge gkc2 and gkc2_log so have catch in numbers, no_crab, and pounds with pots from both and permits.
gkc2 %>% left_join(gkc2_log) -> gkc3
head(gkc3)

# need one pot column - prior to 2000 (ft_pots) 2000 and after (pot_effort)
gkc3 %>% mutate(pots = ifelse(Year <= 1999, ft_pots, pot_effort)) -> gkc3

unique(gkc3$Area)
# Areas: ""                           "East Central GKC"           "Icy Strait GKC"            
# "Lower Chatham Strait GKC"   "Mid-Chatham Strait GKC"     "Misc. Golden King Crab"    
# "North Stephens Passage GKC" "Northern GKC"               "Southern GKC"    
# create area files for each one of the above areas
### Southern  -----
gkc3 %>% filter(Area == "Southern GKC") -> gkc3_south
write.csv(gkc3_south, './results/south_gkc.csv', row.names = FALSE)

ggplot(gkc3_south, aes(Year, biomass)) +geom_point() +geom_line()
ggplot(gkc3_south, aes(pots, biomass)) +geom_point() 

### Northern   -----
gkc3 %>% filter(Area == "Northern GKC") -> gkc3_north
write.csv(gkc3_north, './results/north_gkc.csv', row.names = FALSE)
### North Stephens Passage   -----
gkc3 %>% filter(Area == "North Stephens Passage GKC") -> gkc3_nsp
write.csv(gkc3_nsp, './results/nsp_gkc.csv', row.names = FALSE)

ggplot(gkc3_nsp, aes(Year, biomass)) +geom_point() +geom_line()
ggplot(gkc3_nsp, aes(pots, biomass)) +geom_point()

### Mid-Chatham Strait  -----
gkc3 %>% filter(Area == "Mid-Chatham Strait GKC") -> gkc3_midc
write.csv(gkc3_midc, './results/midc_gkc.csv', row.names = FALSE)
### Lower Chatham  -----
gkc3 %>% filter(Area == "Lower Chatham Strait GKC") -> gkc3_lowerc
write.csv(gkc3_lowerc, './results/lowerc_gkc.csv', row.names = FALSE)

### Icy Strait  -----
gkc3 %>% filter(Area == "Icy Strait GKC") -> gkc3_icy
write.csv(gkc3_icy, './results/icy_gkc.csv', row.names = FALSE)

### East Central -----
gkc3 %>% filter(Area == "East Central GKC") -> gkc3_east
write.csv(gkc3_east, './results/east_central_gkc.csv', row.names = FALSE)

### prod model equlibrium ----------
# data is dataframe with year (year vector), Y (catch in weight), f (fishing effort),
gkc3_east %>% select(year = Year, Y = biomass, f = pots) %>% filter(year <2017) %>% 
  filter(year >= 1985)->east_input
east_input %>% as.data.frame(east_input) %>% select(-Area)-> east_input1

equil_ec <- prod_mod(east_input1, plot = TRUE)
equil_ec

gkc3_nsp %>% select(year = Year, Y = biomass, f = pots) %>% filter(year <2017) %>% 
  filter(year >= 1985)->nsp_input
nsp_input %>% as.data.frame(nsp_input) %>% select(-Area)-> nsp_input1
equil_nsp <- prod_mod(nsp_input1, plot = TRUE)
equil_nsp

gkc3_icy %>% select(year = Year, Y = biomass, f = pots) %>% filter(year <2017) %>% 
  filter(year >= 1985)->icy_input
icy_input %>% as.data.frame(icy_input) %>% select(-Area)-> icy_input1
icy_input %>% as.data.frame(icy_input) %>% select(-Area) %>% 
  filter(year <=2014) -> icy_input2
equil_icy <- prod_mod(icy_input1, plot = TRUE)
equil_icy <- prod_mod(icy_input2, plot = TRUE)
equil_icy

gkc3_south %>% select(year = Year, Y = biomass, f = pots) %>% filter(year <2017) %>% 
  filter(year >= 1985)->south_input
south_input %>% as.data.frame(south_input) %>% select(-Area)-> south_input1
equil_south <- prod_mod(south_input1, plot = TRUE)
equil_south
### prod model time series ------
# these don't appear to be giving realistic estimates....
prod_mod_ts(east_input1, method = "Schaefer", B0_init = NA, B0_est = NA, effort_unit = 1, 
            plot = TRUE)

prod_mod_ts(east_input1, method = "Fox", B0_init = NA, B0_est = NA, effort_unit = 1, 
            plot = TRUE)

prod_mod_ts(icy_input1, method = "Schaefer", B0_init = NA, B0_est = NA, effort_unit = 1, 
            plot = TRUE)
prod_mod_ts(icy_input2, method = "Schaefer", B0_init = NA, B0_est = NA, effort_unit = 1, 
            plot = TRUE)
#prod_mod_ts(icy_input1, method = "Fox", B0_init = NA, B0_est = NA, effort_unit = 1, 
#            plot = TRUE)

prod_mod_ts(south_input1, method = "Schaefer", B0_init = NA, B0_est = NA, effort_unit = 1, 
            plot = TRUE)
#prod_mod_ts(south_input1, method = "Fox", B0_init = NA, B0_est = NA, effort_unit = 1, 
#            plot = TRUE)


### figures with MSY --------------
## East Central 
gkc3_east %>% filter(Year >= 1985 & Year <2017)->gkc3_east1
east <- ggplot(gkc3_east1, aes(Year, biomass)) +geom_point(size =3) +geom_line()+
  ggtitle("East Central GKC")+ylab("Harvest (lb)") + 
  geom_hline(yintercept = 211000, linetype = "dashed") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(name = "Year", labels = waiver(), breaks = seq(1985, 2020, 5), limits = c(1985, 2020))

## Northern 
gkc3_north %>% filter(Year >= 1985& Year <2017)->gkc3_north1
north <- ggplot(gkc3_north1, aes(Year, biomass)) +geom_point(size =3) +geom_line()+
  ggtitle("Northern GKC")+ylab("Harvest (lb)") + 
  geom_hline(yintercept = 138800, linetype = "dashed") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(name = "Year", labels = waiver(), breaks = seq(1985, 2020, 5), limits = c(1985, 2020))

#icy strait -
#only want 1985 on.
gkc3_icy %>% filter(Year >= 1985 & Year <2017)->gkc3_icy1
icy <- ggplot(gkc3_icy1, aes(Year, biomass)) +geom_point(size=3) +geom_line()+
  ggtitle("Icy Strait GKC")+ylab("Harvest (lb)") + 
  geom_hline(yintercept = 53800, linetype = "dashed")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(name = "Year", labels = waiver(), breaks = seq(1985, 2020, 5), limits = c(1985, 2020))

#north stephens pass 
#- only want 1985 on.-
gkc3_nsp %>% filter(Year >= 1985 & Year <2017)->gkc3_nsp1
nsp <- ggplot(gkc3_nsp1, aes(Year, biomass)) +geom_point(size=3) +geom_line()+
  ggtitle("North Stephens Passage GKC")+ylab("Harvest (lb)") + 
  geom_hline(yintercept = 22800, linetype = "dashed")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(name = "Year", labels = waiver(), breaks = seq(1985, 2020, 5), limits = c(1985, 2020))

# mid-chatham strait 
gkc3_midc %>% filter(Year >= 1985 & Year <2017)->gkc3_midc1
mid <- ggplot(gkc3_midc1, aes(Year, biomass)) +geom_point(size =3) +geom_line()+
  ggtitle("Mid-Chatham Strait GKC")+ylab("Harvest (lb)") + 
  geom_hline(yintercept = 90600, linetype = "dashed") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(name = "Year", labels = waiver(), breaks = seq(1985, 2020, 5), limits = c(1985, 2020))

# lower chatham strait 
gkc3_lowerc %>% filter(Year >= 1985 & Year <2017)->gkc3_lowerc1
lower <- ggplot(gkc3_lowerc1, aes(Year, biomass)) +geom_point(size =3) +geom_line()+
  ggtitle("Lower Chatham Strait GKC")+ylab("Harvest (lb)") + 
  geom_hline(yintercept = 21700, linetype = "dashed") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(name = "Year", labels = waiver(), breaks = seq(1985, 2020, 5), limits = c(1985, 2020))

# southern 
gkc3_south %>% filter(Year >= 1985 & Year <2017)->gkc3_south1
south <- ggplot(gkc3_south1, aes(Year, biomass)) +geom_point(size =3) +geom_line()+
  ggtitle("Southern GKC")+ylab("Harvest (lb)") + 
  geom_hline(yintercept = 22800, linetype = "dashed") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(name = "Year", labels = waiver(), breaks = seq(1985, 2020, 5), limits = c(1985, 2020))

png('./results/figure1.png', res= 300, width = 8, height =8, units = "in")
grid.arrange(east, north, icy, nsp, ncol = 2)
dev.off()
png('./results/figure2.png', res= 300, width = 8, height =8, units = "in")
grid.arrange(mid, lower, south, ncol = 2)
dev.off()

## Figure CPUE vs pot lifts
head(gkc3_east)


### average harvest ------
# exclude 2017 since season was not complete when this data was pulled
gkc3 %>% filter(Year >= 2000 & Year <2017) %>% group_by(Area) %>% 
  summarise(avg_biomass = mean(biomass))


### figures CPUE/ effort --------------
gkc3_east1 %>% mutate(CPUE = biomass/ pots, CPUE_no = number/pots)-> gkc3_east1
east_cpue <- ggplot(gkc3_east1, aes(pots, CPUE)) +geom_point(size =3) +
  #geom_point(aes(pots, CPUE_no), size = 3, color = "red")+
  ggtitle("East Central GKC")+ylab("CPUE (lb/pot lift)") + xlab("pot lifts") + 
  scale_y_continuous(breaks = seq(0,50, 10), limits = c(0,50))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(colour = "black", method = 'lm', se = FALSE, linetype = 'dashed') 
  #geom_smooth(aes(pots, CPUE_no), method = "lm", se = FALSE)

gkc3_north1 %>% mutate(CPUE = biomass/ pots)-> gkc3_north1
north_cpue <- ggplot(gkc3_north1, aes(pots, CPUE)) +geom_point(size =3) +
  ggtitle("Northern GKC")+ylab("CPUE (lb/pot lift)") + 
  xlab("pot lifts")+ 
  scale_y_continuous(breaks = seq(0,50, 10), limits = c(0,50))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(colour = "black", linetype = 'dashed', method = 'lm', se = FALSE)

gkc3_icy1 %>% mutate(CPUE = biomass/ pots)-> gkc3_icy1
icy_cpue <- ggplot(gkc3_icy1, aes(pots, CPUE)) +geom_point(size =3) +
  ggtitle("Icy Strait GKC")+ylab("CPUE (lb/pot lift)") + 
  xlab("pot lifts")+ 
  scale_y_continuous(labels = waiver(), breaks = seq(0,50, 10), limits = c(0,50))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(colour = "black", linetype = 'dashed', method = 'lm', se = FALSE)

gkc3_nsp1 %>% mutate(CPUE = biomass/ pots)-> gkc3_nsp1
nsp_cpue <- ggplot(gkc3_nsp1, aes(pots, CPUE)) +geom_point(size =3) +
  ggtitle("North Stephens Passage GKC")+ylab("CPUE (lb/pot lift)") + 
  xlab("pot lifts")+ 
  scale_y_continuous(labels = waiver(), breaks = seq(0,50, 10), limits = c(0,50))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(colour = "black", linetype = 'dashed', method = 'lm', se = FALSE)

gkc3_midc1 %>% mutate(CPUE = biomass/ pots)-> gkc3_midc1
mid_cpue <- ggplot(gkc3_midc1, aes(pots, CPUE)) +geom_point(size =3) +
  ggtitle("Mid-Chatham Strait GKC")+ylab("CPUE (lb/pot lift)") + 
  xlab("pot lifts")+ 
  scale_y_continuous(labels = waiver(), breaks = seq(0,50, 10), limits = c(0,50))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(colour = "black", linetype = 'dashed', method = 'lm', se = FALSE)

gkc3_lowerc1 %>% mutate(CPUE = biomass/ pots)-> gkc3_lowerc1
lower_cpue <- ggplot(gkc3_lowerc1, aes(pots, CPUE)) +geom_point(size =3) +
  ggtitle("Lower Chatham Strait GKC")+ylab("CPUE (lb/pot lift)") + 
  xlab("pot lifts")+ 
  scale_y_continuous(labels = waiver(), breaks = seq(0,50, 10), limits = c(0,50))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(colour = "black", linetype = 'dashed', method = 'lm', se = FALSE)

gkc3_south1 %>% mutate(CPUE = biomass/ pots)-> gkc3_south1
south_cpue <- ggplot(gkc3_south1, aes(pots, CPUE)) +geom_point(size =3) +
  ggtitle("Southern GKC")+ylab("CPUE (lb/pot lift)") + 
  xlab("pot lifts")+ 
  scale_y_continuous(labels = waiver(), breaks = seq(0,50, 10), limits = c(0,50))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(colour = "black", linetype = 'dashed', method = 'lm', se = FALSE)

png('./results/figure3.png', res= 300, width = 8, height =8, units = "in")
grid.arrange(east_cpue, north_cpue, icy_cpue, nsp_cpue, ncol = 2)
dev.off()
png('./results/figure4.png', res= 300, width = 8, height =8, units = "in")
grid.arrange(mid_cpue, lower_cpue, south_cpue, ncol = 2)
dev.off()

