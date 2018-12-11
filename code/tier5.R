# look at Southeast GKC if it was federally managed 
# fishery only data - Tier 5 

# K.Palof, katie.palof@alaska.gov
# 12-10-18

# load -----
source("./code/helper.R")

# data ----------
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

### summarise catch by year ---------
mngmt_areas <- c("East Central GKC", "Icy Strait GKC", "Lower Chatham Strait GKC", "Mid-Chatham Strait GKC", 
                 "North Stephens Passage GKC", "Northern GKC")

gkc1 %>% 
  group_by(Area, Year) %>% 
  summarise(harvest = sum(pounds), no.crab = sum(numbers)) %>% 
  filter(Area %in% mngmt_areas) -> gkc1_year

# averages ----
gkc1_year %>% 
  group_by(Area) %>% 
  summarise(all.avg = mean(harvest, na.rm = TRUE)) -> all_yrs
gkc1_year %>% 
  filter(Year > 1989 & Year < 2001) %>% 
  group_by(Area) %>% 
  summarise(y90s.avg = mean(harvest, na.rm = TRUE)) -> y90s

all_yrs %>% 
  left_join(y90s) -> avgs

gkc1_year %>% 
  left_join(avgs) -> gkc1_year_avgs

# plots -----
gkc1_year_avgs %>% 
  ggplot(aes(x = Year, y = harvest)) +
  geom_point() +
  facet_wrap(~ Area) +
  geom_hline(aes(yintercept = all.avg), color = "grey25") +
  geom_hline(aes(yintercept = y90s.avg), color = "black", linetype = "dashed")
 


## OFL and ABC -----
avgs %>% 
  mutate(ABC.all = 0.8*all.avg, 
         ABC.90s = 0.8*y90s.avg)
