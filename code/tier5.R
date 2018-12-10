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