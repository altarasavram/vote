# https://elections.wi.gov/elections-voting/results/2016/fall-general
#
# https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml
#
# https://www.kaggle.com/joelwilson/2012-2016-presidential-elections#US_County_Level_Presidential_Results_12-16.csv
#
# Mail in Laws by State
# http://www.ncsl.org/research/elections-and-campaigns/all-mail-elections.aspx
#
# Changes in County Names, FIPS
# https://www.cdc.gov/nchs/nvss/bridged_race/county_geography-_changes2015.pdf
#
# Election Officials Directory By County
# https://www.fvap.gov/search-offices
#
# County level Data in SocViz: 
# https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?src=bkmk
#
#
#install.packages("usmap")
#install.packages("maps")
#install.packages("mapproj")
#install.packages("socviz")
#install.packages("shinydashboard")
#install.packages("RColorBrewer")
#install.packages("ggthemes")
#install.packages("plotly")
#install.packages("gdtools", type = "source")
#devtools::install_github("hrbrmstr/albersusa")
#install.packages("dplyr")
remotes::install_github("hrbrmstr/albersusa", force=T)
devtools::install_github('davidgohel/ggiraph')
library(devtools)
library(ggplot2)
library(dplyr)
library(tidyr)
library(shiny)
library(shinydashboard)
library(usmap)  # plot_usmap
library(maps)
library(mapproj)
library(socviz)
library(RColorBrewer)
library(ggrepel)
library(shiny)
library(openintro)
library(ggthemes)
library(stringr)
library(forcats)
library(sp)
library(sf)
library(ggiraph)
library(albersusa)
library(purrr)
library("DT")

options(scipen = 999)

theme_map <- function() { 
  theme_void() + 
    theme(plot.title    = element_text(size = 10, hjust = 0),
          plot.subtitle = element_text(size = 7,  hjust = 0),
          plot.caption  = element_text(size = 7,  hjust = 0),
          legend.title  = element_text(size = 7,  hjust = 0),
          legend.key.size = unit(0.3, "cm"),
          legend.position ="bottom")
}


cnty_map  <- usmap::us_map(regions = "counties")
state_map <- usmap::us_map(regions = "states")
names (cnty_map)[7] <- "id"

dum_names <- cnty_map$county %>% 
  str_remove_all (paste (c("'", " County", " Borough", " Census Area", " Parish",
                         " City", " City City", " Town", " Village"), 
                       collapse = "|")) 

cnty_map$tooltip<-paste (dum_names, ",",  cnty_map$abbr)
############################################
# COVID-19
#
Get_COVID  <- function (){
  pop  <- read.csv('https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_county_population_usafacts.csv', 
                   sep = "," ,header=T) 
  sick <- read.csv('https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv',         
                   sep = "," ,header=T, check.names = F) 
  dead <- read.csv('https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv',            
                   sep = "," ,header=T, check.names = F)

  all_na <- function(x) any(!is.na(x))
  sick <- sick %>% select_if(all_na)
  dead <- dead %>% select_if(all_na)
  
  last_date <- colnames (dead[dim (dead)[2]])

#  names(dead(dim(dead)[2])) <- "last_date_dead"
  
  pop_cty  <- pop %>%
    arrange (countyFIPS) %>%
    filter (countyFIPS > 1000 & countyFIPS != 6000) %>%  # diamond princess
    mutate (fips_cnty  = sprintf ("%05d", countyFIPS)) %>%
    select (fips_cnty, population)

  pop_state  <- pop %>%
    mutate (state_abbr = State) %>%
    group_by (state_abbr) %>%
    summarise (st_population = sum(population)) %>%
    select (state_abbr, st_population)
    
  dead_cty <- dead %>%
    arrange (countyFIPS) %>%
    filter (countyFIPS > 1000 & countyFIPS  != 6000) %>% 
    mutate (fips_cnty  = sprintf ("%05d", countyFIPS),
            state_abbr = State) %>%
    select (fips_cnty, `County Name`, state_abbr, last_date_dead=last_date)
  
  dead_state <- dead_cty %>%
    group_by (state_abbr) %>%
    summarise (st_last_date_dead = sum (last_date_dead)) %>%
    select (state_abbr, st_last_date_dead)
            
  sick_cty <- sick %>%
    arrange (countyFIPS) %>%
    filter (countyFIPS > 1000 & countyFIPS  != 6000) %>% 
    mutate (fips_cnty  = sprintf ("%05d", countyFIPS),
            state_abbr = State) %>%
    select (fips_cnty, `County Name`, state_abbr, last_date_sick=last_date)
  
  sick_state <- sick_cty %>%
    group_by (state_abbr) %>%
    summarise (st_last_date_sick = sum (last_date_sick)) %>%
    select (state_abbr, st_last_date_sick)
  
   covid_cty <- left_join (dead_cty, pop_cty, by="fips_cnty") %>%
     left_join (sick_cty, by="fips_cnty") %>%
     mutate (per_cap_dead = ifelse(population == 0, 0, last_date_dead/population* 1000000),
             per_cap_sick = ifelse(population == 0, 0, last_date_sick/population* 1000000))

   covid_state <- left_join (dead_state, pop_state, by="state_abbr") %>%
     left_join (sick_state, by="state_abbr") %>%
     mutate (st_per_cap_dead = ifelse(st_population == 0, 0, st_last_date_dead/st_population* 1000000),
             st_per_cap_sick = ifelse(st_population == 0, 0, st_last_date_sick/st_population* 1000000))
   
   covid_cty <- covid_cty %>% select (fips_cnty, per_cap_sick, per_cap_dead, last_date_sick, last_date_dead)
   
   covid_state <- covid_state %>% select (state_abbr, st_per_cap_sick, st_per_cap_dead, st_last_date_sick, st_last_date_dead)
   
   covid <- list("covid_cty" = covid_cty, "covid_state" = covid_state)  
  
   return (covid)
}
#
# Confidence
#
Get_County_Confidence  <- function (){
  conf_cty <- read.csv('~/Documents/RStanFiles/2016_Elections_Performance.csv',header=T) %>%
    arrange (countyfips) %>%
    head(n=10200) %>%
    mutate (fips_cnty  = sprintf ("%05d", countyfips),
            Q4         = as.numeric (Q4),
            Q33        = ifelse (Q33  > 4, NA, Q33)) %>%
    group_by(fips_cnty) %>%
    summarize (avgQ33_cty = 100 - (mean(Q33, na.rm = TRUE) -1)/3*100) 
  
  return (conf_cty)
}
#
# Current VVPAT Density by State
#

Get_VVPAT_Precincts <- function (vote){
  prcnct_all <- vote %>% 
    distinct (fips, .keep_all = TRUE) %>% 
    group_by(state_abbr) %>% 
    summarise(prcnct_all = sum(Precincts))

  prcnct_vvpat <- vote %>% 
    filter (VVPAT == "Yes") %>% 
    distinct (fips, .keep_all = TRUE) %>% 
    group_by(state_abbr) %>%
    summarize (prcncts_vvpat = sum(Precincts))

  prcncts <- left_join (prcnct_all, prcnct_vvpat, by="state_abbr") %>%
    mutate (vvpat_dense = prcncts_vvpat/prcnct_all*100)

  prcncts[is.na(prcncts)] <- 0
return (prcncts)
}
#
# State Balances Data
#
Get_State_Balances  <- function (){

  balances <- read.csv('~/Documents/RStanFiles/balances.csv',header=T)  %>% 
    mutate (state_abbr     = as.character (state_abbr),
            BOP_adj        = ifelse(BOP > 0, (BOP)/max(BOP), -(BOP)/min(BOP)),
            BOP_percap_adj = ifelse(BOP_percap > 0, (BOP_percap)/max(BOP_percap), -(BOP_percap)/min(BOP_percap)),
            Return_adj     = ifelse(Return > 1,(Return-1)/max(Return-1), -(Return-1)/min(Return-1))) %>%
    select (state_abbr, BOP, BOP_adj, BOP_percap, BOP_percap_adj, Return, Return_adj)

return (balances)
}

#
# Function: Make Alaska Data 
#
make_alaska_vote <- function (raw_vote, only_county_data){
  
  test <- raw_vote
  
#  raw_alaska <- subset (raw_vote, state_abbr == "AK")
#  alaska <- c(alaska[1,21], alaska[2,21], alaska[3,21])
#  alaska[1,] <- do.call("rbind", replicate(num, test[1,], simplify = FALSE)) 
  
  alaska_fips <- only_county_data %>% filter (state_abbr=="AK") %>% select (fips_cnty)
  num <- dim(alaska_fips)[1] 
  
  bb <- only_county_data %>% filter (state_abbr=="AK")
  
  dum1 <- test[1, ]
  alaska1.df <- do.call("rbind", replicate(num, dum1, simplify = FALSE))  
  alaska1.df$fips <- as.numeric(bb$fips_cnty)*100000
  alaska1.df$fips_cnty <- bb$fips_cnty
  
  alaska1.df$Precincts <- ifelse (floor (bb$pop * 441/736732) > 0, floor (bb$pop * 441/736732), 1)
  alaska1.df$Precincts[29] <- 441 - sum (alaska1.df$Precincts)
  
  alaska1.df$Jurisdiction <- bb$name  
  alaska1.df$Total.Registration <- floor (bb$pop * 569903/736732)
  
  dum2 <- test[2, ]
  alaska2.df <- do.call("rbind", replicate(29, dum2, simplify = FALSE))  
  alaska2.df$fips <- as.numeric(bb$fips_cnty)*100000
  alaska2.df$fips_cnty <- bb$fips_cnty
  
  alaska2.df$Precincts <- ifelse (floor (bb$pop * 441/736732) > 0, floor (bb$pop * 441/736732), 1)
  alaska2.df$Precincts[29] <- 441 - sum (alaska2.df$Precincts)
  
  alaska2.df$Jurisdiction <- bb$name  
  alaska2.df$Total.Registration <- floor (bb$pop * 569903/736732)
  
  dum3 <- test[3, ]
  alaska3.df <- do.call("rbind", replicate(29, dum3, simplify = FALSE))  
  alaska3.df$fips <- as.numeric(bb$fips_cnty)*100000
  alaska3.df$fips_cnty <- bb$fips_cnty
  
  alaska3.df$Precincts <- ifelse (floor (bb$pop * 441/736732) > 0, floor (bb$pop * 441/736732), 1)
  alaska3.df$Precincts[29] <- 441 - sum (alaska3.df$Precincts)
  
  alaska3.df$Jurisdiction <- bb$name  
  alaska3.df$Total.Registration <- floor (bb$pop * 569903/736732)
  
  jj <- rbind (test, alaska1.df, alaska2.df, alaska3.df)
  
  rownames(jj) <- seq(length=nrow(jj)) 
  
  cc <- subset (jj, fips != 200000000)
  
  return (cc)
}
#
#
ALL_fips <- c("06013", "06059", "08031", "12009", "12061", "12085", "18019",
              "18065", "18097", "18127", "24031", "26077", "26125", "37129", 
              "35001", "39035", "39049", "39061", "44001", "44003", "44005", 
              "44007", "44009", "48225", "48453", "49057", "51059", "51540", 
              "53053", "54043", "54075", "55003", "55025", "55037", "55041", 
              "55059", "55079", "55133", "54043", "54075")

vvpat_legis_lab <- c("Yes", "Some", "Maybe", "No")

eqp_vvpat_lab   <- c("Paper Ballot", "Paper+DRE, VVPAT", "Paper+DRE, Some VVPAT",
                     "Paper+DRE, No VVPAT", "DRE, Some VVPAT", "DRE, No VVPAT", "Vote By Mail")
#
var_cty <-   c("pop_dens","VVPAT", "Accessible.Use","Early.Voting",      
               "Absentee.Ballots", "Polling.Place", "Make", "dum", "Equipment.Type", "dum",
               "per_dem_2016", "per_gop_2016", "turnout16", 
               "partywinner16", "flipped", "avgQ33_cty",
               "selected.Counties", "selected.Counties", 
               "last_date_sick", "last_date_dead", 
               "per_cap_sick", "per_cap_dead")

var_county<- c("Demographics", "VVPAT Generation", "Accessible Use","Early Voting",      
               "Absentee Ballots", "Polling Place", "Vendor", "Vendor Prevalence",
               "Equipment Type", "Equipment Prevalence",
               "Dem Vote% in 2016", "GOP Vote% in 2016", "Voter Turnout% in 2016",
               "Winner 2016", "Flipped Counties in 2016", "Voter Confidence",
               "Awarded By EAC", "Existing Relationship", 
               "COVID Cases", "COVID Deaths", 
               "COVID Cases (Per Million)", "COVID Deaths (Per Million)")
#
var_st    <- c("audit", "VVPATL", "equip_vvpat_name", 
               "vvpat_dense", "avgQ33_st","pct_ctzn_voted_st_16",
               "BOP_adj", "BOP_percap_adj", "Return_adj",
               "st_last_date_sick", "st_last_date_dead", 
               "st_per_cap_sick", "st_per_cap_dead")
var_state <- c("Audit Legislation", "VVPAT Legislation", "Equipment&VVPAT Legis.", 
               "Current VVPAT Density", "Voter Confidence", "Voter Participation '16",
               "Balance Of Payments", "Per Capita BOP", "Return On Investment",
               "COVID Cases", "COVID Deaths", 
               "COVID Cases (Per Million)", "COVID Deaths (Per Million)")
#
vendor_types <- c("Election Systems & Software", "Premier/Diebold (Dominion)", 
                  "Dominion Voting Systems", "Hart InterCivic", "Unisyn Voting Solutions", 
                  "Sequoia (Dominion)", "Microvote", "Avante")

eq_types <- c("Optical Scan", "Ballot Marking Device",    
              "DRE-Touchscreen", "Hand Counted Paper Ballots",        
              "DRE-Dial","DRE-Push Button", 
              "Ballot Marking Device/Optical Scan", "DRE")  
#
#Election Administration "Finalists"
#
EAC_counties = read.table(textConnection(
  "name                 fips_cnty on  short_name
'Oakland County       MI' 26125    1 'Oakland,MI'
'Weber County         UT' 49057    1 'Weber,UT'
'Denver County        CO' 08031    2 'Denver,CO'
'Indian River County  FL' 12061    2 'Indian River,FL'
'Pierce County        WA' 53053    2 'Pierce,WA'
'Brevard County       FL' 12009    2 'Brevard,FL'
'Cuyahoga County      OH' 39035    1 'Cuyahoga,OH'
'Montgomery County    MD' 24031    1 'Montgomery,MD'
'Bernalillo County    NM' 35001    1 'Bernalillo,NM'
'Travis County        TX' 48453    2 'Travis,TX'
'Franklin County      OH' 39049    1 'Franklin,OH'
'Hamilton County      OH' 39061    1 'Hamilton,OH'
'New Hanover County   NC' 37129    1 'New Hanover,NC'
'Contra Costa County  CA' 06013    1 'Contra Costa,CA'
'Martin County        FL' 12085    2 Martin,FL'
"),
  colClasses=c('character', 'character', 'factor', 'character'), header=TRUE)
#
# Source: National Conference of State Legislatures
#

audit_type <- read.table(textConnection(
 "state_abbr audit
  AL  'g)No Audit' 
  AK  'e)Non-RL Audit' 
  AZ  'e)Non-RL Audit'
  AR  'g)No Audit'
  CA  'b)Non-RLA, Opt. In 2020'
  CO  a)RLA
  CT  'e)Non-RL Audit'
  DE  'g)No Audit'
  DC  'e)Non-RL Audit'
  FL  'e)Non-RL Audit'
  GA  'g)No Audit'
  HI  'e)Non-RL Audit'
  ID  f)Other
  IL  'e)Non-RL Audit'
  IN  f)Other
  IA  'e)Non-RL Audit'
  KS  'e)Non-RL Audit'
  KY  'e)Non-RL Audit'
  LA  'g)No Audit'
  ME  'g)No Audit'
  MD  'e)Non-RL Audit'
  MA  'e)Non-RL Audit'
  MI  'd)Non-RL-Vote+Process Audit'
  MN  'e)Non-RL Audit'
  MS  'g)No Audit'
  MO  'e)Non-RL Audit'
  MT  'e)Non-RL Audit'
  NE  f)Other
  NV  'e)Non-RL Audit'
  NH  'g)No Audit'
  NJ  'e)Non-RL Audit'
  NM  'd)Non-RL-Vote+Process Audit'
  NY  'e)Non-RL Audit'
  NC  'e)Non-RL Audit'
  ND  f)Other
  OH  'c)Non-RLA w/RLA Recomm-d.' 
  OK  'g)No Audit'
  OR  'd)Non-RL-Vote+Process Audit' 
  PA  'e)Non-RL Audit'
  RI  a)RLA
  SC  f)Other
  SD  'g)No Audit'
  TN  'e)Non-RL Audit'
  TX  'e)Non-RL Audit'
  UT  'e)Non-RL Audit' 
  VT  'e)Non-RL Audit'
  VA  a)RLA
  WA  'c)Non-RLA w/RLA Option' 
  WV  'e)Non-RL Audit'
  WI  'e)Non-RL Audit' 
  WY  f)Other"),
 colClasses=c('character', 'factor'), header=TRUE)

audit_type <- audit_type %>%
  mutate(region   = tolower(abbr2state(state_abbr)),
         audit_sh = abbreviate(substring(audit, 3)))
#
# State Maps
#
usa_sf <-st_as_sf(usa_composite("laea"))  %>% 
  mutate(
    CENTROID = purrr::map(geometry, st_centroid),
    COORDS =   purrr::map(CENTROID, st_coordinates),
    COORDS_X = purrr::map_dbl(COORDS, 1),
    COORDS_Y = purrr::map_dbl(COORDS, 2)
  ) %>%
  as_tibble() %>%
  st_as_sf()

usa_sf$nudge_x <- 0
usa_sf$nudge_y <- 0

x_range <- abs(Reduce("-", range(usa_sf$COORDS_X)))
y_range <- abs(Reduce("-", range(usa_sf$COORDS_Y)))

ix <- usa_sf$name %in% c("New Hampshire", "Vermont", "Massachusetts")
usa_sf$nudge_x[ix] <- -1 * 0.15 * x_range
usa_sf$nudge_y[ix] <- 1 * 0.15 * y_range

ix <- usa_sf$name %in% c(
  "Massachusetts",
  "Rhode Island", "Connecticut", "New Jersey", "West Virginia",
  "Maryland", "Delaware", "District of Columbia"
)
usa_sf$nudge_x[ix] <- 1 * 0.2 * x_range
usa_sf$nudge_y[ix] <- -1 * 0.15 * y_range

names(usa_sf)[6] <- "state_abbr"
#
# Read State Voting Data 2016
#
# https://www.census.gov/data/tables/time-series/demo/voting-and-registration/p20-580.html   
# Table 4-b
#
participation <- read.csv('~/Documents/RStanFiles/by_state_16.csv',header=T)
participation <- participation[7:nrow(participation), ]

participation <- participation %>%
  transmute (state                = s,
             state_abbr           = state2abbr(state),
             pct_ctzn_voted_st_16 = as.numeric(gsub(',', '', X.10)),
             total_voted_16       = as.numeric(gsub(',', '', X.7 )))
#
# Read Countywise Data, Massage
# https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2017/cc-est2017-alldata.pdf
#
#
# YEAR=9:  2016
# AGEGRP=0 : All
# 1=Age 0 to 4 years 
# 2=Age 5 to 9 years
# 3=Age 10 to14 years 
# 4=Age 15 to19 years
#
demog <- read.csv('~/Documents/RStanFiles/demog-est2016-alldata.csv',header=T)

demog <- demog %>% 
  mutate (fips_cnty  = as.character(sprintf ("%05s", demog$fips_cnty)),
          state_abbr = as.character(state_abbr)) %>% 
  select (-X)

#######  COUNTY DATA ##############
#
# Masssage County Data, eliminate States, leave only counties, Attach Voting Age Population
# Clean up changed counties (Names, FIPS) in 2015
# Delete Bedford City - Changed to Benford County, which is already contained
#
# Apportion to each county in AK: total_votes_2016 by population weight
# Fix Riverside County Data
#
only_county_data <- county_data %>% 
  subset (id != "51515") %>%
  filter (as.numeric(id)/1000 != floor(as.numeric(id)/1000)) %>%
  mutate (name = ifelse (id == "02270", "Kusilvak Census Area", name) ,       
          id   = ifelse (id == "02270", "02158", id),
          name = ifelse (id == "46113", "Oglala Lakota County", name) ,       
          id   = ifelse (id == "46113", "46102", id),
          total_votes_2016 = ifelse (state =="AK", 
                                     floor(as.numeric(pop)*246588.0/736732.0),
                                     total_votes_2016),
          fips_cnty   = id,
          state_abbr  = as.character(state), 
          fips_as_num = fips,
          selected.Counties = ifelse (fips_cnty %in% ALL_fips,               4, 5),
          selected.Counties = ifelse (fips_cnty %in% EAC_counties$fips_cnty, 1,
                                      selected.Counties)) %>%
  select (-id, -state, -fips) 

idx <- which (only_county_data$name == "Riverside County")
only_county_data$per_dem_2012[idx]   <- 0.497
only_county_data$per_gop_2012[idx]   <- 0.481
only_county_data$diff_2012[idx]      <- 14717
only_county_data$partywinner12[idx]  <- "Democrat"
only_county_data$winner12[idx]       <- "Obama"
only_county_data$flipped[idx]        <- "No" 
###########################
#
# Add DC
#
state_name <- state.name
state_abbr  <- state.abb

state_name [length(state_name)+1] <- "District of Columbia"
state_abbr [length(state_abbr)+1] <- "DC"
#
# Create DataFrame for VVPAT and Equipment Type By State
#
x <- cbind (data.frame(state_name, state_abbr))
# VVPAT Legislation Status By State
x$VVPATL <- case_when(
  x$state_abbr %in% c("ND","WY","OK","LA","MS","AL","KY","RI","DE","DC","NE") ~ 2,
  x$state_abbr %in% c("AR") ~ 3,
  x$state_abbr %in% c("GA","IN","KS","MA","SC","TX","PA","VA") ~ 4,
  TRUE ~ 1)
# Equipment Type By State
x$equip <- case_when(
  x$state_abbr %in% c("OH","WV","NC","WI","IL","MO","AR","WY","ID","CA","NV","UT","AZ","AK", "HI") ~ 2, 
  x$state_abbr %in% c("KS","MS") ~ 3,
  x$state_abbr %in% c("TX","IN","KY","TN","FL","PA","OK") ~ 4, 
  x$state_abbr %in% c("NJ") ~ 5,
  x$state_abbr %in% c("SC","GA","LA","DE") ~ 6,      
  x$state_abbr %in% c("WA","OR","CO") ~ 7,
  TRUE ~ 1)

x <- x %>%
  mutate (vvpat_legis_name    = vvpat_legis_lab[VVPATL],
          equip_vvpat_name    = eqp_vvpat_lab[equip],
          equip_vvpat_name_sh = abbreviate(equip_vvpat_name),
          state_abbr          = as.character(state_abbr),
          region              = tolower (state_name),
          VVPATL              = as.factor(VVPATL),
          equip               = as.factor(equip))
#
# Read VerifiedVoting data
# 
raw_vote <- read.csv('~/Documents/RStanFiles/verifier-search.csv',header=T) %>%
  data.frame()

num  <- length (raw_vote$State)
raw_vote <- raw_vote[-num, ]
#
# Make County fips, by Jusrisdictin (County): truncate last 4 digits, add a leading zero 
#
# Clean up changed counties (Names, FIPS) in 2015
# Concatenate Make "Hart InterCivic" and "Hart Intercivic", "Microvote and MicroVote
# Make short Names
# Make by county Alaska vote
#
raw_vote <- raw_vote %>% 
  mutate (fips  = FIPS.code,
          state = State,
          fips_cnty  = floor(fips/100000),
          fips_cnty  = ifelse(fips_cnty < 10000, paste0("0", fips_cnty), fips_cnty),
          state_abbr = state2abbr (as.character(state)),
          fips_cnty  = ifelse (fips_cnty == "46113", "46102", fips_cnty),
          Make       = ifelse (Make=="Hart Intercivic", 
                         as.character("Hart InterCivic"), as.character(Make)),
          Make       = ifelse (Make=="MicroVote", 
                         as.character("Microvote"), as.character(Make)),
          Make_sh    = abbreviate(Make),
          Model_sh   = abbreviate(Model),
          Equipment.Type_sh = abbreviate(Equipment.Type)) %>%
  select (fips_cnty, everything()) %>%
  make_alaska_vote (only_county_data)
#
# Infer County-wise Voter Participation 
# a) Use Demographic Data to Estimate > 18 year-old population
# b) Use VotingVerified Registration Data
#
regist_cty <- raw_vote %>% 
  group_by (fips_cnty) %>% 
  distinct (fips, .keep_all = TRUE) %>% 
  summarize (regist_pop16 = sum (Total.Registration))
regist_st <- raw_vote %>% 
  group_by (state_abbr) %>% 
  distinct (fips, .keep_all = TRUE) %>% 
  summarize (total_regist_st  = sum (Total.Registration))

voting_age_cty <- demog %>% 
  group_by (fips_cnty) %>%
  summarize (state_abbr       = first (state_abbr),
             VAP_cty16 = first(TOT_POP) - (sum (TOT_POP) - last(TOT_POP)*.4 - first (TOT_POP)),
             tot_pop_16       = first (TOT_POP))
voting_age_st <- voting_age_cty %>% 
  group_by (state_abbr) %>% 
summarise(voting_age_st = sum (VAP_cty16))

votes_st <-  only_county_data %>% 
  group_by (state_abbr) %>% 
  summarize (total_votes_st  = sum (total_votes_2016, na.rm = TRUE)) %>% 
  left_join (regist_st,     by="state_abbr") %>% 
  left_join (voting_age_st, by="state_abbr") %>%
  mutate (rg_partic = 100 * total_votes_st/total_regist_st,
          va_partic = 100 * total_votes_st/voting_age_st)  

participation <- participation %>%
  left_join (votes_st, by = "state_abbr") %>%
  mutate (rg_error_st = (pct_ctzn_voted_st_16 - rg_partic)/100,
          va_error_st = (pct_ctzn_voted_st_16 - va_partic)/100) 

participation <- participation %>% 
  select (state_abbr, pct_ctzn_voted_st_16, va_partic, va_error_st, rg_partic, rg_error_st)

voting_age_cty <- voting_age_cty %>% 
  select (fips_cnty, state_abbr, VAP_cty16)
#
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ
#
pres <- read.csv('~/Documents/RStanFiles/countypres_2000-2016-3.csv',header=T)
# Fix Oglala Lakota: FIPS 46113 -> 46102
pres$FIPS <- ifelse (pres$FIPS == "46113", "46102", pres$FIPS)

pres_cty <- pres %>% 
  filter (year== "2016") %>% 
  mutate (fips_cnty = sprintf ("%05s", FIPS)) %>%
  group_by (FIPS) %>% 
  summarise (county_votes = sum (candidatevotes),
             fips_cnty    = first(fips_cnty),
             state_abbr   = as.character(first(state_po)))

pres_st <- pres_cty %>% 
  group_by (state_abbr) %>%
  summarise(state_votes = sum (county_votes, na.rm = TRUE)) %>% 
  left_join(voting_age_st, by="state_abbr") %>% 
  mutate (pr_partic = 100 * state_votes/voting_age_st) 

pres_USA <- sum (pres_st$state_votes, na.rm = TRUE)
#
# http://www.electproject.org/home/voter-turnout/faq
# Read Eligible Trunout
# https://docs.google.com/spreadsheets/d/1VAcF0eJ06y_8T4o2gvIL4YcyQy8pxb1zYkgXF76Uu1s/edit#gid=2030096602
#
# VP  - Voting Popultion
# VAP - Voting Age Population
# VEP - Voting Eligible Population
#
VP_st <- read.csv('~/Documents/RStanFiles/Eligible_Turnout16.csv', header=T) 
VP_st <- VP_st[-1,]

VP_st <- VP_st %>% 
  mutate (state_abbr    = as.character(State.Abv),
          VP_votes_cast = as.numeric(gsub(',', '', Highest.Office)),
          VEP_st        = as.numeric(gsub(',', '', Voting.Eligible.Population..VEP.)),
          VAP_st        = as.numeric(gsub(',', '', Voting.Age.Population..VAP.)),
          VEP_partic    = VP_votes_cast/VEP_st *100,
          VEP_to_VAP    = VEP_st/VAP_st) %>% 
  select (state_abbr, VAP_st, VP_votes_cast, VEP_st,  VEP_partic, VEP_to_VAP) 

VEP_cty <- left_join(VP_st, voting_age_cty, by="state_abbr") %>%
  mutate (VEP_cty_p = round(VAP_cty16*VEP_to_VAP)) 


VEP_cty <-  VEP_cty %>%  
  left_join (pres_cty, by=c("fips_cnty", "state_abbr")) %>%
  mutate (VEP_partic_cty = county_votes/VEP_cty_p*100) %>% 
  select (fips_cnty, county_votes, VEP_cty_p, VEP_partic_cty)

voting_age_cty <- voting_age_cty %>% select (-state_abbr)
##########
# Population Weigh County Confidence -> State Confidence
conf_cty <- Get_County_Confidence ()  

conf_cty <- conf_cty %>% filter (is.na(avgQ33_cty) !=TRUE)

conf_cty <- conf_cty %>%
  left_join(only_county_data, by="fips_cnty") %>%
  mutate (pop_times_Q33 = pop * avgQ33_cty) %>%
  select (fips_cnty, state_abbr, avgQ33_cty, pop, pop_times_Q33)

conf_st <- conf_cty %>% group_by(state_abbr) %>%
  summarize (state_pop = sum (pop),
             avgQ33_st = sum (pop_times_Q33)/state_pop) 

conf_cty <- conf_cty %>%
  select (-state_abbr, -pop, -pop_times_Q33)
##############  COVID data
covid <- Get_COVID ()
covid_cty <- covid$covid_cty
covid_state <- covid$covid_state
##########################
# county_votes from pres: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ
# total_votes_2016 from county_data:  SocViz
# county_votes     for Humboldt County, CA = 59246
# total_votes_2016 for Humboldt County, CA = 33636
# Humbold County   for Humboldt County, CA = 
# https://humboldtgov.org/DocumentCenter/View/56808/election-night-final-report?bidId=  : 35223
# https://humboldtgov.org/DocumentCenter/View/57024/final-cumulative?bidId=   (12/6/16) : 60983
#
only_county_data <- only_county_data %>% 
  left_join (participation,  by="state_abbr") %>% 
  left_join (regist_cty,     by="fips_cnty")  %>%
  left_join (voting_age_cty, by="fips_cnty")  %>% 
  left_join (VEP_cty,        by="fips_cnty")  %>%
  left_join (conf_cty,       by="fips_cnty")  %>%
  left_join (covid_cty,      by="fips_cnty")  

only_county_data <- only_county_data %>% 
  mutate (turnout16     = avgQ33_cty/VEP_cty_p, 
          turnout16     = ifelse (is.na(turnout16), total_votes_2016/VEP_cty_p, turnout16),
          turnout16     = ifelse (turnout16 >1.0, 1.0, turnout16),
          pop_dens      = pop/land_area,
#          turnout16     = total_votes_2016/VAP_cty16 + va_error_st,
#          turnout16_fac = total_votes_2016/VAP_cty16 + va_error_st,
#          turnout16     = total_votes_2016/regist_pop16 + rg_error_st,
#          turnout16_fac = total_votes_2016/regist_pop16 + rg_error_st,
          turnout16_fac = turnout16
)

error <- only_county_data %>% 
  select (state_abbr, fips_cnty, name, county_votes, VEP_cty, VEP_partic_cty, total_votes_2016, turnout16) %>% 
  mutate (error = (1-total_votes_2016/county_votes)*100 )
#
# Concat County Data from Socviz to Verified Voting Data
#
vote <- raw_vote %>% left_join (only_county_data, by= c("fips_cnty", "state_abbr")) %>%
  mutate (state_abbr = ifelse (is.na(state_abbr), 
                               state2abbr(as.character(state)), 
                               as.character(state_abbr))) %>%
  select (-FIPS.code) %>%
  select (fips_cnty, everything())
#
# State: VVPPAT Density,  Balances 
#
prcncts  <- Get_VVPAT_Precincts (vote)
balances <- Get_State_Balances ()
#
# Statewise all data
#
us_states_table <- audit_type %>%
  left_join(x,             by="state_abbr") %>% 
  left_join(prcncts,       by="state_abbr") %>%
  left_join(participation, by="state_abbr") %>%
  left_join(conf_st,       by="state_abbr") %>% 
  left_join(balances,      by="state_abbr") %>%
  left_join(covid_state,   by="state_abbr") 

us_states_data <- usa_sf %>% 
  left_join(us_states_table,    by="state_abbr") 

us_states_table <- us_states_table %>%  
  transmute (State = state_name,
             "Population (ppl)"           = state_pop,
#             "Audit Legislation"          = substring (audit, 3),
#             "VVPAT Legislation"          = vvpat_legis_name,
#             "Equipment & VVPAT Category" = equip_vvpat_name,
#             "Turnout16(%) Votes/VEP"     = pct_ctzn_voted_st_16,
#             "Confidence(%)"              = round(avgQ33_st),
             "COVID Cases"               = st_last_date_sick,
             "COVID Cases (Per Million)" = round(st_per_cap_sick ),
             "COVID Deaths"               = st_last_date_dead,
             "COVID Deaths (Per Million)" = round(st_per_cap_dead )) %>%
  
  datatable(options = list(
    scrollY = '500px', scrollX=TRUE, paging = FALSE, 
    columnDefs = list(list(className = 'dt-center', targets = c(1))), # centered column indices
    pageLength = 10,
    lengthMenu = c(5, 10, 15))) %>% 
  formatCurrency(currency = "", digits=0, c('Population (ppl)' )) %>%
  formatCurrency(currency = "", digits=0, c('COVID Cases (Per Million)' ))  %>%
  formatCurrency(currency = "", digits=0, c('COVID Cases' )) %>%
  formatCurrency(currency = "", digits=0, c('COVID Deaths (Per Million)' ))  %>%
  formatCurrency(currency = "", digits=0, c('COVID Deaths' )) 
#
# Read conty level turnout data 
#
county_params <- read.csv('~/Documents/RStanFiles/county_params.csv', header=T) %>%
  transmute (fips_cnty = sprintf ("%05d", fips_cnty),
             dummy     = turnout16)
only_county_data <- left_join(only_county_data, county_params, by="fips_cnty")

only_county_table <- only_county_data %>%
  transmute (County                 = name,
             State                  = state_abbr,
             "Population (ppl)"     = pop, 
             "Area (sq-mi)"         = land_area,
             "Pop Dens (ppl/sq-mi)" = pop/land_area, 
#             "Awarded By EAC"       = ifelse (selected.Counties == 1|selected.Counties == 2, 
#                                              "EAC Awarded", "No"),
#             "Selected Counties"    = ifelse (selected.Counties == 1|selected.Counties == 4, 
#                                              "Selected", "No"),
             Winner16               = ifelse (partywinner16 == "Republican", "GOP", "Dem"),         
             "Black(%)"             = black,
#             Flipped.16             = case_when (
#                  flipped == "Yes" & Winner16 == "Dem" ~ "Flipped Dem",
#                  flipped == "Yes" & Winner16 == "GOP" ~ "Flipped GOP",
#                  TRUE ~ "No"),
#             "Turnout(%) Votes/VEP" = round(100 * dummy),
#             "Confidence(%)"        = round(avgQ33_cty),
             "COVID Cases"          = last_date_sick,
             "COVID Cases/(mm)"     = round(per_cap_sick),
             "COVID Deaths"         = last_date_dead,
             "COVID Deaths/(mm)"    = round(per_cap_dead),
             FIPS                   = fips_cnty) %>%
  datatable(options = list(
    scrollY = '500px', scrollX=TRUE, paging = FALSE, 
    columnDefs = list(list(className = 'dt-center', targets = c(1, 2, 6, 7, 10))),
    pageLength = 10,
    lengthMenu = c(5, 10, 15))) %>% 
  formatCurrency(currency = "", digits=0, c('Population (ppl)'     )) %>%
  formatCurrency(currency = "", digits=1, c('Area (sq-mi)'         )) %>%
  formatCurrency(currency = "", digits=1, c('Pop Dens (ppl/sq-mi)' )) %>%
  formatCurrency(currency = "", digits=0, c('COVID Cases'          )) %>%
  formatCurrency(currency = "", digits=0, c('COVID Cases/(mm)'     )) %>%
  formatCurrency(currency = "", digits=0, c('COVID Deaths'         )) %>%
  formatCurrency(currency = "", digits=0, c('COVID Deaths/(mm)'    )) 

######################
# Division Level
#
jj <- vote %>% filter(state_abbr == "RI") %>% 
  left_join(us_states_data, by= c("state_abbr", "pct_ctzn_voted_st_16" )) %>%
#  group_by (fips_cnty, Jurisdiction) %>% 
  distinct (fips, .keep_all = TRUE) 

kkk <- vote %>% filter (state_abbr == "RI") %>% 
  group_by (fips) %>% 
  distinct (Equipment.Type)

ggg <- kkk %>% group_by(fips) %>% 
  summarise(eq = as.character (first (Equipment.Type)),
            n_eq = n_distinct(Equipment.Type))

k <- 1
for (i in 1:dim (ggg)[1]){
  for (j in 2:ggg$n_eq[i]){
    k <- k+1
    ggg$eq[i] <- paste (ggg$eq[i], ",",kkk$Equipment.Type[k]) 
  }
  k <- k+1
}

jj$eq <- ggg$eq

jj$county <- c(NA)
for (i in 1:dim(jj)[1]){
  jj$county[i] <- (fips_info (jj$fips_cnty[i]))$county
}
jj$county <- str_remove_all (jj$county, "County")

#https://www.nytimes.com/elections/2016/results/rhode-island
RI_towns <- read.csv('~/Documents/RStanFiles/RI_Towns.csv',stringsAsFactors=FALSE, header=T)
colnames (RI_towns)[1] <- "Division"
colnames (RI_towns)[2] <- "VClinton"
colnames (RI_towns)[3] <- "VTrump"

RI_towns$VClinton <- as.numeric (str_remove_all (RI_towns$VClinton, ","))
RI_towns$VTrump   <- as.numeric (str_remove_all (RI_towns$VTrump, ","))

RI_towns$Town_Winner12 <- ifelse (RI_towns$Division %in% c("East Greenwich", "West Greenwich", "Scituate"), 
                                  "GOP", "Dem")
RI_towns$Town_Winner16 <- ifelse (RI_towns$VClinton > RI_towns$VTrump, "Dem", "GOP")
RI_towns$Town_Flipped16 <- ifelse (RI_towns$Town_Winner12 == RI_towns$Town_Winner16, "No", "Yes")

jj <- left_join(jj, RI_towns, by="Division")

jj %>% select (fips_cnty, state_abbr, county, Jurisdiction, Division, Precincts, 
                VClinton, VTrump, Town_Winner16,
                Make_sh, Total.Registration, eq, pop_dens, partywinner16, flipped) 

jj <- jj %>% 
 transmute(State            = state_abbr,
           County           = county,
           Jurisdiction     = Jurisdiction,
           Division         = Division,
           Precincts        = formatC(Precincts, format="f", big.mark = ",", digits=0),
           Registration     = formatC(Total.Registration, format="f", big.mark = ",", digits=0),
           Clinton16        = VClinton,
           Trump16          = VTrump,
           Div.Winner.16    = Town_Winner16,
           Div.Flipped      = Town_Flipped16,
           Make             = Make_sh,
           Equipment.Type   = eq,
           EAC.Award.Winner = ifelse (selected.Counties == 1|selected.Counties == 2, "Yes", "No"),
           County.Population= formatC(pop, format="f", big.mark = ",", digits=0),
           County.Pop.Dens  = formatC(pop/land_area, format="f", digits = 1),   
           County.Winner16  = partywinner16,    
           County.Flipped   = flipped, 
           County.Turnout   = formatC(100 * turnout16,    format="f", digits = 1),
           State.Turnout    = pct_ctzn_voted_st_16,
           County.Confidence= round(avgQ33_cty),
           State.Confidence = round(avgQ33_st),
           Relationship     = "  ",
           Prohibiting.Laws = "  ",
           Committed.Lead   = "  ",
           E2E.Candidate    = "  ",
           RLA.Candidate    = "  ",
           Audit.Type       = substring (audit, 3),
           VVPAT.Legislation= vvpat_legis_name,
           Equip.VVPAT.Cat  = equip_vvpat_name,
           FIPS             = fips_cnty)
jj

#
# County Level
#
jj <- vote %>% filter(state_abbr == "RI") %>% 
  group_by (fips_cnty, Jurisdiction) %>% 
  distinct (fips, .keep_all = TRUE) # %>% 
  summarize (precs = sum (Precincts), 
             name = first(name)) %>% 
  summarize (num_precincts = sum (precs),
             name  = first(name))

kk <- vote %>% filter (state_abbr == "RI") %>% 
  group_by (fips_cnty) %>% 
  distinct (Equipment.Type)

gg <- kk %>% group_by(fips_county) %>% 
  summarise(eq = as.character (first (Equipment.Type)),
            n_eq = n_distinct(Equipment.Type))

k <- 1
for (i in 1:dim (gg)[1]){
  for (j in 2:gg$n_eq[i]){
    k <- k+1
    gg$eq[i] <- paste (gg$eq[i], ",",kk$Equipment.Type[k]) 
  }
  k <- k+1
}

jj$eq <- gg$eq
#############
select_vote <- vote %>% filter (fips_cnty %in% ALL_fips) %>%
  left_join(us_states_data, by= c("state_abbr", "pct_ctzn_voted_st_16" )) %>%
  transmute (Jurisdiction    = Jurisdiction,
            State            = state_abbr,
            Division         = Division,
            Precincts        = Precincts, 
            Registration     = Total.Registration,
            Make             = Make,
            Equipment.Type   = Equipment.Type,
            EAC.Award.Winner = ifelse (selected.Counties == 1|selected.Counties == 2, "Yes", "No"),
            Population       = pop,
            Population.Dens  = formatC(pop/land_area, format="f", digits = 1),   
            Winner16         = partywinner16,    
            Flipped.16       = flipped, 
            County.Turnout   = formatC(100 * turnout16,    format="f", digits = 1),
            State.Turnout    = pct_ctzn_voted_st_16,
            County.Confidence= round(avgQ33_cty),
            State.Confidence = round(avgQ33_st),
            Relationship     = "  ",
            Prohibiting.Laws = "  ",
            Committed.Lead   = "  ",
            E2E.Candidate    = "  ",
            RLA.Candidate    = "  ",
            Audit.Type       = substring (audit, 3),
            VVPAT.Legislation= vvpat_legis_name,
            Equip.VVPAT.Cat  = equip_vvpat_name,
            FIPS             = fips_cnty)
          

select_vote %>% group_by (FIPS) %>% summarize (num = n ())

#zzz <- select_vote %>% group_by (FIPS) %>%
#  arrange (FIPS, desc(VVPAT))

#qqq <- select_vote %>% group_by (fips_cnty) %>%
#  arrange (FIPS, desc(VVPAT)) %>% 
#  distinct (FIPS, .keep_all = TRUE)

bbb <- select_vote %>%
  distinct (FIPS, .keep_all = TRUE)
  
#write.csv (bbb, "jurisdicition_matrix.csv")
county_params <- only_county_data %>% select (fips_cnty, state_abbr, turnout16, travel_time)
#write.csv (county_params, "county_params.csv")
#write.csv (select_vote, "jurisdicition_detailed_matrix.csv")
#########################################
  plot_county <- function (fac_grad_grad2 , title, subtitle, caption, breaks, labels, labs_fill, 
                           palette, type, direction, county_full, trans){

    states <- usmap::us_map(regions = "states")    
    state_base <- ggplot(data = states, 
                          mapping = aes(x = x, y = y, fill =NULL, group = group)) + 
    coord_fixed(1) + theme_void() #+ geom_polygon (color = "black", fill = "gray") 
    
    p0 <- state_base + geom_polygon_interactive (data=county_full,  
#        p0 <- ggplot () + geom_polygon_interactive (data=county_full,  
                      mapping = aes(tooltip=tooltip, x=x, y=y, fill=var1_stat, group=group), 
                      inherit.aes = FALSE, color="black", size=0.03) +
      theme_map() + 
      labs(title = title, fill=labs_fill, caption=caption, subtitle = subtitle) +
#      geom_polygon(color = "gray50", fill = NA, size=0.05) +
      geom_polygon(color = "black", fill = NA, size=0.03) +
      geom_text_repel(data=us_states_data, mapping=aes(x = COORDS_X,y = COORDS_Y,label = name),
                      nudge_x = usa_sf$nudge_x,
                      nudge_y = usa_sf$nudge_y,
                      inherit.aes = FALSE,
                      size = 2,
                      min.segment.length = 0,
                      point.padding = NA, 
                      segment.color = "grey50") #+
    
    switch (fac_grad_grad2 ,
      "Fac"   = { p0 <- p0 + scale_fill_tableau (palette = palette, labels=labels, type = type, direction = direction) },
      "Grad"  = { p0 <- p0 + scale_fill_gradient_tableau (palette = palette, trans=trans, guide='legend') },
      "Grad2" = { p0 <- p0 + scale_fill_gradient2_tableau (palette = palette, trans=trans, guide='legend', breaks=breaks, labels=labels) }
    )
    return(p0)
  }
###########################################
  plot_state <- function (fac_grad_grad2, 
                          title, subtitle, caption, breaks, labels, labs_fill, 
                          palette, type, direction, us_states_data, trans){
    
    p0 <- ggplot(data = us_states_data) +
      geom_sf_interactive (mapping = aes(tooltip=tooltip, fill=var1)) +
      geom_text_repel(mapping = aes(x = COORDS_X,y = COORDS_Y,label = name),
                      nudge_x = usa_sf$nudge_x,
                      nudge_y = usa_sf$nudge_y,
                      size = 2,
                      min.segment.length = 0,
                      point.padding = NA,
                      segment.color = "grey50") +
      coord_sf(crs = st_crs(usa_sf), datum = NA) +
      theme_map() +
      xlim(min(usa_sf$COORDS_X) * 1.1, max(usa_sf$COORDS_X) * 1.15)+
      labs (title=title, subtitle=subtitle, caption = caption, fill=labs_fill) 

    switch (fac_grad_grad2,
      "Fac"   = { p0 <- p0 + scale_fill_tableau(palette = palette, labels=labels, type = type, direction = direction) },
      "Grad"  = { p0 <- p0 + scale_fill_gradient_tableau(palette = palette, trans=trans, guide='legend') },     
      "Grad2" = { p0 <- p0 + scale_fill_gradient2_tableau(palette = palette, trans=trans, guide='legend', breaks=breaks, labels=labels) }
    )
    return (p0)
  }
#########################################
#
# Prevalence Plots
#
plot2 <- function (var1_indx){
  
  switch (var1_indx,
          "Vendor Prevalence" = {
            vendor <- c("Election Systems & Software", 
                         "Premier/Diebold (Dominion)",   
                         "Dominion Voting Systems",   
                         "Hart InterCivic",
                         "Unisyn Voting Solutions", 
                         "Sequoia (Dominion)",  
                         "MicroVote", 
                         "Avante")
          vendor_pct <- c(47.0, 17.0, 14.0, 13.0, 6.7, 4.6, 3.0, 0.0)
          vendor_data <- data.frame (vendor, vendor_pct)
          vendor_data$vendor <-factor(vendor_data$vendor, 
                                      levels=vendor_data$vendor)
          p0 <-ggplot(vendor_data, aes(vendor, vendor_pct))+
            geom_bar(stat = "identity", aes(fill = vendor)) +
            theme(axis.text.x=element_text(angle=90,hjust=1, vjust = 0)) + coord_flip()+
            labs (y="Percentage of All Counties", x="Vendor", fill="Vendor", 
                  title="Prevalence of Vendors",
                  caption ="(Avante Manufactures VVPAT Equip. Only)") +
            guides(fill=guide_legend(reverse=TRUE), colour=guide_legend(reverse=TRUE)) +
            theme(legend.position = "none") +
            theme(plot.title = element_text(size = 15, hjust = 0),
            plot.caption =  element_text(size = 7, hjust = -0.6)) 
          },
          
          "Equipment Prevalence" = {
            equip <- c("Optical Scan", 
                       "Ballot Marking Device",  
                       "DRE-Touchscreen",
                       "DRE-Dial", 
                       "DRE-Push Button", 
                       "Hand Counted Paper Ballots",
                       "Ballot Marking Device/Optical Scan", 
                       "DRE")
          equip_pct <- c(94, 57, 26, 7.8, 6.1, 6.0, 2.8, 2.5 )
          equip_data <- data.frame (equip, equip_pct)
          equip_data$equip <-factor(equip_data$equip, 
                                    levels =equip_data$equip)
          p0 <-ggplot(equip_data, aes(equip, equip_pct))+
            geom_bar(stat = "identity", aes(fill = equip)) +
            theme(axis.text.x=element_text(angle=90,hjust=1, vjust = 0)) + coord_flip()+
            labs (y="Percentage of All Counties", x="Equipment Type", fill="Eqiupment Type", 
                  title="Prevalence of Equipment Type", 
                  caption = "(DRE - Direct Recording Electronic Voting Machines)") +
            guides(fill=guide_legend(reverse=TRUE), colour=guide_legend(reverse=TRUE)) +
            theme(legend.position = "none") +
            theme(plot.title   = element_text(size = 15, hjust = 0),
                  plot.caption = element_text(size = 7, hjust = -1.05)) 
          })
  return (p0)
}
####################
# By County
####################
plot1 <- function (var1_indx, var2_indx, var3_indx){

  cc<- vote

  title     <- NULL
  caption   <- NULL
  labels    <- NULL
  palette   <- NULL
  labs_fill <- NULL
  breaks    <- NULL
  trans     <- 'identity' 
  type      <- "regular"
  direction <- 1
  
  vote_col_indx <- which (colnames(cc) ==  var_cty[which (var_county == var1_indx)])
  colnames(cc)[vote_col_indx] <- "var1"
  
  cc_cnty <- cc %>% group_by (fips_cnty, .keep_all = TRUE)

  if(var1_indx %in% c("Existing Relationship", "Awarded By EAC")){
    
    cc$var1 <- ifelse (is.na(cc$var1), 5, cc$var1)

    switch (var1_indx,
            "Existing Relationship" = {
              cnty <- cc %>% filter (var1 == 1 | var1 == 2 | var1 == 4 ) %>% 
                distinct (fips_cnty, .keep_all = TRUE)},
            "Awarded By EAC"      = {
              cnty <- cc %>% filter (var1 == 1 | var1 == 2) %>% 
                distinct (fips_cnty, .keep_all = TRUE)},
            "ALL"                 = {
              cnty <- cc %>% filter (var1 == 1 | var1 == 2 | var1 == 4) %>% 
                distinct (fips_cnty, .keep_all = TRUE)}
    )

    cn <- cc %>% filter (fips_cnty %in% cnty$fips_cnty) 
    cn <- left_join(cn, us_states_data, by="state_abbr") 
    
    cnty$Jurisdiction <- str_remove_all (cnty$Jurisdiction, 
                            paste (c(" County", " Borough", " Census Area", " Parish",
                                     " City", " City City", " Town", " Village"), 
                                   collapse = "|")) 
    cnty$cnty_id <- paste (cnty$Jurisdiction, ",", cnty$state_abbr)
  
    gg <- cn %>% group_by(fips_cnty, .keep_all=TRUE) %>% 
      summarize (n_make =    n_distinct(Make_sh),
                 n_model =   n_distinct(Model_sh),
                 n_eq_type = n_distinct(Equipment.Type_sh),
                 aa = first(Make_sh),  bb= first(Model_sh), cc=first(Equipment.Type_sh),
                 dd = first(audit_sh), ee= first(VVPATL),   ff= first(equip_vvpat_name_sh))
    
    descr1 <- paste0 ("RLA:",      gg$dd, 
                      "\nVVPATL:", ifelse(gg$ee==1, "Yes", "No"),  
                      "\nEq+VVL:", gg$ff) 
    descr2 <- paste0 ("Vendor:",   gg$aa) 
    descr3 <- paste0 ("Model:",    gg$bb)
    descr4 <- paste0 ("EqTyp:",    gg$cc)

    descr6 <- paste0 (descr1, "\n",descr2, "\n",descr3, "\n",descr4)

    cnty$name <- paste0 (cnty$cnty_id, "\n", descr6)   
    #  
    # County Names for Labelling the graph
    #
    county_loc <- cnty_map %>% distinct(id, .keep_all = TRUE) %>% 
      filter (id %in% cnty$fips_cnty)
    
    # lookup
    cnty <- cnty %>% mutate (id = fips_cnty)

    county_loc <- left_join(county_loc, cnty, by="id") 
    
    cc_cnty <- cc %>% group_by (fips_cnty, .keep_all = TRUE)
    
    df <- cc_cnty %>% distinct (fips_cnty, .keep_all = TRUE) %>%
      summarize (var1_stat = min (var1))

    switch (var1_indx,
            "Existing Relationship" = {
              df$var1_stat <- ifelse (df$var1_stat == 4, 1, 5)},
            "Awarded By EAC"      =   {
              df$var1_stat <- ifelse (df$var1_stat >2, 5, df$var1_stat)},
            "ALL"                 = {}
    )
    
    df$var1_stat <- factor (df$var1_stat) 

    idx <- which (colnames(df) == "fips_cnty") 
    colnames(df)[idx] <- "id"
    county_full <- left_join(cnty_map, df, by="id") 

    title <- "Voting Method/System and Legislation Information"
    subtitle <- paste("Selection Criteria:",var1_indx) 
    labels <- c("Yes","Maybe","No")
    labs_fill <- "Selected Counties:"
    
    fac_grad_grad2  <- "Fac"
    palette = "Color Blind"
    county_full$var1_stat <- factor (county_full$var1_stat)
  }

  if(colnames(vote)[vote_col_indx] %in% c("VVPAT","Accessible.Use","Early.Voting",
    "Absentee.Ballots","Polling.Place")){

    cc <- cc %>%
      mutate (var1= case_when(
        var1 == "Yes" ~ 1, 
        var1 == "No"  ~ 2,
        var1 == "N/A" ~ 3))

    cc_cnty <- cc %>% group_by (fips_cnty, .keep_all = TRUE)    
    df <- cc_cnty %>% summarise(var1_stat=min(var1))
    
    num_counties <- sum(df$var1_stat==1)
    tot_counties  <- length(unique (vote$fips_cnty))
    
    idx <- which (colnames(df) == "fips_cnty") 
    colnames(df)[idx] <- "id"
    county_full <- left_join(cnty_map, df, by="id")

    title <- paste(var1_indx, ": ",prettyNum(num_counties), "counties found - ", 
          formatC(num_counties/tot_counties*100, format="f", digits = 2), "%of total")
    subtitle <- "(Source: Verified Voting)"
    if (colnames(vote)[vote_col_indx] %in% c("VVPAT"))
      caption <- paste("VVPAT - Voter Verification Paper Audit Trail",
                       "\nDRE     - Direct Recording Electronic Voting Machines")
    labels <- c("Yes", "No") 
    labs_fill <- paste(var1_indx, ":")

    county_full$var1_stat <- as_factor(case_when(
      county_full$var1_stat == 1 ~ "Yes",
      county_full$var1_stat == 2 ~ "No",
      TRUE           ~ "N/A"))

    county_full$tooltip <- paste (county_full$tooltip, county_full$var1_stat)

    fac_grad_grad2  <- "Fac"
    palette <-  "Color Blind"
    county_full$var1_stat <- factor (county_full$var1_stat)
  }
      
  if(colnames(vote)[vote_col_indx] %in% c("partywinner12", "partywinner16")){
    
    df <- cc_cnty %>% summarise(var1_stat=first(var1))
    
    num_dem_counties <- sum(df$var1_stat=="Democrat",   na.rm = TRUE)
    num_gop_counties <- sum(df$var1_stat=="Republican", na.rm = TRUE)

    if (colnames(vote)[vote_col_indx] %in% c("partywinner12"))
      title <- ("2012 Presidential Elections: ")
    if (colnames(vote)[vote_col_indx] %in% c("partywinner16"))
      title <- ("2016 Presidential Elections: ")
        
    title <- paste (title, 
                    "Democratic Counties:", prettyNum(num_dem_counties, big.mark=","),
                    " - Republican Counties:", prettyNum(num_gop_counties, big.mark=",") )
    subtitle <- "(Source: US Census Bureau -Via Socviz)"
    labels <- c("Democrat", "Republican")
    labs_fill <- paste(var1_indx, ":")
        
    idx <- which (colnames(df) == "fips_cnty") 
    colnames(df)[idx] <- "id"
    county_full <- left_join(cnty_map, df, by="id")

    fac_grad_grad2  <- "Fac"
    palette <-  "Color Blind"

    county_full$tooltip <- paste0 (county_full$tooltip, "\n", county_full$var1_stat)
    county_full$var1_stat <- factor (county_full$var1_stat)
  }
  
  if(colnames(vote)[vote_col_indx] %in% c("pop_dens")){
    
    df <- cc_cnty %>% distinct (fips, .keep_all = TRUE) %>%
      summarise (var1_stat = first(var1),
                 registr   = sum(Total.Registration),
                 precincts = sum(Precincts),
                 pop       = first(pop),
                 land_area = first(land_area))
    
    aa <- df %>% group_by(var1_stat) %>% summarise(aa = n())

    title <- paste(var1_indx, "- People Per Square Mile")  
    subtitle <- "(Source: US Census Bureau)"

    labs_fill <- paste ("Population Density :")
    
    idx <- which (colnames(df) == "fips_cnty") 
    colnames(df)[idx] <- "id"
    county_full <- left_join(cnty_map, df, by="id")

    trans <- 'log10'
    palette <- "Classic Area-Brown"
    fac_grad_grad2  <- "Grad"

    county_full$tooltip <- paste (
      county_full$tooltip, "\n", 
      "Population:", format(county_full$pop,              big.mark=",", nsmall = 0),"ppl\n",
      "Area:",       format(round(county_full$land_area), big.mark=",", nsmall = 0),"sq-mi\n",
      "Pop Dens:",   format(round(county_full$var1_stat), big.mark=",", nsmall = 0),"ppl/sq-mi\n",
      "#Precincts:", format(county_full$precincts,        big.mark=",", nsamll = 0))
  }
  
  if(colnames(vote)[vote_col_indx] %in% c("last_date_sick", "last_date_dead")){
    
    df <- cc_cnty %>% distinct (fips, .keep_all = TRUE) %>%
      summarise (var1_stat = first(var1)+1)
    
    title <- var1_indx  
    subtitle <- "(Source: USA Facts)"
    
    labs_fill <- var1_indx
    
    idx <- which (colnames(df) == "fips_cnty") 
    colnames(df)[idx] <- "id"
    county_full <- left_join(cnty_map, df, by="id")
    
    trans <- 'log10'
    palette <- "Red"
    fac_grad_grad2  <- "Grad"
    
    county_full$tooltip <- paste (
      county_full$tooltip, "\n", 
      var1_indx, format(county_full$var1_stat-1, big.mark=",", nsmall = 0))
  }
  
  if(colnames(vote)[vote_col_indx] %in% c("per_cap_sick", "per_cap_dead")){
    
    df <- cc_cnty %>% distinct (fips, .keep_all = TRUE) %>%
      summarise (var1_stat = round(first(var1)+1))
    
    title <- var1_indx  
    subtitle <- "(Source: USA Facts)"
    
    labs_fill <- var1_indx
    
    idx <- which (colnames(df) == "fips_cnty") 
    colnames(df)[idx] <- "id"
    county_full <- left_join(cnty_map, df, by="id")
    
    trans <- 'log10'
    palette <- "Orange"
    fac_grad_grad2  <- "Grad"
    
    county_full$tooltip <- paste (
      county_full$tooltip, "\n", 
      var1_indx, format(county_full$var1_stat-1, big.mark=",", nsmall = 0))
  }
  
  if(colnames(vote)[vote_col_indx] %in% c("turnout16_fac")){

    county_params <- read.csv('~/Documents/RStanFiles/county_params.csv', header=T) %>%
      transmute (id   = sprintf ("%05d", fips_cnty),
                 var1 = turnout16)

    mean_var1 <- mean (county_params$var1, na.rm = TRUE)
    sd_var1   <- sd   (county_params$var1, na.rm = TRUE)
  
    df <- county_params %>%
      mutate (var1_stat = factor(case_when
              (var1 > mean_var1-sd_var1 & var1 < mean_var1+sd_var1 | is.na(var1) ~ 
                  paste ("b)Mean +/- 1sd"), 
               var1 > mean_var1+sd_var1 ~ paste ("c)> Mean + 1sd"), 
               TRUE                     ~ paste ("a)< Mean - 1sd")))) 
 
    aa <- df %>% group_by(var1_stat) %>% summarise(aa = n())
  
    title <- paste(var1_indx, ":Votes/Eligible Voters\nMean=", round(mean_var1*100),
                   "%  -- SD=", round(sd_var1*100), "%")  
    subtitle <- "(Source: US Census Bureau -Via Socviz)"
   
    labels <- substring (levels(df$dum), 3)
    labs_fill <- paste(var1_indx, ":")
  
    county_full <- left_join(cnty_map, df, by="id")
    county_full <- county_full %>% 
    mutate (tooltip = paste0 (tooltip, "\n", "Turnout:", floor (100*var1),   "%"))
    
    fac_grad_grad2  <- "Fac"
    palette <-  "Color Blind"
    county_full$var1_stat <- factor (county_full$var1_stat)
  }

  if(colnames(vote)[vote_col_indx] %in% c("flipped")){
    
    df <- cc_cnty %>% summarise(var1=min(var1), var2=min(partywinner12))
    
    df <- df %>% 
      mutate (var1_stat = case_when (
                var1 == "Yes" & var2 == "Democrat"    ~ "c)Flipped Republican",
                var1 == "Yes" & var2 == "Republican"  ~ "a)Flipped Democrat",
                TRUE                                  ~ "b)No Flip"))

    num_dem_counties <- sum(df$var1_stat=="a)Flipped Democrat",   na.rm = TRUE)
    num_gop_counties <- sum(df$var1_stat=="c)Flipped Republican", na.rm = TRUE)
    
    title <- paste ("Flipped Democrat:",   prettyNum(num_dem_counties, big.mark=","), " counties - ",
                    "Flipped Republican:", prettyNum(num_gop_counties, big.mark=","), "counties")
    subtitle <- "(Source: US Census Bureau)"
    labels <- c("Democrat", "No Change", "Republican") 
    labs_fill <- paste(var1_indx, ":")
    
    idx <- which (colnames(df) == "fips_cnty") 
    colnames(df)[idx] <- "id"
    county_full <- left_join(cnty_map, df, by="id")

    fac_grad_grad2  <- "Fac"
    palette <-  "Red-Blue-Brown"
    
    county_full <- county_full %>%
      mutate (tooltip = case_when(
      var1_stat == "a)Flipped Democrat" ~ 
        paste0 (county_full$tooltip, "\nFlipped Democrat"),
      var1_stat == "c)Flipped Republican" ~ 
        paste0 (county_full$tooltip, "\nFlipped Republican"),
      TRUE ~ 
        paste0 (county_full$tooltip, "\nNo Change")))
                      
   county_full$var1_stat <- factor (county_full$var1_stat)
  }

  if(colnames(vote)[vote_col_indx] %in% c("Equipment.Type","Make")){

    descr <- NULL
    df <- cc_cnty %>% summarise(var1_stat = 0)

    switch(colnames(vote)[vote_col_indx],
           "Equipment.Type" =       {
             sub_types <- eq_types
             sub_indx  <- var2_indx
             caption   <- paste("DRE - Direct Recording Electronic Voting Machines")},
           "Make" =                 {
             sub_types <- vendor_types
             sub_indx  <- var3_indx})
    

    cc$var1_stat <- ifelse (cc$var1 == sub_indx[1], 1, 0)
      
    gg <- cc %>% group_by (fips_cnty, .keep_all = TRUE) %>%
      summarise(var1_stat = sum(var1_stat))
      
    gg$var1_stat <- ifelse (gg$var1_stat >= 1, 1, 0)
      
    df <- cc %>% distinct (fips_cnty) %>%
      left_join(gg, by="fips_cnty")                   
      
    descr <- paste(sub_indx[1])

    
    if (length(sub_indx) > 1){
      for (i in 2:length(sub_indx)){
        dum <- cc_cnty %>% summarise(var1_stat = ifelse(sum(var1==sub_indx[i]) >=1, TRUE, FALSE))
        descr <- paste (descr, " &", sub_indx[i])
        df$var1_stat = df$var1_stat * dum$var1_stat
      }
    }

    num_counties <- sum(df$var1_stat)
    tot_counties  <- length(unique (vote$fips_cnty))

    df$var1_stat <- case_when (df$var1_stat == TRUE  ~ 1, 
                               df$var1_stat == FALSE ~ 2, 
                               TRUE                  ~ 3)
    
    df$var1_stat <- factor (df$var1_stat)
    idx <- which (colnames(df) == "fips_cnty") 
    colnames(df)[idx] <- "id"
    county_full <- left_join(cnty_map, df, by="id")
    
    title <- paste(var1_indx, ":", descr, 
                   "\n", num_counties, "  counties found - ", 
                   formatC(num_counties/tot_counties*100, format="f", digits = 2), "%of total")
    subtitle <- "(Source: Verified Voting)"
    labels <- c("Yes", "No")
    labs_fill <- paste(var1_indx, ":")

    county_full <- county_full %>% 
      mutate (tooltip = case_when 
              (var1_stat == 1 ~ paste0 (county_full$tooltip, "\nYes"),
               var1_stat == 2 ~ paste0 (county_full$tooltip, "\nNo"),
               TRUE           ~ paste0 (county_full$tooltip, "\nNA")))

    fac_grad_grad2  <- "Fac"   
    palette <- "Color Blind"
    county_full$var1_stat <- factor (county_full$var1_stat)
  }

  if(colnames(vote)[vote_col_indx] %in% c("Division","Precincts","Total.Registration")){

    df <- cc %>% distinct (fips, .keep_all = TRUE) %>%
      group_by(fips_cnty) %>%
      summarize (var1_stat = sum (var1))
    
    title=paste (prettyNum(sum(df$var1_stat), big.mark=","), colnames(vote)[vote_col_indx])
    subtitle <- "(Source: Verified Voting)"
    labs_fill <- paste(var1_indx, ":")
    
    trans <- 'log10'

    idx <- which (colnames(df) == "fips_cnty") 
    colnames(df)[idx] <- "id"
    county_full <- left_join(cnty_map, df, by="id")
    
    county_full$var1_stat <- ifelse(is.na(county_full$var1_stat), 3, county_full$var1_stat)

    county_full$tooltip <- paste0 (county_full$tooltip, "\n", #, 
          formatC(county_full$var1_stat, format="d", big.mark=",", digits=0))

    palette <- "Blue-Teal"
    if (colnames(vote)[vote_col_indx] %in% c("Precincts")) 
      palette <- "Classic Area-Brown"
    fac_grad_grad2  <- "Grad"
  }

  if(var1_indx %in% c("Voter Confidence")){
    
    df <- cc %>% 
      group_by(fips_cnty) %>%
      summarize (var1_stat = first (var1))
    
    title=paste (var1_indx)
    subtitle <- paste ("(Source: Survey Of The Performance Of American Elections - MIT\n",
                       "200 Data Points Per State)")
    
    labs_fill <- paste(var1_indx, ":")

    colnames(df)[1] <- "id"
    county_full <- left_join(cnty_map, df, by="id")
    
    county_full$tooltip <- paste0 (county_full$tooltip, "\n", #, 
                                   formatC(county_full$var1_stat, format="d", big.mark=",", digits=0))

    palette <- "Blue-Teal"
    fac_grad_grad2  <- "Grad"
  }
  
  if(colnames(vote)[vote_col_indx] %in% c("per_dem_2016", "per_gop_2016")){

    df <- cc %>% distinct (fips_cnty, .keep_all = TRUE) %>%
      mutate (pct = 100* var1)
    
    title <- var1_indx
    subtitle <- "(Source: US Census Bureau -Via Socviz)"
    labs_fill <- paste(var1_indx, ":")

    idx <- which (colnames(df) == "fips_cnty") 
    colnames(df)[idx] <- "id"
    county_full <- left_join(cnty_map, df, by="id")
    
    mid_var1 <- 50.0 
    min_var1 <- min (county_full$pct, na.rm = TRUE)
    max_var1 <- max (county_full$pct, na.rm = TRUE)
    
    county_full <- county_full %>%
      mutate (var1_stat = case_when (
        is.na(pct)     ~  0,
        pct > mid_var1 ~  (pct-mid_var1)/max(pct-mid_var1, na.rm = TRUE), 
        TRUE           ~ -(pct-mid_var1)/min(pct-mid_var1, na.rm = TRUE)))

    breaks= c(-1, -0.5, 0, 0.5, 1)
    labels= floor (c(min_var1, (mid_var1+min_var1)/2, mid_var1, 
              (max_var1+mid_var1)/2, max_var1))
    
    if (colnames(vote)[vote_col_indx] %in% c("per_gop_2016")){
      trans <- "reverse"
    }

    switch (colnames(vote)[vote_col_indx],
            "per_dem_2016" = {
              county_full$tooltip <- 
                paste0 (county_full$tooltip, "\n", 
                        floor (county_full$pct), "%")},
            "per_gop_2016" = {
              county_full$tooltip <- 
                paste0 (county_full$tooltip, "\n", 
                        floor (county_full$pct), "%")})
            

    fac_grad_grad2  <- "Grad2"    
    palette <- "Red-Blue Diverging"
  }

  if(colnames(vote)[vote_col_indx] %in% c("turnout16")){
    
    df <- cc %>% group_by (fips_cnty) %>%
      summarise (id        = min(fips_cnty),
#                var1     = min (var1, na.rm = TRUE),
                 dem_16    = first(per_dem_2016),
                 gop_16    = first(per_gop_2016),
                 black     = first(black),
                 pop       = first(pop)) 
    
    county_params <- read.csv('~/Documents/RStanFiles/county_params.csv', header=T) %>%
      transmute (id    = sprintf ("%05d", fips_cnty),
                  var1 = turnout16)
    
    county_full <- left_join(cnty_map, df, by="id")
    county_full <- left_join(county_full, county_params, by="id")

    mid_var1 <- mean(county_params$var1, na.rm = TRUE)
    min_var1 <- min (county_params$var1, na.rm = TRUE)
    max_var1 <- max (county_params$var1, na.rm = TRUE)
    sd_var1  <- sd  (county_params$var1, na.rm = TRUE)
    
    county_full <-  county_full %>%
            mutate (var1_stat = 
                      ifelse(var1 > mid_var1, 
                            (var1-mid_var1)/(max_var1-mid_var1), 
                           -(var1-mid_var1)/(min_var1-mid_var1)),
                    tooltip   = paste (tooltip, "\n", 
                        "Turnout:", floor (100*var1),  "%\n",
                        "Dem:",     floor (100*dem_16), "%\n",
                        "GOP:",     floor (100*gop_16), "%\n",                        
                        "Black:",   black,              "%\n", 
                        "Pop:",     formatC(pop, format="d", big.mark=",", digits=0))) 

    title <- paste(var1_indx, ":Votes/Eligible Voters\nMean=", round(mid_var1*100),
                   "%  -- SD=", round(sd_var1*100), "%")  
    subtitle <- "(Source: US Census Bureau + United States Elections Projects + Inference)"
    labs_fill <- paste(var1_indx, ":")
    breaks= c(-1, -0.5, 0, 0.5, 1)
    labels= floor (c(100 * min_var1, 100 * (mid_var1+min_var1)/2, 100 * mid_var1, 
                     100 * (max_var1+mid_var1)/2, 100 * max_var1))
    trans <- "reverse"
    fac_grad_grad2  <- "Grad2"    
    palette <- "Red-Blue Diverging"
  }
  
  p0 <- plot_county(fac_grad_grad2, title, subtitle, caption, breaks, labels, labs_fill, 
                    palette, type, direction, county_full, trans=trans) 
  if(var1_indx %in% c("Awarded By EAC", "Existing Relationship")){
    p0 <- p0 +  geom_label_repel(data = county_loc, aes(long, lat, label=name), 
                                 inherit.aes = FALSE,size = 1.75, point.padding = .5) 
  }
#  girafe(code=print(p0))
  return (p0)
}
###########################################
#
# By States
#
plot0 <- function (var4_indx){
  
  cc <- us_states_data
  us_states_col_indx <- which (colnames(cc) ==  var_st[which (var_state == var4_indx)])
  colnames(cc)[us_states_col_indx] <- "var1"

  caption   <- NULL
  labs_fill <- NULL
  type      <- "regular"
  direction <- 1
  trans     <- 'identity'
  labels    <- NULL
  breaks    <- NULL
  
  switch(var4_indx,
    "Audit Legislation" =       {
      num <- length(levels(cc$var1))        
      
      cc$tooltip <- paste (cc$name, ":\n", substring (cc$var1, 3))      
      title <- "Audit Legislation"
      subtitle <- "(Source: National Conference Of State Legislatures)"
      caption <- "RLA - Risk Limiting Audit"
      #    for (i in 1:num)
      #      caption <- paste (caption, levels(audit_type$audit)[i],":", summ$type[i], "\n")
      labels <- substring(levels(audit_type$audit), 3)
      palette <-  "Tableau 10"
      fac_grad_grad2  <- "Fac"
    },
    "VVPAT Legislation" =       {
      cc$tooltip <- paste (cc$name, ":\n", cc$vvpat_legis_name)
      num <- length(levels(cc$var1))
      
      title <- "Voter Verified Paper Audit Trail Legislation"
      subtitle <- "(Source: Verified Voting)"
      labels <- c("Yes","Some","Weak","No")
      palette <-  "Tableau 10"
      fac_grad_grad2  <- "Fac"
     },
    "Equipment&VVPAT Legis." =  {
      cc$tooltip <- paste (cc$name, ":\n", cc$var1)
      num <- length(levels(as.factor(cc$var1)))
      
      title <- "Polling Place Equipment Type And VVPAT"
      subtitle <- "(Source: Verified Voting)"
      caption <- paste ("VVPAT - Voter Verification Paper Audit Trail",
                  "\nDRE     - Direct Recording Electronic Voting Machines")
#      labels <- levels(factor (cc$var1))
      labels <- eqp_vvpat_lab
      palette <-  "Tableau 10"
      fac_grad_grad2  <- "Fac"
    },
    "Current VVPAT Density" =   {
      cc$tooltip <- paste (cc$name, ":\n", floor (cc$var1), "%")
      title <- "Percent Of Precincts With Affirmative VVPAT"
      subtitle <- "(Source: Verified Voting)"
      labels    <- "VVPAT Precinct Density"
      labs_fill <- "Paper Trail Density (%)"
      palette <-  "Blue-Teal"
      fac_grad_grad2  <- "Grad"
     },
    "Voter Confidence" =   {
      cc$tooltip <- paste (cc$name, ":\n", floor (cc$var1), "%")
      title <- "Voter Confidence"
      subtitle <- paste ("(Source: Survey Of The Performance Of American Elections - MIT\n",
        "200 Data Points Per State)")
      labels    <- "Voter Confidence"
      labs_fill <- "Voter Confidence (%)"
      palette <-  "Blue-Teal"
      fac_grad_grad2  <- "Grad"
    },
    "Voter Participation '16" = {
      cc$tooltip <- paste (cc$name, ":\n", floor (cc$var1), "%")
      title <- "Voter Participation Rate in 2016: Votes/Eligible Voters"
      subtitle <- paste("(Source: United States Census: Voting and Registration)\nMean:", 
                        round(mean (cc$var1)), "%, SD:", round (sd(cc$var1)), "%",
                        "\nLows- Hawaii:47%, West Virginia:51%  --  ",
                        "Highs- District of Columbia:74%, Maine:73%")
      labels <- "Voter Participation"
      labs_fill <- "Participation Rate (%)"
      palette <-  "Orange-Blue Diverging" 
      fac_grad_grad2  <- "Grad2"
      breaks= c(52, 57, 62, 67, 72)
      labels= c("-2SD", "-1SD", "Mean", "+1SD", "+2SD")
      trans = 'reverse'
    },
    "Balance Of Payments" =  {
      cc$tooltip <- paste (cc$name, ":\n", formatC(cc$BOP, format="d", big.mark=",", digits=0), "($M)")
      title <- "Balance Of Payments With Federal Government (2016/2017)"
      subtitle <- "(Rockefeller Institute Of Government)"
                        
      fac_grad_grad2  <- "Grad2"
      palette = "Red-Green Diverging"
      trans='identity' 
      breaks= c(-1, -0.5, 0,0.5, 1)
      labs_fill <- "Balance Of Payments ($M)"
      mid_BOP <- 0
      min_BOP <- min(balances$BOP)
      max_BOP <- max(balances$BOP)
      labels= c(formatC(min_BOP, format="d", big.mark=",", digits=0),
                formatC((mid_BOP+min_BOP)/2, format="d", big.mark=",", digits=0),
                formatC(mid_BOP, format="d", big.mark=",", digits=0),
                formatC((max_BOP+mid_BOP)/2, format="d", big.mark=",", digits=0),
                formatC(max_BOP, format="d", big.mark=",", digits=0))
    },
    "Per Capita BOP" =  {
      cc$tooltip <- paste (cc$name, ":\n", formatC(cc$BOP_percap, format="d", big.mark=",", digits=0), "($)")
      title <- "Per Capita BOP With Federal Government (2016/2017)"
      subtitle <- "(Rockefeller Institute Of Government)"
      
      fac_grad_grad2  <- "Grad2"
      palette = "Red-Green Diverging"
      trans='identity' 
      breaks= c(-1, -0.5, 0,0.5, 1)
      labs_fill <- "Balance Of Payments Per Capita ($)"
      mid_BOP_percap <- 0
      min_BOP_percap <- min(balances$BOP_percap)
      max_BOP_percap <- max(balances$BOP_percap)
      labels= c(formatC(min_BOP_percap, format="d", big.mark=",", digits=0),
                formatC((mid_BOP_percap+min_BOP_percap)/2, format="d", big.mark=",", digits=0),
                formatC(mid_BOP_percap, format="d", big.mark=",", digits=0),
                formatC((max_BOP_percap+mid_BOP_percap)/2, format="d", big.mark=",", digits=0),
                formatC(max_BOP_percap, format="d", big.mark=",", digits=0))
    },
    "Return On Investment"      =  {
      cc$tooltip <- paste (cc$name, ":\n", cc$Return)
      title <- "Return On Federal Taxes Paid (2016-2017)"
      subtitle <- "(Rockefeller Institute Of Government)"
      
      fac_grad_grad2  <- "Grad2"
      palette = "Red-Green Diverging"
      trans='identity' 
      breaks= c(-1, -0.5, 0,0.5, 1)
      labs_fill <- "Transfers Received/Taxes Remitted"
      mid_Return <- 1
      min_Return <- min(balances$Return)
      max_Return <- max(balances$Return)
      labels= c(min_Return, (mid_Return+min_Return)/2, mid_Return, 
                (max_Return+mid_Return)/2, max_Return) 
    },
  "COVID Cases" =   {
    cc$tooltip <- paste (cc$name, ":\n", format(cc$var1, big.mark=",", nsmall = 0))
    title <- "Total COVID Cases as of 5/25/20"
    subtitle <- "(Source: USA Facts)"
    labels    <- ""
    labs_fill <- "Total Covid Cases"
    palette <-  "Red"
    trans <- 'log10'
    fac_grad_grad2  <- "Grad"
  },
  "COVID Cases (Per Million)" =   {
    cc$tooltip <- paste (cc$name, ":\n", format(round (cc$var1), big.mark=",", nsmall = 0))
    title <- "COVID Cases Per Million"
    subtitle <- "(Source: USA Facts)"
    labels    <- ""
    labs_fill <- "Covid Cases Per Million"
    palette <-  "Orange"
    trans <- 'log10'
    fac_grad_grad2  <- "Grad"
  },
  "COVID Deaths" =   {
    cc$tooltip <- paste (cc$name, ":\n", format(cc$var1, big.mark=",", nsmall = 0))
    title <- "COVID Death Total as of 5/25/20"
    subtitle <- "(Source: USA Facts)"
    labels    <- ""
    labs_fill <- "Total Covid Deaths"
    palette <-  "Red"
    trans <- 'log10'
    fac_grad_grad2  <- "Grad"
  },
  "COVID Deaths (Per Million)" =   {
    cc$tooltip <- paste (cc$name, ":\n", format(round (cc$var1), big.mark=",", nsmall = 0))
    title <- "COVID Deaths Per Million"
    subtitle <- "(Source: USA Facts)"
    labels    <- ""
    labs_fill <- "Covid Deaths Per Million"
    palette <-  "Orange"
    trans <- 'log10'
    fac_grad_grad2  <- "Grad"
  },
  print ("Invalid value")
  )
  
  p0 <- plot_state (fac_grad_grad2 , title, subtitle, caption, breaks, labels, labs_fill,
                    palette, type, direction, cc, trans) 
#  girafe(code=print(p0))
  return (p0) 
}
#
# Send Input To Plotters
#
library(htmlwidgets)
plot_aa <- function (var0, var1, var2, var3, var4){
  
  if (var0 == 2 & !(var1 == "Vendor Prevalence" |var1 == "Equipment Prevalence"))
    p <- plot1 (var1, var2, var3)
  if (var0 == 2 &  (var1 == "Vendor Prevalence" |var1 == "Equipment Prevalence"))
    p <- plot2 (var1)
  if (var0 == 1)
    p <- plot0 (var4)
  
  x <- print(p)               
}
#################
giraph_aa <- function (var0, var1, var2, var3, var4){
  
  if (var0 == 2 & !(var1 == "Vendor Prevalence" |var1 == "Equipment Prevalence"))
    p <- plot1 (var1, var2, var3)
  if (var0 == 2 &  (var1 == "Vendor Prevalence" |var1 == "Equipment Prevalence"))
    p <- plot2 (var1)
  if (var0 == 1)
    p <- plot0 (var4)
  
#  x <- girafe(code=print(p))
    x <- girafe(ggobj = p)
  girafe_options(x, opts_zoom(max=5), 
                 sizingPolicy(viewer.defaultHeight = 200),
                 opts_sizing(rescale =TRUE),
                 opts_tooltip(opacity = .5),
                 viewer.fill = TRUE) 
}
###
# Shiny Code
###
#https://rstudio.github.io/shinydashboard/structure.html#sidebar-menu-items-and-tabs

sidebar <- dashboardSidebar(
#  width = 100,
  sidebarMenu(
    menuItem("State Maps",   tabName = "smaps",   icon = icon("globe-americas")),
    menuItem("County Maps",  tabName = "cmaps",   icon = icon("map")),
    menuItem("State Table",  tabName = "stable",  icon = icon("table")),
    menuItem("County Table", tabName = "ctable",  icon = icon("table"))
  )
)
#
body <- dashboardBody(
  fluidRow(
    tabItems(
      tabItem(tabName = "smaps",
          column (width = 4, offset = 0, 
                  selectInput ("var4", label = NULL, choices = var_state, 
                              selected="Audit Legislation")),
          fluidRow(
            box (width = 12, offset = 0, 

            fillPage(tags$style(type = "text/css", "#plot1 {height: calc(91vh - 80px) !important;}"),
             ggiraphOutput("plot1", width = "100%", height = "100%")
            )     

            )
          )
          
      ),
       tabItem(tabName = "cmaps",
           column (width = 4, offset = 0, 
                  selectInput ("var1", label = NULL, choices = var_county,   
                               selected="COVID Cases")),
          conditionalPanel(condition = "input.var1 =='Vendor' ",
               column (width = 5, offset = 0, 
                  selectInput ("var3", label = NULL, choices = vendor_types,                                selected="Election Systems & Software"))),
          conditionalPanel(condition = "input.var1=='Equipment Type' ",
              column (width = 5, offset = 0, 
                  selectInput ("var2", label = NULL, choices = eq_types,     
                               selected="Optical Scan"))),
          fluidRow(
            column (width = 12, offset = 0, 
                    
                    
        fillPage(tags$style(type = "text/css", "#plot2 {height: calc(91vh - 80px) !important;}"),
            ggiraphOutput("plot2", width = "100%",height = "100%"))
        
            )
          )
      ),
      tabItem(tabName = "stable", DTOutput("statetable")),
      tabItem(tabName = "ctable", DTOutput("countytable"))
    )
  )
)
#
#
ui <- dashboardPage(
  dashboardHeader(title = "Election Security", titleWidth=NULL, disable = FALSE),
  sidebar,
  body
)
#
server <- function(input, output, session){
  output$plot1 <- renderggiraph({ 
    giraph_aa (1, input$var1, input$var2, input$var3, input$var4)
  })
  output$plot2 <- renderggiraph({ 
    giraph_aa (2, input$var1, input$var2, input$var3, input$var4)
  })
  output$statetable  <- DT::renderDT(us_states_table)
  output$countytable <- DT::renderDT(only_county_table)
}
shinyApp(ui, server)



