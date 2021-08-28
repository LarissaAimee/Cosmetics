#VP2Code
#Loading important packages
library(tidyverse)
library(sf)
library(spdep)
install.packages("SpatialEpi")
library(SpatialEpi)
install.packages("epitools")
library(epitools)
library(tmap)
library(dplyr)

##Reading in datasets of all-cause mortality rates among Hispanic/Non-Hispanic populations. 
#Bringing in data set of Hispanic mortality data
HisDeath <- read.table('Compressed Mortality, 2006-2016HALL.txt', 
                    sep = '\t', 
                    header = T, 
                    row.names = NULL, 
                    na.strings = 'Missing',
                    stringsAsFactors = F)

view(HisDeath)

#Bringing in dataset of non-Hispanic mortality data
NHisDeath <- read.table('Compressed Mortality, 2006-2016NHALL.txt', 
                       sep = '\t', 
                       header = T, 
                       row.names = NULL, 
                       na.strings = 'Missing',
                       stringsAsFactors = F)

#Checking datasets imported correctly
summary(HisDeath)
summary(NHisDeath)


#converting deaths into numeric variables by coercion 
x<- as.numeric(HisDeath$Deaths)
y<- as.numeric(NHisDeath$Deaths)

#Checking to see how many NA's there were in the data 
sum(is.na(x)) #There are 1064 NAs
sum(is.na(y)) #There are 163 NAs

#Subsetting original data set to see which ones are NA
death[is.na(x),] #Archer, Armstrong, Baylor, Blanco, Borden, Bosque, Bowie, Briscoe, Callahan have suppressed values over the year periods specified


#Cleaning Hispanic/Latinx data set to make deaths numeric and calculate crude rate to give numeric variable

#HDeath <- ifelse(death$deaths[death$Hispanic.Origin == 'Hispanic or Latino'], death, 0)
#NHDeath <- ifelse(deaths$Hispanic.Origin == 'Not Hispanic or Latino', 1, 0) #Letting this try go for now. 
HisDeath2 <- HisDeath %>%
  mutate(NewDeaths = ifelse(is.na(x), NA, Deaths), 
         NewDeaths = as.numeric(NewDeaths), 
         Hcrude_rate = NewDeaths / Population * 100000 )%>%
  mutate(FIPS = sprintf('%05d', County.Code)) %>%
  select(County, FIPS, NewDeaths, Population, Hcrude_rate, Year)

#Cleaning NON-Hispanic/Latinx data set to make deaths numeric and calculate crude rate to give numeric variable
NHisDeath2 <- NHisDeath %>%
  mutate(NewDeaths = ifelse(is.na(y), NA, Deaths), 
         NewDeaths = as.numeric(NewDeaths), 
         NHcrude_rate = NewDeaths / Population * 100000 )%>%
  mutate(FIPS = sprintf('%05d', County.Code)) %>%
  select(County, FIPS, NewDeaths, Population, NHcrude_rate, Year)

#Calculating Rate Ratio 
RateRatioDeaths<- NHisDeath %>%
  mutate(Rate_Ratio = HisDeath2$Hcrude_rate/ NHisDeath2$NHcrude_rate) %>% 
  mutate(FIPS = sprintf('%05d', County.Code)) %>%
  select(County, FIPS, Rate_Ratio, Population, Year)

#Trying to import census products --- Think that may have been pointless...
tidycensus::census_api_key('94ab154d8e8c70e0ee7882bce19c77de11ca7581', install = T)
readRenviron("~/.Renviron")
library(tidycensus)
all_vars <- load_variables(year = 2016, dataset = 'acs5', cache = T)

names(all_vars)
view(all_vars)

#TRYING TO GET GEOGRAPHY DATA 
library(tigris)
library(sf)

options (tigris_use_cache = T)
us <- counties (cb = TRUE, resolution = '5m', 
                class = 'sf', 
                year = 2016) %>%
  st_transform(102003)
head(us)
plot(st_geometry(us))

#Filtering the map only have Texas
Texas <- us %>%
  filter((STATEFP %in% c('48'))) %>%
  select(GEOID, STATEFP, COUNTYFP, NAME)
plot(st_geometry(us))

#MERGING ATTRIBUTES AND GEOGRAPHY
TexasM<- Texas %>%
  left_join(RateRatioDeaths, by = c('GEOID'= 'FIPS'))
summary(TexasM)
#Creating 2006 Map
Texas2007 <- filter(TexasM, Year == '2007')
#us2[is.na(us2$Population), ] #Finding the counties with missing population 
 
library(tmap)
T2007 <- tm_shape(Texas2007) +
  tm_fill('Rate_Ratio', 
          style = 'quantile', 
          palette = 'BuPu', 
          title = 'Rate per 100, 000 py') +
  tm_borders(alpha=.2) +
  tm_credits('Source: CDC Wonder', 
             position = c('RIGHT', 'BOTTOM')) +
  tm_layout(main.title = 'All-cause mortality rate among the Hispanic Latino population', 
            bg.color = 'grey85')
T2007

#Creating 2016 Map
Texas2016 <- filter(TexasM, Year == '2016')
#us2[is.na(us2$Population), ] #Finding the counties with missing population 

library(tmap)
T2016 <- tm_shape(Texas2016) +
  tm_fill('Rate_Ratio', 
          style = 'quantile', 
          palette = 'BuPu', 
          title = 'Rate per 100, 000 py') +
  tm_borders(alpha=.2) +
  tm_credits('Source: CDC Wonder', 
             position = c('RIGHT', 'BOTTOM')) +
  tm_layout(main.title = 'All-cause mortality rate among the Hispanic Latino population', 
            bg.color = 'grey85')
T2016
