##State Warehouse Tracker
##Created by Mike McCarthy, Radical Research LLC
##First created June 2024
##Last modified June 2025
##This script is intended to be the evergreen CEQA tracking app
## (1) Key info should be the list of CEQA Projects that are warehouses
## (2) Spatial Polygons
## (3) Current status (to the extent I can track it)

'%ni%' <- Negate('%in%') ## not in operator

library(googlesheets4)
library(sf)
library(leaflet)
library(tidyverse)
library(tigris)

sf_use_s2(FALSE)

wd <- 'C:/Dev/CEQA_Tracker/'
CEQA_dir <- 'C:/Dev/CEQA_Tracker/CEQA_docs/'

## Start with CEQA tracked warehouses since 2019

##find most recent CEQA documents file
ceqa_docs <- file.info(list.files(CEQA_dir, full.names =T)) #file names
most_recent_doc <- rownames(ceqa_docs)[which.max(ceqa_docs$mtime)] #pull out most recent save

#list of CEQA industrial projects
##Includes non-warehouses
industrial_projects <- read_csv(most_recent_doc) |>   
  janitor::clean_names() |> 
  dplyr::select(sch_number, lead_agency_name, lead_agency_title,
                project_title, received, document_portal_url, counties, cities,
                document_type) |> 
  mutate(recvd_date = lubridate::mdy(received))

industrial_most_recent <- industrial_projects |> 
  group_by(sch_number) |> 
  summarize(recvd_date = max(recvd_date)) |> 
  left_join(industrial_projects) |> 
  filter(document_portal_url != 'https://ceqanet.opr.ca.gov/2017121007/4')

# NOD Processing script
source('Process_NODs.R')

#this script reads in all warehouse geojson files
#it also reads in Y_N_WH list and checks for warehouse projects
#From sheets central valley and Y_N_WH list
#Use wh_Y_list moving forward
source('C:/Dev/CEQA_Tracker/new_warehouses_list.R')

rm(ls = tempsf, tmp, ceqa_docs)
##Last published plannedWH dataset
plannedWH <- sf::st_read('https://github.com/RadicalResearchLLC/CEQA_tracker/raw/main/CEQA_WH.geojson') |> 
  st_transform(crs = 4326) |> 
  ## FIXME - why are these projects filtered out?
  filter(project %ni% c('South Perris Industrial Project',
                        'Northern Gateway Commerce Center',
                        'The Oasis at Indio Project'#,
                       # 'Tracy Northeast Business Park Project') 
                       )) |> 
  filter(!is.na(sch_number)) #|> 
 # filter(sch_number != '2025010343')
# select CEQA industrial warehouses with core info
recent_industrial_CEQA_WH <- wh_Y_list |> 
  select(sch_number, project, ceqa_url, recvd_date, document_type, Notes) |> 
  mutate(recvd_date = as.Date(recvd_date))

tidy_NODs4_join <- industrial_NODs |> 
  select(sch_number, date1) |> 
  group_by(sch_number) |>
  summarize(NOD_date = min(date1)) |> 
  mutate(document_type = 'NOD') |> 
  rename(NOD_type = document_type) 

## join CEQA_WH_w_NODs
## identify stage - NODs required after 2024 under SB 69
## NODs are available with county clerk after 11/3/2020 but before 1/1/2024
recent_CEQA_WH_w_NODs <- recent_industrial_CEQA_WH |> 
  left_join(tidy_NODs4_join) |> 
  mutate(recvd_date2 = as.Date(ifelse(is.na(NOD_type),recvd_date, NOD_date)),
         document_type2 = ifelse(is.na(NOD_type), document_type, NOD_type)) |> 
  select(sch_number, project, ceqa_url, recvd_date2, document_type2, Notes) |> 
  rename(recvd_date = recvd_date2, document_type = document_type2) |> 
  mutate(stage_pending_approved = case_when(
    document_type %in% c('NOD', 'ADM', 'NEG', 'SBE', 'SIR') ~ 'Approved',
    document_type %in% c('FIN', 'MND') & recvd_date < mdy('01-01-2024') ~ 'Approved',
    TRUE ~ document_type
  ))

rm(ls = industrial_projects, industrial_most_recent)

## This pulls the old process of polygons pre-June 2024 and CEQA Tracker process
source('ProcessOldPolygons.R')

##FIXME - don't think I need these
#wh_list_not_in_plannedWH <- plannedWH |> 
#  select(sch_number, project) |> 
#  anti_join(recent_CEQA_WH_w_NODs, by = 'sch_number') 

#wh_list_not_inCEQA <- recent_CEQA_WH_w_NODs |> 
#  anti_join(plannedWH, by = 'sch_number')

#pre2020_projects <- wh_list_not_inCEQA |> 
#  filter(sch_number < 2020000000)

names(newWH7)

newWH <- newWH7 |> 
  select(sch_number, geometry) |> 
  left_join(recent_CEQA_WH_w_NODs, by = 'sch_number')

area <- as.numeric(st_area(newWH))

newWH$parcel_area <- area*10.7639

str(oldWH)
str(newWH)

OldNewWH <- bind_rows(newWH, oldWH) |> 
  select(-parcel_area)

rm(ls = ceqa_docs)

narrow_planned <- plannedWH |> 
  select(sch_number, ceqa_url, parcel_area, county, project, geometry)

sf_use_s2(FALSE)

## PAUSE HERE for googlesheets Authentication

planned_list4antiJoin <- plannedWH |> 
  select(sch_number) |> 
  st_set_geometry(value = NULL) |> 
  distinct() 

OldNewWH_antiJoin <- OldNewWH |> 
  select(sch_number) |> 
  st_set_geometry(value = NULL) |> 
  distinct() #|> 

##Note, these are all Central Valley warehouses from EA018 and EA078 class from 2023
MissingWHList <-  planned_list4antiJoin |> 
  anti_join(OldNewWH_antiJoin) |> 
  left_join(plannedWH) |> 
  select(sch_number, geometry) |> 
  left_join(recent_CEQA_WH_w_NODs, by = 'sch_number') |> 
  filter(!is.na(project))

GreenValley <-  planned_list4antiJoin |> 
  anti_join(OldNewWH_antiJoin) |> 
  left_join(plannedWH)  |> 
  filter(sch_number == '1989032707') |> 
  select(sch_number, project, ceqa_url, recvd_date, document_type, stage_pending_approved, geometry)

##Fix missing Warehouses
OldNewWH2 <- bind_rows(OldNewWH, GreenValley, MissingWHList)

##antijoin check2
planned_list4antiJoin |> 
  anti_join(OldNewWH2)

##Add BIG Barstow

BIG <- sf::st_read(dsn= 'C:/Dev/CEQA_Tracker/OutlierCEQA/Barstow International Gateway.geojson') 
area2 <- as.numeric(st_area(BIG))
BIG_ceqa <- read_csv('C:/Dev/CEQA_Tracker/OutlierCEQA/BIG_Ceqanet_info.csv') |> 
  janitor::clean_names() |>
  filter(document_type== 'NOP') |> 
  mutate(recvd_date = lubridate::mdy(received)) |> 
  select(sch_number, lead_agency_name, lead_agency_title,
         project_title, received, document_portal_url, counties, cities,
         document_type, recvd_date) |> 
  mutate(year_rcvd = lubridate::year(recvd_date),
         stage_pending_approved = document_type,
         parcel_area = area2) |> 
  rename(ceqa_url = document_portal_url) |> 
  select(sch_number, ceqa_url, parcel_area, recvd_date, document_type,
         project_title) |> 
  rename(project = project_title) |> 
  mutate(stage_pending_approved = 'NOP')
  
BIG2 <- BIG |> 
  bind_cols(BIG_ceqa) |> 
  select(-name)

  ## update CEQA info

names(OldNewWH2)
names(BIG2)

plannedWH2 <- bind_rows(OldNewWH2, BIG2) |> 
  mutate(category = ifelse(stage_pending_approved != 'Approved', 'CEQA Review', 'Approved'))

area <- as.numeric(st_area(plannedWH2))

plannedWH2$parcel_area <- round(area*10.7639, -1)

rm(ls = OldNewWH2, BIG2, BIG, oldWH, OldNewWH, OldNewWH_antiJoin)
rm(ls = newWH, newWH7, newWH_list, BIG_ceqa, GreenValley, MissingWHList)
## Industrial 

distinct_SCH <- plannedWH2 |> 
  select(sch_number) |> 
  st_set_geometry(value = NULL) |> 
  group_by(sch_number) |> 
  summarize(count = n())

#Import county shapefile
###FIXME 
##Currently tigris is broken due to Trump
#CA_counties <- tigris::counties(state = 'CA', cb = T, year = 2023) |> 
#  st_transform(crs = 4326) |> 
#  select(NAME) |> 
#  rename(county = NAME)

CA_counties <- arcgislayers::arc_read('https://tigerweb.geo.census.gov/arcgis/rest/services/TIGERweb/State_County/MapServer/5',
                                      where = "STATE = '06'") |> 
  st_transform(crs = 4326) |> 
  select(BASENAME) |> 
  rename(county = BASENAME)

CA_cities <- places(state = 'CA', cb = T, #pull for California, lower detail level
                    year = 2024, 
                    progress_bar = FALSE) |> 
  st_transform(crs = 4326) |> 
  select(NAME) |> 
  rename(city_CDP = NAME)

#types <- plannedWH2 |> 
#  st_set_geometry(value = NULL) |> 
#  group_by(document_type) |> 
#  summarize(count = n())

names(plannedWH2)

#tracked_warehouses3 <- tracked_warehouses2 |> 
#  left_join(industrial_most_recent) |> 
#  select(project, ceqa_url, sch_number, parcel_area, recvd_date, document_type,
#         stage_pending_approved, year_rcvd, category, lead_agency_title) |> 
#  mutate(year_rcvd = year(recvd_date)) #|> 

#FIXME anomaly projects - none
#anomaly_projects <- plannedWH2 |> 
#  filter(is.na(recvd_date)) #|> 
#  mutate(recvd_date = case_when(
#    project == 'Freedom Business Park' ~ mdy('10/23/2023'),
#    project == 'Oak Valley Town Center' ~ mdy('8/26/2022'),
#    project == 'Project Viento' ~ mdy('2/23/2022'),
#    project == 'The HUB Industrial Development' ~ mdy('1/30/2023')
#  )) |> 
#  mutate(year_rcvd = year(recvd_date),
#         document_type = case_when(
#           project == 'Freedom Business Park' ~ 'NOP',
#           project == 'Oak Valley Town Center' ~ 'NOD',
#           project == 'Project Viento' ~ 'ADM',
#           project == 'The HUB Industrial Development' ~ 'NOD'
#        )) 

resubmittedWHList <- ('https://ceqanet.opr.ca.gov/2023100642')

tracked_warehouses4 <- plannedWH2 |> 
  #filter(!is.na(recvd_date)) |> 
  #bind_rows(anomaly_projects) |> 
  filter(ceqa_url %ni% resubmittedWHList)|> 
 #left_join(built_WH_june2024, by = c('project' = 'apn')) |> 
#  mutate(category = ifelse(is.na(category2), category, category2)) |> 
  mutate(document_type_bins = case_when(
    stage_pending_approved == 'Approved' ~ 'Approved',
    stage_pending_approved %in% c('ADM', 'NEG', 'NOD', 'FIN') ~ 'Approved',
    stage_pending_approved == 'NOI' ~ 'CEQA Review',
    stage_pending_approved == 'RIR' ~ 'CEQA Review',
    stage_pending_approved == 'RC' ~ 'CEQA Review',
    stage_pending_approved == 'SIR' ~ 'CEQA Review',
    stage_pending_approved == 'REV' ~ 'CEQA Review',
    stage_pending_approved == 'SBE' ~'CEQA Review',
    stage_pending_approved %in% c('MND', 'NOP', 'EIR') ~'CEQA Review',
    TRUE ~ stage_pending_approved
    )
  )

tracked_centroids_county <- st_centroid(tracked_warehouses4) |> 
  st_intersection(CA_counties) |> 
  st_set_geometry(value = NULL) 

tracked_centroids_places <- st_centroid(tracked_warehouses4) |> 
  st_intersection(CA_cities) |> 
  st_set_geometry(value = NULL)

tracked_warehouses5 <- tracked_warehouses4 |> 
  left_join(tracked_centroids_county)  |> 
  left_join(tracked_centroids_places) |> 
  select(-category)

rm(ls =  newWH_list, newWH7, tmp,
   county_stats1, county_stats2, industrial_most_recent,
   tracked_warehouses2, tracked_warehouses3,
   tracked_centroids_county, tracked_warehouses4, tracked_centroids_places
   )

##Trying to fix weird string encodings of project names
project_names <- tracked_warehouses5 |> 
  select(sch_number, project) |> 
  st_set_geometry(value = NULL)

wd <- getwd()
write.csv(project_names, 'project_names.csv')
clean_names <- read_csv('project_names.csv', locale = readr::locale(encoding = 'latin1')) |> 
  select(sch_number, project) |> 
  rename(project3 = project) |> 
  mutate(project3 = case_when(
    sch_number == '2024071016' ~ 'Menifee Business Park',
    sch_number == '2024030521' ~ 'Gallo Glass Company, PLN2023-0166', 
    sch_number == '2020080354' ~ 'Roseville 80, PLN19-0363',
    TRUE ~ project3)
    )

## Built SoCal warehouses - SCH list - need to map to APNs
builtWH_list1 <- c(2022030012, 2020090441, 2015081081,
                   2020050079, 2020049052, 2014011009,
                   2020019017, 2022110076, 2022060066,
                   2022040166, 2021090555, 2021020280,
                   2021010163, 2020040155, 2019070040,
                   2020059021, 2022020501, 2020059013,
                   2021020421
)

rejectWithdrawnList <- c(
  '2022060349', #Airport Gateway - withdrawn 2023
  '2020039038', #Moreno Valley Trade Center - rejected 2023
  '2021110304', #West Campus - rejected 2025
  '2021090378', #Beaumont Summit - rejected 2022
  '2022070365'#, #Eddie Jones Warehouse - rejected 2025
  #2023120462 Newland Simpson Road - blocked 2025
  #2013102053 Tracy Hills SP Commerce Project - blocked 2025
)

tracked_warehouses <- tracked_warehouses5 |> 
  left_join(clean_names, by = c('sch_number')) |> 
  select(-project) |> 
  rename(project = project3) |> 
  distinct() |> 
  st_make_valid() |> 
  mutate(status = case_when(
    sch_number %in% builtWH_list1 ~ 'Built',
    sch_number %in% rejectWithdrawnList ~ 'Blocked' 
    )
  )
##FIXME - Note that project is including non UTF-8 characters that aren't being fixed 
## Always use project3

rm(ls = anomaly_projects, antiJoinList, built_WH_june2024, clean_names, county_counts)
rm(ls = distinct_SCH, planned_list4antiJoin, plannedWH2, project_names, tempsf)
rm(ls = types, plannedWH_noCEQA, wh_Y_list, wh_missing_list, Y_N_WH)

unlink('CEQA_WH.geojson')
sf::st_write(tracked_warehouses, 'CEQA_WH.geojson')

#Import population data for app
county_pop <- readxl::read_excel('E-1_2025.xlsx', sheet = 'E-1 CountyState2025', skip = 5)

colnames(county_pop) <- c('county', 'pop2024', 'pop2025', 'pctChange')

# preprocess stats
county_stats1 <- tracked_warehouses |> 
  st_set_geometry(value = NULL) |> 
  group_by(county, document_type_bins) |> 
  summarize(count = n(), sum_area = sum(parcel_area), .groups = 'drop') |> 
  mutate(acres = round(sum_area/43560, 0)) |> 
  left_join(county_pop) |> 
  select(-pctChange) |> 
  mutate(WH_SF_per_capita = round(sum_area/pop2025, 1))

county_stats2 <- county_stats1 |> 
  group_by(county) |> 
  summarize(countAll = sum(count), acresAll = sum(acres),
            WH_SF_per_capitaAll = sum(WH_SF_per_capita),
            .groups = 'drop') 

county_stats <- left_join(county_stats1, county_stats2)

setwd(str_c(wd, '/CEQA_Tracker'))
getwd()
save.image('.RData')
setwd(wd)

