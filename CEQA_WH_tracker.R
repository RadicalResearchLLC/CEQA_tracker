##State Warehouse Tracker
##Created by Mike McCarthy, Radical Research LLC
##First created June 2024
##Last modified January 2025
##This script is intended to be the evergreen CEQA tracking app
## (1) Key info should be the list of CEQA Projects that are warehouses
## (2) Spatial Polygons
## (3) Current status (to the extent I can track it)

'%ni%' <- Negate('%in%') ## not in operator

library(googlesheets4)
library(sf)
library(leaflet)
library(tidyverse)

CEQA_dir <- 'C:/Dev/CEQA_Tracker/CEQA_docs/'

plannedWH <- sf::st_read('https://github.com/RadicalResearchLLC/CEQA_tracker/raw/main/CEQA_WH.geojson') |> 
  st_transform(crs = 4326) |> 
  filter(project %ni% c('South Perris Industrial Project',
                        'Northern Gateway Commerce Center',
                        'The Oasis at Indio Project'#,
                       # 'Tracy Northeast Business Park Project')
                       )) |> 
  filter(!is.na(sch_number)) #|> 
 # filter(sch_number != '2025010343')

sf_use_s2(FALSE)

## Built SoCal warehouses - SCH list - need to map to APNs
builtWH_list1 <- c(2022030012, 2020090441, 2015081081,
                  2020050079, 2020049052, 2014011009,
                  2020019017, 2022110076, 2022060066,
                  2022040166, 2021090555, 2021020280,
                  2021010163, 2020040155, 2019070040,
                  2020059021, 2022020501, 2020059013,
                  2021020421
)

#
##FIXME
mostRecentCEQAList <- str_c(CEQA_dir, 'CEQA_industrial_2019_011125.csv')

industrial_projects <- read_csv(mostRecentCEQAList) |>   
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

source('C:/Dev/CEQA_Tracker/new_warehouses_list.R')

## PAUSE HERE for googlesheets Authentication

planned_list4antiJoin <- plannedWH |> 
  select(sch_number) |> 
  st_set_geometry(value = NULL) |> 
  distinct() 

names(wh_missing_list)
#names(newWH_list)
antiJoinList <- newWH7 |> 
  #select(sch_number) |>
  #st_set_geometry(value = NULL) |> 
  anti_join(planned_list4antiJoin) |>
  left_join(industrial_most_recent) |> 
  select(project, ceqa_url, sch_number, parcel_area,
         recvd_date, document_type, category,
         stage_pending_approved, lead_agency_title, geometry) |> 
  mutate(year_rcvd = lubridate::year(recvd_date)) 

names(antiJoinList)

##FIXME - add error handling for no new warehouses?

#antiJoinList3 <- sf::st_read('C:/Dev/CEQA_Tracker/wh_geojson/2025010343.geojson') |> 
#  mutate(year_rcvd = 2025,
#         lead_agency_title = 'City of Tracy',
#         document_type = 'NOP',
#         stage_pending_approved = 'NOP',
#         recvd_date = as.Date('2025-01-03'),
#         category = 'CEQA Review')

antiJoinList2 <- antiJoinList |> 
  filter(sch_number != 1989032707) #|> 
  #mutate(project = 'Green Valley Specific Plan',
  #       ceqa_url = 'https://ceqanet.opr.ca.gov/1989032707/',
  #       sch_number = 1989032707,
  #       recvd_date = as.Date('1989-03-27'),
  #       document_type = 'NOD',
  #       category = 'Approved',
  #       stage_pending_approved = 'Approved',
  #       lead_agency_title = 'City of Perris') |> 
#  bind_rows(antiJoinList3) |> 
  #select(-category)
names(antiJoinList2)


# Fix missing CEQA info
plannedWH_noCEQA <- plannedWH |> 
  filter(is.na(recvd_date)) |> 
  select(project, sch_number, parcel_area, stage_pending_approved) |> 
  left_join(industrial_most_recent, by = 'sch_number') |> 
  select(project, document_portal_url, sch_number, parcel_area, recvd_date, 
    document_type) |> 
    mutate(year_rcvd = lubridate::year(recvd_date),
           category = 'CEQA Review',
           stage_pending_approved = document_type) |> 
  rename(ceqa_url = document_portal_url)

names(plannedWH)
names(plannedWH_noCEQA)

plannedWH2 <- plannedWH |> 
  filter(!is.na(recvd_date)) |> 
  bind_rows(plannedWH_noCEQA)

tracked_warehouses2 <- plannedWH2 |> 
  left_join(industrial_most_recent) |> 
  select(project, ceqa_url, sch_number, parcel_area,
    recvd_date, document_type, category,
    stage_pending_approved, lead_agency_title, geometry) |>
  bind_rows(antiJoinList2) |> 
  mutate(year_rcvd = lubridate::year(recvd_date)) |> 
  mutate(year_rcvd = ifelse(year_rcvd < 2018, 2018, year_rcvd))

distinct_SCH <- tracked_warehouses2 |> 
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

#Import population data
county_pop <- readxl::read_excel('E-1_2024.xlsx', sheet = 'E-1 CountyState2024', skip = 5)

colnames(county_pop) <- c('county', 'pop2023', 'pop2024', 'pctChange')


WHPal <- colorFactor(palette = 'Reds',
                      domain = tracked_warehouses2$year_rcvd)

leaflet() |> 
  addProviderTiles(providers$Esri.WorldImagery, group = 'Imagery') |> 
 # addProviderTiles(providers$OpenRailwayMap, group = 'Rail') |> 
  addProviderTiles(providers$CartoDB.Positron, group = 'Basemap')  |>
  addLayersControl(
    baseGroups = c('Basemap', 'Imagery'),
    overlayGroups = c('Planned Warehouses'), 
    options = layersControlOptions(collapsed = FALSE),
    position = 'topright'
  ) |> 
  addPolygons(data = tracked_warehouses2,
              color = ~WHPal(year_rcvd),
              weight = 3, 
              label = ~paste(sch_number, stage_pending_approved),
              fillOpacity = 0.7
              ) |> 
  addLegend(data = tracked_warehouses2, 
            title = 'Year',
            pal = WHPal,
            values = ~year_rcvd,
            position = 'bottomleft')

types <- tracked_warehouses2 |> 
  st_set_geometry(value = NULL) |> 
  group_by(document_type) |> 
  summarize(count = n())

names(plannedWH)
rm(ls = Central_WH_tracked, centralPlanning, CentralValleyPlanned,
   CEQA_WH, plannedWH, areaM2)

tracked_warehouses3 <- tracked_warehouses2 |> 
  left_join(industrial_most_recent) |> 
  select(project, ceqa_url, sch_number, parcel_area, recvd_date, document_type,
         stage_pending_approved, year_rcvd, category, lead_agency_title) |> 
  mutate(year_rcvd = year(recvd_date)) #|> 

#Fix anomaly projects
anomaly_projects <- tracked_warehouses3 |> 
  filter(is.na(recvd_date)) |> 
  mutate(recvd_date = case_when(
    project == 'Freedom Business Park' ~ mdy('10/23/2023'),
    project == 'Oak Valley Town Center' ~ mdy('8/26/2022'),
    project == 'Project Viento' ~ mdy('2/23/2022'),
    project == 'The HUB Industrial Development' ~ mdy('1/30/2023')
  )) |> 
  mutate(year_rcvd = year(recvd_date),
         document_type = case_when(
           project == 'Freedom Business Park' ~ 'NOP',
           project == 'Oak Valley Town Center' ~ 'NOD',
           project == 'Project Viento' ~ 'ADM',
           project == 'The HUB Industrial Development' ~ 'NOD'
         )) 

resubmittedWHList <- ('https://ceqanet.opr.ca.gov/2023100642')

tracked_warehouses4 <- tracked_warehouses3 |> 
  filter(!is.na(recvd_date)) |> 
  bind_rows(anomaly_projects) |> 
  filter(ceqa_url %ni% resubmittedWHList)|> 
 #left_join(built_WH_june2024, by = c('project' = 'apn')) |> 
#  mutate(category = ifelse(is.na(category2), category, category2)) |> 
  mutate(document_type_bins = case_when(
    document_type == 'ADM' ~ 'Other',
    document_type == 'NEG' ~ 'Other',
    document_type == 'NOD' ~ 'Other',
    document_type == 'NOI' ~ 'Other',
    document_type == 'RIR' ~ 'Other',
    document_type == 'RC' ~ 'Other',
    document_type == 'SIR' ~ 'Other',
    document_type == 'REV' ~ 'Other',
    document_type == 'SBE' ~'Other',
    TRUE ~ document_type
    )
  )

tracked_centroids <- st_centroid(tracked_warehouses4) |> 
  st_intersection(CA_counties) |> 
  st_set_geometry(value = NULL) 

tracked_warehouses <- tracked_warehouses4 |> 
  left_join(tracked_centroids)  |> 
  select(-category)

county_counts <- tracked_warehouses |> 
  select(county, parcel_area) |> 
  st_set_geometry(value = NULL) |> 
  group_by(county) |> 
  summarize(count = n(), area = round(sum(parcel_area)/43560, 0))
 
county_stats1<- tracked_centroids |> 
  group_by(county, document_type_bins) |> 
  summarize(count = n(), sum_area = sum(parcel_area), .groups = 'drop') |> 
  mutate(acres = round(sum_area/43560, 0)) |> 
  left_join(county_pop) |> 
  select(-pctChange) |> 
  mutate(WH_SF_per_capita = round(sum_area/pop2024, 1))

county_stats2 <- county_stats1 |> 
  group_by(county) |> 
  summarize(countAll = sum(count), acresAll = sum(acres),
            WH_SF_per_capitaAll = sum(WH_SF_per_capita),
            .groups = 'drop') 

county_stats <- left_join(county_stats1, county_stats2)

CA_stats <- county_stats2 |> 
  summarize(count = sum(countAll),
            acres = sum(acresAll)) |> 
  mutate(totalPop = 39128162) |> 
  mutate(WH_SF_per_capita = round(acres*43560/totalPop, 1),
         Acres_per_county = round(acres/58, 0)) 

ggplot() +
  geom_col(data = county_stats, aes(x = acres, y = reorder(county, acresAll), 
                                    fill = document_type_bins)) +
  theme_bw() +
  labs(x = 'New/Proposed Warehouse area (Acres)', y = '', title = 'Acreage of CEQA warehouse projects 2020-2024',
       fill = 'CEQA document',
       caption = str_c('Project data from CEQANET through ', Sys.Date())
       ) +
  scale_fill_manual(values = c('red', 'darkred', 'grey40', 'black', 'maroon'), 
                    breaks = c('MND', 'EIR', 'NOP', 'FIN', 'Other')
                    ) +
  #annotate('text', x = 2500, y = 7, label = 'Median - 10 acres', color = 'blue') +
  annotate('text', x = 3300, y = 10, label = str_c('Mean - ', CA_stats$Acres_per_county,
                                                   ' acres')) +
  geom_vline(xintercept = CA_stats$Acres_per_county, color = 'grey20', linetype = 'twodash') + 
  scale_x_continuous(label = scales::comma_format(),
                     breaks = c(0, 2000, 4000, 6000, 8000, 10000, 12000))

month_day <- str_c(lubridate::month(Sys.Date()), '_',
                   lubridate::day(Sys.Date())
)

ggsave(str_c('CEQA_warehouses', month_day, '.png'), dpi = 300)

ggplot() +
  geom_col(data = county_stats, aes(x = WH_SF_per_capita, 
    y = reorder(county, WH_SF_per_capitaAll), 
    fill = document_type_bins)) +
  theme_bw() +
  labs(x = 'Additional Warehouse SQ FT per capita', y = '', title = 'CEQA warehouse project 2020-2024 SQ FT per resident',
       caption = str_c('Warehouse data from CEQANET through ', Sys.Date(), '\n',
       'Population from CA DoF Table E-1'),
       fill = 
         'CEQA document') +
  scale_fill_manual(values = c('red', 'darkred', 'grey40', 'black', 'maroon'), 
                   breaks = c('MND', 'EIR', 'NOP', 'FIN', 'Other')
  ) +
  #annotate('text', x = 90, y = 7, label = 'Median - 1 SQ FT per capita', color = 'blue') +
  annotate('text', x = 110, y = 10, 
           label = str_c('Mean - ', CA_stats$WH_SF_per_capita, ' SQ FT per capita'), 
           color = 'black') +
  geom_vline(xintercept = CA_stats$WH_SF_per_capita, color = 'grey20', linetype = 'twodash')

ggsave(str_c('CEQA_per_capita',  month_day, '.png'), dpi = 300)

rm(ls =  newWH_list, newWH7, tmp,
   county_stats1, county_stats2, industrial_most_recent,
   tracked_warehouses2, tracked_warehouses3,
   tracked_centroids, tracked_warehouses4)

##Trying to fix weird string encodings of project names
project_names <- tracked_warehouses |> 
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

tracked_warehouses <- tracked_warehouses |> 
  left_join(clean_names, by = c('sch_number')) |> 
  select(-project) |> 
  rename(project = project3) |> 
  distinct() |> 
  st_make_valid()
##FIXME - Note that project is including non UTF-8 characters that aren't being fixed 
## Always use project3

unlink('CEQA_WH.geojson')
sf::st_write(tracked_warehouses, 'CEQA_WH.geojson')

rm(ls = anomaly_projects, antiJoinList, built_WH_june2024, clean_names, county_counts)
rm(ls = distinct_SCH, planned_list4antiJoin, plannedWH2, project_names, tempsf)
rm(ls = types, plannedWH_noCEQA, wh_Y_list, wh_missing_list, Y_N_WH)

tracked_warehouses |> 
  st_set_geometry(value = NULL) |> 
  filter(!is.na(year_rcvd)) |> 
  filter(year_rcvd > 2020) |> 
  ggplot() +
  geom_col(aes(x = parcel_area, 
               y = as.factor(year_rcvd), 
              fill = document_type_bins)) +
  theme_bw() +
  labs(x = 'Warehouse area (sq.ft.)',
       y = 'Year',
       fill = 'CEQA type',
       caption = 'Project data from CEQANET through January 11, 2025')  +
  scale_fill_manual(values = c('red', 'darkred', 'grey40', 'black', 'maroon'), 
                   breaks = c('MND', 'EIR', 'NOP', 'FIN', 'Other')) +
  scale_x_continuous(labels = scales::comma_format())

ggsave('warehouses_by_yr.png')

setwd(str_c(wd, '/CEQA_Tracker'))
getwd()
save.image('.Rdata')
setwd(wd)

