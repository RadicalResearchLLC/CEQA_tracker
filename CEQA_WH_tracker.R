##State Warehouse Tracker
##Created by Mike McCarthy, Radical Research LLC
##First created June 2024
##Last modified September 2024
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
                        'The Oasis at Indio Project')) |> 
  filter(!is.na(sch_number))


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

built_WH_june2024 <- sf::st_read('built_listJune2024.geojson') |> 
  select(apn) |> 
  st_set_geometry(value = NULL) |> 
  distinct() |> 
  mutate(category2 = 'Built')

#wh_url <- 'https://raw.githubusercontent.com/RadicalResearchLLC/WarehouseMap/main/WarehouseCITY/geoJSON/comboFinal.geojson'
#approved_warehouses <- sf::st_read(dsn = wh_url) |> 
#  filter(category == 'Approved') |> 
#  mutate(sch_number = str_sub(class, str_length(class)-9, str_length(class))) #|> 
#  filter(sch_number %in% builtWH_list)
#warehouses <- sf::st_read(dsn = wh_url) 

mostRecentCEQAList <- str_c(CEQA_dir, 'CEQA_industrial_101424.csv')

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
  bind_rows(antiJoinList) |> 
  mutate(year_rcvd = lubridate::year(recvd_date)) |> 
  mutate(year_rcvd = ifelse(year_rcvd < 2018, 2018, year_rcvd))

distinct_SCH <- tracked_warehouses2 |> 
  select(sch_number) |> 
  st_set_geometry(value = NULL) |> 
  group_by(sch_number) |> 
  summarize(count = n())

#Import county shapefile
CA_counties <- sf::st_read(dsn = 'C:/Dev/CA_spatial_data/CA_counties') |> 
  st_transform(crs = 4326) |> 
  select(NAME) |> 
  rename(county = NAME)

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

tracked_warehouses4 <- tracked_warehouses3 |> 
  filter(!is.na(recvd_date)) |> 
  bind_rows(anomaly_projects) |> 
  left_join(built_WH_june2024, by = c('project' = 'apn')) |> 
  mutate(category = ifelse(is.na(category2), category, category2)) |> 
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
  ) |> 
  select(-category2)

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
  rename(project3 = project)

tracked_warehouses <- tracked_warehouses |> 
  left_join(clean_names, by = c('sch_number')) |> 
  distinct() |> 
  st_make_valid()
  
#source('BuiltWH_intersect.R')
##FIXME in Warehouse CITY not here

unlink('CEQA_WH.geojson')
sf::st_write(tracked_warehouses, 'CEQA_WH.geojson')

rm(ls = anomaly_projects, antiJoinList, built_WH_june2024, clean_names, county_counts)
rm(ls = distinct_SCH, planned_list4antiJoin, plannedWH2, project_names, tempsf)
rm(ls = types, plannedWH_noCEQA, wh_Y_list, wh_missing_list, Y_N_WH)

setwd(str_c(wd, '/CEQA_Tracker'))
save.image('.RData')
setwd(wd)

