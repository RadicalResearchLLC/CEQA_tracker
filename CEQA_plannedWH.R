##SJV Warehouse Map App V1.1
##Created by Mike McCarthy, Radical Research LLC
##First created February 2023
##Last modified November 2023
##This script acquires and tidy parcel data for the app

library(googlesheets4)
library(sf)
library(leaflet)
library(tidyverse)

CentralValleyPlanned <- read_sheet('https://docs.google.com/spreadsheets/d/1Dw-HLvt5AzTY8or3ZFiDdlXX5Xv1u-ASD9t-153FwNc/edit#gid=0',
                                   sheet = 'WarehouseCoordinates') |>  
  select(1:3) |> 
  # rename(lng = longitude, lat = latitude) %>% 
  filter(!is.na(lng)) |> 
  filter(!is.na(lat)) |> 
  filter(ProjectName != 'Administrative Use Permit No. PA-2300022') |> 
  filter(ProjectName != 'Tracy Costco Depot Annex Project')

CEQA_WH <- CentralValleyPlanned %>%
  st_as_sf(coords = c('lng', 'lat'), crs = 4326) %>%
  group_by(ProjectName) %>%
  summarise(geom = st_combine(geometry)) %>%
  st_cast("POLYGON")

leaflet() |> 
  addTiles()

#leaflet() |> 
#  addProviderTiles(provider = providers$CartoDB.Positron) #|> 
#  addPolygons(data = CEQA_WH,
#              color = 'black',
#              fillOpacity = 0.6,
#              weight = 1,
#              label = ~ProjectName)

Central_WH_tracked <- read_sheet('https://docs.google.com/spreadsheets/d/1Dw-HLvt5AzTY8or3ZFiDdlXX5Xv1u-ASD9t-153FwNc/edit#gid=0',
  sheet = 'PlannedCentralWarehouses') |> 
  janitor::clean_names() |> 
  select(project_name, sch_number, url_to_ceqa, size_sq_ft,
         category, stage_pending_approved) |> 
  rename(project = project_name, ceqa_url = url_to_ceqa)

centralPlanning <- CEQA_WH |> 
  left_join(Central_WH_tracked, by = c('ProjectName' = 'project')) |> 
  filter(!is.na(sch_number)) |> 
  rename(project = ProjectName) |> 
  select(1, 4, 3, 7, 6, 5, 2) |> 
  select(-size_sq_ft)

sf_use_s2(FALSE)
areaM2 <- as.numeric(st_area(centralPlanning))

centralPlanning$parcel_area <- areaM2*10.7639


st_geometry(centralPlanning) <- 'geometry'

plannedWH <- sf::st_read('https://raw.githubusercontent.com/RadicalResearchLLC/PlannedWarehouses/main/CEQA_WH.geojson') 
wh_url <- 'https://raw.githubusercontent.com/RadicalResearchLLC/WarehouseMap/main/WarehouseCITY/geoJSON/comboFinal.geojson'

names(plannedWH)
names(centralPlanning)
tracked_warehouses <- rbind(plannedWH, centralPlanning)
warehouses <- sf::st_read(dsn = wh_url) 

names(plannedWH)
rm(ls = Central_WH_tracked, centralPlanning, CentralValleyPlanned,
   CEQA_WH, plannedWH, areaM2)
