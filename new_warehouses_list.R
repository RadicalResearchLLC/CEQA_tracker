library(sf)
library(tidyverse)

wd <- getwd()
wh_directory <- 'C:/Dev/CEQA_Tracker/wh_geojson/'

list_geojson <- list.files(wh_directory)

length(list_geojson)

tmp <- st_sfc()
class(tmp)[1] <- 'sfc_POLYGON'
newWH_list <- st_sf(sch_number = integer(0), 
                    project = character(0),
                    ceqa_url = character(0),
                    stage_pending_approved = character(0),
                    parcel_area = integer(0),
                    category = character(0),
                    geometry = tmp, crs = 4326)

setwd(wh_directory)

for(i in 1:length(list_geojson)) {
 print(i)
 print(list_geojson[i])  
 tempsf <- st_read(dsn = list_geojson[i])
 newWH_list <- bind_rows(newWH_list, tempsf)
}
getwd()
setwd(wd)

##pass in mostRecentCEQAList from document call
#industrial_projects <- read_csv(mostRecentCEQAList) |>   
#  janitor::clean_names() |> 
#  dplyr::select(sch_number, lead_agency_name, lead_agency_title,
#                project_title, received, document_portal_url, counties, cities,
#                document_type) |> 
#  mutate(recvd_date = lubridate::mdy(received))

#industrial_most_recent <- industrial_projects |> 
#  group_by(sch_number) |> 
#  summarize(recvd_date = max(recvd_date)) |> 
#  left_join(industrial_projects) |> 
#  filter(document_portal_url != 'https://ceqanet.opr.ca.gov/2017121007/4')

newWH7 <- newWH_list |> 
  left_join(industrial_most_recent, by = c('sch_number')) |> 
  select(sch_number, project_title, document_portal_url, document_type)  |> 
  rename(project = project_title,
         ceqa_url = document_portal_url
         ) #|> 
    #mutate(parcel_area = area*10.7639) 

##Check for duplicates
industrial_multiples <- industrial_most_recent |> 
  select(sch_number) |> 
  group_by(sch_number) |> 
  summarize(count = n(), .groups = 'drop') |> 
  filter(count > 1)

rm(ls = industrial_multiples, industrial_projects)

gs = 'https://docs.google.com/spreadsheets/d/1Dw-HLvt5AzTY8or3ZFiDdlXX5Xv1u-ASD9t-153FwNc/edit#gid=0'
Y_N_WH <- read_sheet(gs, sheet = 'Y_N_WH') |> 
  filter(Y_N_WH == 'Y') |> 
  select(sch_number, Notes)

wh_Y_list <- industrial_most_recent |> 
  inner_join(Y_N_WH, by = 'sch_number') |> 
  rename(project = project_title,
         lead_agency = lead_agency_title,
         ceqa_url = document_portal_url) |> 
  select(sch_number, project, recvd_date, document_type, lead_agency, Notes, ceqa_url) |> 
  distinct() 



#sheet_append(gs, sheet = 'Y_N_WH', data = wh_missing_list)
