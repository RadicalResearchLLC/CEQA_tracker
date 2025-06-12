plannedWH_old <- sf::st_read(dsn = 'C:/Dev/CEQA_Tracker/OutlierCEQA/CEQA_WH.geojson') |> 
  filter(!is.na(sch_number))

pre2020 <- plannedWH_old |> 
  filter(sch_number < 2020000000)

ceqa_2020 <- plannedWH_old |> 
  filter(sch_number >= 2020000000) 

##check for proper ceqa joins
ceqa_2020_narrow <- ceqa_2020 |> 
  select(sch_number, geometry) |> 
  mutate(sch_number = as.numeric(sch_number)) |> 
  left_join(recent_CEQA_WH_w_NODs) 

##select proper joins
plannedCEQA <- ceqa_2020_narrow |> 
  filter(!is.na(project))

##select missing CEQA joins
missingCEQA_old <- ceqa_2020_narrow |> 
  filter(is.na(project)) |> 
  select(sch_number, geometry)

#fix outliers
FreedomBP <- read_csv('C:/Dev/CEQA_Tracker/OutlierCEQA/FreedomBP.csv') |> 
  janitor::clean_names() |>
  rename(project = project_title,
         ceqa_url = document_portal_url) |> 
  mutate(recvd_date = lubridate::mdy(received),
         Notes = 'listed as commercial',
         stage_pending_approved = 'NOP') |> 
  select(sch_number, project, ceqa_url, recvd_date, document_type, Notes, stage_pending_approved)

OntarioGP <-  read_csv('C:/Dev/CEQA_Tracker/OutlierCEQA/Ontario2050.csv') |> 
  janitor::clean_names() |>
  rename(project = project_title,
         ceqa_url = document_portal_url,
         Notes = document_title) |>
  filter(Notes == 'The Hub Industrial Development / PDEV21-047' &
         received == '3/23/2023') |> 
  mutate(recvd_date = lubridate::mdy(received),
         stage_pending_approved = 'Approved') |> 
  select(sch_number, project, ceqa_url, recvd_date, document_type, Notes, stage_pending_approved) |> 
  bind_rows(FreedomBP)

## Found 5 extra warehouse approvals under a single SCH number here
## Ontario General Plan projects
PDEV22_050 <- sf::st_read(dsn = 'C:/Dev/CEQA_Tracker/OutlierCEQA/PDEV22-050.geojson') |> 
  mutate(received = '8/28/2023')
PDEV22_025 <- sf::st_read(dsn = 'C:/Dev/CEQA_Tracker/OutlierCEQA/PDEV22-025.geojson')  |> 
  mutate(received = '4/3/2023')
PDEV22_033 <- sf::st_read(dsn = 'C:/Dev/CEQA_Tracker/OutlierCEQA/PDEV22-033.geojson') |> 
  mutate(received = '6/21/2024')
PDEV22_034 <- sf::st_read(dsn = 'C:/Dev/CEQA_Tracker/OutlierCEQA/PDEV22-034.geojson') |> 
  mutate(received = '11/22/2023')
PDEV22_040 <- sf::st_read(dsn = 'C:/Dev/CEQA_Tracker/OutlierCEQA/PDEV22-040.geojson') |> 
  mutate(received = '4/19/2024')
OntarioProjects1 <- bind_rows(PDEV22_025,PDEV22_033, PDEV22_034, PDEV22_040, PDEV22_050) 

#
OntProj2 <- read_csv('C:/Dev/CEQA_Tracker/OutlierCEQA/Ontario2050.csv') |> 
  janitor::clean_names() |>
  rename(project = project_title,
         ceqa_url = document_portal_url,
         Notes = document_title) |> 
  right_join(OntarioProjects1) |> 
  mutate(recvd_date = lubridate::mdy(received),
         stage_pending_approved = 'Approved') |> 
  select(sch_number, project, ceqa_url, recvd_date, document_type, received, Notes, stage_pending_approved) 

OntProjs <- OntarioProjects1 |> 
  left_join(OntProj2, by = 'received') |> 
  select(-name, -received)

rm(ls = plannedWH_old, ceqa_2020, ceqa_2020_narrow, OntarioProjects)
rm(ls = PDEV22_025, PDEV22_033, PDEV22_034, PDEV22_040, PDEV22_050)

fixedOld <- missingCEQA_old |> 
  left_join(OntarioGP) 

names(plannedCEQA)
names(fixedOld)
names(OntProj2)
oldWH7 <- bind_rows(plannedCEQA, fixedOld, OntProjs) 

rm(ls = plannedCEQA, fixedOld, OntProj2, OntProjs)
rm(ls = FreedomBP, OntarioGP) 
rm(ls = OntarioProjects1)

names(oldWH7)
names(pre2020)
names(newWH7)

schList <- pre2020 |> 
  pull(sch_number)
##Process old CEQA docs
oldCEQA <- read_csv('C:/Dev/CEQA_Tracker/OutlierCEQA/CEQA_industrial_2002_2019.csv') |> 
  janitor::clean_names() |> 
  dplyr::select(sch_number, project_title, received, document_portal_url, counties, cities,
                document_type) |> 
  mutate(recvd_date = lubridate::mdy(received),
         sch_number = as.numeric(sch_number)) |>
  filter(sch_number %in% schList) 

recent_CEQA_WH_w_NODs_old <- recent_CEQA_WH_w_NODs |> 
  filter(sch_number %in% schList)

oldCEQA_base <- oldCEQA |> 
  rename(project = project_title,
         ceqa_url = document_portal_url) |>
  bind_rows(recent_CEQA_WH_w_NODs_old) #|> 

oldCEQA_singular <- oldCEQA_base |> 
  group_by(sch_number) |> 
  summarize(recvd_date = max(recvd_date)) |> 
  left_join(oldCEQA_base) |> 
  select(sch_number, recvd_date, project, document_type, ceqa_url) |> 
  filter(ceqa_url %ni%  c('https://ceqanet.lci.ca.gov/2019119061/2',
                          'https://ceqanet.opr.ca.gov/2019099005/2')
  )

pre2020_tidy <- pre2020 |> 
  select(sch_number, parcel_area, geometry) |> 
  left_join(oldCEQA_singular) 

pre2020_complete <- pre2020_tidy |> 
  filter(!is.na(recvd_date))

pre2020_missingList <- pre2020_tidy |> 
  filter(is.na(recvd_date)) |> 
  pull(sch_number)

pre2020_toFix <- pre2020 |> 
  filter(sch_number %in% pre2020_missingList) #|> 

##import ceqa files for outliers

proj1 <- read_csv('C:/Dev/CEQA_Tracker/OutlierCEQA/OakValley.csv') |> 
  janitor::clean_names() |> 
  dplyr::select(sch_number, lead_agency_name, lead_agency_title,
                project_title, received, document_portal_url, counties, cities,
                document_type)

proj2 <- read_csv('C:/Dev/CEQA_Tracker/OutlierCEQA/DistrictSouthBay.csv') |> 
  janitor::clean_names() |> 
  dplyr::select(sch_number, lead_agency_name, lead_agency_title,
                project_title, received, document_portal_url, counties, cities,
                document_type)
proj3 <- read_csv('C:/Dev/CEQA_Tracker/OutlierCEQA/FreewayCorridor.csv') |> 
  janitor::clean_names() |> 
  dplyr::select(sch_number, lead_agency_name, lead_agency_title,
                project_title, received, document_portal_url, counties, cities,
                document_type)
proj4 <- read_csv('C:/Dev/CEQA_Tracker/OutlierCEQA/WalnutBP.csv') |> 
  janitor::clean_names() |> 
  dplyr::select(sch_number, lead_agency_name, lead_agency_title,
                project_title, received, document_portal_url, counties, cities,
                document_type)
proj5 <- read_csv('C:/Dev/CEQA_Tracker/OutlierCEQA/RioVistaSP.csv') |> 
  janitor::clean_names() |> 
  dplyr::select(sch_number, lead_agency_name, lead_agency_title,
                project_title, received, document_portal_url, counties, cities,
                document_type)
proj6 <- read_csv('C:/Dev/CEQA_Tracker/OutlierCEQA/ProjectViento.csv') |> 
  janitor::clean_names() |> 
  dplyr::select(sch_number, lead_agency_name, lead_agency_title,
                project_title, received, document_portal_url, counties, cities,
                document_type)
proj7 <- read_csv('C:/Dev/CEQA_Tracker/OutlierCEQA/NinthVineyard.csv') |> 
  janitor::clean_names() |> 
  dplyr::select(sch_number, lead_agency_name, lead_agency_title,
                project_title, received, document_portal_url, counties, cities,
                document_type)

fixCEQA <- bind_rows(proj1, proj2, proj3, proj4, proj5, proj6, proj7) |> 
  mutate(recvd_date = lubridate::mdy(received),
         sch_number = as.numeric(sch_number)) |> 
  select(sch_number, recvd_date, project_title, document_type, document_portal_url) |> 
  rename(project = project_title,
         ceqa_url = document_portal_url) 

pre2020Fixed <- pre2020_toFix |> 
  select(sch_number, parcel_area, geometry) |> 
  left_join(fixCEQA) 
  

pre2020_full <- bind_rows(pre2020_complete, pre2020Fixed) |> 
  mutate(document_type = case_when(
    sch_number == '2019080276' ~ 'NOD',
    sch_number == '2017051060' ~ 'NOD',
    sch_number == '2006121062' ~ 'NOD',
    sch_number == '2009091089' ~ 'NOD',
    sch_number == '2019039033' ~ 'NOD',
    sch_number == '2018011008' ~ 'NOD',
    sch_number == '2002071089' ~ 'NOD',
    sch_number == '2008071060' ~ 'NOD',
    sch_number == '2012071058' ~ 'NOD',
    sch_number == '1995052002' ~ 'NOD',
    sch_number == '2012071058' ~ 'NOD',
    sch_number == '2019039133' ~ 'NOD',
    sch_number == '2016081061' ~ 'NOD',
    sch_number == '2018091021' ~ 'NOD',
    sch_number == '2019049079' ~ 'NOD',
    sch_number == '2019050018' ~ 'NOD',
    sch_number == '2019059042' ~ 'NOD',
    sch_number == '2019060002' ~ 'NOD',
    sch_number == '2019090706' ~ 'NOD',
    sch_number == '2019100514' ~ 'NOD',
    sch_number == '2019110418' ~ 'NOD',
    sch_number == '2012021045' ~ 'NOD',
    sch_number == '2019090335' ~ 'EIR',
    sch_number == '2019100297' ~ 'NOD',
    TRUE ~ document_type
    )
  ) |> 
  mutate(stage_pending_approved = case_when(
    document_type %in% c('NOD', 'ADM', 'NEG', 'SBE', 'SIR') ~ 'Approved',
    document_type %in% c('FIN', 'MND') & recvd_date < mdy('01-01-2024') ~ 'Approved',
    sch_number %in% c('2013061037', '2019090335', '2019080101') ~ 'Approved',
    TRUE ~ document_type
    )
  ) 

rm(ls = pre2020_complete, pre2020_tidy, pre2020_toFix, pre2020Fixed)
rm(ls= proj1, proj2, proj3, proj4,proj5,proj6,proj7)
rm(ls = oldCEQA, oldCEQA_singular,pre2020, fixCEQA,missingCEQA_old)

names(oldWH7)
names(pre2020_full)

oldWH <- bind_rows(oldWH7, pre2020_full)
rm(ls = oldWH7, pre2020_full)
