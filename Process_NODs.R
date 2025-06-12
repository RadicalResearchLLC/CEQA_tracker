##State NODs list
##Created by Mike McCarthy, Radical Research LLC
##First created June 2025
##Last modified June 2025

##called by CEQA_WH_tracker.R

NOD_dir <- 'C:/Dev/CEQA_Tracker/NODs/'

setwd(NOD_dir)

pre2022 <- read_csv('NODs_pre2022.csv') |> 
  janitor::clean_names() |> 
  select(sch_number, nod_agency, nod_approved_by_lead_agency, nod_approved_date,
         nod_significant_environmental_impact, nod_statement_of_overriding_considerations_adopted,
         nod_mitigation_measures, nod_mitigation_reporting_or_monitoring_plan)
NOD2022_23 <- read_csv('NODs_2022_2023.csv') |> 
  janitor::clean_names() |> 
  select(sch_number, nod_agency, nod_approved_by_lead_agency, nod_approved_date,
         nod_significant_environmental_impact, nod_statement_of_overriding_considerations_adopted,
         nod_mitigation_measures, nod_mitigation_reporting_or_monitoring_plan)
NODs_2024 <- read_csv('NODs_2024.csv') |> 
  janitor::clean_names() |> 
  select(sch_number, nod_agency, nod_approved_by_lead_agency, nod_approved_date,
         nod_significant_environmental_impact, nod_statement_of_overriding_considerations_adopted,
         nod_mitigation_measures, nod_mitigation_reporting_or_monitoring_plan)

# UPDATE ME
NODs_2025 <- read_csv('NODs_2025_0604.csv') |> 
  janitor::clean_names() |> 
  select(sch_number, nod_agency, nod_approved_by_lead_agency, nod_approved_date,
         nod_significant_environmental_impact, nod_statement_of_overriding_considerations_adopted,
         nod_mitigation_measures, nod_mitigation_reporting_or_monitoring_plan)

names(pre2022)
names(NOD2022_23)
names(NODs_2024)
names(NODs_2025)

NODs <- bind_rows(pre2022, NOD2022_23, NODs_2024, NODs_2025) 

industrial_NODs <- industrial_projects |> 
  select(sch_number, lead_agency_name) |> 
  inner_join(NODs) |> 
  distinct() |> 
  mutate(date1 = lubridate::mdy(nod_approved_date)) 

rm(ls= NODs, NODs_2024, NODs_2025, pre2022, NOD2022_23)

setwd(wd)
