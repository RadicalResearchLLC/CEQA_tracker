##State NODs list
##Created by Mike McCarthy, Radical Research LLC
##First created June 2025
##Last modified February 2026

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
NODs_2024_25 <- read_csv('NODs_2024_25.csv') |> 
  janitor::clean_names() |> 
  select(sch_number, nod_agency, nod_approved_by_lead_agency, nod_approved_date,
         nod_significant_environmental_impact, nod_statement_of_overriding_considerations_adopted,
         nod_mitigation_measures, nod_mitigation_reporting_or_monitoring_plan)

# UPDATE ME
NODs_2026<- read_csv('NODs_020326.csv') |> 
  janitor::clean_names() |> 
  select(sch_number, nod_agency, nod_approved_by_lead_agency, nod_approved_date,
         nod_significant_environmental_impact, nod_statement_of_overriding_considerations_adopted,
         nod_mitigation_measures, nod_mitigation_reporting_or_monitoring_plan)

names(pre2022)
names(NOD2022_23)
names(NODs_2024_25)
names(NODs_2026)

NODs <- bind_rows(pre2022, NOD2022_23, NODs_2024_25, NODs_2026) 

NODs_per_SCH <- NODs |> 
  filter(nod_approved_by_lead_agency == 'Yes') |> 
  mutate(nod_approved_date = lubridate::mdy(nod_approved_date)) |> 
  group_by(sch_number) |> 
  summarize(count = n(), last_approval_date = last(nod_approved_date))

industrial_NODs <- industrial_projects |> 
  select(sch_number, lead_agency_name) |> 
  inner_join(NODs_per_SCH) |> 
  distinct() 

##check for NOD not lead agency
check_nonLeadAgency <- NODs |> 
  filter(nod_approved_by_lead_agency == 'No') |> 
  mutate(nod_approved_date = lubridate::mdy(nod_approved_date)) |> 
  group_by(sch_number) |> 
  summarize(count = n(), last_approval_date = last(nod_approved_date)) |> 
  anti_join(NODs_per_SCH, by = 'sch_number')

industrial_NODs_non <- industrial_projects |> 
  select(sch_number, lead_agency_name) |> 
  inner_join(check_nonLeadAgency) |> 
  distinct() 

##

all_NODs <- bind_rows(industrial_NODs, industrial_NODs_non)

##check for dups 
all_NODs_chk <- all_NODs |> 
  select(sch_number) |> 
  group_by(sch_number) |> 
  summarize(count = n()) |> 
  filter(count > 1)

rm(ls= NODs, NODs_2024_25, pre2022, NOD2022_23, NODs_2026)
rm(ls = industrial_NODs, industrial_NODs_non, all_NODs_chk, NODs_per_SCH)

setwd(wd)
