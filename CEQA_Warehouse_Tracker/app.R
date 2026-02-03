#
## To Do: Add Warehouse CENTRAL parcels
## Last modified August 2024

#
library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(bslib)
library(bsicons)
library(shinycssloaders)
library(DT)
library(googlesheets4)
library(leaflet.extras)

'%ni%' <- Negate('%in%') ## not in operator
sf_use_s2(FALSE)
deploy_date <- 'February 2, 2026'
version <- 'CEQA Warehouse Tracker alpha v1.04, last updated'
##FIXME
link_github <- tags$a(shiny::icon('github'), 'GitHub Repo', 
                      href = 'https://github.com/RadicalResearchLLC/CEQA_tracker')

##Update
current_list <- 'C:/Dev/CEQA_Tracker/CEQA_docs/CEQA Documents_020226.csv'

##load most recent list of industrial projects from CEQANET
industrial_projects <- read_csv(current_list) |>   
  janitor::clean_names() |> 
  dplyr::select(sch_number, lead_agency_name, lead_agency_title,
    project_title, received, document_portal_url, counties, cities,
    document_type) |> 
  mutate(recvd_date = lubridate::mdy(received))

## summarize by project number
industrial_most_recent <- industrial_projects |> 
  group_by(sch_number) |> 
    summarize(recvd_date = max(recvd_date)) |> 
  left_join(industrial_projects) |> 
  filter(document_portal_url != 'https://ceqanet.opr.ca.gov/2017121007/4')

## CEQANET warehouse list from last iteration
tracked_warehouses <- sf::st_read('https://github.com/RadicalResearchLLC/CEQA_tracker/raw/main/CEQA_WH.geojson') |> 
  st_transform(crs = 4326) |> 
  filter(project %ni% c('South Perris Industrial Project', ### FIXME - not sure why these are handled here
                        'Northern Gateway Commerce Center',
                        'Tracy Northeast Business Park Project')) |> 
  filter(sch_number != '2025010343') ## FIXME why is this here?

## existing warehouses from WarehouseCITY
warehouses <- sf::st_read('https://raw.githubusercontent.com/RadicalResearchLLC/WarehouseMap/main/WarehouseCITY/geoJSON/comboFinal.geojson') |> 
  st_transform(crs = 4326) |> 
  filter(category == 'Existing') |> 
  select(apn, category, geometry) |> 
  rename(project = apn) 

## list of projects as previously checked for warehouse
gs = 'https://docs.google.com/spreadsheets/d/1Dw-HLvt5AzTY8or3ZFiDdlXX5Xv1u-ASD9t-153FwNc/edit#gid=0'

Y_N_WH <- read_sheet(gs, sheet = 'Y_N_WH') |> 
  filter(Y_N_WH != '') #|> 
 # select(sch_number, sch_number1, Y_N_WH)

##FIXME
##Is that still used
#plannedWH <- sf::st_read('https://raw.githubusercontent.com/RadicalResearchLLC/PlannedWarehouses/main/CEQA_WH.geojson')

source('C:/Dev/CEQA_Tracker/new_warehouses_list.R')

tracked_warehouses2 <- bind_rows(tracked_warehouses, newWH7) |> 
  rename(category = document_type_bins)

## FIXME
##Embed in spreadsheet rather than code
builtWH_list <- c('2022030012', '2020090441', '2015081081',
                  '2020050079', '2020049052', '2014011009',
                  '2020019017', '2022110076', '2022060066',
                  '2022040166', '2021090555', '2021020280',
                  '2021010163', '2020040155', '2019070040',
                  '2020059021', '2022020501', '2020059013',
                  '2021020421', '2021050330'
                  )

drawWH <- Y_N_WH |> 
  filter(Y_N_WH == 'Y') |> 
  anti_join(tracked_warehouses2, by = c('sch_number')) |> 
  filter(sch_number %ni% builtWH_list) |> 
  select(-lead_agency) |> 
  mutate(recvd_date = as.Date(recvd_date)) |> 
  arrange(desc(recvd_date)) |> 
  filter(is.na(Notes))


all_warehouses <- tracked_warehouses2 |> 
  select(project, category, geometry) |> 
  #rename(category = stage_pending_approved) |> 
  bind_rows(warehouses)

tracked_nogeom <- tracked_warehouses2 |> 
  st_set_geometry(value = NULL)

unknownProjects <- industrial_most_recent |>
    anti_join(tracked_warehouses2, by = c('sch_number')) |> 
    anti_join(Y_N_WH, by = c('sch_number')) |> 
    #anti_join(newWH_list, by = c('sch_number')) |> 
      #st_set_geometry(value = NULL) |> 
    select(project_title, document_portal_url, sch_number, lead_agency_name, counties, cities,
           recvd_date, document_type) |> 
    rename(project = project_title,
           url = document_portal_url, 
           lead_agency = lead_agency_name,
           type = document_type) |> 
    mutate(sch_number1 = paste0("<a href='", url, "'>",
                               sch_number, "</a>")) |> 
    select(-url)  |> 
    select(sch_number, sch_number1, project, lead_agency, recvd_date, counties) |> 
    mutate(Y_N_WH = '') |> 
    arrange(desc(recvd_date)) |> 
    slice(1:10) 
     # filter(counties %in% c('Riverside', 'San Bernardino', 'Los Angeles', 'Orange', 
  #                         'San Joaquin', 'Stanislaus'))




ui <- navbarPage(
  title = 'CEQA Warehouse Tracker',
  nav_menu(title = 'Links',
           align = 'right',
           nav_item(link_github)),
  theme = bs_theme(
    bootswatch = 'united',
    font_scale = 0.7, 
    spacer = "0.2rem",
    primary = '#F7971D',
    "navbar-bg" = '#F7971D',
    bg = '#333',
    fg = 'white'),
  #icon = shiny::img(height = 60, src = 'RNOW.png')),
  nav_panel(
    title = 'Untracked Projects',
    
    fluidRow(
      column(width = 3, 
             selectizeInput(
               label = 'Select rows for action buttons',
               inputId = 'rows',
               choices = 1:nrow(unknownProjects),
               multiple = 10),
             actionButton(
               inputId = 'Export',
               label = 'Append to sheet of projects')
      ),
      column(width = 9, DTOutput('untrackedProj'))
    ),
    verbatimTextOutput('print')
  ),
   nav_panel(title = 'Tracked Projects',
    p(
    fileInput(
      inputId = 'projects',
      label = 'Choose CEQA Projects .csv file',
      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
      multiple = F
      ),
    # Show the file
      shinycssloaders::withSpinner(leafletOutput("map", height = 300)),
      myDTmodUI('trackedWH'),
      DTOutput('trackedWH')
    )
  ),

  nav_panel(
    title = 'Draw Warehouses',
    fluidRow(
    column(width = 4,
   
    DTOutput('WarehouseChoices')),
    column(width = 7,
     shinycssloaders::withSpinner(leafletOutput('DrawMap')),
      verbatimTextOutput('coordinates'),
      downloadButton('ExportPoly',
                   'Download polygon',
                     icon = icon('download')
      ))
  ))
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$contents <- renderTable({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      req(input$projects)
      inFile <- input$projects
      
      if (is.null(inFile))
        return(NULL)
      
      read.csv(inFile$datapath
                      #col_types =  'cccccccccccccccccccnndccccnnc')
      ) |> 
        janitor::clean_names() |> 
        dplyr::select(sch_number, lead_agency_name, lead_agency_title,
                      project_title, received, document_portal_url,
                      counties, cities) |> 
        slice(1:10)
               })

tracked_projects <- reactive({
  tracked_WH <-  tracked_warehouses2 |> 
    st_set_geometry(value = NULL) |> 
    left_join(industrial_most_recent) |>
    select(project, ceqa_url, sch_number, stage_pending_approved, category, 
           lead_agency_name, counties, cities, parcel_area, recvd_date) |> 
    mutate(parcel_area_sqft = round(parcel_area, -3),
           sch_number = paste0("<a href='", ceqa_url, "'>",
                               sch_number, "</a>")
           ) |> 
    rename(CEQA_type = stage_pending_approved) |> 
    select(-parcel_area, -category, -ceqa_url) |> 
    arrange(desc(recvd_date))
})

output$map <- renderLeaflet({
  map1 <- leaflet() |> 
    setView(lat = 36.46, lng = -118.90, zoom = 5) |> 
    addProviderTiles(providers$Esri.WorldImagery, group = 'Imagery') |> 
    addProviderTiles(providers$CartoDB.Positron, group = 'Basemap')  |>
    addLayersControl(
      baseGroups = c('Basemap', 'Imagery'),
      overlayGroups = c('Planned Warehouses'), 
      options = layersControlOptions(collapsed = FALSE),
      position = 'topright'
      ) 
})

WHPal <- colorFactor( palette = c('black', 'maroon'),
                        domain = tracked_warehouses$category)
    
observe({
  leafletProxy("map", data = tracked_warehouses2) |> 
    clearGroup(group = 'Planned Warehouses') |> 
      addPolygons(
        weight = 0.8, 
        opacity = 0.8,
        color = ~WHPal(category),
        label = ~sch_number,
        group = 'Planned Warehouses') |> 
      addLegend(title = 'CEQA stage',
        pal = WHPal,
        values = ~category,
        position = 'bottomleft')
})

myDTmodServer('trackedWH', tracked_projects()) 

rv <- shiny::reactiveValues(df = unknownProjects)

shiny::observe({
  shiny::updateSelectInput(
    session, 
    inputId = "rows",
    choices = 1:nrow(rv$df))
  })

merged <- reactive({
  rv$df
})

output$untrackedProj <- DT::renderDT(
  datatable(
    callback = JS("$.fn.dataTable.ext.errMode = 'none';"),
    merged(),
    rownames = FALSE,
    escape = FALSE,
    editable = list(
      target = 'cell', 
      disable = list(columns = c(0:3))),
    selection = 'none',
    options = list(dom = 'Bt') 
  )
)

observeEvent(input$Export, {
  sheet_append('https://docs.google.com/spreadsheets/d/1Dw-HLvt5AzTY8or3ZFiDdlXX5Xv1u-ASD9t-153FwNc/edit#gid=1641064942',
               rv$df,
               sheet = 'Y_N_WH'
  )
})

observeEvent(input$untrackedProj_cell_edit, {
  info <- input$untrackedProj_cell_edit
  str(info)
  i = info$row
  j = info$col
  v = info$value
  
  rv$df[i,j+1] <- isolate(DT::coerceValue(v, rv$df[i,j]))
})

output$print <- renderPrint({
  rv$df
})



output$DrawMap <- renderLeaflet({
  leaflet() |> 
    addDrawToolbar(rectangleOptions = FALSE,
                   editOptions = editToolbarOptions(edit = FALSE,
                                                    remove = TRUE),
                   polylineOptions = FALSE,
                   circleOptions = FALSE,
                   circleMarkerOptions = FALSE,
                   markerOptions = FALSE) |> 
    setView(lat = 36.46, lng = -118.90, zoom = 5) |> 
    addProviderTiles(providers$Esri.WorldImagery, group = 'Imagery') |> 
    addProviderTiles(providers$CartoDB.Positron, group = 'Basemap')  |>
    addLayersControl(
      baseGroups = c('Basemap', 'Imagery'),
      overlayGroups = c('Warehouses'), 
      options = layersControlOptions(collapsed = FALSE),
      position = 'topright'
    ) |> 
    addPolygons(data = all_warehouses,
                color = ~WHPal2(category), 
                weight = 1,
                fillOpacity = 0.5,
                group = 'Warehouses'#,
                #label = ~sch_number
                ) |> 
    addLegend(data = all_warehouses,
              pal = WHPal2,
              title = 'Status',
              values = ~category,
              group = 'Warehouses',
              position = 'bottomright') 
})

WHPal2 <- colorFactor(palette = c('darkred', 'grey10', 'red'), domain = all_warehouses$category)

observeEvent(input$DrawMap_draw_new_feature, {
  output$coordinates <- renderPrint(new_geom())
    #renderPrint({input$DrawMap_draw_new_feature$geometry})
    #renderPrint({input$DrawMap_draw_new_feature$geometry$coordinates[[1]]})
  })

new_geom <- reactive({
  req(input$DrawMap_draw_new_feature)
  #req(input$WarehouseChoices)
  
  sch <- drawWH[input$WarehouseChoices_rows_selected, ]$sch_number
  proj <- drawWH[input$WarehouseChoices_rows_selected, ]$project
  geo <- input$DrawMap_draw_new_feature$geometry$coordinates[[1]]
  lng <- map_dbl(geo, `[[`, 1)
  lat <- map_dbl(geo, `[[`, 2)
  shp <- st_as_sf(tibble(
                 #sch = row,
                 lon = lng, lat = lat), 
                 coords = c("lon", "lat"),
                 crs = 4326) %>%
    summarise(geometry = st_combine(geometry)) |> 
    st_cast("POLYGON") |> 
    mutate(sch_number = sch,
           project = proj)

  return(shp)
})

WH4export <- reactive({
  area <- as.numeric(st_area(new_geom()))
  
  add_columns <- new_geom() |> 
    left_join(industrial_most_recent) |> 
    select(sch_number, project, document_portal_url, document_type) |> 
    mutate(parcel_area = area*10.7639,
           category = '') |> 
    rename(ceqa_url = document_portal_url,
           stage_pending_approved = document_type)
}) 

## Export polygon

output$ExportPoly <- downloadHandler(
  filename = function() {
    str_c(drawWH[input$WarehouseChoices_rows_selected, ]$sch_number, '.geojson')
  },
  content = function(filename) {
    sf::st_write(WH4export(), filename)
  }
)

output$WarehouseChoices <- renderDT({
  drawWH |> 
    select(-Y_N_WH) |>
    datatable(
    #callback = JS("$.fn.dataTable.ext.errMode = 'throw';"),
    rownames = FALSE,
    escape = FALSE) 
})

}
# Run the application 
shinyApp(ui = ui, server = server)
