#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
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
deploy_date <- 'April 22, 2024'
version <- 'CEQA Warehouse Tracker alpha v1.00, last updated'
##FIXME
link_github <- tags$a(shiny::icon('github'), 'GitHub Repo', 
                      href = 'https://github.com/RadicalResearchLLC/SJV_warehouses')

industrial_projects <- read_csv('CEQA_industrial_2020_24.csv') |>   
  janitor::clean_names() |> 
  dplyr::select(sch_number, lead_agency_name, lead_agency_title,
    project_title, received, document_portal_url, counties, cities,
    document_type) |> 
  mutate(recvd_date = lubridate::mdy(received))

industrial_most_recent <- industrial_projects |> 
  group_by(sch_number) |> 
    summarize(recvd_date = max(recvd_date)) |> 
  left_join(industrial_projects)

tracked_nogeom <- tracked_warehouses |> 
  st_set_geometry(value = NULL)

Y_N_WH <- read_sheet(gs, sheet = 'TestAppend') |> 
  filter(Y_N_WH != '') #|> 
  #select(sch_number, sch_number1, Y_N_WH)

drawWH <- Y_N_WH |> 
  filter(Y_N_WH == 'Y') |> 
  select(-lead_agency) |> 
  mutate(recvd_date = as.Date(recvd_date)) |> 
  arrange(desc(recvd_date))

unknownProjects <- industrial_most_recent |>
    anti_join(tracked_nogeom) |> 
    anti_join(Y_N_WH, by = c('sch_number')) |> 
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

gs = 'https://docs.google.com/spreadsheets/d/1Dw-HLvt5AzTY8or3ZFiDdlXX5Xv1u-ASD9t-153FwNc/edit#gid=0'


#plannedWH <- sf::st_read('https://raw.githubusercontent.com/RadicalResearchLLC/PlannedWarehouses/main/CEQA_WH.geojson')

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
  nav_panel(title = 'Tracked Projects',
    p(
    # Sidebar with a slider input for number of bins 
    fileInput(
      inputId = 'projects',
      label = 'Choose CEQA Projects .csv file',
      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
        multiple = F
      ),
    # Show the file
      shinycssloaders::withSpinner(leafletOutput("map", height = 300)),
      myDTmodUI('trackedWH')
      #dataTableOutput('trackedWH')
    )
  ),
  nav_panel(
    title = 'Untracked Projects',
    
    fluidRow(
      column(width = 3, 
        selectizeInput(
          label = 'Select rows for action buttons',
          inputId = 'rows',
          choices = 1:nrow(unknownProjects),
          multiple = 10),
      actionButton(inputId = 'Export',
                     label = 'Append to sheet of projects'),
      ),
      column(width = 9, dataTableOutput('untrackedProj'))
      ),
    verbatimTextOutput('print')
    ),
  nav_panel(
    title = 'Draw Warehouses',
    fluidRow(
    column(width = 4,
      actionButton(inputId = 'writePoly',
        label = 'Export this polygon'),    
      dataTableOutput('WarehouseChoices')),
    column(width = 7,
      shinycssloaders::withSpinner(leafletOutput('DrawMap'))),
      verbatimTextOutput('coordinates')
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
      
      read.csv(inFile$datapath, fileEncoding = 'latin1'
                      #col_types =  'cccccccccccccccccccnndccccnnc')
      ) |> 
        janitor::clean_names() |> 
        dplyr::select(sch_number, lead_agency_name, lead_agency_title,
                      project_title, received, document_portal_url,
                      counties, cities) |> 
        slice(1:10)
               })

tracked_projects <- reactive({
  tracked_WH <-  tracked_warehouses |> 
    st_set_geometry(value = NULL) |> 
    left_join(industrial_most_recent) |>
    select(project, ceqa_url, sch_number, stage_pending_approved, category, 
           lead_agency_name, counties, cities, parcel_area, recvd_date) |> 
    mutate(parcel_area_sqft = round(parcel_area, -3),
           sch_number = paste0("<a href='", ceqa_url, "'>",
                               sch_number, "</a>")
           ) |> 
    rename(CEQA_type = stage_pending_approved) |> 
    select(-parcel_area, -category, -ceqa_url)
})

output$map <- renderLeaflet({
  map1 <- leaflet() |> 
    setView(lat = 36.46, lng = -118.90, zoom = 5) |> 
    addProviderTiles(providers$Esri.WorldImagery, group = 'Imagery') |> 
    addProviderTiles(providers$OpenRailwayMap, group = 'Rail') |> 
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
  leafletProxy("map", data = tracked_warehouses) |> 
    clearGroup(group = 'Planned Warehouses') |> 
      addPolygons(
        weight = 0.8, 
        opacity = 0.8,
        color = ~WHPal(category),
        label = ~(paste(project, stage_pending_approved)),
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

output$untrackedProj <- renderDataTable(
  datatable(
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
               sheet = 'TestAppend'
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

output$WarehouseChoices <- renderDataTable({
  drawWH |> 
    select(-Y_N_WH) |> 
    datatable(
      rownames = FALSE,
      escape = FALSE)
})

output$DrawMap <- renderLeaflet({
  leaflet() |> 
    addDrawToolbar() |> 
    setView(lat = 36.46, lng = -118.90, zoom = 5) |> 
    addProviderTiles(providers$Esri.WorldImagery, group = 'Imagery') |> 
    addProviderTiles(providers$CartoDB.Positron, group = 'Basemap')  |>
    addLayersControl(
      baseGroups = c('Basemap', 'Imagery'),
      overlayGroups = c('Warehouses'), 
      options = layersControlOptions(collapsed = FALSE),
      position = 'topright'
    ) |> 
    addPolygons(data = warehouses,
                color = ~WHPal2(category), 
                weight = 1,
                fillOpacity = 0.5,
                group = 'Warehouses',
                label = ~paste(apn, class, round(shape_area,-3), ' sq.ft.',  year_built)) |> 
    addLegend(data = warehouses,
              pal = WHPal2,
              title = 'Status',
              values = ~category,
              group = 'Warehouses',
              position = 'bottomright') 
})

WHPal2 <- colorFactor(palette = c('red', 'black'), domain = warehouses$category)

observeEvent(input$DrawMap_draw_new_feature, {
  output$coordinates <- renderPrint({input$DrawMap_draw_new_feature$geometry$coordinates[[1]]})
})

}
# Run the application 
shinyApp(ui = ui, server = server)
