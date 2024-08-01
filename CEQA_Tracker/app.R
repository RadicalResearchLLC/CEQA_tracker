
library(leaflet)
library(shiny)
library(bslib)
library(DT)
library(tidyverse)
library(sf)

#header stuff
deploy_date <- 'August 1, 2024.'
version <- 'CEQA Warehouse Tracker Dashboard alpha v0.2, last updated'
data_download_date <- 'CEQA Industrial data downloaded July 30, 2024'

# Define UI for application that draws a histogram
ui <- navbarPage(
  title = 'CEQA Warehouse Tracker',
  theme = bs_theme(
    bootswatch = 'united',
    font_scale = 0.7, 
    spacer = "0.2rem",
    primary = '#F7971D',
    "navbar-bg" = '#F7971D',
    bg = '#333',
    fg = 'white'),

  nav_panel(title = 'Tracked Projects',
    layout_columns(
      selectizeInput(inputId = 'County', label = 'Select up to 8 Counties',
        choices = c('', sort(CA_counties$county)), 
        options = list(maxItems = 8)),
      selectizeInput(inputId = 'Project', label = 'Search by project name',
        choices = c('No Warehouse Selected', tracked_warehouses$project3),
        options = list(maxItems = 1)
        )
      ),
      shinycssloaders::withSpinner(leafletOutput("map", height = 500)),
      myDTmodUI('trackedWH'),
      layout_columns(
        plotOutput('County_SQFT', width = 500), 
        plotOutput('County_perCapita', width = 500)
        ),
      h5('Version and Deployment info'),
      div(
        p(paste(version, deploy_date, data_download_date)),
        style = 'font-size:80%; margin-top:-10px'
      )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  WHPal <- colorFactor( palette = c('red', 'darkred', 'grey40', 'black', 'maroon'), 
                        domain = tracked_warehouses$document_type_bins
  )
  
  output$map <- renderLeaflet({
    map1 <- leaflet() |> 
      # addTiles() %>%
      setView(lat = 35.8, lng = -118.7, zoom = 6) |> 
      addProviderTiles(providers$Esri.WorldImagery, group = 'Imagery') |> 
      addProviderTiles(providers$OpenRailwayMap, group = 'Rail') |> 
      addProviderTiles(providers$CartoDB.Positron, group = 'Basemap')  |>
      addLayersControl(baseGroups = c('Basemap', 'Imagery'),
                       overlayGroups = c('Warehouses', 'Jurisdictions',
                                         'Rail', 'Counties'), 
                       options = layersControlOptions(collapsed = FALSE),
                       position = 'topright'
      )  |> 
      hideGroup(c('Rail')) |>
      addPolygons(data = tracked_warehouses,
                  color = ~WHPal(document_type_bins),
                  weight = 3,
                  label = ~paste(sch_number, document_type)
      ) |> 
      addLegend(data = tracked_warehouses, 
              title = 'CEQA stage',
              pal = WHPal,
              values = ~document_type_bins,
              position = 'bottomleft')
   
})
  
 
  
  observe({
    req(countyPoly())
    leafletProxy("map", data = countyPoly()) %>%
      clearGroup(group = 'Counties') %>%
      addPolylines(color = 'black',
                 #fillOpacity = 0.05,
                   weight = 1,
                   group = 'Counties')
  })
  
  output$County_SQFT <- renderPlot({
    ggplot() +
      geom_col(data = county_stats, aes(x = acres, y = reorder(county, acresAll), fill = document_type_bins)) +
      theme_bw() +
      labs(x = 'Acres', y = '', title = 'CEQA warehouse projects 2020-2024 - Acres') +
      scale_fill_manual(values = c('red', 'darkred', 'grey40', 'black', 'maroon'), 
                        breaks = c('MND', 'EIR', 'NOP', 'FIN', 'Other')
      )
  })
  
  output$County_perCapita <- renderPlot({
    ggplot() +
      geom_col(data = county_stats, aes(x = WH_SF_per_capita, y = reorder(county, WH_SF_per_capitaAll), fill = document_type_bins)) +
      theme_bw() +
      labs(x = 'Additional Warehouse SQ FT per capita', y = '', title = 'CEQA warehouse projects 2020-2024 per Capita',
           caption = 'Warehouse data from CEQANET
       Population from CA DoF Table E-1') +
      scale_fill_manual(values = c('red', 'darkred', 'grey40', 'black', 'maroon'), 
                        breaks = c('MND', 'EIR', 'NOP', 'FIN', 'Other')
      )
    
  })

  whList <- reactive({
    if(is.null(input$County)) {
      whList1 <- tracked_warehouses |> 
        st_set_geometry(value = NULL) |> 
        arrange(desc(parcel_area)) |> 
        mutate(sch_number = paste0("<a href='", ceqa_url, "'>",
                                    sch_number, "</a>"),
               acres = round(parcel_area/43560, 0)) |> 
        select(project3, sch_number, lead_agency_title, acres, county, year_rcvd, document_type,
               recvd_date) |> 
        rename(year = year_rcvd,
               project = project3,
               date = recvd_date)
      }
    else {
      whList1 <- tracked_warehouses |> 
        st_set_geometry(value = NULL) |> 
        filter(county %in% input$County) |> 
        arrange(desc(parcel_area)) |> 
        mutate(sch_number = paste0("<a href='", ceqa_url, "'>",
                                   sch_number, "</a>"),
               acres = round(parcel_area/43560, 0)) |> 
        select(project3, sch_number, lead_agency_title, acres, county, 
               year_rcvd, document_type, recvd_date) |> 
        rename(year = year_rcvd,
               project = project3,
               date = recvd_date)
    }
    return(whList1)
  })
    
  myDTmodServer('trackedWH', whList()) 
  
  countyPoly <- reactive({
    # Select County Polygons
    if(!is.null(input$County)) {
      selectedCounty <- CA_counties |> 
        filter(county %in% input$County)
      } else {
        selectedCounty <- CA_counties
        }
    
    return(selectedCounty)
  })
  
  whSelect <- reactive({
    selectedWH <- tracked_warehouses |> 
        filter(project3 %in% input$Project) 
  
    return(selectedWH)
  })
  
  ##create bounding box for map zoom
  bbox <- reactive({
    if(input$Project == 'No Warehouse Selected') {
      bounds <- tracked_warehouses |> 
        st_make_valid() |> 
        select(geometry) |> 
        summarize() |> 
        st_bbox() |> 
        as.numeric()
    }
     # } else if(str_length(input$County > 0)) {
    #    bounds <- countyPoly() |> 
     #     st_bbox() |> 
     #     as.numeric()
    #    }
      else {
        bounds <- whSelect() |> 
          st_bbox() |> 
          as.numeric()
        }
    return(bounds)
    
  })
  
  #observe extent of polygons if available
  observe({
   leafletProxy('map', data = bbox()) |> 
      fitBounds(
        lng1 = bbox()[1], 
        lat1 = bbox()[2], 
        lng2 = bbox()[3], 
        lat2 = bbox()[4]
      )
  }) 
    
  
}

# Run the application 
shinyApp(ui = ui, server = server)
