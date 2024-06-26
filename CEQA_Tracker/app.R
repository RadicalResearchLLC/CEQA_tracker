
library(leaflet)
library(shiny)
library(bslib)
library(DT)
library(tidyverse)
library(sf)

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
            p(
              selectizeInput(inputId = 'County', label = 'Select up to 8 Counties',
                             choices = c('', sort(CA_counties$county)), 
                             options = list(maxItems = 8)),
              shinycssloaders::withSpinner(leafletOutput("map", height = 500)),
              myDTmodUI('trackedWH'),
              layout_columns(
                plotOutput('County_SQFT', width = 500), 
                plotOutput('County_perCapita', width = 500)
              )
            )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  WHPal <- colorFactor( palette = c('red', 'darkred', 'maroon'),
                        domain = tracked_warehouses$category)
  
  output$map <- renderLeaflet({
    map1 <- leaflet() |> 
      # addTiles() %>%
      setView(lat = 35.8, lng = -118.7, zoom = 6) |> 
      addProviderTiles(providers$Esri.WorldImagery, group = 'Imagery') |> 
      addProviderTiles(providers$OpenRailwayMap, group = 'Rail') |> 
      addProviderTiles(providers$CartoDB.Positron, group = 'Basemap')  |>
      addLayersControl(baseGroups = c('Basemap', 'Imagery'),
                       overlayGroups = c('Warehouses', 'Jurisdictions',
                                         'Rail'), 
                       options = layersControlOptions(collapsed = FALSE),
                       position = 'topright'
      )  |> 
      hideGroup(c('Rail')) |> 
      addPolygons(data = tracked_warehouses,
                color = ~WHPal(category),
                weight = 3,
                label = ~paste(sch_number, document_type)
      ) |> 
    addLegend(data = tracked_warehouses, 
              title = 'CEQA stage',
              pal = WHPal,
              values = ~category,
              position = 'bottomleft')
   
})
  
  countyPoly <- reactive({
    if(is.null(input$County)) {}
    else {
      countyList <- filter(CA_counties, county %in% input$County) 
    }
  })
  
  observe({
    req(countyPoly())
    leafletProxy("map", data = countyPoly()) %>%
      clearGroup(group = 'Counties') %>%
      addPolylines(color = 'black',
                   #fillOpacity = 0.05,
                   weight = 2,
                   group = 'Counties')
  })
  
  output$County_SQFT <- renderPlot({
    ggplot() +
      geom_col(data = county_stats, aes(x = acres, y = reorder(county, acresAll), fill = category)) +
      theme_bw() +
      labs(x = 'Acres', y = '', title = 'CEQA warehouse projects 2020-2024 - Acres') +
      scale_fill_manual(values = c('red', 'darkred', 'grey40'), 
                        breaks = c('Approved', 'CEQA Review', 'Not characterized')
      )
  })
  
  output$County_perCapita <- renderPlot({
    ggplot() +
      geom_col(data = county_stats, aes(x = WH_SF_per_capita, y = reorder(county, WH_SF_per_capitaAll), fill = category)) +
      theme_bw() +
      labs(x = 'Additional Warehouse SQ FT per capita', y = '', title = 'CEQA warehouse projects 2020-2024 per Capita',
           caption = 'Warehouse data from CEQANET
       Population from CA DoF Table E-1') +
      scale_fill_manual(values = c('red', 'darkred', 'grey40'), 
                        breaks = c('Approved', 'CEQA Review', 'Not characterized'))
    
  })

  whList <- reactive({
    if(is.null(input$County)) {
      whList1 <- tracked_warehouses |> 
        st_set_geometry(value = NULL) |> 
        arrange(desc(parcel_area)) |> 
        mutate(sch_number = paste0("<a href='", ceqa_url, "'>",
                                    sch_number, "</a>"),
               acres = round(parcel_area/43560, 0)) |> 
        select(project, sch_number, acres, county, year_rcvd, document_type) |> 
        rename(year = year_rcvd)
      }
    else {
      whList1 <- tracked_warehouses |> 
        st_set_geometry(value = NULL) |> 
        filter(county %in% input$County) |> 
        arrange(desc(parcel_area)) |> 
        mutate(sch_number = paste0("<a href='", ceqa_url, "'>",
                                   sch_number, "</a>"),
               acres = round(parcel_area/43560, 0)) |> 
        select(project, sch_number, acres, county, year_rcvd, document_type) |> 
        rename(year = year_rcvd)
    }
    return(whList1)
  })
    
  myDTmodServer('trackedWH', whList()) 
    
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
