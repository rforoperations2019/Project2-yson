# Green Infrastructure
library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(shinyjs)
library(rgeos)
library(ggplot2)
library(stringr)
library(tools)
library(plotly)

# Data Source: https://data.cityofnewyork.us/Social-Services/NYC-Wi-Fi-Hotspot-Locations/a9we-mtpn
wifi.load <- readOGR("https://data.cityofnewyork.us/api/geospatial/a9we-mtpn?method=export&format=GeoJSON")
names(wifi.load)[c(7,11,12,13,15,21,28)] <- c("wifi_provider","wifi_location","wifi_type","postal_code","borough_code","longitude", "latitude")
boros.load <- readOGR("https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=GeoJSON")
# Add Boro centroids to dataframe
boros.load@data <- cbind(boros.load@data, rgeos::gCentroid(boros.load, byid = TRUE)@coords)

# Define UI for application
ui <- navbarPage("NYC Wi-Fi Hotspot Locations",
                 theme = shinytheme("sandstone"),
                 tabPanel("Map",
                          sidebarLayout(
                            sidebarPanel(
                              # Select NYC Borough
                              radioButtons("boro",
                                           "Borough Filter:",
                                           choices = levels(wifi.load$boro),
                                           selected = "BK"),
                              # Select variable for y-axis 
                              selectInput(inputId = "geoChart", 
                                          label = "Choose geo data for graph (left)!",
                                          choices = c("Postcode" ="postal_code",
                                                      "Boro Code" = "borough_code"
                                                      ), 
                                          selected = "borocd"),
                              #ssid, provider, location
                              selectInput(inputId = "wifiChart", 
                                          label = "Choose wifi information for graph (right)!",
                                          choices = c("Wifi Type" ="wifi_type",
                                                      "Wifi Provider" = "wifi_provider",
                                                      "Wifi Location" = "wifi_location"
                                          ), 
                                          selected = "city"),
                              # download button
                              downloadButton(outputId = "downloadData",
                                             label = "Download raw data!")
                            ),
                            mainPanel(
                              # Using Shiny JS
                              shinyjs::useShinyjs(),
                              # Style the background and change the page
                              tags$style(type = "text/css", ".leaflet {height: calc(100vh - 90px) !important;}
                                         body {background-color: #D4EFDF;}"),
                              # Map Output
                              leafletOutput("leaflet"),
                              br(), br(), 
                              # Bar chart
                              fluidRow(
                                column(6,plotOutput("geoBar")
                                ),
                                column(6,plotOutput("wifiBar")
                                )
                              )
                            )
                          )
                        ),
                 # Data Table Pannel
                 tabPanel("Data",
                          fluidPage(
                            wellPanel(DT::dataTableOutput("table"))
                          )
                 )
)

# Define server logic required to create a map
server <- function(input, output) {
   values <- reactiveValues(removed = c())
   # Basic Map
   output$leaflet <- renderLeaflet({
     leaflet() %>%
       addProviderTiles("Stamen.TonerHybrid", group = "Esri") %>%
       addProviderTiles("OpenTopoMap", group = "Topo") %>%
       # Add layers control
       addLayersControl(
         baseGroups = c("Stamen", "Topo"),
         options = layersControlOptions(collapsed = FALSE)) %>%
       setView(-74.0060, 40.7128, 9)
   })
   ## Wifi Filtered data
   wifiInputs <- reactive({
     wifi <- wifi.load
     req(input$boro)
     # Boros
     wifi <- subset(wifi, boro == input$boro)
     return(wifi)
   })
   # 
   # # Replace layer with filtered greenInfrastructure
    observe({
      wifi <- wifiInputs()
      # Data is greenInf
      leafletProxy("leaflet", data = wifi) %>%
        # In this case either lines 92 or 93 will work
        clearMarkers() %>%
        addMarkers()
    })
    # A plot gives detailed counts for geo data (zipcode, borough)
    output$geoBar <- renderPlot({
      ggplot(data = data.frame(wifiInputs())) +
        geom_bar(aes_string(input$geoChart)) + 
        labs(toTitleCase(str_replace_all(input$geoChart, "_", " "))) +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        #                  labels= axisTitles.7sat, 
        #                  limit = c("1","2","3","4","5","6","7")) +
        # scale_y_continuous(paste("Satisfaction with", toTitleCase(str_replace_all(input$y, "_", " "))),
        #                    breaks = c(1,2,3,4,5,6,7), label = axisTitles.7sat) +
        # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        # labs(title = pretty_plot_title())
    })
    # A plot gives detailed counts for wifi (type, provider, location)
    output$wifiBar <- renderPlot({
      ggplot(data = data.frame(wifiInputs())) +
        geom_bar(aes_string(input$wifiChart)) + 
        labs(toTitleCase(str_replace_all(input$wifiChart, "_", " ")))+
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
    })
    # Data table
    output$table <- DT::renderDataTable(wifiInputs()@data, options = list(scrollX = T))
    # Download data
    output$downloadData <- downloadHandler(
      filename = function(){
        paste0("wifiNYC",Sys.Date(),".csv")
      },
      content = function(file) {
        write.csv(wifiInputs(), file)
      })
 }

# Run the application 
shinyApp(ui = ui, server = server)

