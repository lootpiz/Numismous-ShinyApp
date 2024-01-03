#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
# Written by Heewon
# Updated on January 01, 2024
# App version: 0.3
# (@'3(o.o);
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(leaflet)

data <- readRDS("./data/Numismous.RDS")

iconSet <- lapply(seq_along(data$Country), function(idx) {
        return(makeIcon(iconUrl = file.path("icons", paste0(data[idx, "Flag"], ".png")), iconWidth = 50, iconHeight = 50, shadowWidth = 10, shadowHeight = 10))
})
names(iconSet) <- data$Country
class(iconSet) <- "leaflet_icon_set"

ui <- bootstrapPage(
        tags$head(includeCSS("style.css")),
        tags$head(HTML("<title>Numismous</title>")),
        tags$style(type = "text/css", "html, body {width:100%;height:100%}"),        
        leafletOutput("map", width = "100%", height = "100%"),
        absolutePanel(
                id = "controls", fixed = TRUE,
                top = 10, right = 10,
                left = "auto", bottom = "auto",
                width = 330, 
                draggable = TRUE,
                h2("Numismous"),
                pickerInput("selectContinent",
                        label = "Select a Continent:",
                        choices = list("ALL", "Africa", "North/Central America", "South America", "Asia", "Oceania", "Europe", "Antarctica/Uncirculated notes"),
                        options = list(`live-search` = TRUE)
                ),                
                textOutput("numismCountry"),
                uiOutput("numismWindow")
        ),
        tags$div(id="cite", "© 2010-2024 Lootpiz | All Rights Reserved.")
)

server <- function(input, output, session) {
        filteredData <- reactive({
                if (input$selectContinent == "ALL") {
                        data
                } else {
                        filter(data, Continent == input$selectContinent)
                }
        })
        filteredIcon <- reactive({
                if (input$selectContinent == "ALL") {
                        iconSet
                } else {
                        subData <- filter(data, Continent == input$selectContinent)
                        iconSet[subData$Country]
                }
        })

        output$map <- renderLeaflet({
                leaflet(filteredData()) %>%                        
                        addProviderTiles(providers$Esri.WorldTopoMap) %>%
                        addMarkers(~Longitude, ~Latitude, icon = filteredIcon(), label = ~Country, labelOptions = labelOptions(textsize = "20px"), layerId = ~Folder)
        })

        observe({
                leafletProxy("map", data = filteredData()) %>%
                        clearShapes() %>%
                        addMarkers(~Longitude, ~Latitude, icon = filteredIcon(), label = ~Country, labelOptions = labelOptions(textsize = "20px"), layerId = ~Folder)
        })

        # when a marker clicks
        observe({
                folder <- input$map_marker_click$id
                if (!is.null(folder)) {
                        oneCountry <- filter(data, Folder == folder)
                        country <- oneCountry$Country
                        parentFolder <- toupper(oneCountry$ParentFolder)
                        folder <- toupper(folder)

                        imgFiles <- list.files(file.path("www", parentFolder, folder), pattern = ".jpg", full.names = TRUE)
                        outline <- c("<center>", sapply(imgFiles, function(imgFile) { 
                                filename <- basename(imgFile)
                                buff <- unlist(str_split(gsub(pattern = "\\.jpg$", "", filename), "__"))
                                return(paste0("<img src=\"", str_replace(imgFile, "www/", ""), "\" width=250><br>", buff[2], " ", buff[3], " (", str_replace(buff[4], "_", " "), ")")) 
                                }), "</center>"
                        )
                        output$numismCountry <- renderText(paste0("Country: ", country))
                        output$numismWindow <- renderUI({ tags$div(id = "gallery", HTML(paste(outline, collape="<br><br>"))) })
                } else {
                        output$numismCountry <- renderText("Click one country.")
                }
        
        })
}

shinyApp(ui, server)

# maptypes <- c("MapQuestOpen.Aerial",
#                "Stamen.TerrainBackground",
#                "Esri.WorldImagery",
#                "OpenStreetMap",
#                "Stamen.Watercolor")
