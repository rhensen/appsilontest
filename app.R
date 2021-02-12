library(shiny)
library(tidyverse)
library(data.table)
library(sf)
library(leaflet)
library(shiny.semantic)

#setwd("Desktop/appsilon/")
ship<- read_csv("ships.csv")
s.sf<- st_as_sf(ship, coords = c("LON", "LAT"), 
                crs = 4326, agr = "constant")

mod1<- function(input){
    df<-ship %>% filter(ship_type == input) %>% distinct(SHIPNAME)
    return(df$SHIPNAME)
}

calc1<- function(input) {
    df<- ship %>% filter(SHIPNAME == input) %>% mutate(LAT2= lag(LAT), LON2= lag(LON))
    df<- df[-c(1),]
    dt <- as.data.table(df)
    sf <- dt[
        , {
            geometry <- sf::st_linestring(x = matrix(c(LON, LON2, LAT, LAT2), nrow = 2, ncol = 2))
            geometry <- sf::st_sfc(geometry)
            geometry <- sf::st_sf(geometry = geometry)
        }
        , by = c("DATETIME","SHIPNAME","SPEED","date", "LON", "LON2", "LAT", "LAT2")
    ]
    
    fin<- sf::st_as_sf(sf,  crs = 4326)
    fin$dist <- st_length(fin)
    fin<- fin[which.max(fin$dist),]
    return(fin)
}

my_layout <- grid_template(default = list(
    areas = rbind(
        c("top", "top"),
        c("top_left", "top_right"),
        c("bottom", "bottom"))))
        
# Define UI for application that draws a histogram
ui <- semanticPage(
theme = "cerulean",
            grid(my_layout, top=segment(h1("test home app", align= "center")),top_left=segment(h2("Ship Type Choice"),
            selectInput("ship_type", "Select Ship Type",
                        choices = unique(ship$ship_type),
                        selected =  "Cargo", multiple=F
            )), top_right=segment(h2("Ship Name Choice"),
            selectInput("ship_name", "Select Ship Name",choices=mod1("Cargo"),  multiple=F
            )), bottom= segment(h2("Map Output", align="center"), h3("hover for more info", align="center"),
    # outputs),
            leafletOutput("map",height = 600)
        )))

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    c1<- reactive({mod1(input$ship_type)})
    observe({
        updateSelectInput(session, "ship_name",
                          choices = mod1(input$ship_type)
        )})
    output$map<- renderLeaflet({

        tryCatch({
    path<- calc1(input$ship_name)
    #path<- calc1("LE VOYAGEUR")
    labels <- sprintf(
        "Ship Name: <strong> %s </strong><br/> Date: %s <br/> Distance Travelled: %g Meters",
        path$SHIPNAME,as.character(path$date),round(path$dist,2)) %>% lapply(htmltools::HTML)
    leaflet(path) %>%
        addPolylines(color = "black", opacity = 1,  label = ~labels)%>% 
        addMarkers(~path$LON2, ~path$LAT2, label = "Start")%>% 
        addMarkers(~path$LON, ~path$LAT, label = "Stop") %>% 
        addTiles()
        },
    error=function(cond) {
        return(leaflet())
        
    }
    
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
