#set working directory
setwd("D:/EIT/Git/InfoVis/BarcelonaChoropleths")

# load necessary packages
library( geojsonio )
library(leaflet)
library(plotly)
library(tidyr)
library(shiny)


#load in Data
unemployment <- read.csv("Data/Barcelona_unemployment/2012_unemployment.csv", sep=",")
#unemployment_2013 <- read.csv("Data/Barcelona_unemployment/2013_unemployment.csv", sep=",")
#unemployment_2014 <- read.csv("Data/Barcelona_unemployment/2014_unemployment.csv", sep=",")
#unemployment_2015 <- read.csv("Data/Barcelona_unemployment/2015_unemployment.csv", sep=",")
unemployment_2016 <- read.csv("Data/Barcelona_unemployment/2016_unemployment.csv", sep=",")

str(unemployment)

################Map data##################

# transfrom .json file into a spatial polygons data frame
geojson_bracelona <- 
  geojson_read( 
    x = "https://cdn.rawgit.com/martgnz/bcn-geodata/master/barris/barris_geo.json"
    , what = "sp"
  )



# check the class of the object
class( geojson_bracelona )

data_start = as.numeric(as.character(sub("," , ".",unemployment$Gener)))

print(data)

# Code to call a specific element in the list
bins <- c(0, 4, 8, 12, 16, Inf)
pal <- colorBin("YlOrRd", domain = data_start, bins = bins)



#Create variable for the labels, shown when hovering over the different Neighbourhoods
labels <- sprintf(
  "<strong>Name of Hood: </strong> %s <br/> <strong>Name of District: </strong> %s <br/> <strong>Number of unemployed: </strong> %g",
  geojson_bracelona$N_Barri, geojson_bracelona$N_Distri, data_start
) %>% lapply(htmltools::HTML)

print("LOG: loading of labels done")

#create object m 

m <- leaflet(geojson_bracelona) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))


print("LOG: loading of variables done")



##################################Shiny##########################################


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("m", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Magnitudes", min(data_start), max(data_start),
                            value = range(data_start), step = 0.1
                ),
                
                selectInput("var", 
                            label = "Choose a month",
                            choices = c("January", 
                                        "February",
                                        "March", 
                                        "April",
                                        "May", 
                                        "June", 
                                        "July", 
                                        "August",
                                        "September", 
                                        "October", 
                                        "November",
                                        "December"),
                            selected = "January"),
                
                selectInput("var", 
                            label = "Choose a year",
                            choices = c("2012", 
                                        "2013",
                                        "2014", 
                                        "2015",
                                        "2016"),
                            selected = "2012")

  )
)





server <- function(input, output, session) {
  
  output$m <- renderLeaflet({
    
    #unemployment <- switch(input$var, 
     #                     "2012" = unemployment_2012,
      #                    "2013" = unemployment_2013,
       #                   "2014" = unemployment_2014,
        #                  "2015" = unemployment_2015,
         #                 "2016" = unemployment_2016)
    
    
    
    switch_month <- switch(input$var, 
                   "January" = unemployment$Gener,
                   "February" = unemployment$Febrer,
                   "March" = unemployment$Agost,
                   "April" = unemployment$Abril,
                   "May" = unemployment$Maig,
                   "June" = unemployment$Juny,
                   "July" = unemployment$Juliol,
                   "August" = unemployment$Agost,
                   "September" = unemployment$Setembre,
                   "October" = unemployment$Octubre,
                   "November" = unemployment$Novembre,
                   "December" = unemployment$Desembre)
    
    

    
    data = as.numeric(as.character(sub("," , ".",switch_month)))
    
    
    
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    
    
    leaflet(geojson_bracelona) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
    
    m %>% addPolygons(  
      #fill of tiles depending on bins and population density
      fillColor = ~pal(data),
      
      #creates the dashed line in between the districts
      
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      
      #defines the properties of the highlightingline when hovering over the different neighbourhoods
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      
      #creates the labels
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>%
      
      #adds the legend in the right hand corner
      
      addLegend(pal = pal, values = unemployment$Gener, opacity = 0.7, title = NULL,
                position = "bottomright")
    
    })}
      

shinyApp(ui, server)


