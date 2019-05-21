

# load necessary packages
library( geojsonio )
library(leaflet)
library(shiny)

#load in Data
unemployment <- read.csv("Data/Barcelona_unemployment/2016_unemployment.csv", sep=",")

str(unemployment)




# transfrom .json file into a spatial polygons data frame
geojson_bracelona <- 
  geojson_read( 
    x = "https://cdn.rawgit.com/martgnz/bcn-geodata/master/barris/barris_geo.json"
    , what = "sp"
  )


# check the class of the object
class( geojson_bracelona )
names(geojson_bracelona)



bins <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, Inf)
pal <- colorBin("YlOrRd", domain = unemployment$Gener, bins = bins)


#Create the basic map with districts

m <- leaflet(geojson_bracelona) %>%
  #setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))


#Create variable for the labels, shown when hovering over the different Neighbourhoods
labels <- sprintf(
  "<strong>Name of Hood: </strong> %s <br/> <strong>Name of District: </strong> %s <br/> <strong>Name of District: </strong> %g",
  geojson_bracelona$N_Barri, geojson_bracelona$N_Distri, unemployment$Gener
) %>% lapply(htmltools::HTML)


#add poligons to the map

m %>% addPolygons(  
  
  #fill of tiles depending on bins and population density
  fillColor = ~pal(as.numeric(unemployment$Gener)),
  
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


  addLegend(pal = pal, values = unemployment$Població.16.64.anys, opacity = 0.7, title = NULL,
            position = "bottomright")


