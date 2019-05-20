

# load necessary packages
library( geojsonio )
library(leaflet)

# transfrom .json file into a spatial polygons data frame
geojson_bracelona <- 
  geojson_read( 
    x = "https://cdn.rawgit.com/martgnz/bcn-geodata/master/barris/barris_geo.json"
    , what = "sp"
  )

# check the class of the object
class( geojson_bracelona )


# end of script #
names(geojson_bracelona)

# Code to call a specific element in the list
#geojson_bracelona $ N_Barri


#Create the basic map with districts

m <- leaflet(geojson_bracelona) %>%
  #setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))


#Create variable for the labels, shown when hovering over the different Neighbourhoods
labels <- sprintf(
  "<strong>Name of Hood: </strong> %s <br/> <strong>Name of District: </strong> %s",
  geojson_bracelona$N_Barri, geojson_bracelona$N_Distri
) %>% lapply(htmltools::HTML)


#add poligons to the map

m %>% addPolygons(  
  
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
    direction = "auto")
)


