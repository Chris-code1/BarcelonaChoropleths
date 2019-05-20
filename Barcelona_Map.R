

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

#geojson_bracelona $ N_Barri




m <- leaflet(geojson_bracelona) %>%
  #setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))

labels <- sprintf(
  "<strong>Name of Hood: </strong> %s <br/> <strong>Name of District: </strong> %s",
  geojson_bracelona$N_Barri, geojson_bracelona$N_Distri
) %>% lapply(htmltools::HTML)


m %>% addPolygons(  
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  
  highlight = highlightOptions(
  weight = 5,
  color = "#666",
  dashArray = "",
  fillOpacity = 0.7,
  bringToFront = TRUE
),
label = labels,
labelOptions = labelOptions(
  style = list("font-weight" = "normal", padding = "3px 8px"),
  textsize = "15px",
  direction = "auto")
)


