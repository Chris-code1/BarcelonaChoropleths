

# load necessary packages
library( geojsonio )
library(leaflet)

# transfrom .json file into a spatial polygons data frame
states <- 
  geojson_read( 
    x = "https://cdn.rawgit.com/martgnz/bcn-geodata/master/barris/barris_geo.json"
    , what = "sp"
  )

# check the class of the object
class( states )
# [1] "SpatialPolygonsDataFrame"
# attr(,"package")
# [1] "sp"

# end of script #
names(states)

m <- leaflet(states) %>%
  #setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))

m %>% addPolygons()


