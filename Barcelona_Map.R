# load necessary packages
library( geojsonio )
library(leaflet)
library(plotly)

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

# Code to call a specific element in the list
#geojson_bracelona $ N_Barri


bins <- c(0, 4, 8, 12, 15, Inf)
pal <- colorBin("YlOrRd", domain = unemployment$Gener, bins = bins)

data = as.numeric(as.character(sub("," , ".",unemployment$Gener)))

#Create the basic map with districts

m <- leaflet(geojson_bracelona) %>%
  #setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))


#Create variable for the labels, shown when hovering over the different Neighbourhoods
labels <- sprintf(
  "<strong>Name of Hood: </strong> %s <br/> <strong>Name of District: </strong> %s <br/> <strong>Number of unemployed: </strong> %g",
  geojson_bracelona$N_Barri, geojson_bracelona$N_Distri, data
) %>% lapply(htmltools::HTML)


#add poligons to the map

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


##################################Barchart##########################################

## create Dataframe for the barchart

barplotdata <- data.frame(unemployment$Barris, data, stringAsFactors = FALSE)

#sort the dataframe by unemployment rate

barplotdata$unemployment.Barris <- factor(barplotdata$unemployment.Barris, levels = unique(barplotdata$unemployment.Barris)[order(barplotdata$data, decreasing = TRUE)])

#create the plot

barplot <- plot_ly(barplotdata,
                   x = ~data,
                   y = ~unemployment.Barris,
                   type = "bar",
                   name = "unemployment rate") %>%
  
            layout(yaxis = list(title = 'Neighbourhood'), xaxis = list(title = 'Amount of unemployed in percent'))
  
barplot

##################################Linechart##########################################