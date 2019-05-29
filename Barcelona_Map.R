# load necessary packages
library( geojsonio )
library(leaflet)
library(plotly)
library(tidyr)


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


# Create Df to calculate mean

linechartdf <- data.frame(cbind(as.numeric(as.character(sub("," , ".",unemployment$Gener))),as.numeric(as.character(sub("," , ".",unemployment$Febrer))),as.numeric(as.character(sub("," , ".",unemployment$Març))),as.numeric(as.character(sub("," , ".",unemployment$Abril))),as.numeric(as.character(sub("," , ".",unemployment$Maig))),as.numeric(as.character(sub("," , ".",unemployment$Juny))),as.numeric(as.character(sub("," , ".",unemployment$Juliol))),as.numeric(as.character(sub("," , ".",unemployment$Agost))),as.numeric(as.character(sub("," , ".",unemployment$Setembre))),as.numeric(as.character(sub("," , ".",unemployment$Octubre))),as.numeric(as.character(sub("," , ".",unemployment$Novembre))),as.numeric(as.character(sub("," , ".",unemployment$Desembre)))))

data_long <- gather(linechartdf, factor_key=TRUE)

# Calculate mean
meanunemployment <- data_long %>% group_by(key) %>% summarise(mean = mean(value))

# Add array with month names

month <- c('January', 'February', 'March', 'April', 'May', 'June', 'July',
           'August', 'September', 'October', 'November', 'December')

meanunemployment$month <- month

#The default order will be alphabetized unless specified as below:
meanunemployment$month <- factor(meanunemployment$month, levels = meanunemployment[["month"]])

p <- plot_ly(meanunemployment, x = ~month, y = ~mean, type = 'scatter', mode = 'lines')

p
