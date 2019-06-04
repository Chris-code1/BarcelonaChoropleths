#set working directory
setwd("D:/EIT/Git/InfoVis/BarcelonaChoropleths")

# load necessary packages
library( geojsonio )
library(leaflet)
library(plotly)
library(tidyr)
library(RColorBrewer)


unemployment_2012 <- read.csv("Data/Barcelona_unemployment/2012_unemployment.csv", sep=",")
unemployment_2016 <- read.csv("Data/Barcelona_unemployment/2014_unemployment.csv", sep=",")

data_2012 = as.numeric(as.character(sub("," , ".",unemployment_2012$Gener)))
data_2016 = as.numeric(as.character(sub("," , ".",unemployment_2016$Desembre)))

data <- data_2016 - data_2012

#load in Data
#unemployment <- read.csv("Data/Barcelona_unemployment/2016_unemployment.csv", sep=",")

str(data)


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


bins <- c( -2, -1.5, -1, -0.5,  0, 0.5, 1, 1.5, 2)
pal <- colorBin("BrBG", domain = data, bins = bins)



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
  
  addLegend(pal = pal, values = data, opacity = 0.7, title = NULL,
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

unemployment = unemployment_2016


unemployment_comp = unemployment_2012

# Create Df to calculate mean

linechartdf <- data.frame(cbind(as.numeric(as.character(sub("," , ".",unemployment$Gener))),
                                as.numeric(as.character(sub("," , ".",unemployment$Febrer))),
                                as.numeric(as.character(sub("," , ".",unemployment$Febrer))),
                                as.numeric(as.character(sub("," , ".",unemployment$Abril))),
                                as.numeric(as.character(sub("," , ".",unemployment$Maig))),
                                as.numeric(as.character(sub("," , ".",unemployment$Juny))),
                                as.numeric(as.character(sub("," , ".",unemployment$Juliol))),
                                as.numeric(as.character(sub("," , ".",unemployment$Agost))),
                                as.numeric(as.character(sub("," , ".",unemployment$Setembre))),
                                as.numeric(as.character(sub("," , ".",unemployment$Octubre))),
                                as.numeric(as.character(sub("," , ".",unemployment$Novembre))),
                                as.numeric(as.character(sub("," , ".",unemployment$Desembre)))))


linechartdf_comp <- data.frame(cbind(as.numeric(as.character(sub("," , ".",unemployment_comp$Gener))),
                                     as.numeric(as.character(sub("," , ".",unemployment_comp$Febrer))),
                                     as.numeric(as.character(sub("," , ".",unemployment_comp$Febrer))),
                                     as.numeric(as.character(sub("," , ".",unemployment_comp$Abril))),
                                     as.numeric(as.character(sub("," , ".",unemployment_comp$Maig))),
                                     as.numeric(as.character(sub("," , ".",unemployment_comp$Juny))),
                                     as.numeric(as.character(sub("," , ".",unemployment_comp$Juliol))),
                                     as.numeric(as.character(sub("," , ".",unemployment_comp$Agost))),
                                     as.numeric(as.character(sub("," , ".",unemployment_comp$Setembre))),
                                     as.numeric(as.character(sub("," , ".",unemployment_comp$Octubre))),
                                     as.numeric(as.character(sub("," , ".",unemployment_comp$Novembre))),
                                     as.numeric(as.character(sub("," , ".",unemployment_comp$Desembre)))))


data_long <- gather(linechartdf, factor_key=TRUE)
data_long_comp <- gather(linechartdf_comp, factor_key=TRUE)

# Calculate mean
meanunemployment <- data_long %>% group_by(key) %>% summarise(mean = mean(value))
meanunemployment_comp <- data_long_comp %>% group_by(key) %>% summarise(mean = mean(value))

#subtract one from another to get the comparison

mean = as.numeric(as.character(sub("," , ".",meanunemployment$mean))) - as.numeric(as.character(sub("," , ".",meanunemployment_comp$mean)))

meanunemployment_sub_df <- data.frame(meanunemployment_sub)


# Add array with month names

month <- c('January', 'February', 'March', 'April', 'May', 'June', 'July',
           'August', 'September', 'October', 'November', 'December')

meanunemployment_sub_df$month <- month

#The default order will be alphabetized unless specified as below:
meanunemployment_sub_df$month <- factor(meanunemployment_sub_df$month, levels = meanunemployment_sub_df[["month"]])

p <- plot_ly(meanunemployment_sub_df, x = ~month, y = ~mean, type = 'scatter', mode = 'lines') %>%
layout(title = "Barcelona unemployed",
       yaxis = list(title = 'Unemployed in Percent'), 
       xaxis = list(title = 'Month'))

p
