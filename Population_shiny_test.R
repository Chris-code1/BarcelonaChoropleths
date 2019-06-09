#set working directory
setwd("D:/EIT/Git/InfoVis/BarcelonaChoropleths")

# load necessary packages
library(geojsonio)
library(leaflet)
library(plotly)
library(tidyr)
library(shiny)
library(knitr)
library(scales)
library(shinydashboard)
library(dbplyr)
library(roperators)


population_2012 <- read.csv("Data/Barcelona_ population/2012_padro_edat_any_a_any_per_sexe.csv")
population_2013 <- read.csv("Data/Barcelona_ population/2013_padro_edat_any_a_any_per_sexe.csv")
population_2014 <- read.csv("Data/Barcelona_ population/2014_padro_edat_any_a_any_per_sexe.csv")
population_2015 <- read.csv("Data/Barcelona_ population/2015_padro_edat_any_a_any_per_sexe.csv")
population_2016 <- read.csv("Data/Barcelona_ population/2016_padro_edat_any_a_any_per_sexe.csv")

population <- population_2012

str(population)

################Map data##################

# transfrom .json file into a spatial polygons data frame
geojson_bracelona <- 
  geojson_read( 
    x = "https://cdn.rawgit.com/martgnz/bcn-geodata/master/barris/barris_geo.json"
    , what = "sp"
  )



# check the class of the object
class( geojson_bracelona )



# Code to call a specific element in the list
bins <- c(0, 3, 6, 9, 12, 15, 18, Inf)

#create object m 

m <- leaflet(geojson_bracelona) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))

print("LOG: m created")


print("LOG: loading of variables done")



##################################Shiny##########################################


ui <- dashboardPage(
  dashboardHeader(title = "Barcelona"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Unemployment", tabName = "unemployment", icon = icon("dashboard")),
      menuItem("Unemployment compare", tabName = "unemployment_comparison", icon = icon("dashboard")),
      menuItem("AirQuality", tabName = "AirQuality", icon = icon("dashboard")),
      menuItem("Population", tabName = "Population", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "unemployment",
              fluidRow(
                box(title = "Map of Barcelona", width = 8, height = 800, solidHeader = TRUE, collapsible = TRUE, status = "danger",
                    leafletOutput(outputId = "m", height = 700)
                ),
                box(title = "Gender ratio per district", width = 4,height = 300, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                    plotlyOutput(outputId = "barPopulation", height = 230)),
                box(title = "Select Year and/or Month", width = 4,height = 310, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                    selectInput("var_year", 
                                label = "Choose the year of interest",
                                choices = c("2012", 
                                            "2013",
                                            "2014", 
                                            "2015",
                                            "2016"),
                                selected = "2016"),
                    
                    print("Check the gender to be shown:"),
                    checkboxInput("gender_male", "Male", TRUE),
                    checkboxInput("gender_female", "Female", TRUE),
                    
                    sliderInput("range", "Age:",
                                min(0), max(99),
                                value = c(0, 99)
                              
                    )
                  )

              )
      ),
      
      # Second tab content
      tabItem(tabName = "AirQuality",
              h2("Widgets tab content")
      )
    )
  )
)


server <- function(input, output, session) {
  
  #########Bar Chart 1
  output$barPopulation <- renderPlotly({
    
    population <- switch(input$var_year, 
                         "2012" = population_2012,
                         "2013" = population_2013,
                         "2014" = population_2014,
                         "2015" = population_2015,
                         "2016" = population_2016)
    
    # Population by year
    population %>%
      filter(Codi_Barri==input$m_shape_click$id) %>%
      group_by(Sexe) %>%
      summarise(count=sum(Nombre)) %>%
      mutate(percent=paste0(round((count/sum(count))*100, 2), "%")) %>%
      ggplot(aes(x="Barri", y=count)) +
      geom_bar(stat="identity", aes(fill=Sexe)) +
      geom_text(aes(label=percent, group=Sexe), position=position_stack(vjust=0.5)) +
      scale_y_continuous(labels=comma) +
      labs(x="Barri", y="Population", title=paste("Year",input$var_year)) +
      theme_bw()
  })
  
  #########Bar Chart 2

  
  #########Line Chart
  
 
  
  ################Map################
  
  output$m <- renderLeaflet({
    
    print("go in renderLeaflet")
    
    #sets the year
    
    population <- switch(input$var_year, 
                         "2012" = population_2012,
                         "2013" = population_2013,
                         "2014" = population_2014,
                         "2015" = population_2015,
                         "2016" = population_2016)
    
    
    print("go in ifelse function")
    if (input$gender_male == FALSE) {
      fil <- c("Dona")
      print("Male selected")
      
    } else if (input$gender_female == FALSE) {
      fil <- c("Home")
      print("Female selected")
    } else
      fil <- c("Home", "Dona")
      print("All selected")
    

    population <- population[population$Sexe %in% fil, ]

    
    as.numeric(population$Edat.any.a.any %-=% '[a-z,Ã©]')
    population <- filter(population, Edat.any.a.any  > input$range[1])
    population <- filter(population, Edat.any.a.any  < input$range[2] )
    
    
    newdata <-    population %>% 
      group_by(population$Codi_Barri) %>% 
      #filter(Sexe == switch_gender) %>%
      summarise(Nombre = sum(Nombre)) 
    
    
    
    print(newdata)
    
    newdata = as.numeric(as.character(sub("," , ".",newdata$Nombre)))
    
    
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
    
    
    #### Create automatic custom bins
    
    newbin = ceiling(max(newdata)/6)
    
    for (i in 0:6) {
      bins[i+1] <- newbin * i
    }
    
    #bins <- c( 0, 10000, 20000, 30000, 40000 , 50000, 60000 , Inf )
    
    pal <- colorBin("Blues", domain = data, bins = bins)
    
    
    
    #Create the basic map with districts
    
    m <- leaflet(geojson_bracelona) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
    
    #Create click listener
    observeEvent(input$m_shape_click,{
      print(input$m_shape_click$id)
    })
    
    #Create variable for the labels, shown when hovering over the different Neighbourhoods
    labels <- sprintf(
      "<strong>Name of Hood: </strong> %s <br/> <strong>Name of District: </strong> %s <br/> <strong>Number of population: </strong> %g",
      geojson_bracelona$N_Barri, geojson_bracelona$N_Distri, newdata[c(1:73)]
    ) %>% lapply(htmltools::HTML)
    
    
    #add poligons to the map
    
    m %>% addPolygons(  
      
      #fill of tiles depending on bins and population density
      fillColor = ~pal(newdata),
      
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
                position = "topright")
    
  })
  
  
}


shinyApp(ui, server)