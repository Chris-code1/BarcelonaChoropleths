#set working directory
#setwd("D:/EIT/Git/InfoVis/BarcelonaChoropleths")

# load necessary packages
library(geojsonio)
library(leaflet)
library(plotly)
library(tidyr)
library(shiny)
library(knitr)
library(scales)
library(shinydashboard)

#load in Data
unemployment_2012 <- read.csv("Data/Barcelona_unemployment/2012_unemployment.csv", sep=",")
unemployment_2013 <- read.csv("Data/Barcelona_unemployment/2013_unemployment.csv", sep=",")
unemployment_2014 <- read.csv("Data/Barcelona_unemployment/2014_unemployment.csv", sep=",")
unemployment_2015 <- read.csv("Data/Barcelona_unemployment/2015_unemployment.csv", sep=",")
unemployment_2016 <- read.csv("Data/Barcelona_unemployment/2016_unemployment.csv", sep=",")

unemployment <- unemployment_2012

str(unemployment)

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

data_start = as.numeric(as.character(sub("," , ".",unemployment$Gener)))

print(data)

# Code to call a specific element in the list
bins <- c(0, 3, 6, 9, 12, 15, 18, Inf)
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

print("LOG: m created")







print("LOG: loading of variables done")



##################################Shiny##########################################


# ui <- bootstrapPage(
#   tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
#   leafletOutput("m", width = "100%", height = "100%"),
#   absolutePanel(top = 10, right = 10,
ui <- dashboardPage(
  dashboardHeader(title = "Barcelona"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Unemployement", tabName = "unemployement", icon = icon("dashboard")),
      menuItem("Air Quality", tabName = "AirQuality", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "unemployement",
      fluidRow(
       box(title = "Unemployement per year", width = 4,height = 300, solidHeader = TRUE, collapsible = TRUE, status = "primary",
         plotlyOutput(outputId = "histCentile", height = 230)),
       box(title = "Gender ratio per district", width = 4,height = 300, solidHeader = TRUE, collapsible = TRUE, status = "primary",
         plotlyOutput(outputId = "barPopulation", height = 230)),
       box(title = "Select Year and/or Month", width = 4,height = 300, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                selectInput("var_year", 
                            label = "Choose a year",
                            choices = c("2012", 
                                        "2013",
                                        "2014", 
                                        "2015",
                                        "2016"),
                            selected = "2012"),
                
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
                            selected = "January"))
     ),
     fluidRow(
     box(title = "Map of Barcelona", width = 12, height = 800, solidHeader = TRUE, collapsible = TRUE, status = "danger",
       leafletOutput(outputId = "m", height = 700)
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
  
  output$barCentile <- renderPlotly({

    unemployment <- switch(input$var_year,
                           "2012" = unemployment_2012,
                           "2013" = unemployment_2013,
                           "2014" = unemployment_2014,
                           "2015" = unemployment_2015,
                           "2016" = unemployment_2016)



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

  })
  
  #########Line Chart
  
  #Graph with unemployment in average
  
  
  output$histCentile <- renderPlotly({
    
    
    unemployment <- switch(input$var_year, 
                           "2012" = unemployment_2012,
                           "2013" = unemployment_2013,
                           "2014" = unemployment_2014,
                           "2015" = unemployment_2015,
                           "2016" = unemployment_2016)

    # Create Df to calculate mean
    
    linechartdf <- data.frame(cbind(as.numeric(as.character(sub("," , ".",unemployment$Gener))),as.numeric(as.character(sub("," , ".",unemployment$Febrer))),as.numeric(as.character(sub("," , ".",unemployment$Febrer))),as.numeric(as.character(sub("," , ".",unemployment$Abril))),as.numeric(as.character(sub("," , ".",unemployment$Maig))),as.numeric(as.character(sub("," , ".",unemployment$Juny))),as.numeric(as.character(sub("," , ".",unemployment$Juliol))),as.numeric(as.character(sub("," , ".",unemployment$Agost))),as.numeric(as.character(sub("," , ".",unemployment$Setembre))),as.numeric(as.character(sub("," , ".",unemployment$Octubre))),as.numeric(as.character(sub("," , ".",unemployment$Novembre))),as.numeric(as.character(sub("," , ".",unemployment$Desembre)))))
    
    data_long <- gather(linechartdf, factor_key=TRUE)
    
    # Calculate mean
    meanunemployment <- data_long %>% group_by(key) %>% summarise(mean = mean(value))
    
    # Add array with month names
    
    month <- c('January', 'February', 'March', 'April', 'May', 'June', 'July',
               'August', 'September', 'October', 'November', 'December')
    
    meanunemployment$month <- month
    
    #The default order will be alphabetized unless specified as below:
    meanunemployment$month <- factor(meanunemployment$month, levels = meanunemployment[["month"]])
    
    plot_ly(meanunemployment, x = ~month, y = ~mean, type = 'scatter', mode = 'lines') %>%
      layout(title = input$var_year,
             yaxis = list(title = 'Unemployed in %'), 
             xaxis = list(title = 'Month'))
  })
  
  ################Map################
  
  output$m <- renderLeaflet({
    
    # popup1 <- paste0("<span style='color: #7f0000'><strong>18-25 year olds 2000</strong></span>",
    #                  "<br><span style='color: salmon;'><strong>District: </strong></span>",
    #                  unemployment$Barris,
    #                  "<br><span style='color: salmon;'><strong>relative amount: </strong></span>",
    #                  unemployment$Barris
    #                  ,"<br><span style='color: salmon;'><strong>absolute amount: </strong></span>",
    #                  unemployment$Barris
    # )
    
    unemployment <- switch(input$var_year, 
                         "2012" = unemployment_2012,
                          "2013" = unemployment_2013,
                          "2014" = unemployment_2014,
                          "2015" = unemployment_2015,
                          "2016" = unemployment_2016)
    
    
    
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
    
    #Create variable for the labels, shown when hovering over the different Neighbourhoods
    labels <- sprintf(
      "<strong>Name of Hood: </strong> %s <br/> <strong>Name of District: </strong> %s <br/> <strong>Number of unemployed: </strong> %g",
      geojson_bracelona$N_Barri, geojson_bracelona$N_Distri, data
    ) %>% lapply(htmltools::HTML)
    
    print("LOG: loading of labels done")
    
    
    
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    
    
    leaflet(geojson_bracelona) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
    
    #Create click listener
    observeEvent(input$m_shape_click,{
      print("LOG: clicked!")
      print(input$m_shape_click$id)
    })
    
    m %>% addPolygons(  
      #fill of tiles depending on bins and population density
      fillColor = ~pal(data),
      
      #creates the dashed line in between the districts
      
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      #popup = popup1,
      layerId = unemployment$Codi_Barri,
      
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
  
  })
  
  
  }
      

shinyApp(ui, server)


