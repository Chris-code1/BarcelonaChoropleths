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



# Code to call a specific element in the list
bins <- c(0, 3, 6, 9, 12, 15, 18, Inf)

#create object m 

m2<- leaflet(geojson_bracelona) %>%
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
      tabItem(tabName = "unemployment_comparison",
              fluidRow(
                box(title = "Unemployement per year", width = 4,height = 300, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                    plotlyOutput(outputId = "histCentile2", height = 230)),
                box(title = "Gender ratio per district", width = 4,height = 300, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                    plotlyOutput(outputId = "barPopulation2", height = 230)),
                box(title = "Select Year and/or Month", width = 4,height = 350, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                    selectInput("var_year2", 
                                label = "Choose the year of interest",
                                choices = c("2012", 
                                            "2013",
                                            "2014", 
                                            "2015",
                                            "2016"),
                                selected = "2016"),
                    
                    selectInput("var2", 
                                label = "Choose the month of interest",
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
                    
                    #Stuff to be compared with
                    
                    selectInput("var_year_comp", 
                                label = "Choose the year to compare with",
                                choices = c("2012", 
                                            "2013",
                                            "2014", 
                                            "2015",
                                            "2016"),
                                selected = "2012"),
                    
                    selectInput("var_month_comp", 
                                label = "Choose the month to compare with",
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
                    leafletOutput(outputId = "m2", height = 700)
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "unemployment_comparison",
              h2("Comparison of unemployment")
      )
    )
  )
)


server <- function(input, output, session) {
  
  #########Bar Chart 1
  output$barPopulation2 <- renderPlotly({
    
    population <- switch(input$var_year2, 
                           "2012" = population_2012,
                           "2013" = population_2013,
                           "2014" = population_2014,
                           "2015" = population_2015,
                           "2016" = population_2016)
    
    # Population by year
    population %>%
      filter(Codi_Barri==input$m2_shape_click$id) %>%
      group_by(Sexe) %>%
      summarise(count=sum(Nombre)) %>%
      mutate(percent=paste0(round((count/sum(count))*100, 2), "%")) %>%
      ggplot(aes(x="Barri", y=count)) +
      geom_bar(stat="identity", aes(fill=Sexe)) +
      geom_text(aes(label=percent, group=Sexe), position=position_stack(vjust=0.5)) +
      scale_y_continuous(labels=comma) +
      labs(x="Barri", y="Population", title=paste("Year",input$var_year2)) +
      theme_bw()
  })
  
  #########Bar Chart 2
  
  output$barCentile <- renderPlotly({

    unemployment <- switch(input$var_year2,
                           "2012" = unemployment_2012,
                           "2013" = unemployment_2013,
                           "2014" = unemployment_2014,
                           "2015" = unemployment_2015,
                           "2016" = unemployment_2016)



    switch_month <- switch(input$var2,
                           "January" = unemployment$Gener,
                           "February" = unemployment$Febrer,
                           "March" = unemployment$Gener,
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
  
  
  output$histCentile2 <- renderPlotly({
    
    
    unemployment <- switch(input$var_year2, 
                           "2012" = unemployment_2012,
                           "2013" = unemployment_2013,
                           "2014" = unemployment_2014,
                           "2015" = unemployment_2015,
                           "2016" = unemployment_2016)
    
    unemployment_comp <- switch(input$var_year_comp, 
                           "2012" = unemployment_2012,
                           "2013" = unemployment_2013,
                           "2014" = unemployment_2014,
                           "2015" = unemployment_2015,
                           "2016" = unemployment_2016)

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
    meanunemployment_comp <- data_long_comp %>% group_by(key) %>% summarise(mean_comp = mean(value))
    
    #subtract one from another to get the comparison
    
    #mean = as.numeric(as.character(sub("," , ".",meanunemployment$mean))) - as.numeric(as.character(sub("," , ".",meanunemployment_comp$mean)))
    
    #meanunemployment_sub_df <- data.frame(mean)
    
    
    # Add array with month names
    
    month <- c('January', 'February', 'March', 'April', 'May', 'June', 'July',
               'August', 'September', 'October', 'November', 'December')
    
    
    
    meanunemployment = merge(meanunemployment, meanunemployment_comp, by.x="key", by.y="key")
    
    meanunemployment$month <- month

    #The default order will be alphabetized unless specified as below:
    meanunemployment$month <- factor(meanunemployment$month, levels = meanunemployment[["month"]])
    
    print(meanunemployment)
    
    plot_ly(meanunemployment, x = ~month, y = ~mean, type = 'scatter', mode = 'lines+markers', name = input$var_year2) %>%
        add_trace(y = ~mean_comp, name = input$var_year_comp, mode = 'lines+markers') %>%
      
      
      layout(title = paste("Mean unemployment of Barcelona"),
             yaxis = list(title = 'Unemployed in %'), 
             xaxis = list(title = 'Month'))
  })
  
  ################Map################
  
  output$m2<- renderLeaflet({
    
    # Sets the bins
    bins <- c( -Inf , -2.5, -2, -1.5, -1, -0.5,  0, 0.5, 1, 1.5, 2)
    
    
    #sets the year
    
    unemployment_high <- switch(input$var_year2, 
                         "2012" = unemployment_2012,
                          "2013" = unemployment_2013,
                          "2014" = unemployment_2014,
                          "2015" = unemployment_2015,
                          "2016" = unemployment_2016)
    
    #sets the month, based on the year selected
    
    switch_month_high <- switch(input$var2, 
                   "January" = unemployment_high$Gener,
                   "February" = unemployment_high$Febrer,
                   "March" = unemployment_high$Agost,
                   "April" = unemployment_high$Abril,
                   "May" = unemployment_high$Maig,
                   "June" = unemployment_high$Juny,
                   "July" = unemployment_high$Juliol,
                   "August" = unemployment_high$Agost,
                   "September" = unemployment_high$Setembre,
                   "October" = unemployment_high$Octubre,
                   "November" = unemployment_high$Novembre,
                   "December" = unemployment_high$Desembre)
    
    #sets the year
    
    unemployment_low <- switch(input$var_year_comp, 
                           "2012" = unemployment_2012,
                           "2013" = unemployment_2013,
                           "2014" = unemployment_2014,
                           "2015" = unemployment_2015,
                           "2016" = unemployment_2016)
    
    #sets the month, based on the year selected
    
    switch_month_low <- switch(input$var_month_comp, 
                           "January" = unemployment_low$Gener,
                           "February" = unemployment_low$Febrer,
                           "March" = unemployment_low$Gener,
                           "April" = unemployment_low$Abril,
                           "May" = unemployment_low$Maig,
                           "June" = unemployment_low$Juny,
                           "July" = unemployment_low$Juliol,
                           "August" = unemployment_low$Agost,
                           "September" = unemployment_low$Setembre,
                           "October" = unemployment_low$Octubre,
                           "November" = unemployment_low$Novembre,
                           "December" = unemployment_low$Desembre)

    

    data_high = as.numeric(as.character(sub("," , ".",switch_month_high)))
    print("Data high:", data_high)
    
    data_low = as.numeric(as.character(sub("," , ".",switch_month_low)))
    print("Data low:",data_low)
    
    data = data_high - data_low
    
    #print("Data:", data)
    
    pal <- colorBin("BrBG", domain = data, bins = bins)
    
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
    
    m2%>% addPolygons(  
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
                position = "topright")
  
  })
  
  
  }
      

shinyApp(ui, server)


