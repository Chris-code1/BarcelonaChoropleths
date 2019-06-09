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
library(ggplot2)
library(dbplyr)
library(roperators)

source("helpers.R")

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

deathsAll <- read.csv("Data/Barcelona_deaths/All_defuncions_edats-quinquennals.csv", sep=",", fileEncoding="UTF-8")

str(deathsAll)

deathsAll_2 <- read.csv("Data/Barcelona_deaths/All_version_2.csv")

str(deathsAll_2)

accidents_2015 <- read.csv("Data/Barcelona_ population/2012_padro_edat_any_a_any_per_sexe.csv")
accidents_2016 <- read.csv("Data/Barcelona_ population/2012_padro_edat_any_a_any_per_sexe.csv")
accidents_2017 <- read.csv("Data/Barcelona_ population/2012_padro_edat_any_a_any_per_sexe.csv")

accidents <- accidents_2016

ui <- dashboardPage(
  dashboardHeader(title = "Barcelona"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Unemployment", tabName = "unemployment", icon = icon("dashboard")),
      menuItem("Unemployment comparison", tabName = "unemployment_comparison", icon = icon("dashboard")),
      menuItem("Population", tabName = "population", icon = icon("dashboard")),
      menuItem("Deaths", tabName = "deaths_districts", icon = icon("dashboard"))
    )
  ),
  source("ui_unemployement.R", local = TRUE)$value
)


server <- function(input, output, session) {
  
  ########################################## Tab 1 #############################################
  #########Bar Chart 1
  output$barPopulation <- renderPlotly({
    
    population <- switch(input$var_year, 
                           "2012" = population_2012,
                           "2013" = population_2013,
                           "2014" = population_2014,
                           "2015" = population_2015,
                           "2016" = population_2016)
    
    if (is.null(input$m_shape_click$id)) {
      Barris <- "el Raval"
    } else {
      Barris <- input$m_shape_click$id
    }
    
    # Population by year
    population %>%
      filter(Nom_Barri==Barris) %>%
      group_by(Sexe) %>%
      summarise(count=sum(Nombre)) %>%
      mutate(percent=paste0(round((count/sum(count))*100, 2), "%")) %>%
      ggplot(aes(x="Barri", y=count)) +
      geom_bar(stat="identity", aes(fill=Sexe)) +
      geom_text(aes(label=percent, group=Sexe), position=position_stack(vjust=0.5)) +
      scale_y_continuous(labels=comma) +
      labs(x=Barris, y="Population", title=paste("Year",input$var_year)) +
      theme_bw()
  })
  
  #########Bar Chart 2
  
  output$barCentile6 <- renderPlotly({

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

      layout(title = input$var_year,
             yaxis = list(title = 'Neighbourhood'), 
             xaxis = list(title = 'Unemployed in percent'))
    


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
    
    pal <- colorBin("YlOrRd", domain = data, bins = bins)
    
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
      
      "<strong>Neighbourhood: </strong> %s
      <br/> <strong>District: </strong> %s
      <br/> <strong>Population 16 - 64 years: </strong> %g
      <br/> <strong>Percent of unemployed: </strong> %g",
      geojson_bracelona$N_Barri, geojson_bracelona$N_Distri, unemployment$Poblacio.16.64.anys, data
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
      layerId = unemployment$Barris,
      
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
  
  ########################################## Tab 2 #############################################
  #########Bar Chart 1
  output$barPopulation2 <- renderPlotly({
    
    population <- switch(input$var_year2, 
                         "2012" = population_2012,
                         "2013" = population_2013,
                         "2014" = population_2014,
                         "2015" = population_2015,
                         "2016" = population_2016)
    
    if (is.null(input$m2_shape_click$id)) {
      Barris <- "el Raval"
    } else {
      Barris <- input$m2_shape_click$id
    }
    
    # Population by year
    population %>%
      filter(Nom_Barri==Barris) %>%
      group_by(Sexe) %>%
      summarise(count=sum(Nombre)) %>%
      mutate(percent=paste0(round((count/sum(count))*100, 2), "%")) %>%
      ggplot(aes(x="Barri", y=count)) +
      geom_bar(stat="identity", aes(fill=Sexe)) +
      geom_text(aes(label=percent, group=Sexe), position=position_stack(vjust=0.5)) +
      scale_y_continuous(labels=comma) +
      labs(x=Barris, y="Population", title=paste("Year",input$var_year2)) +
      theme_bw()
  })
  
  #########Bar Chart 2
  
  output$barCentile_comp <- renderPlotly({
    
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
      
      layout(yaxis = list(title = 'Neighbourhood'), 
             xaxis = list(title = 'Amount of unemployed in percent'))
    
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
      
      
      layout(
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
      "<strong>Neighbourhood: </strong> %s 
      <br/> <strong>District: </strong> %s 
      <br/> <strong>Difference in percent of unemployed: </strong> %g",
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
      layerId = unemployment$Barris,
      
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
 
  ########################################## Tab 4 Population #############################################
  #########Bar Chart 1
  #########Bar Chart 1
  output$barPopulation5 <- renderPlotly({
    
    population <- switch(input$var_year5, 
                         "2012" = population_2012,
                         "2013" = population_2013,
                         "2014" = population_2014,
                         "2015" = population_2015,
                         "2016" = population_2016)
    
    if (is.null(input$m5_shape_click$id)) {
      Barris <- "el Raval"
    } else {
      Barris <- input$m5_shape_click$id
    }
    
    # Population by year
    population %>%
      filter(Nom_Barri==Barris) %>%
      group_by(Sexe) %>%
      summarise(count=sum(Nombre)) %>%
      mutate(percent=paste0(round((count/sum(count))*100, 2), "%")) %>%
      ggplot(aes(x="Barri", y=count)) +
      geom_bar(stat="identity", aes(fill=Sexe)) +
      geom_text(aes(label=percent, group=Sexe), position=position_stack(vjust=0.5)) +
      scale_y_continuous(labels=comma) +
      labs(x=Barris, y="Population", title=paste("Year",input$var_year5)) +
      theme_bw()
  })
  
  #########Bar Chart 2
  
  
  #########Line Chart
  
  
  
  ################Map################
  
  output$m5 <- renderLeaflet({
    
    print("go in renderLeaflet")
    
    #sets the year
    
    population <- switch(input$var_year5, 
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
    
    m5 <- leaflet(geojson_bracelona) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
    
    #Create click listener
    observeEvent(input$m5_shape_click,{
      print(input$m5_shape_click$id)
    })
    
    
    #Create variable for the labels, shown when hovering over the different Neighbourhoods
    labels <- sprintf(
      "<strong>Name of Hood: </strong> %s <br/> <strong>Name of District: </strong> %s <br/> <strong>Number of population: </strong> %g",
      geojson_bracelona$N_Barri, geojson_bracelona$N_Distri, newdata[c(1:73)]
    ) %>% lapply(htmltools::HTML)
    
    
    #add poligons to the map
    
    m5 %>% addPolygons(  
      
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
  
  ########################################## Tab 3 #############################################
  ################# Stacked bar graph
 
  # deathsAll$Edat_quinquennal <- ordered(deathsAll$Edat_quinquennal, levels=c("0-4 anys", "5-9 anys", "10-14 anys", "15-19 anys",
  #                                                                            "20-24 anys", "25-29 anys", "30-34 anys", "35-39 anys",
  #                                                                            "40-44 anys", "45-49 anys", "50-54 anys", "55-59 anys",
  #                                                                            "60-64 anys", "65-69 anys", "70-74 anys", "75-79 anys",
  #                                                                            "80-84 anys", "85-89 anys", "90-94 anys", "95-99 anys"))
  
  deathsAll_2$Edat_quinquennal <- ordered(deathsAll_2$Edat_quinquennal, levels=c("0-19 anys", "20-39 anys", "40-59 anys", "60-79 anys", "80-99 anys"))
  
  output$deaths_graph <- renderPlot({
  
  #   deathsAll %>% 
  #     filter(Any==input$var_year3) %>%
  #     ggplot(aes(x = Nom_Districte, y = Nombre, fill = Edat_quinquennal)) + geom_col(position=input$radio) +
  #     labs(x="District", y="Value", fill="Age", title="Deaths by district and age group")
  # })
  
  deathsAll_2 %>% 
    filter(Any==input$var_year3) %>%
    ggplot(aes(x = Nom_Districte, y = Nombre, fill = Edat_quinquennal)) + geom_col(position=input$radio) +
    labs(x="District", y="Value", fill="Age", title="Deaths by district and age group") +
    scale_fill_brewer(palette = "Set1")
})
  print("LOG: loading of deaths done")
  
  # output$victims_graph <- renderPlot({
    # # Victims by district
    # accidents %>%
    #   group_by(Nom_districte, Descripcio_torn) %>%
    #   #summarise(Victims=sum(Numero_victimes)) %>%
    #   #filter(Nom_districte!="Unknown") %>%
    #   ggplot(aes(x=reorder(Nom_districte, Numero_victimes, sum), y=Numero_victimes, fill=Descripcio_torn)) + 
    #   geom_col(position=position_stack(reverse=FALSE),  colour="black") +
    #   geom_text(aes(label=Numero_victimes, group=Descripcio_torn), position=position_stack(vjust=0.5)) +
    #   scale_fill_manual(values=c("sienna1", "darkolivegreen1", "slateblue1")) +
    #   labs(x="District name", y="Victims", title="Victims by district (2016)") +
    #   theme_bw() +
    #   theme(legend.position="bottom",
    #         legend.title=element_blank()) +
    #   coord_flip()
    # 
    # Accidents by weekday (2017)
#     accidents %>%
#       count(Descripcio_dia_setmana, Descripcio_torn) %>% 
#       mutate(Descripcio_dia_setmana=factor(Descripcio_dia_setmana, levels=c("diumenge", "dissabte", "divendres", "dijous",
#                                                                                   "dimecres", "dimarts", "dilluns"))) %>%
#       ggplot(aes(x=Descripcio_dia_setmana, y=n, fill=Descripcio_torn)) + 
#       geom_col(position=position_stack(reverse=FALSE),  colour="black") +
#       geom_text(aes(label=n, group=Descripcio_torn), position=position_stack(vjust=0.5)) +
#       scale_fill_manual(values=c("sienna1", "darkolivegreen1", "slateblue1")) +
#       labs(x="Weekday", y="Accidents", title="Accidents by weekday (2017)") +
#       theme_bw() +
#       theme(legend.position="bottom",
#             legend.title=element_blank()) +
#       coord_flip()
#     
#     
# })
}
      

shinyApp(ui, server)


