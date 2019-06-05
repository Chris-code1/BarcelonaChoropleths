# Load libraries
library(shiny)
library(tidyverse)
library(knitr)
library(scales)

# Read the data
deathsAll <- read.csv("Data/All_defuncions_edats-quinquennals.csv", sep=",", fileEncoding="UTF-8")
deaths2017 <- read.csv("Data/2017_defuncions_edats-quinquennals.csv", sep=",", fileEncoding="UTF-8")
deaths2016 <- read.csv("Data/2016_defuncions_edats-quinquennals.csv", sep=",", fileEncoding="UTF-8")
deaths2015 <- read.csv("Data/2015_defuncions_edats-quinquennals.csv", sep=",", fileEncoding="UTF-8")

ui <- fluidPage(
  titlePanel("Deaths by age"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Graph of deaths by age throught the years (2015-2017)"),
      
      sliderInput("range", 
                  label = "Range of interest:",
                  min = 2015, max = 2017, value = c(2015))
    ),
    
    mainPanel(
      plotOutput("ggplot1",height = 400),
      plotOutput("ggplot2",height = 400))
  )
)
# Define server logic ----
server <- function(input, output) {
  # Ordered levels
  deathsAll$Edat_quinquennal <- ordered(deathsAll$Edat_quinquennal, levels=c("0-4 anys", "5-9 anys", "10-14 anys", "15-19 anys",
                                                                             "20-24 anys", "25-29 anys", "30-34 anys", "35-39 anys",
                                                                             "40-44 anys", "45-49 anys", "50-54 anys", "55-59 anys",
                                                                             "60-64 anys", "65-69 anys", "70-74 anys", "75-79 anys",
                                                                             "80-84 anys", "85-89 anys", "90-94 anys", "95-99 anys"))
  
  output$ggplot1 <- renderPlot({
    deathsAll %>%
      filter(Any==input$range[1]) %>%
      group_by(Edat_quinquennal) %>%
      summarise(Count=sum(Nombre)) %>%
      ggplot(aes(x=Edat_quinquennal, y=Count)) +
      geom_bar(stat="identity", aes(fill=Count), show.legend=FALSE) + 
      geom_label(aes(label=Count), size=2.5) +
      scale_fill_gradient(low="paleturquoise", high="paleturquoise4") +
      labs(x="Age", y="Deaths", title="Deaths by age") +
      scale_y_continuous(labels=comma) +
      coord_flip() +
      theme_bw() 
  })
  
  output$ggplot2 <- renderPlot({
    deathsAll %>%
      filter(Any==input$range[1]) %>%
      group_by(Nom_Districte) %>%
      summarise(Count=sum(Nombre)) %>%
      arrange(desc(Count)) %>%
      ggplot(aes(x=reorder(Nom_Districte, Count), y=Count)) +
      geom_bar(stat="identity", aes(fill=Count), show.legend=FALSE) +
      geom_label(aes(label=Count)) +
      scale_fill_gradient(low="paleturquoise", high="paleturquoise4") +
      labs(x="District", y="Deaths", title="Deaths by district") +
      scale_y_continuous(labels=comma) +
      coord_flip() +
      theme_bw() 
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)