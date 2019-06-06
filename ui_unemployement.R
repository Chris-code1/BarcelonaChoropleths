dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "unemployment",
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
    tabItem(tabName = "unemployment_comparison",
            fluidRow(
              box(title = "Unemployement per year", width = 4,height = 300, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                  plotlyOutput(outputId = "histCentile2", height = 230)),
              box(title = "Gender ratio per district", width = 4,height = 300, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                  plotlyOutput(outputId = "barPopulation2", height = 230)),
              box(title = "Select Year and/or Month", width = 4,height = 360, solidHeader = TRUE, collapsible = TRUE, status = "primary",
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
    
    tabItem(tabName = "deaths_districts",
            fluidRow(
              box(title = "Select Year and/or Month", width = 3,height = 300, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                  selectInput("var_year3",
                              label = "Choose a year",
                              choices = c("2015",
                                          "2016",
                                          "2017"),
                              selected = "2015"),
                  
                  radioButtons("radio",
                               label = h3("Relative / Absolute"),
                               choices = list("Relative" = "fill", "Absolute" = "stack"),
                               selected = "fill")
        ),
        box(title = "Deaths per district per age group", width = 9,height = 500, solidHeader = TRUE, collapsible = TRUE, status = "primary",
            plotOutput(outputId = "deaths_graph", height = 430) 
        )
      )
      # ),
      # fluidRow(
      #   box(title = "Victims by district", width = 12,height = 600, solidHeader = TRUE, collapsible = TRUE, status = "primary",
      #       plotOutput(outputId = "vicitms_graph", height = 530) 
      #   )
      # )
    )
  )
)
