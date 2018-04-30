library(shiny)
library(tidyverse)
library(tidycensus)

source("api-keys.R")
census_api_key(api.key.census)

#Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("American Community Survey"),
  
  # Sidebar Layout
  sidebarLayout(
    
    # SelectInput for State and Type of data
    sidebarPanel(
      
      selectInput("State", "State",
                  choices = state.abb,
                  selected = "NJ"),
      
      radioButtons("Type", "Type",
                   choices = list("MedianGrossRent",
                                  "MedianHouseholdIncome",
                                  "Ratio"), 
                   selected = "ratio")),
    
    mainPanel(plotOutput("Plot"))
  )
)

# Define server logic required to draw a plot
server <- function(input, output) {
  
  reduced_df <- reactive({
    get_acs(
      geography = "tract",
      variables = c(MedianHouseholdIncome = "B19013_001", MedianGrossRent = "B25064_001"),
      state = input$State,
      geometry = TRUE
    ) %>% 
      .[, -5] %>% 
      data.frame() %>% 
      spread(key = variable, value = estimate) %>% 
      mutate(ratio = MedianGrossRent / MedianHouseholdIncome)
  })
  
  output$Plot <- renderPlot({
    reduced_df() %>% 
      ggplot(aes_string(fill = input$Type)) + 
      geom_sf() + 
      ggtitle(input$Type) + 
      scale_fill_gradientn(colours = rainbow(7))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)