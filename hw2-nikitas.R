#Homework 2 - Nikita Setia

library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)

setwd("C:/Users/nikit/Documents/git/hw2-nikitas")

pitt <- read.csv("pitt_clean.csv", header = TRUE, sep = ',')

pdf(NULL)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Pittsburgh 311 Tabset"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      # Creating a checkbox group input for the 'request origin' variable
      checkboxGroupInput("source_select",
                  "Source Type:",
                  choices = levels(pitt$REQUEST_ORIGIN),
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = c("Call Center", "Website", "Control Panel")),
      # Creating a slider input for the 'year' variable
      sliderInput("year_select",
                  "Year:",
                  min = min(pitt$YEAR),
                  max = max(pitt$YEAR),
                  value = c(min(pitt$YEAR), max(pitt$YEAR)),
                  step = 1),
      # Creating a drop down select for the 'neighborhood' variable
      selectInput("neighborhood_select",
                         "Neighborhood:",
                         choices = levels(pitt$NEIGHBORHOOD),
                         multiple = TRUE,
                         selectize = TRUE,
                         selected = c("Brookline", "Carrick")),
      # Create Reset button
      actionButton("reset", "Reset Filters", icon = icon("refresh"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", 
                 plotlyOutput("plot")
        ),
        tabPanel("Table",
                 inputPanel(
                   downloadButton("downloadData","Download Pittsburgh 311 Data")
                 ),
                 fluidPage(DT::dataTableOutput("table"))
        )
      )
    )
  )
)

# Defining server logic
# server <- function(input, output, session) {
#   # Filtered Starwars data
#   pittInput <- reactive({
#     pitt <- pitt %>%
#       # Slider Filter
#       filter(birth_year >= input$birthSelect[1] & birth_year <= input$birthSelect[2])
#     # Homeworld Filter
#     if (length(input$worldSelect) > 0 ) {
#       starwars <- subset(starwars, homeworld %in% input$worldSelect)
#     }
#     
#     return(starwars)
#   })
#   # Reactive melted data
#   mwInput <- reactive({
#     swInput() %>%
#       melt(id = "name")
#   })
#   # Point plot showing Mass, Height and Species
#   output$plot <- renderPlotly({
#     dat <- swInput()
#     ggplotly(
#       ggplot(data = dat, aes(x = mass, y = height, color = species, text = paste0("<b>", name, ":</b> ",
#                                                                                   "<br>Homeworld: ", homeworld,
#                                                                                   "<br>Mass: ", mass,
#                                                                                   "<br>Height: ", height))) + 
#         geom_point() +
#         guides(color = FALSE)
#       , tooltip = "text")
#   })
#   # Data Table
#   output$table <- DT::renderDataTable({
#     starwars <- swInput()
#     
#     subset(starwars, select = c(name, height, mass, birth_year, homeworld, species, films))
#   })
#   # Updating the URL Bar
#   observe({
#     print(reactiveValuesToList(input))
#     session$doBookmark()
#   })
#   onBookmarked(function(url) {
#     updateQueryString(url)
#   })
#   # Download data in the datatable
#   output$downloadData <- downloadHandler(
#     filename = function() {
#       paste("star-wars-", Sys.Date(), ".csv", sep="")
#     },
#     content = function(file) {
#       write.csv(swInput(), file)
#     }
#   )
#   # Reset Filter Data
#   observeEvent(input$reset, {
#     updateSelectInput(session, "worldSelect", selected = c("Naboo", "Tatooine"))
#     updateSliderInput(session, "birthSelect", value = c(min(starwars.load$birth_year, na.rm = T), max(starwars.load$birth_year, na.rm = T)))
#     showNotification("You have successfully reset the filters", type = "message")
#     #alert("You have reset the application!!! <3")
#   })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server, enableBookmarking = "url")