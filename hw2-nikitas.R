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
                         selected = c("Call Center", "Website", "Control Panel")),
      # Creating a slider input for the 'year' variable
      sliderInput("year_select",
                  "Year:",
                  min = min(pitt$YEAR),
                  max = max(pitt$YEAR),
                  value = c(min(pitt$YEAR), max(pitt$YEAR)),
                  step = 1),
      # Creating a drop down select for the 'neighborhood' variable
      selectInput("nbhd_select",
                  "Neighborhood:",
                  choices = levels(pitt$NEIGHBORHOOD),
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = c("Brookline", "Carrick", "South Side Slopes", "Bloomfield", "Squirrel Hill South",
                               "South Side Flats", "Central Lawrenceville", "Knoxville", "Shadyside", 
                               "Lincoln-Lemington-Belmar", "Stanton Heights", "Overbrook", "Squirrel Hill North",
                               "Beechview", "Highland Park")),
      # Creating a drop down select for the 'request type' variable
      selectInput("type_select",
                  "Request Type:",
                  choices = levels(pitt$REQUEST_TYPE),
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = c("Potholes", "Weeds/Debris", "Snow/Ice removal", "Refuse Violations",
                               "Building Maintenance", "Missed Pick Up", "Abandoned Vehicle (parked on street)",
                               "Replace/Repair a Sign", "Litter", "Overgrowth", "Street Light - Repair")),
      # Create Reset button
      actionButton("reset", "Reset Filters", icon = icon("refresh"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plots", 
                 plotlyOutput("year_plot"), plotlyOutput("nbhd_plot"), plotlyOutput("source_plot")
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
server <- function(input, output, session) {
  # Creating filtered 
  pittFiltered <- reactive({
    filt <- pitt %>%
      filter(YEAR >= input$year_select[1] & YEAR <= input$year_select[2] & 
               REQUEST_TYPE == input$type_select)
    if (length(input$source_select) > 0) {
      filt <- subset(filt, REQUEST_ORIGIN %in% input$source_select)
    }
    if (length(input$nbhd_select) > 0) {
      filt <- subset(filt, NEIGHBORHOOD %in% input$nbhd_select)
    }
    return(filt) 
  })

   # Histogram showing Request Types by Year
  output$year_plot <- renderPlotly({
    dat <- pittFiltered() %>% group_by(REQUEST_TYPE, YEAR) %>% summarise(COUNT = n())
    ggplotly(
      ggplot(data = dat, aes(x = YEAR, y = COUNT, color = REQUEST_TYPE)) + 
        geom_line(stat = "identity"))
    })
  
  output$source_plot <- renderPlotly({
    dat <- pittFiltered() %>% group_by(REQUEST_TYPE, REQUEST_ORIGIN) %>% summarise(COUNT = n())
    ggplotly(
      ggplot(data = dat, aes(x = REQUEST_ORIGIN, y = COUNT, fill = REQUEST_TYPE)) +
        geom_bar(stat = "identity")
      )
    })
   
   output$nbhd_plot <- renderPlotly({
     dat <- pittFiltered() %>% group_by(NEIGHBORHOOD, REQUEST_TYPE) %>% summarise(COUNT = n())
     ggplotly(
       ggplot(data = dat, aes(x = NEIGHBORHOOD, y = COUNT, fill = REQUEST_TYPE,
                              text = paste0("<b>", "<br>Neighborhood: ", NEIGHBORHOOD,
                                            "<br>Request Type: ", REQUEST_TYPE, 
                                            "<br>Count: ", COUNT))) +
         geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
         guides(color = FALSE)
       , tooltip = "text")
     })
     
   # Data Table
   output$table <- DT::renderDataTable({
     pitt <- pittFiltered()
     subset(pitt, select = c(REQUEST_TYPE, REQUEST_ORIGIN, STATUS, DEPARTMENT, NEIGHBORHOOD))
   })
   # Updating the URL Bar
   observe({
     print(reactiveValuesToList(input))
     session$doBookmark()
   })
   onBookmarked(function(url) {
     updateQueryString(url)
   })
   # Download data in the datatable
   output$downloadData <- downloadHandler(
     filename = function() {
       paste("pitt-311-", Sys.Date(), ".csv", sep="")
     },
     content = function(file) {
       write.csv(pittFiltered(), file)
     }
)
    # Reset Filter Data
   observeEvent(input$reset, {
     updateSelectInput(session, "worldSelect", selected = c("Naboo", "Tatooine"))
     updateSliderInput(session, "birthSelect", value = c(min(starwars.load$birth_year, na.rm = T), max(starwars.load$birth_year, na.rm = T)))
     alert("You have reset the application!!! <3")
})
}

 # Run the application
 shinyApp(ui = ui, server = server, enableBookmarking = "url")