#Homework 2 - Nikita Setia

library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)
library(shinyjs)

setwd("C:/Users/nikit/Documents/git/hw2-nikitas")

pitt <- read.csv("pitt_clean.csv", header = TRUE, sep = ',')

pdf(NULL)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Add Shinyjs
  useShinyjs(),
  # Application title
  titlePanel("Pittsburgh 311 Tabset"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      # Creating a checkbox group input for the 'request origin' or source variable
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
      # Creating a select input for the 'neighborhood' variable
      selectInput("nbhd_select",
                  "Neighborhood:",
                  choices = levels(pitt$NEIGHBORHOOD),
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = c("Brookline", "Carrick", "South Side Slopes", "Bloomfield", "Squirrel Hill South",
                               "South Side Flats", "Central Lawrenceville", "Knoxville", "Shadyside", 
                               "Lincoln-Lemington-Belmar", "Stanton Heights", "Overbrook", "Squirrel Hill North",
                               "Beechview", "Highland Park")),
      # Creating a select input for the 'request type' variable
      selectInput("type_select",
                  "Request Type:",
                  choices = levels(pitt$REQUEST_TYPE),
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = c("Potholes", "Weeds/Debris", "Snow/Ice removal", "Refuse Violations",
                               "Building Maintenance", "Missed Pick Up", "Abandoned Vehicle (parked on street)",
                               "Replace/Repair a Sign", "Litter", "Overgrowth", "Street Light - Repair")),
      # Creating a Reset button
      actionButton("reset", "Reset Filters", icon = icon("refresh"))
    ),
    # Main panel consisting of 2 tabs: one for plots and another for the data table
    mainPanel(
      tabsetPanel(
        tabPanel("Plots", 
                 plotlyOutput("year_plot"), 
                 # adding space between plots
                 br(),
                 br(),
                 plotlyOutput("nbhd_plot"), 
                 # adding space between plots
                 br(),
                 br(),
                 plotlyOutput("source_plot")
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
  # Creating filtered pitt 311 data
  pittFiltered <- reactive({
    filt <- pitt %>%
      # creating filters for year_select, type_select, source_select and nbhd_select inputs
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

   # Line graph showing count of Request Types by Year
  output$year_plot <- renderPlotly({
    dat <- pittFiltered() %>% group_by(REQUEST_TYPE, YEAR) %>% summarise(COUNT = n())
    ggplotly(
      ggplot(data = dat, aes(x = YEAR, y = COUNT, color = REQUEST_TYPE)) + 
        xlab("Year") + ylab("Count") +
        labs(color = "Request Type") + 
        ggtitle("Request Types by Year") +
        geom_line(stat = "identity"))
    })
  # Bar graph showing count of Request Types by Request Source/Origin
  output$source_plot <- renderPlotly({
    dat <- pittFiltered() %>% group_by(REQUEST_TYPE, REQUEST_ORIGIN) %>% summarise(COUNT = n())
    ggplotly(
      ggplot(data = dat, aes(x = REQUEST_ORIGIN, y = COUNT, fill = REQUEST_TYPE)) +
        xlab("Source") + ylab("Count") +
        labs(fill = "Request Type") + 
        ggtitle("Request Types by Source") +
        geom_bar(stat = "identity")
      )
    })
  
   # Point chart showing count of Request Types by Neighborhood
   output$nbhd_plot <- renderPlotly({
     dat <- pittFiltered() %>% group_by(NEIGHBORHOOD, REQUEST_TYPE) %>% summarise(COUNT = n())
     ggplotly(
       ggplot(data = dat, aes(x = NEIGHBORHOOD, y = COUNT, fill = REQUEST_TYPE,
                              text = paste0("<b>", "<br>Neighborhood: ", NEIGHBORHOOD,
                                            "<br>Request Type: ", REQUEST_TYPE, 
                                            "<br>Count: ", COUNT))) +
         geom_point() + 
         xlab("Neighborhood") + ylab("Count") +
         labs(fill = "Request Type") + 
         ggtitle("Request Types by Neighborhood") +
         theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
         guides(color = FALSE)
       , tooltip = "text")
     })
     
   # Data Table with 311 data filters
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
   # Downloading data in the datatable
   output$downloadData <- downloadHandler(
     filename = function() {
       paste("pitt-311-", Sys.Date(), ".csv", sep="")
     },
     content = function(file) {
       write.csv(pittFiltered(), file)
     }
)
    # Reseting Filter Data
   observeEvent(input$reset, {
     updateSelectInput(session, "nbhd_select", selected = c("Brookline", "Carrick", "South Side Slopes", "Bloomfield", "Squirrel Hill South",
                                                              "South Side Flats", "Central Lawrenceville", "Knoxville", "Shadyside", 
                                                              "Lincoln-Lemington-Belmar", "Stanton Heights", "Overbrook", "Squirrel Hill North",
                                                              "Beechview", "Highland Park"))
     updateSliderInput(session, "year_select", value = c(min(pitt$YEAR), max(pitt$YEAR)))
     updateSelectInput(session, "type_select",  selected = c("Potholes", "Weeds/Debris", "Snow/Ice removal", "Refuse Violations",
                                                               "Building Maintenance", "Missed Pick Up", "Abandoned Vehicle (parked on street)",
                                                               "Replace/Repair a Sign", "Litter", "Overgrowth", "Street Light - Repair"))
     updateCheckboxGroupInput(session, "source_select", selected = c("Call Center", "Website", "Control Panel"))
     alert("You have reset the application!")
})
}

 # Running the application
 shinyApp(ui = ui, server = server, enableBookmarking = "url")