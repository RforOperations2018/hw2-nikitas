#Homework 4 - Nikita Setia

library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)
library(shinyjs)
library(httr)
library(jsonlite)

# Function to load data from API
ckanSQL <- function(url) {
  # Make the Request
  r <- RETRY("GET", URLencode(url))
  # Extract Content
  c <- content(r, "text")
  # Basic gsub to make NA's consistent with R
  json <- gsub('NaN', 'NA', c, perl = TRUE)
  # Create Dataframe
  data.frame(jsonlite::fromJSON(json)$result$records)
}

# Unique values for Resource Field
ckanUniques <- function(id, field) {
  url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
  c(ckanSQL(URLencode(url)))
}

request_types <- sort(ckanUniques("76fda9d0-69be-4dd5-8108-0de7907fc5a4", "REQUEST_TYPE")$REQUEST_TYPE)
neighborhoods <- sort(ckanUniques("76fda9d0-69be-4dd5-8108-0de7907fc5a4", "NEIGHBORHOOD")$NEIGHBORHOOD)
sources <- sort(ckanUniques("76fda9d0-69be-4dd5-8108-0de7907fc5a4", "REQUEST_ORIGIN")$REQUEST_ORIGIN)

#pdf(NULL)

# Define UI for application
ui <- fluidPage(
  # Add Shinyjs
  useShinyjs(),
  # Application title
  titlePanel("Pittsburgh 311 Tabset"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      # Creating a checkbox input for the 'request origin'
      checkboxGroupInput("source_select",
                         "Source Type:",
                         choices = sources,
                         selected = "Call Center"),
      # Creating a date range input for the date variable
      dateRangeInput("date_select",
                     "Select Dates",
                     start = Sys.Date()-30,
                     end = Sys.Date()),
      # Creating a select input for the 'neighborhood' variable
      selectInput("nbhd_select",
                  "Neighborhood:",
                  choices = neighborhoods,
                  selectize = TRUE,
                  selected = "Highland Park"),
      # Creating a select input for the 'request type' variable
      selectInput("type_select",
                  "Request Type:",
                  choices = request_types,
                  selectize = TRUE,
                  selected = "Potholes"),
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
    # Build API Query with proper encodes
    # Added filters for request date, type, source, and neighborhood
    url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%2276fda9d0-69be-4dd5-8108-0de7907fc5a4%22")
#%20WHERE%20%22CREATED_ON%22%20%3E=%20%27", input$date_select[1], "%27%20AND%20%22CREATED_ON%22%20%3C=%20%27", input$date_select[2], "%27%20AND%20%22REQUEST_TYPE%22%20=%20%27", input$type_select, "%27%20AND%20%22REQUEST_ORIGIN%22%20=%20%27", input$source_select, "%27%20AND%20%22NEIGHBORHOOD%22%20=%20%27", input$nbhd_select, "%27")
    # Load and clean data
    data <- ckanSQL(url) 
    #%>%
      #mutate(DATE = as.Date(CREATED_ON),
       #      STATUS = ifelse(STATUS == 1, "Closed", "Open")) 
    # find row #s where geographic location is out of bounds
    #index <- which(data$GEO_ACCURACY == "OUT_OF_BOUNDS")
    # replace neighborhood values where the location is out of bounds
    #data$NEIGHBORHOOD[index] <- c("Out of Bounds")
    # removing data where neighborhood is missing
    #filt311 <- data[-which(data$NEIGHBORHOOD == ''),]
    
    return(data) 
  })

   # Line graph showing count of requests by types over time
  output$year_plot <- renderPlotly({
    dat <- pittFiltered() %>% group_by(REQUEST_TYPE, DATE) %>% summarise(COUNT = n())
    ggplotly(
      ggplot(data = dat, aes(x = DATE, y = COUNT, color = REQUEST_TYPE)) + 
        xlab("Date") + ylab("Count") +
        labs(color = "Request Type") + 
        ggtitle("Requests by Type Over Time") +
        geom_line(stat = "identity"))
    })
  # Bar graph showing count of requests by source/origin over time
  output$source_plot <- renderPlotly({
    dat <- pittFiltered() %>% group_by(REQUEST_ORIGIN, DATE) %>% summarise(COUNT = n())
    ggplotly(
      ggplot(data = dat, aes(x = DATE, y = COUNT, fill = REQUEST_ORIGIN)) +
        xlab("Date") + ylab("Count") +
        labs(fill = "Request Origin") + 
        ggtitle("Requests by Source Over Time") +
        geom_bar(stat = "identity")
      )
    })
   # Point chart showing count of requests by neighborhood over time
   output$nbhd_plot <- renderPlotly({
     dat <- pittFiltered() %>% group_by(NEIGHBORHOOD, DATE) %>% summarise(COUNT = n())
     ggplotly(
       ggplot(data = dat, aes(x = DATE, y = COUNT, fill = NEIGHBORHOOD,
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
    # These selected lists are a little messy
     updateSelectInput(session, "nbhd_select", selected = "Highland Park")
     updateSelectInput(session, "type_select",  selected = "Potholes")
     updateCheckboxGroupInput(session, "source_select", selected = "Call Center")
     alert("You have reset the application!")
})
}

 # Running the application
 shinyApp(ui = ui, server = server, enableBookmarking = "url")