# devtools::install_github("JoeyBernhardt/singer")

# Load required libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(rsconnect)

# Load and prepare data
library(singer) 
data("songs")
data("locations")

# Join and rename columns for better readability
songs_data <- full_join(songs, locations, by = c("title", "artist_name")) %>%
  rename(
    Song = title,
    Artist = artist_name,
    Year = year,
    City = city,
    Album = release
  )

# Define UI layout
ui <- fluidPage(
  # App title
  div(style = "text-align: center;color:salmon;", titlePanel("Song Explorer")),
  
  # Image
  div(style = "text-align: center;", img(src = "fanta_sea.jpg", height = "200px", width = "300px")),
  
  br(),  
  
  # Artist input for dynamic plot
  fluidRow(
    column(3),
    column(6, textInput("artistInput", "Enter Artist Name for Yearly Song Releases:", value = ""), align = "center"),
    column(3)
  ),
  
  # Histogram plot output
  fluidRow(
    column(3),
    column(6, plotOutput("histPlot"), align = "center"),
    column(3)
  ),
  
  br(),
  
  # DataTable output
  fluidRow(
    column(3),
    column(6, DTOutput("songsTable"), align = "center"),
    column(3)
  ),
  
  # Download button for data
  fluidRow(
    column(3),
    column(6, downloadButton("downloadData", "Download Data (CSV)")),
    column(3)
  ),
  
  br()
)

# Server logic
server <- function(input, output, session) {
  
  # Reactive data for filtering based on artist input
  filteredData <- reactive({
    if (input$artistInput != "") {
      songs_data %>% filter(Artist == input$artistInput)
    } else {
      songs_data
    }
  })
  
  # Generate histogram plot
  output$histPlot <- renderPlot({
    data <- filteredData()
    ggplot(data, aes(x = Year)) +
      geom_histogram(bins = 30, fill = "cornflowerblue", color = "black") +
      theme_minimal() +
      labs(x = "Year", y = "Number of Songs", title = "Number of Songs Released Over Years")
  })
  
  # Render interactive DataTable
  output$songsTable <- renderDT({
    datatable(filteredData(), filter = 'top', options = list(
      pageLength = 5,
      searchHighlight = TRUE,
      info = TRUE,
      lengthChange = FALSE
    ))
  }, server = TRUE)
  
  # Download handler for data export
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("songs-data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(filteredData(), file, row.names = FALSE)
    }
  )
}

# Run the Shiny application
shinyApp(ui = ui, server = server)
