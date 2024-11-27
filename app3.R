# Clear the workspace
rm(list = ls())

# Load required libraries
library(tidyverse)
library(shiny)
library(readr)
library(stringr)

# Set encoding option
options(encoding = "UTF-8")

# Fetch and process data for the first dataset (Population by gender and age)
url_csv <- "https://data.ssb.no/api/v0/dataset/1082.csv?lang=no"

# Read data into 'pop' data frame with UTF-8 encoding
pop <- read_delim(
  url_csv,
  delim = ";",
  locale = locale(encoding = "UTF-8"), # Changed to UTF-8
  trim_ws = TRUE
) %>% as_tibble()

# Rename columns
colnames(pop) <- c("region", "gender", "age", "year", "statisticalVariable", "population")

# Convert data types
pop <- pop %>%
  mutate(
    year = as.numeric(year),
    population = as.numeric(population)
  )

# Fetch and process data for the second dataset (Population changes)
url_csv_new <- "https://data.ssb.no/api/v0/dataset/49626.csv?lang=no"

# Read data into 'pop_changes' data frame with UTF-8 encoding
pop_changes <- read_delim(
  url_csv_new,
  delim = ";",
  locale = locale(encoding = "UTF-8"), # Changed to UTF-8
  trim_ws = TRUE,
  na = ".."
) %>% as_tibble()

# Rename columns
colnames(pop_changes) <- c("region", "year", "contents", "value")

# Convert data types
pop_changes <- pop_changes %>%
  mutate(
    year = as.numeric(year),
    value = as.numeric(value)
  )

# Ensure the 'contents' column has the correct encoding
Encoding(pop_changes$contents) <- "UTF-8"

# Define UI
ui <- fluidPage(
  titlePanel("Norsk Befolkningsdashboard"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year_range", "Velg år:",
                  min = max(min(pop$year), min(pop_changes$year)),
                  max = min(max(pop$year), max(pop_changes$year)),
                  value = c(max(min(pop$year), min(pop_changes$year)),
                            min(max(pop$year), max(pop_changes$year))),
                  step = 1,
                  sep = "")
    ),
    mainPanel(
      fluidRow(
        plotOutput("popPlot"),
        plotOutput("birthsDeathsPlot")
      ),
      fluidRow(
        tableOutput("popTable")
      )
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  # Reactive data for the first plot
  filtered_data <- reactive({
    pop %>%
      filter(
        year >= input$year_range[1],
        year <= input$year_range[2]
      ) %>%
      group_by(year) %>%
      summarise(totalPopulation = sum(population, na.rm = TRUE))
  })
  
  # Reactive data for the second plot
  births_deaths_data <- reactive({
    pop_changes %>%
      filter(
        str_detect(contents, "Levendefødte|Døde"),
        year >= input$year_range[1],
        year <= input$year_range[2]
      )
  })
  
  # Render the first plot
  output$popPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = year, y = totalPopulation)) +
      geom_line(color = "blue") +
      theme_minimal() +
      labs(
        title = "Befolkningsutvikling i Norge",
        x = "År",
        y = "Total Befolkning"
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  # Render the second plot
  output$birthsDeathsPlot <- renderPlot({
    ggplot(births_deaths_data(), aes(x = year, y = value, color = contents)) +
      geom_line() +
      theme_minimal() +
      labs(
        title = "Levendefødte og Døde i Norge",
        x = "År",
        y = "Antall personer",
        color = "Hendelse"
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  # Render the table
  output$popTable <- renderTable({
    filtered_data()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
