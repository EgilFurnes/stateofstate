# Load required libraries
library(tidyverse)
library(shiny)
library(rsconnect)

rsconnect::setAccountInfo(name='egil',
                          token='57EBC2F76C680FB0C837C5E32A14718C',
                          secret='OQtRjjZjFGRB+R/mhqYvVbSp+HjPgDL7AqXcdb/w')

# Fetch and read the CSV data using base R's read.csv2
url_csv <- "https://data.ssb.no/api/v0/dataset/1082.csv?lang=no"

population <- read.csv2(
  url_csv,
  stringsAsFactors = FALSE
)

# Check the original column names
colnames(population)
# Output:
# [1] "region"
# [2] "kj.nn"
# [3] "alder"
# [4] "X.r"
# [5] "statistikkvariabel"
# [6] "X07459..Befolkning..etter.region..kj.nn..alder...r.og.statistikkvariabel"

# Rename columns to avoid Norwegian characters
population <- population %>%
  rename(
    region = region,
    gender = kj.nn,
    age = alder,
    year = X.r,
    statisticVariable = statistikkvariabel,
    population = X07459..Befolkning..etter.region..kj.nn..alder...r.og.statistikkvariabel
  )

# Verify the new column names
colnames(population)
# Output:
# [1] "region"
# [2] "gender"
# [3] "age"
# [4] "year"
# [5] "statisticVariable"
# [6] "population"

# Convert data types
population <- population %>%
  mutate(
    age = as.character(age),
    year = as.numeric(year),
    population = as.numeric(population)
  )

# Debug: Check the structure of the cleaned dataset
str(population)

# Example Plot: Total Population for a Specific Group
population_filtered <- population %>%
  filter(region == "0 Hele landet", gender == "2 Kvinner") %>%
  group_by(year) %>%
  summarise(totalPopulation = sum(population))

ggplot(population_filtered, aes(x = year, y = totalPopulation)) +
  geom_line(color = "steelblue") +
  theme_minimal() +
  labs(
    title = "Total Female Population in Norway Over Time",
    x = "Year",
    y = "Population"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Define UI
ui <- fluidPage(
  titlePanel("Norwegian Population Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("region_filter", "Select Region:",
                  choices = unique(population$region),
                  selected = "0 Hele landet"),
      checkboxGroupInput("gender_filter", "Select Gender:",
                         choices = unique(population$gender),
                         selected = c("1 Menn", "2 Kvinner")),
      sliderInput("year_range", "Select Year Range:",
                  min = min(population$year), max = max(population$year),
                  value = c(min(population$year), max(population$year))),
      selectInput("age_filter", "Select Age Group:",
                  choices = c("All Ages", unique(population$age)),
                  selected = "All Ages")
    ),
    mainPanel(
      plotOutput("popPlot"),
      tableOutput("popTable")
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  # Reactive expression to filter data
  filtered_data <- reactive({
    data <- population %>%
      filter(
        region == input$region_filter,
        gender %in% input$gender_filter,
        year >= input$year_range[1],
        year <= input$year_range[2]
      )
    if (input$age_filter != "All Ages") {
      data <- data %>% filter(age == input$age_filter)
    }
    data %>%
      group_by(year, gender) %>%
      summarise(totalPopulation = sum(population, na.rm = TRUE))
  })
  
  # Render the population plot
  output$popPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = year, y = totalPopulation, color = gender)) +
      geom_line() +
      theme_minimal() +
      labs(
        title = paste("Population Trend in", input$region_filter),
        x = "Year",
        y = "Total Population",
        color = "Gender"
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  # Render the filtered data table
  output$popTable <- renderTable({
    filtered_data()
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)

