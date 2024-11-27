# Load required libraries
library(tidyverse)
library(shiny)

# Fetch and process data
url_csv <- "https://data.ssb.no/api/v0/dataset/1082.csv?lang=no"
population <- read.csv2(url_csv, stringsAsFactors = FALSE) %>%
  rename(
    region = region,
    gender = kj.nn,
    age = alder,
    year = X.r,
    statisticVariable = statistikkvariabel,
    population = X07459..Befolkning..etter.region..kj.nn..alder...r.og.statistikkvariabel
  ) %>%
  mutate(
    age = as.character(age),
    year = as.numeric(year),
    population = as.numeric(population)
  )

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
  
  output$popTable <- renderTable({
    filtered_data()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
