library(shiny)
library(shinydashboard)
library(leaflet)
library(tidyverse)
library(plotly)
library(purrr)
library(ggplot2)
library(dplyr)

list.files("MaariaApiTracker/R") %>%
  here::here("MaariaApiTracker/R") %>%
  purrr::walk(
    ~ source(.)
  )

df <- read_csv(here::here("./data/COVID_data_2023-03-09.csv"))
df$date <- as.Date(df$date, format = "%Y-%m-%d")

coords <- read_csv(here::here("./data/world_country_and_usa_states_latitude_and_longitude_values.csv"))

coords_df <- df  %>%
  inner_join(coords, by = c("country" = "country"))

# print(coords_df)
countrys_groupings(df)


ui <- dashboardPage(
  dashboardHeader(
    titleWidth = "300px",
    title = "Covid Data Analysis, Worldwide",
    tags$li(
      class = "dropdown",
      tags$a(
        href = "#", class = "dropdown-toggle", `data-toggle` = "dropdown",
        " Map ", tags$b(class = "caret")
      ),
      tags$ul(
        class = "dropdown-menu",
        tags$li(tags$a(href = "#shiny-tab-annualy", "Annual Parasite Incidence Map", `data-toggle` = "tab")), # nolint
        tags$li(tags$a(href = "#shiny-tab-yearly", "Yearly API MAP", `data-toggle` = "tab")), # nolint
        tags$li(tags$a(href = "#shiny-tab-time", "Time Series API MAP", `data-toggle` = "tab")), # nolint
        tags$li(tags$a(href = "#shiny-tab-bednet", "Bed Net Coverage Map", `data-toggle` = "tab")), # nolint
        tags$li(tags$a(href = "#shiny-tab-malariavector", "Malaria Vector Map", `data-toggle` = "tab")), # nolint
        tags$li(tags$a(href = "#shiny-tab-hfhw", "Hf and HW Distribution Map", `data-toggle` = "tab")), # nolint
      ),
    ),
    tags$li(
      class = "dropdown",
      tags$a(
        href = "#", class = "dropdown-toggle", `data-toggle` = "dropdown",
        " Graph And Chart ", tags$b(class = "caret")
      ),
      tags$ul(
        class = "dropdown-menu",
        tags$li(tags$a(href = "#shiny-tab-yearly1", "Yearly Malaria Cases", `data-toggle` = "tab")), # nolint
        tags$li(tags$a(href = "#shiny-tab-monthly1", "Manthly Malaria Cases", `data-toggle` = "tab")), # nolint
        tags$li(tags$a(href = "#shiny-tab-yearlydeaths1", "Yearly Malaria Cases and Deaths", `data-toggle` = "tab")), # nolint
        tags$li(tags$a(href = "#shiny-tab-ageandgender1", "Age and Gender", `data-toggle` = "tab")), # nolint
        tags$li(tags$a(href = "#shiny-tab-species1", "Shecies Wish Malaria Cases (Pf/Pv/Mixed/Total)", `data-toggle` = "tab")), # nolint
        tags$li(tags$a(href = "#shiny-tab-casevsservere1", "Countrys Category Plot", `data-toggle` = "tab")), # nolint
        tags$li(tags$a(href = "#shiny-tab-tast1", "Countrys Plot", `data-toggle` = "tab")), # nolint
      ),
    ),
    tags$li(
      class = "dropdown",
      tags$a(
        href = "#", class = "dropdown-toggle", `data-toggle` = "dropdown",
        " Data Analysis Tool ", tags$b(class = "caret")
      ),
      tags$ul(
        class = "dropdown-menu",
        tags$li(tags$a(href = "#shiny-tab-pivot2", "Pivot Table", `data-toggle` = "tab")), # nolint
        tags$li(tags$a(href = "#shiny-tab-physical2", "Physical Accessibility Model", `data-toggle` = "tab")), # nolint
        tags$li(tags$a(href = "#shiny-tab-risk2", "Risk Analysis", `data-toggle` = "tab")), # nolint
      ),
    ),
    tags$li(
      class = "dropdown",
      tags$a(
        href = "#shiny-tab-VillApiViz3", " Vill API", `data-toggle` = "tab"
      )
    )
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    tabItems(
      tabItem(
        tabName = "annualy",
        leafletOutput("map1Output", height = 500)
      ),
      tabItem(
        tabName = "yearly",
        leafletOutput("map2Output", height = 500)
      ),
      tabItem(
        tabName = "time",
        div(
          class = "row",
          div(
            class = "col-sm-2 col-md-2 col-lg-2",
            selectInput("date", "Select Date:", choices = unique(coords_df$date), selected = "2023-02-16")
          ),
          div(
            class = "col-sm-10 col-md-10 col-lg-10",
leafletOutput("covidMap", width = "100%", height = "740px")
          )
        )
      ),
      tabItem(
        tabName = "bednet",
        h1("Time Series API MAP")
      ),
      tabItem(
        tabName = "malariavector",
        h1("Time Series API MAP")
      ),
      tabItem(
        tabName = "hfhw",
        h1("Time Series API MAP")
      ),
      tabItem(
        tabName = "yearly1",
        h1("Time Series API MAP")
      ),
      tabItem(
        tabName = "monthly1",
        h1("Time Series API MAP")
      ),
      tabItem(
        tabName = "yearlydeaths1",
        h1("Time Series API MAP")
      ),
      tabItem(
        tabName = "ageandgender1",
        h1("Time Series API MAP")
      ),
      tabItem(
        tabName = "species1",
        h1("Time Series API MAP")
      ),
      tabItem(
        tabName = "casevsservere1",
        div(
          class = "row",
          div(
            class = "col-sm-3 col-md-3 col-lg-3",
            selectInput("country1", "Select Country:", choices = NULL, selected = NULL),
            selectInput("metric", "Select Metric:",
              choices = c(
                "Cumulative Cases" = "cumulative_cases",
                "New Cases Past Week" = "new_cases_past_week",
                "Cumulative Deaths" = "cumulative_deaths",
                "New Deaths Past Week" = "new_deaths_past_week"
              )
            )
          ),
          div(
            class = "col-sm-9 col-md-9 col-lg-9",
            plotOutput("pieChart")
          )
        )
      ),
      tabItem(
        tabName = "tast1",
        div(
          class = "row",
          div(
            class = "col-sm-3 col-md-3 col-lg-3",
            selectInput("country", "Select Country:", choices = NULL, selected = NULL),
            dateRangeInput("dateRange", "Select Date Range:",
              start = "2020-01-22", end = Sys.Date()
            )
          ),
          div(
            class = "col-sm-9 col-md-9 col-lg-9",
            plotOutput("cumulativeCasesPlot")
          )
        )
      ),
      tabItem(
        tabName = "pivot2",
        h1("Time Series API MAP Last test")
      ),
      tabItem(
        tabName = "physical2",
        h1("Time Series API MAP Last test")
      ),
      tabItem(
        tabName = "risk2",
        h1("Time Series API MAP Last test")
      ),
      tabItem(
        tabName = "VillApiViz3",
        h1("Vill API")
      )
    )
  ),
  skin = "blue"
)

server <- function(input, output, session) {
  updateTabItems(session, "tabs", "annualy")

  output$map1Output <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng = -122.4194, lat = 37.7749, popup = "Map 1 - San Francisco") # nolint
  })

  output$map2Output <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng = -74.0060, lat = 40.7128, popup = "Map 2 - New York")
  })

  observe({
    updateSelectInput(session, "country", choices = unique(df$country))
  })

  filtered_data <- reactive({
    req(input$country)
    df %>%
      filter(
        country == input$country,
        date >= input$dateRange[1],
        date <= input$dateRange[2]
      )
  })
  output$cumulativeCasesPlot <- renderPlot(
    {
      ggplot(filtered_data(), aes(x = date, y = cumulative_cases)) +
        geom_line(color = "blue", size = 1) +
        labs(
          title = "Cumulative COVID-19 Cases Over Time",
          x = "date", y = "cumulative_cases"
        ) +
        theme_minimal()
    },
    height = 700,
    width = 1000
  )

  # Pai Chart

  observe({
    updateSelectInput(session, "country1", choices = unique(df$country))
  })
  filtered_data1 <- reactive({
    req(input$country1)
    df %>% filter(country == input$country1)
  })
  output$pieChart <- renderPlot(
    {
      req(filtered_data1())

      data <- filtered_data1()
      metric <- input$metric

      # Create a summary for the pie chart
      summary_data <- data %>%
        summarise(
          Value = sum(!!sym(metric), na.rm = TRUE)
        ) %>%
        mutate(Remaining = sum(df[[metric]], na.rm = TRUE) - Value) %>%
        pivot_longer(cols = c(Value, Remaining), names_to = "Category", values_to = "Count") %>%
        mutate(Percentage = Count / sum(Count) * 100)

      # Create the pie chart
      ggplot(summary_data, aes(x = "", y = Count, fill = Category)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y") +
        labs(title = paste(input$country1, metric), x = NULL, y = NULL) +
        theme_void() +
        theme(legend.position = "right") +
        scale_fill_manual(values = c("Value" = "#ff4000", "Remaining" = "#d9ff00")) +
        geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
        theme(plot.margin = margin(10, 10, 10, 10, "pt")) +
        theme(plot.title = element_text(size = 20), legend.text = element_text(size = 12))
    },
    height = 700,
    width = 1000
  )
  globalVariables(c("country", "Value", "Count", "Category"))

  #Covid Map

    filtered_data2 <- reactive({
    req(input$date)
    coords_df %>% filter(date == input$date)
  })

    output$covidMap <- renderLeaflet({
    leaflet(filtered_data2()) %>%
      addTiles() %>%
      addCircleMarkers(
      ~longitude, ~latitude,
      radius = ~sqrt(cumulative_cases) / 500, # Adjust the size for better visibility
      color = "red",
      stroke = FALSE, fillOpacity = 0.7,
      popup = ~paste0(
      "<strong>Country: </strong>", country, "<br>",
      "<strong>Date: </strong>", date, "<br>",
      "<strong>Cumulative Cases: </strong>", cumulative_cases, "<br>",
      "<strong>New Cases Past Week: </strong>", new_cases_past_week, "<br>",
      "<strong>Cumulative Deaths: </strong>", cumulative_deaths, "<br>",
      "<strong>New Deaths Past Week: </strong>", new_deaths_past_week
      )
      ) %>%
      setView(lng = mean(filtered_data2()$longitude), lat = mean(filtered_data2()$latitude), zoom = 2) # Adjust the zoom level for better visibility
    })



}

shinyApp(ui = ui, server = server)
