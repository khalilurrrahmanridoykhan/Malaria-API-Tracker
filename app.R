library(shiny)
library(shinydashboard)
library(leaflet)
library(tidyverse)
library(plotly)
library(purrr)
library(ggplot2)
library(dplyr)
library(shinyjs)
library(shinyauthr)
list.files("MaariaApiTracker/R") %>%
  here::here("MaariaApiTracker/R") %>%
  purrr::walk(
    ~ source(.)
  )

df <- read_csv(here::here("./data/COVID_data_2023-03-09.csv"))
df$date <- as.Date(df$date, format = "%Y-%m-%d")

coords <- read_csv(here::here("./data/world_country_and_usa_states_latitude_and_longitude_values.csv"))

coords_df <- df %>%
  inner_join(coords, by = c("country" = "country"))

df_quarantine_bangladesh_24_03_2020 <- read_csv("./data/covid-19_district-wise-quarantine_bangladesh_24.03.2020.csv")


# print(coords_df)
countrys_groupings(df)

user_base <- tibble::tibble(
  user = c("admin", "user2"),
  password = c("123456", "pass2"),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
)



ui <- dashboardPage(
  dashboardHeader(
    # id = "header",
    titleWidth = "300px",
    title = "Covid Data Analysis, Worldwide",
    tags$li(
      class = "dropdown",
      tags$a(
        href = "#shiny-tab-time", "Map", `data-toggle` = "tab"
      )
    ),
    tags$li(
      class = "dropdown",
      tags$a(
        href = "#", class = "dropdown-toggle", `data-toggle` = "dropdown",
        " Graph And Chart ", tags$b(class = "caret")
      ),
      tags$ul(
        class = "dropdown-menu",
        tags$li(tags$a(href = "#shiny-tab-casevsservere1", "Countrys Category Plot", `data-toggle` = "tab")), # nolint
        tags$li(tags$a(href = "#shiny-tab-tast1", "Countrys Plot", `data-toggle` = "tab")), # nolint
      ),
    )
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    shinyauthr::loginUI(id = "login"),
    div(
      id = "show-page-content",
      tabItems(
        tabItem(
          tabName = "time",
          div(
            class = "row",
            div(
              class = "col-sm-2 col-md-2 col-lg-2",
              selectInput("date", "Select Date:", choices = unique(coords_df$date), selected = "2023-02-16"),
              box(
                title = div(style = "", "Country"),
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                h3(id = "countryName", style = "text-align: center;", "Bangladesh"),
                height = "120px"
              ),
              box(
                title = div(style = "text-align: center;", "Cumulative Cases"),
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                h3(id = "cumulativeCases", style = "text-align: center;", "2037730"),
                height = "120px"
              ),
              box(
                title = div(style = "text-align: center;", "New Cases Past Week"),
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                h3(id = "newCasesPastWeek", style = "text-align: center;", "75"),
                height = "120px"
              ),
              box(
                title = div(style = "text-align: center;", "Cumulative Deaths"),
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                h3(id = "cumulativeDeaths", style = "text-align: center;", "29445"),
                height = "120px"
              ),
              box(
                title = div(style = "text-align: center;", "New Deaths Past Week"),
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                h3(id = "newDeathsPastWeek", style = "text-align: center;", "1"),
                height = "120px"
              )
            ),
            div(
              class = "col-sm-10 col-md-10 col-lg-10",
              leafletOutput("covidMap", width = "100%", height = "740px")
            )
          )
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
        )
      )
    )  %>% hidden(),
  ),
  skin = "blue"
)

server <- function(input, output, session) {


    observe({
      req(credentials()$user_auth)
      show(id = "show-page-content")
    })


  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    log_out = reactive(logout_init())
  )
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )

  # observe({
  #   if (credentials()$user_auth) {
  #     shinyjs::show("tabs") # Show tabs after authentication
  #     updateTabItems(session, "tabs", "time") # Redirect to the "time" tab
  #   } else {
  #     shinyjs::hide("tabs") # Hide tabs if not authenticated
  #   }
  # })

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

      summary_data <- data %>%
        summarise(
          Value = sum(!!sym(metric), na.rm = TRUE)
        ) %>%
        mutate(Remaining = sum(df[[metric]], na.rm = TRUE) - Value) %>%
        pivot_longer(cols = c(Value, Remaining), names_to = "Category", values_to = "Count") %>%
        mutate(Percentage = Count / sum(Count) * 100)

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

  filtered_data2 <- reactive({
    req(input$date)
    coords_df %>% filter(date == input$date)
  })

  output$covidMap <- renderLeaflet({
    leaflet(filtered_data2()) %>%
      addTiles() %>%
      addCircleMarkers(
        ~longitude, ~latitude,
        radius = ~ sqrt(cumulative_cases) / 500,
        color = "red",
        stroke = FALSE, fillOpacity = 0.7,
        popup = ~ paste0(
          "<strong>Country: </strong>", country, "<br>",
          "<strong>Date: </strong>", date, "<br>",
          "<strong>Cumulative Cases: </strong>", cumulative_cases, "<br>",
          "<strong>New Cases Past Week: </strong>", new_cases_past_week, "<br>",
          "<strong>Cumulative Deaths: </strong>", cumulative_deaths, "<br>",
          "<strong>New Deaths Past Week: </strong>", new_deaths_past_week
        ),
        layerId = ~country
      ) %>%
      setView(lng = mean(filtered_data2()$longitude), lat = mean(filtered_data2()$latitude), zoom = 2)
  })

  observeEvent(input$covidMap_marker_click, {
    click <- input$covidMap_marker_click
    selected_country <- click$id
    selected_data <- filtered_data2() %>% filter(country == selected_country)

    updateCountryDetails(selected_data)
  })

  updateCountryDetails <- function(selected_data) {
    selected_data <- selected_data[1, ]

    country <- selected_data$country
    cumulative_cases <- selected_data$cumulative_cases
    new_cases_past_week <- selected_data$new_cases_past_week
    cumulative_deaths <- selected_data$cumulative_deaths
    new_deaths_past_week <- selected_data$new_deaths_past_week

    runjs(paste0("$('#countryName').text('", country, "');"))
    runjs(paste0("$('#cumulativeCases').text('", cumulative_cases, "');"))
    runjs(paste0("$('#newCasesPastWeek').text('", new_cases_past_week, "');"))
    runjs(paste0("$('#cumulativeDeaths').text('", cumulative_deaths, "');"))
    runjs(paste0("$('#newDeathsPastWeek').text('", new_deaths_past_week, "');"))
  }
}

shinyApp(ui = ui, server = server)
