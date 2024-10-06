library(shiny)
library(shinydashboard)
library(leaflet)
library(tidyverse)
library(plotly)
library(purrr)

list.files("MaariaApiTracker/R") %>%
  here::here("MaariaApiTracker/R") %>%
  purrr::walk(
    ~ source(.)
  )

ui <- dashboardPage(
  dashboardHeader(
    titleWidth = "300px",
    title = "Malaria API Tracker, Bangladesh",
    tags$li(
      class = "dropdown",
      tags$a(
        href = "#", class = "dropdown-toggle", `data-toggle` = "dropdown",
        " Map ", tags$b(class = "caret")
      ),
      tags$ul(
        class = "dropdown-menu",
        tags$li(tags$a(href = "#shiny-tab-annualy", "Annual Parasite Incidence Map", `data-toggle` = "tab")),# nolint
        tags$li(tags$a(href = "#shiny-tab-yearly", "Yearly API MAP", `data-toggle` = "tab")),# nolint
        tags$li(tags$a(href = "#shiny-tab-time", "Time Series API MAP", `data-toggle` = "tab")),# nolint
        tags$li(tags$a(href = "#shiny-tab-bednet", "Bed Net Coverage Map", `data-toggle` = "tab")),# nolint
        tags$li(tags$a(href = "#shiny-tab-malariavector", "Malaria Vector Map", `data-toggle` = "tab")),# nolint
        tags$li(tags$a(href = "#shiny-tab-hfhw", "Hf and HW Distribution Map", `data-toggle` = "tab")),# nolint
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
        tags$li(tags$a(href = "#shiny-tab-yearly1", "Yearly Malaria Cases", `data-toggle` = "tab")),# nolint
        tags$li(tags$a(href = "#shiny-tab-monthly1", "Manthly Malaria Cases", `data-toggle` = "tab")),# nolint
        tags$li(tags$a(href = "#shiny-tab-yearlydeaths1", "Yearly Malaria Cases and Deaths", `data-toggle` = "tab")),# nolint
        tags$li(tags$a(href = "#shiny-tab-ageandgender1", "Age and Gender", `data-toggle` = "tab")),# nolint
        tags$li(tags$a(href = "#shiny-tab-species1", "Shecies Wish Malaria Cases (Pf/Pv/Mixed/Total)", `data-toggle` = "tab")),# nolint
        tags$li(tags$a(href = "#shiny-tab-casevsservere1", "Case Vs Severe Malaria", `data-toggle` = "tab")),# nolint
        tags$li(tags$a(href = "#shiny-tab-tast1", "Tast, Case and Test Positivily Rate", `data-toggle` = "tab")),# nolint
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
        tags$li(tags$a(href = "#shiny-tab-physical2", "Physical Accessibility Model", `data-toggle` = "tab")),# nolint
        tags$li(tags$a(href = "#shiny-tab-risk2", "Risk Analysis", `data-toggle` = "tab")),# nolint
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
        h1("Time Series API MAP")
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
        h1("Time Series API MAP")
      ),
      tabItem(
        tabName = "tast1",
        h1("Time Series API MAP Last test")
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
      addMarkers(lng = -122.4194, lat = 37.7749, popup = "Map 1 - San Francisco")# nolint
  })

  output$map2Output <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng = -74.0060, lat = 40.7128, popup = "Map 2 - New York")
  })
}

shinyApp(ui = ui, server = server)
