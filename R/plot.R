# Define the about tab
plotTab <- function() {
  tabPanel("pieChart",
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
  )
}
