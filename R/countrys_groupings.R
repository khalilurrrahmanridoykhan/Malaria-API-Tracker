countrys_groupings <- function(df){

  filtered_data <- reactive({
    req(input$country)
    df %>%
      filter(
        country == input$country,
        date >= input$dateRange[1],
        date <= input$dateRange[2]
      )
  })

}