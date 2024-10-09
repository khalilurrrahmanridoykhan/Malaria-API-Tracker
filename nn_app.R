library(shiny)
library(DT)
library(markdown)
library(shinyauthr)
library(shinyjs)

# User data for authentication
user_base <- tibble::tibble(
  user = c("admin", "user2"),
  password = c("123456", "pass2"),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
)

ui <- navbarPage(
  "Navbar!",
  id = "navbar",
  header = tagList(
    useShinyjs(),
    tags$head(tags$style(HTML(
      "
      #login-panel {
        max-width: 500px;
        margin: 50px auto;
      }
      "
    )))
  ),
  footer = NULL,
  div(id = "login-panel",
    shinyauthr::loginUI("login")
  ),
  hidden(
    div(id = "app-content",
      tabPanel("Plot",
        sidebarLayout(
          sidebarPanel(
            radioButtons("plotType", "Plot type",
              c("Scatter" = "p", "Line" = "l")
            )
          ),
          mainPanel(
            plotOutput("plot")
          )
        )
      ),
      tabPanel("Summary",
        verbatimTextOutput("summary")
      ),
      navbarMenu("More",
        tabPanel("Table",
          DT::dataTableOutput("table")
        ),
        tabPanel("About",
          fluidRow(
            column(6,
              includeMarkdown("about.md")
            ),
            column(3,
              img(class = "img-polaroid",
                  src = paste0("http://upload.wikimedia.org/",
                               "wikipedia/commons/9/92/",
                               "1919_Ford_Model_T_Highboy_Coupe.jpg")),
              tags$small(
                "Source: Photographed at the Bay State Antique ",
                "Automobile Club's July 10, 2005 show at the ",
                "Endicott Estate in Dedham, MA by ",
                a(href = "http://commons.wikimedia.org/wiki/User:Sfoskett",
                  "User:Sfoskett")
              )
            )
          )
        )
      ),
      theme = shinythemes::shinytheme("cerulean")
    )
  )
)

server <- function(input, output, session) {
  # Call the login module
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    log_out = reactive(logout_init())
  )

  # Call the logout module
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )

  # Observe login status and show/hide the app content
  observe({
    if (credentials()$user_auth) {
      hide(id = "login-panel")
      show(id = "app-content")
    } else {
      show(id = "login-panel")
      hide(id = "app-content")
    }
  })

  # Plot output
  output$plot <- renderPlot({
    plot(cars, type = input$plotType)
  })

  # Summary output
  output$summary <- renderPrint({
    summary(cars)
  })

  # Table output
  output$table <- DT::renderDataTable({
    DT::datatable(cars)
  })
}

shinyApp(ui, server)
