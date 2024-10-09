library(shiny)
library(shinyauthr)
library(sodium)


# Define a data frame with user credentials
user_base <- data.frame(
  user = c("user1", "user2"),
  password = sapply(c("password1", "password2"), sodium::password_store),
  permissions = c("admin", "standard"), stringsAsFactors = FALSE
)

# Define UI
ui <- fluidPage(
  shinyauthr::loginUI(id = "login"),
  uiOutput("navbar_ui")
)

# Define server logic
server <- function(input, output, session) {
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user_base$user,
    pwd_col = user_base$password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  observe({
    req(credentials()$user_auth)

    output$navbar_ui <- renderUI({
      navbarPage(
        title = "Shiny App with Navbar and Auth",
        tabPanel("Home",
                 h2("Welcome to the Home Page")
        ),
        tabPanel("Dashboard",
                 h2("Dashboard Page")
        ),
        navbarMenu("More",
                   tabPanel("About",
                            h2("About Page")
                   ),
                   tabPanel("Contact",
                            h2("Contact Page")
                   )
        ),
        tags$ul(class = "nav navbar-nav navbar-right",
                tags$li(a(icon("sign-out"), "Logout", href = "#", onclick = "Shiny.setInputValue('logout', true)"))
        )
      )
    })
  })

  observeEvent(input$logout, {
    shinyauthr::logoutServer(id = "logout")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
