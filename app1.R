library(shiny)
library(shinyjs)

ui <- tagList(
    useShinyjs(),
    navbarPage(
        "shinyjs with navbarPage",
        tabPanel("tab1",
                 actionButton("button", "Click me"),
                 div(id = "hello", "Hello!")),
        tabPanel("tab2")
    )
)

server <- function(input, output, session) {
    observeEvent(input$button, {
        toggle("hello")
    })
}

shinyApp(ui, server)