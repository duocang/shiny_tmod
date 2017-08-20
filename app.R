library(shiny)
library(shinydashboard) 
ui <- dashboardPage(
    dashboardHeader(
        title = "demo"
    ), 
    dashboardSidebar(), 
    dashboardBody(
        tags$head(tags$style(HTML(
            '.myClass { 
            font-size: 20px;
            line-height: 50px;
            text-align: left;
            font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
            padding: 0 15px;
            overflow: hidden;
            color: white;
            }
            '))),
        tags$script(HTML('
                         $(document).ready(function() {
                         $("header").find("nav").append(\'<span class="myClass"> Text Here </span>\');
                         })
                         '))
                         )
        ) 
server <- function(input, output) { } 
shinyApp(ui, server)