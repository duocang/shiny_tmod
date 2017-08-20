library(shiny)
library(shinyjs)
library(shinydashboard)
jscode <- "
shinyjs.init = function() {
$(document).keypress(function(e) { alert('Key pressed: ' + e.which); });
}"

shinyApp(
    ui = fluidPage(
        useShinyjs(),
        extendShinyjs(text = jscode),
        "Press any key"
    ),
    server = function(input, output) {}
)