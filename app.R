library(shiny)
library(shinydashboard)
library(data.table)
library(tmod)
library(shinyjs)
data(tmod)
options(shiny.maxRequestSize=30*1024^2)

mset <- tmod


ui <- dashboardPage(
dashboardHeader(title = "For Testing"),
dashboardSidebar(
sidebarMenu(
# Setting id makes input$tabs give the tabName of currently-selected tab
id = "tabs",
menuItem("File Check", icon = icon("th"), tabName = "widgets",
fileInput("files", label = h6(""), multiple = TRUE, accept = c("text/csv", ".csv")),
fluidRow(column(8,tableOutput("upload_files"))),
#textInput( "which_file", "Which file do you want to test?"),
uiOutput("choose_file"),
textInput("geneName", "Which column includes geneName?")
),

menuItem("Tests", icon = icon("bar-chart-o"),
selectInput("sort_by", "which column to use for sorting genes?",
choices = c("logFC" = "logFC",
"t" = "t",
"msd" = "msd",
"SE" = "SE",
"d" = "d",
"ciL" = "ciL",
"ciR" = "ciR",
"qval" = "qval"
),selected = "logFC"),
selectInput("inc_dec", "Trend",
choices = c("Increasing" = "inc",
"Decreasing" = "dec"),
selected = "inc"
)
)
),

# fileInput("files", label = h3("Choose File"), multiple = TRUE, accept = c("text/csv", ".csv")),
#fluidRow(column(8,tableOutput("upload_files"))),

#textInput( "which_file", "Which file do you want to test?"),
#textInput("geneName", "Which column includes geneName?"),


actionButton("test_mode_button", "Clik to Test"),

sidebarMenu(menuItemOutput("menuitem")),


textOutput("testbox"),

textOutput("abstract_file", container = pre)),

dashboardBody(
#numericInput("go_btns_quant","Number of GO buttons",value = 0,min = 0,max = 10),
uiOutput("go_buttons"),
DT::dataTableOutput('table')
)
)

server <- function(input, output, session) {
    
    print("I was called")
    
    output$choose_file <- renderUI({
        selectInput("which_file", "File Preview", as.list(input$files$name))
    })
    
    
    output$table = DT::renderDataTable( file_data(), options=list(scrollX=TRUE))
    
    # this function will get the content of file
    file_data <- reactive({
        n <- 0
        file_typein <- input$which_file
        infile <- input$files
        if(is.null(infile))
        # user has not uploaded a file yet
        return(NULL)
        for(i in 1:file_num()){
            if(infile$name[i] == file_typein){
                n <- i
            }
        }
        temp <- infile$datapath[1]
        a <- fread(temp , nrows = 10)
    })
    
    # this function will get the number of files uploaded
    file_num <- reactive({
        infile <- input$files
        length(infile$size)
    })
    
    # print file name on the left side
    output$upload_files <- renderTable({
        filename <- c()
        for(i in 1:file_num()){
            filename <- c(filename, input$files$name[i])
        }
        filename
    }, colnames = FALSE
    )
    
    # read data and  put into a list
    
    loadData <- function(){
        infile <- input$files$datapath
        if(is.null(infile)){
            # User has not uploaded files yet
        }
        data <- sapply(infile,
        function(x) fread(x, header = TRUE, stringsAsFactors = FALSE), simplify=FALSE)
        data
    }
    # the following can combine all the data in files
    #mycsvs<-reactive({ rbindlist(lapply(input$csvs$datapath, fread), use.names = TRUE, fill = TRUE)
    
    
    
    
    
    
    output$testbox <- renderText(
    "以下为测试"
    )
    
    output$abstract_file <- renderPrint({
        str(input$files)
        #loadData()
    })
    
    
}



shinyApp(ui = ui, server = server)

