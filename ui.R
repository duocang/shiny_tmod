library(shiny)
library(shinydashboard)
library(data.table)
library(tmod)
library(shinyjs)
library(markdown)
data(tmod)
options(shiny.maxRequestSize=30*1024^2)

mset <- tmod

gallery.dir <- 'www/gallery/'
## creates a gallery with figures found in www/gallery
get.gallery <- function() {
    files <- list.files(gallery.dir, pattern=".*\\.png$" )  # song: produce a character vector of the names of files or directories
    l <- list(fixedRow( # song: Functions for creating fixed page layouts. A fixed page layout consists of rows which in turn include columns. 
        column(10,          # song: Create a column for use within a fluidRow or fixedRow
               includeMarkdown("md/gallery.md")
        )))
    nf <- length(files)
    rowN <- 3
    
    nr <- ceiling(nf / rowN)
    i <- 1
    for(r in 1:nr) {
        clist <- list()
        for(c in 1:rowN) {
            if(i > nf) break ;
            clist[[i]] <- column(floor(12/rowN), img.link(files[i]))
            i <- i + 1
        }
        l <- c(l, list(fixedRow(clist)))
    }
    return(fixedPage(l))
}

## creates a div element with image thumb, description, 
img.link <- function(file) {
    if(file.exists(paste0(gallery.dir, "thumbs/", file))) {
        thumb.file <- paste0("gallery/thumbs/", file)
    } else {
        thumb.file <- paste0("gallery/", file)
    }
    img  <- tags$img(src=thumb.file, width="250px", height="250px", alt=sprintf("[%s]", file))
    link <- paste0("gallery/", file)
    img  <- tags$a(href=link, img)
    name <- tags$a(href=link, file)
    mdname <- paste0(gallery.dir, "md/", file, ".md")
    if(file.exists( mdname )) {
        desc <- tagList(tags$br(), 
                        includeMarkdown(mdname), 
                        tags$br())
    } else {
        desc <- tags$br()
    }
    return(div(class="gallerypanel", img, tags$br(), tags$hr(), name, desc))  # song: div: A division of text with a uniform style
} 

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Test", tabName = "file", icon = icon("file-o"),
                 menuSubItem("Upload File", tabName = "file_preview"),
                 menuSubItem("Tests", tabName = "tests"),
                 menuSubItem("Test1", tabName = "test1"),
                 startExpanded = TRUE
        ),
        menuItem("Help", tabName = "help", icon = icon("question")
        ),
        menuItem("Download", tabName = "download", icon = icon("download"), startExpanded = FALSE
        ),
        menuItem("Gallery", tabName = "gallery", icon = icon("file-picture-o"),startExpanded = FALSE
        ),
        menuItem("Logs", tabName = "logs", icon = icon("info"), startExpanded = FALSE
        )
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "file_preview",
                fluidRow(
                    column(3, 
                           fileInput("files", label = h6(""), 
                                     multiple = TRUE, 
                                     accept = c("text/csv", ".csv")
                                     
                           )) ,
                    column(3, 
                           uiOutput("choose_preview_file")),
                    column(3, 
                           uiOutput("genename_col"))
                ),
                fluidRow(
                    column(10, offset=0, htmlOutput( "message" ), class="tmodMsg" )
                ),
                class="params",
                DT::dataTableOutput("table")
        ),
        tabItem(tabName = "tests",
                fluidRow(
                    column(4, selectInput("sort_by", "which column to use for sorting genes?",
                                          choices = c("",
                                                      "logFC" = "logFC",
                                                      "t" = "t",
                                                      "msd" = "msd",
                                                      "SE" = "SE",
                                                      "d" = "d",
                                                      "qval" = "qval")
                    )) ,
                    column(2, selectInput("inc_dec", "Trend",
                                          choices = c("Increasing" = "False",
                                                      "Decreasing" = "TRUE"),
                                          selected = "Decreasing"
                    )),
                    column(2, selectInput("abs", "Abs",
                                          choices = c("YES", "NO"),
                                          selected = "YES")),
                    column(2, selectInput("test_type", "test",
                                          choices = c("NO SELECT",
                                                      "tmodCERNOtest" = "tmodCERNOtest",
                                                      "tmodUtest" = "tmodUtest")
                    )),
                    column(2, actionButton("run", "Polt it")
                    )
                ),
                plotOutput("plot", height = "1200px")
                # fluidRow(
                #     column(2,  
                #            numericInput("pie.pval", 
                #                         "pie.pval", 
                #                         0.05, min = 0, max = 0.1, step = 0.01)
                #     ),
                #     column(2,
                #            numericInput("pie.lfc",
                #                         "pie.lfc",
                #                         1, min = 0, max = 5, step = 0.5)
                #     ),
                #     column(2, actionButton("run1", "Polt it")
                #     ),
                # plotOutput("plot", height = "1200px"),
                # plotOutput("plot1" )
                # 
                # 
                # )
        ),
        tabItem(tabName = "test1",
                fluidRow(
                    column(2,  
                           numericInput("pie.pval", 
                                        "pie.pval", 
                                        0.05, min = 0, max = 0.1, step = 0.01)
                    ),
                    column(2,
                           numericInput("pie.lfc",
                                        "pie.lfc",
                                        1, min = 0, max = 5, step = 0.5)
                    ),
                    column(2, actionButton("run1", "Polt it")
                    ),
                
                    plotOutput("plot1" , height = "1200px")
                )
            
        ),
        tabItem(tabName = "help",
                includeMarkdown("md/help.md")
        ),
        tabItem(tabName = "download",
                includeMarkdown("md/downloads.md")
        ),
        tabItem(tabName = "gallery",
                get.gallery()
                #includeMarkdown("md/gallery.md")
        ),
        tabItem(tabName = "logs",
                fluidRow(
                    column(10, htmlOutput( "messageLog"), br()
                    ) 
                )
        )
    )
)


# Put them together into a dashboardPage
dashboardPage(
    skin = "black",
    dashboardHeader(title = "tmod tests beta"),
    sidebar,
    body
)