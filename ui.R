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

popupWindow <- function(varname, contents) {
    
    # hooks for reactive elements to show / dismiss the window
    show    <- paste0( "show_", varname)
    dismiss <- paste0( "dismiss_", varname)
    
    conditionalPanel(
        condition=sprintf('input.%s == 1', show),
        
        # variable to keep track of showing the overlay
        div(style="display:none;", textInput(show, "", 0)),
        
        div(class="overlay", draggable="true",
            p(actionButton(dismiss, label="Dismiss [X]" )), contents)
    )
}

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Test", tabName = "file", icon = icon("file-o"),
                 menuSubItem("Upload File(s)", tabName = "file_preview"),
                 menuSubItem("Test", tabName = "tests"),
                 menuSubItem("Test1", tabName = "test1"),
                 menuSubItem("Test2", tabName = "test2"),
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
                #fluidRow(
                #   column(10, offset=0, htmlOutput( "message" ), class="tmodMsg" )
                #),
                class="params",
                DT::dataTableOutput("table")
        ),
        tabItem(tabName = "tests",
                fluidRow(
                    column(3, "  Gene module"),
                    column(2, "  Gene sort:" ),
                    column(2, "  Trend:" ),
                    column(1, "  Abs:" ),
                    column(2, "  Test type:" ),
                    column(2, "  test:" ),
                    class="paramHeader"
                ),
                fluidRow(
                    column(3, 
                           selectInput( "mset", NULL,
                                        list( ""
                                              ,"Li et al. and B. Pulendran (LI)"="LI"
                                              ,"Damien Chaussabel et al. (DC)"="DC"
                                              ,"LI + DC"="all"
                                              ,"MSigDB Hallmark gene sets"="msigH"
                                              ,"MSigDB Positional gene sets (C1)"="msigC1"
                                              ,"MSigDB Curated gene sets (C2)"="msigC2"
                                              ,"MSigDB Motif gene sets (C3)"="msigC3"
                                              ,"MSigDB Computational signatures (C4)"="msigC4"
                                              ,"MSigDB GO gene sets (C5)"="msigC5"
                                              ,"MSigDB Oncogenic signatures (C6)"="msigC6"
                                              ,"MSigDB Immunologic signatures (C7)"="msigC7"
                                        ))
                           ),
                    column(2, selectInput("sort_by", NULL,
                                          choices = c("",
                                                      "logFC" = "logFC",
                                                      "t" = "t",
                                                      "msd" = "msd",
                                                      "SE" = "SE",
                                                      "d" = "d",
                                                      "qval" = "qval")
                    )) ,
                    column(2, selectInput("inc_dec", NULL,
                                          choices = c("",
                                                      "Increasing" = "False",
                                                      "Decreasing" = "TRUE")
                    )),
                    column(1, selectInput("abs", NULL,
                                          choices = c("", 
                                                      "YES",
                                                      "NO")
                                          )
                    ),
                    column(2, selectInput("test_type", NULL,
                                          choices = c("" ,
                                                      "NO SELECT",
                                                      "tmodCERNOtest" = "tmodCERNOtest",
                                                      "tmodUtest" = "tmodUtest")
                                          )
                    ),
                    column(2, actionButton("run", "Plot it")
                    )
                ),
                fluidRow( column(12, offset=0, htmlOutput("message", inline=TRUE), class="tmodMsg" )),
                plotOutput("plot", height = "1200px")
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
                
                    plotOutput("plot1" , height = "1500px")
                )
            
        ),
        tabItem(tabName = "test2",
                # ----------------------------------------------------------------------
                # HTML code for the parameters header of the Tests tab
                # ----------------------------------------------------------------------
                paramBar <- div(
                    fluidRow(
                        column(3, "Test type:" ),
                        column(2, "Input file(s):" ),
                        column(3, "Module subset:" ),
                        column(2, "Load example data:" ),
                        column(2, "Actions:" ),
                        class="paramHeader"
                    ),
                    fluidRow(
                        column(3,
                               selectInput( "testType", NULL,
                                            list( 
                                                "CERNO test (single list)" = "cerno",
                                                "U-test (single list)" = "utest",
                                                "hypergeometric test (two lists)" = "hg" ))
                        ),
                        column(2, 
                               uiOutput("fileEntry1"),
                               uiOutput("fileEntry2")
                        ), 
                        column(3, selectInput( "mset", NULL,
                                               list( "Li et al. and B. Pulendran (LI)"="LI"
                                                     ,"Damien Chaussabel et al. (DC)"="DC"
                                                     ,"LI + DC"="all"
                                                     ,"MSigDB Hallmark gene sets"="msigH"
                                                     ,"MSigDB Positional gene sets (C1)"="msigC1"
                                                     ,"MSigDB Curated gene sets (C2)"="msigC2"
                                                     ,"MSigDB Motif gene sets (C3)"="msigC3"
                                                     ,"MSigDB Computational signatures (C4)"="msigC4"
                                                     ,"MSigDB GO gene sets (C5)"="msigC5"
                                                     ,"MSigDB Oncogenic signatures (C6)"="msigC6"
                                                     ,"MSigDB Immunologic signatures (C7)"="msigC7"
                                               ))),
                        column(2, 
                               selectInput( "example", NULL,
                                            list("------"="exempty", 
                                                 "Load example for CERNO test"="cerno", 
                                                 "Load example for U test"="utest", 
                                                 "Load example for hg test"="hg",
                                                 "Reset"="reset"))
                        ),
                        column(2, 
                               actionButton( "submit1", label= "▶ Run tmod", class="tmodAct" ),
                               uiOutput("tagcloudButton"),
                               uiOutput("exportButton"),
                               actionButton( "reset", label= "☒ Reset", class="tmodAct" )
                        )
                    ),
                    class="params"
                ),
                
                
                # 这个函数是一个临时替代函数。
                # fluidRow( column(12, offset=0, htmlOutput("message", inline=TRUE), class="tmodMsg" )),
                
                # these are required for button to be reactive
                div(id="glist", class="shiny-input-radiogroup", 
                    div(id="row", class="shiny-input-radiogroup", 
                        
                        # hidden buttons with value 0 
                        div(class="hidden",
                            HTML('<input type="radio" name="row" value="0" id="r0" /><label for="r0">Plot</label>'),
                            HTML('<input type="radio" name="glist" value="0" id="r0" /><label for="r0">Plot</label>')
                        ), hr(),
                        dataTableOutput( "results" )
                    ) 
                ),
                
                # plot popup panel
                popupWindow("plotpanelW", 
                            div(plotOutput( "evidencePlot2" ))),
                
                popupWindow("genelistW",  
                            div(class="glist",
                                p(tags$b(textOutput("genelist_title"))),
                                p(HTML("Genes shown in <b>bold</b> are in the main data set")),
                                p(uiOutput("genelist")))
                ),
                
                popupWindow("tagcloudW",
                            div(plotOutput( "tagcloudPlot" ), style="width:600px;height:600px;" ))
                

               
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