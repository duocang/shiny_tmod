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
    width = 150,
    sidebarMenu(
        menuItem("Test", tabName = "file", icon = icon("file-o"),
                 menuSubItem("Upload File(s)", tabName = "file_preview"),
                 menuSubItem("Test", tabName = "tests"),
                 startExpanded = TRUE),
        menuItem("Help", tabName = "help", icon = icon("question")),
        menuItem("Download", tabName = "download", icon = icon("download"), startExpanded = FALSE),
        menuItem("Gallery", tabName = "gallery", icon = icon("file-picture-o"),startExpanded = FALSE),
        menuItem("Logs", tabName = "logs", icon = icon("info"), startExpanded = FALSE)
        ,menuItem("实验", tabName = "shiyan")
    )
)

body <- dashboardBody(
    useShinyjs(),
    # This will call message-handler.js
    tags$head(tags$script(src = "js/alert_message.js"),
              # tags$script(src = "//code.jquery.com/jquery-3.2.1.min.js"),
              tags$link(rel = "stylesheet", type = "text/css", href = "css/tmod.css")),
    # tags$script(HTML('$(document).ready(function() {
    #                  $("header").find("nav").append(\'<span class="myClass"> Text Here </span>\');
    #                  })
    #                  ')),
    tabItems(
        tabItem(tabName = "file_preview",
                fluidRow( column(12, offset=0, uiOutput("message_upload_page"),class="tmodMsg")),
                fluidRow(
                    column(3, 
                           fileInput("files", label = "Upload file(s)", 
                                     multiple = TRUE, 
                                     accept = c("text/csv", ".csv"))) ,
                    column(3,
                           selectInput("example", "Or use example",
                                       list("------"="exempty", 
                                            "Load example for CERNO test"="cerno", 
                                            "Load example for U test"="utest"))),
                    column(3, 
                           uiOutput("choose_preview_file")),
                    column(3, 
                           uiOutput("genename_col"))
                ),
                class="params",
                DT::dataTableOutput("table")),
        tabItem(tabName = "tests",
                fluidRow( column(12, offset=0, htmlOutput("message", inline=TRUE), class="tmodMsg" )),
                fluidRow(
                    column(3, "  Gene module"),
                    column(2, "  Gene sort" ),
                    column(2, "  Trend" ),
                    column(1, "  Abs" ),
                    column(2, "  Test type" ),
                    class="paramHeader"),
                fluidRow(
                    column(3, 
                           selectInput( "gene_module", NULL,
                                        list(  "Li et al. and B. Pulendran (LI)"="LI"
                                               ,"Damien Chaussabel et al. (DC)"="DC"
                                               ,"LI + DC"="all"
                                               ,"MSigDB Hallmark gene sets"="msigH"
                                               ,"MSigDB Positional gene sets (C1)"="msigC1"
                                               ,"MSigDB Curated gene sets (C2)"="msigC2"
                                               ,"MSigDB Motif gene sets (C3)"="msigC3"
                                               ,"MSigDB Computational signatures (C4)"="msigC4"
                                               ,"MSigDB GO gene sets (C5)"="msigC5"
                                               ,"MSigDB Oncogenic signatures (C6)"="msigC6"
                                               ,"MSigDB Immunologic signatures (C7)"="msigC7"),
                                        selected = "LI")),
                    column(2, selectInput("sort_by", NULL,
                                          choices = c("",
                                                      "logFC" = "logFC",
                                                      "t" = "t",
                                                      "msd" = "msd",
                                                      "SE" = "SE",
                                                      "d" = "d",
                                                      "qval" = "qval"),
                                          selected = "logFC")) ,
                    column(2, selectInput("inc_dec", NULL,
                                          choices = c("",
                                                      "Increasing" = "FALSE",
                                                      "Decreasing" = "TRUE"),
                                          selected = "FALSE")),
                    column(1, selectInput("abs", NULL,
                                          choices = c("", 
                                                      "YES",
                                                      "NO"),
                                          selected = "NO")),
                    column(2, selectInput("test_type", NULL,
                                          choices = c("" ,
                                                      "tmodCERNOtest" = "tmodCERNOtest",
                                                      "tmodUtest" = "tmodUtest"),
                                          selected = "tmodUtest")),
                    column(2, actionButton("run", "Plot heatmap-like", class="tmodAct"))),
                fluidRow(
                    column(2, "  pie.pval"),
                    column(2, "  pie.lfc" ),
                    class="paramHeader"),
                fluidRow(
                    column(2,  
                           numericInput("pie.pval", 
                                        NULL, 0.05, min = 0, max = 0.1, step = 0.01)),
                    column(2,
                           numericInput("pie.lfc",
                                        NULL, 1, min = 0, max = 5, step = 0.5)),
                    column(2, actionButton("run1", "Plot rug-like", class="tmodAct")),
                    column(2, uiOutput("tagcloudButton"))#,
                    #column(2, uiOutput("exportButton"))
                ),
                uiOutput("testOrExample_result")
        ),
        tabItem(tabName = "help",
                includeMarkdown("md/help.md")),
        tabItem(tabName = "download",
                includeMarkdown("md/downloads.md")),
        tabItem(tabName = "gallery",
                get.gallery()),
        tabItem(tabName = "logs",
                fluidRow(column(10, htmlOutput( "messageLog"), br()) ))
        ,tabItem(tabName = "shiyan",
                 checkboxGroupButtons(
                     inputId = "somevalue", label = "Make a choice :", 
                     choices = c("Choice A", "Choice B", " Choice C", "Choice D"), 
                     justified = TRUE, status = "primary",
                     checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))),
                 searchInput(
                     inputId = "id", 
                     label = "Enter your search :", 
                     placeholder = "This is a placeholder", 
                     btnSearch = icon("search"), 
                     btnReset = icon("remove"), 
                     width = "100%"
                 ),
                 dropdownButton(
                     tags$h3("List of Input"),
                     selectInput(inputId = 'xcol', label = 'X Variable', choices = names(iris)),
                     selectInput(inputId = 'ycol', label = 'Y Variable', choices = names(iris), selected = names(iris)[[2]]),
                     sliderInput(inputId = 'clusters', label = 'Cluster count', value = 3, min = 1, max = 9),
                     circle = FALSE, status = "danger", icon = icon("gear"), width = "300px",
                     tooltip = tooltipOptions(title = "Click to see inputs !")
                 ),
                 materialSwitch(inputId = "id", label = "Primary switch", status = "primary", right = TRUE)
                 ,switchInput(inputId = "Id012", 
                              onLabel = "Example", offLabel = "Files")
                 ,pickerInput(
                     inputId = "id", 
                     label = "Select/deselect all options", 
                     choices = LETTERS, options = list(`actions-box` = TRUE), 
                     multiple = TRUE
                 )
                 ,
                 
                  # image of rstudio logo
                 img(src = "RStudio-Ball.png", id = "RStudio"),
                  
                  # text output
                 textOutput("text")
                     

                 
                 
                 
                 )
    ),class="params"
)

# Put them together into a dashboardPage
dashboardPage(
    skin = "black",
    dashboardHeader(title = "tmod tests beta", 
                    tags$li(uiOutput("messageInHeader", class="tmodMsgHeader"),
                        class= "dropdown"),
                    tags$li(uiOutput("exportButton"),class="dropdown"),
                    
                    # tags$li(
                    #     actionLink("dd", "", icon = icon("download"), class = "btn shiny-download-link headerButton", href = "", target = "_blank"),
                    #     class = "dropdown"),
                    tags$li(
                        actionLink("refresh", "", icon("refresh")),
                        class = "dropdown"),
                    dropdownMenuOutput("downloadMenu"),
                    dropdownMenu(type = "messages",
                                 icon = icon("user"),
                                 headerText = "",
                                 messageItem(
                                     from = "王雪松(Xuesong Wang)",
                                     message = "wangxuesong29@gmail.com",
                                     href = "mailto:wangxuesong29@gmail.com"
                                     ),
                                 messageItem(
                                     from = "Dr. January Weiner 3",
                                     message = "+49-30-28460514"
                                     )),
                    #tags$li(actionButton("download", "",icon = icon("download"), class="headerButton"), class = "dropdown"),
                    # dropdownMenu(
                    #     tags$li(a(href = 'http://shinyapps.company.com',
                    #               icon("download"),
                    #               title = "Back to Apps Home"),
                    #             class = "dropdown"),
                    #     tags$li(
                    #         actionLink("refresh", "Refresh", icon("refresh")),
                    #         class = "dropdown")
                    # ),
                    titleWidth = 150),
    sidebar,
    body
)