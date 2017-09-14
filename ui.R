gallery.dir <- 'www/gallery/'
## creates a gallery with figures found in www/gallery
get.gallery <- function() {
    files <- list.files(gallery.dir, pattern=".*\\.png$" ) 
    l <- list(fixedRow( 
        column(10, includeMarkdown("md/gallery.md"))))
    nf <- length(files)
    rowN <- 3
    
    nr <- ceiling(nf / rowN)
    i <- 1
    for(r in 1:nr) {
        clist <- list()
        for(c in 1:rowN) {
            if (i > nf) break ;
            clist[[i]] <- column(floor(12/rowN), img.link(files[i]))
            i <- i + 1
        }
        l <- c(l, list(fixedRow(clist)))
    }
    return(fixedPage(l))
}

## creates a div element with image thumb, description, 
img.link <- function(file) {
    if (file.exists(paste0(gallery.dir, "thumbs/", file))) {
        thumb.file <- paste0("gallery/thumbs/", file)
    } else {
        thumb.file <- paste0("gallery/", file)
    }
    img  <- tags$img(src=thumb.file, width="250px", height="250px", alt=sprintf("[%s]", file))
    link <- paste0("gallery/", file)
    img  <- tags$a(href=link, img)
    name <- tags$a(href=link, file)
    mdname <- paste0(gallery.dir, "md/", file, ".md")
    if (file.exists( mdname )) {
        desc <- tagList(tags$br(), 
                        includeMarkdown(mdname), 
                        tags$br())
    } else {
        desc <- tags$br()
    }
    return(div(class="gallerypanel", img, tags$br(), tags$hr(), name, desc)) 
} 
# sidebar will dispaly on the left side of web page
sidebar <- dashboardSidebar(
    width = 200,
    sidebarMenu(collapsed=TRUE,
        menuItem("Upload File(s)", tabName = "file_preview", icon = shiny::icon("upload")),
        menuItem("Test", tabName = "tests", icon = shiny::icon("area-chart"), badgeLabel =" Hit me", badgeColor = "blue"),
        menuItem("Help", tabName = "help", icon = icon("question")),
        menuItem("Download", tabName = "download", icon = icon("download")),
        menuItem("Gallery", tabName = "gallery", icon = icon("file-picture-o")),
        menuItem("Logs", tabName = "logs", icon = icon("info")))
)

# tables, results and plots will displayed here, including configuration setting as well
body <- dashboardBody(
    useShinyjs(),
    # This will call message-handler.js, used for showing alert message.
    tags$head(tags$script(src = "js/alert_message.js"),
              tags$link(rel = "stylesheet", type = "text/css", href = "css/tmod.css")),
    tabItems(
        tabItem(tabName = "file_preview",
                fluidRow(
                    column(3,
                           div( class="dropdownBySong",
                                fileInput("files", label = "Upload file(s)",multiple = TRUE, accept = c("text/csv", ".csv")),
                                div(class = "dropdownBySong-content",
                                              p("If this button is unclickable, please click Refresh button on top righ.")))),
                    column(3,
                           div( class="dropdownBySong",
                                selectInput("example", "Or use example",
                                            list("------"="exempty",
                                                 "Load example for CERNO test"="cerno",
                                                 "Load example for U test"="utest")),
                                div(class = "dropdownBySong-content",
                                    p("If this button is unclickable, please click Refresh button on top righ.")))),
                    column(3, uiOutput("choosePreviewFile")),
                    column(3, uiOutput("genename_col"))),
                class="params",
                DT::dataTableOutput("table")),
        tabItem(tabName = "tests",  # elements under "tests"
                fluidRow(
                    column(3, "  Gene module"),
                    column(2, "  Gene sort" ),
                    column(2, "  Trend" ),
                    column(1, "  Abs" ),
                    column(2, "  Test type" ),
                    column(1, ""),
                    class="paramHeader"),
                fluidRow(
                    column(3,
                           selectInput( "geneModule", NULL,
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
                    column(2, selectInput("sortByWhich", NULL,
                                          choices = c("",
                                                      "logFC" = "logFC",
                                                      "t" = "t",
                                                      "msd" = "msd",
                                                      "SE" = "SE",
                                                      "d" = "d",
                                                      "qval" = "qval"),
                                          selected = "logFC")) ,
                    column(2, selectInput("incOrDec", NULL,
                                          choices = c("",
                                                      "Increasing" = "FALSE",
                                                      "Decreasing" = "TRUE"),
                                          selected = "TRUE")),
                    column(1, selectInput("abs", NULL,
                                          choices = c("",
                                                      "YES",
                                                      "NO"),
                                          selected = "YES")),
                    column(2, selectInput("testType", NULL,
                                          choices = c("" ,
                                                      "tmodCERNOtest" = "tmodCERNOtest",
                                                      "tmodUtest" = "tmodUtest"),
                                          selected = "tmodCERNOtest")),
                    column(1,uiOutput("operation"))),
                uiOutput("testOrExampleResult")),# it will show the table of result 
        tabItem(tabName = "help",
                includeMarkdown("md/help.md")),
        tabItem(tabName = "download",
                includeMarkdown("md/downloads.md")),
        tabItem(tabName = "gallery",
                get.gallery()),
        tabItem(tabName = "logs",
                fluidRow(column(10, htmlOutput( "messageLog"), br())))),
    class="params"
)

# Put them together into a dashboardPage
dashboardPage(
    skin = "black",
    dashboardHeader(title = "tmod tests beta", 
                    tags$li(uiOutput("messageInHeader", class ="tmodMsgHeader"),class= "dropdown"),
                    #tags$li(uiOutput("uploadExportButton"), class = "dropdown"),
                    tags$li(actionButton("refresh", "", icon("refresh"), class="headerButton"),class = "dropdown"),
                    titleWidth = 158),
    sidebar,
    body
)