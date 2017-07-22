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
    id = "sidebarmenu",
    menuItem("Test", tabName = "file",
             menuSubItem("File Preview", tabName = "file_preview", icon = icon("file-0")),
             menuSubItem("Tests", tabName = "tests")
    ),
    menuItem("Test Beta", tabName = "test_beta", icon = icon("line-chart"),
             actionButton("drowdownTest", "Start Test"),
             actionButton("download_reuslt", "download reuslt")
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

body <- dashboardBody(
    
)


# Put them together into a dashboardPage
dashboardPage(
    skin = "black",
    dashboardHeader(title = "tmod tests beta"),
    sidebar,
    body
)