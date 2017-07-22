library(shiny)
library(shinydashboard)
library(data.table)
library(tmod)
library(shinyjs)

source("server.R")

data(tmod)
mset <- tmod
tmodTest <- tmodCERNOtest

# below will do many things:
# 1. sort data by selected column
# 2. abs
# 3. increasing or decreasing
# 4. tmod test 
stat_test1 <- reactive({
    input$run# track "plot it" button, onece clicked, run this code bolck
    dat <- isolate(loaded_data())
    if(is.null(dat) || length(dat)==0) {
        print("no data yet")
        return(NULL)
    }
    print("running tests")
    sort_col <- isolate(input$sort_by)# isolate() is used, we donot want to rerun this code block every time, 
    sort_abs <- isolate(input$abs)    # when we change sorting column or othre choices
    sort_decr <- isolate(input$inc_dec)
    geneName <- isolate(input$which_col_genename)
    res <- sapply(dat, function(x) {
        x <- data.frame(x)
        genes <- x[ , geneName ]
        ord   <- x[ , sort_col ]
        if(sort_abs == "YES") ord <- abs(ord)
        ord <- order(ord, decreasing=sort_decr)
        tmodCERNOtest(genes[ord], mset=mset, qval=1)
    }, simplify=FALSE
    )
    if(is.null(names(res))) names(res) <- paste0("N.", 1:length(res))
    return(res)
})
