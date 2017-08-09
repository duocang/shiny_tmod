library(shiny)
library(shinydashboard)
library(data.table)
library(tmod)
library(shinyjs)
library(markdown)
library(tagcloud)


source("R/all_sessions.R")
data(tmod)
mset <- tmod
load("data/annotObject.RData")
load("data/msig.rda")


# It allows file bigger than 25M to be uploaded
options(shiny.maxRequestSize=30*1024^2)

function(input, output, session) {
    
    # global variables holding the state of the statistical tests
    fg           <- NULL
    bg           <- NULL
    Utest        <- "hg"
    example      <- FALSE
    log          <- ""
    
    # reactive values
    rv <- reactiveValues()
    rv$results <- NULL
    
    
    
    si <- sessionInfo()
    # load the code
    source("R/data_loading.R", local=TRUE)
    source("R/visualizations.R", local=TRUE)
    source("R/helpers.R", local = TRUE)
    
    # this variable will be used for keeping file data
    loaded_data <- reactiveVal(value=NULL)

    print("I was called")
    
    # dispaly choosebox for which file to preview
    output$choose_preview_file <- renderUI({
        if(is.null(loaded_data()))
            return(NULL)
        selectInput("which_preview_file", "File Preview", 
                    as.list(input$files$name), selected = NULL)
    })
    
    # go through all loaded files and find the common columns
    common_columns <- reactive({
        # go through all loaded files and find the common columns
        dat <- loaded_data()
        foo <- lapply(dat, colnames)
        Reduce(intersect, foo)# 对列表foo不断做intersect操作，进而得出共同行
    })
    
    # display choosebox for which column is genename
    output$genename_col <- renderUI({
        if(is.null(loaded_data())) 
            return(NULL)
        selectInput("which_col_genename", "Select genename column", 
                    choices = c("-----------------", common_columns()))
    })
 
    # this will show the table of file in page
    output$table = DT::renderDataTable( preview_8_lines(),
                                        options=list(scrollX=TRUE)
    )
 
    # this function will show first 8 rows of selected file to preview
    preview_8_lines <- reactive({
        tryCatch({
            n <- 1
            file_typein <- input$which_preview_file# which file selected for preview
            infile <- input$files
            if(is.null(infile)) return(NULL)# user has not uploaded a file yet
            for(i in 1:file_num())
                if(infile$name[i] == file_typein)
                    n <- i
            a <- loaded_data()[[n]]
            head(a, n=8)
        },
        error = function(e){
            print("gene_column_selection alert needs to be showed and confirmed first")
            return(NULL)
        })
    })
    
    # this function will get the number of files uploaded
    file_num <- reactive({length(input$files$size)})
    
    # print file name on sidebar
    output$upload_files <- renderTable({
        filename <- c()
        for(i in 1:file_num()){
            filename <- c(filename, input$files$name[i])
        }
        filename
    }, colnames = FALSE
    )
    
    # read data and  put into a list
    loadData <- observe({
        infile <- input$files$datapath
        if(is.null(infile)){
            # User has not uploaded files yet
        } else {
            data <- lapply(infile, 
           function(x) {
               fread(x, header = TRUE, stringsAsFactors = FALSE) 
            })
        loaded_data(data) # load file data into global variable loaded_dtat, which is a reactive value
        print("data loaded")
    }
    data
    })
    

    
    # below will do many things:
    # 1. sort data by selected column
    # 2. abs
    # 3. increasing or decreasing
    # 4. tmod test 
    stat_test <- reactive({
        input$run
        dat <- isolate(loaded_data())
        if(is.null(dat) || length(dat)==0) {
          print("no data yet")
          return(NULL)
        }
        sort_col <- isolate(input$sort_by)# isolate() is used, we donot want to rerun this code block every time, 
        sort_abs <- isolate(input$abs)    # when we change sorting column or othre choices
        sort_decr <- isolate(input$inc_dec)
        geneName <- isolate(input$which_col_genename)
        print("查看哪一种test")
        print(input$test_type)
        if(input$test_type == "tmodCERNOtest"){
            print("这是一个类型")
            print("tmodCERNOtest")
            res <- sapply(dat, function(x) {
                x <- data.frame(x)
                genes <- x[ , geneName ]
                ord   <- x[ , sort_col ]
                if(sort_abs == "YES") ord <- abs(ord)
                ord <- order(ord, decreasing=sort_decr)

                tmodCERNOtest(genes[ord], mset=mset, qval=1)
            }, simplify=FALSE
            )
        }else{
            print("这是一个类型")
            print("tmodUtest")
            res <- sapply(dat, function(x) {
                x <- data.frame(x)
                genes <- x[ , geneName ]
                ord   <- x[ , sort_col ]
                if(sort_abs == "YES") ord <- abs(ord)
                ord <- order(ord, decreasing=sort_decr)

                tmodUtest(genes[ord], mset=mset, qval=1)
            }, simplify=FALSE
            )
        }
        # res <- sapply(dat, function(x) {
        #     x <- data.frame(x)
        #     genes <- x[ , geneName ]
        #     ord   <- x[ , sort_col ]
        #     if(sort_abs == "YES") ord <- abs(ord)
        #     ord <- order(ord, decreasing=sort_decr)
        #     if(input$test_type == "tmodCERNOtest"){
        #         print("这是一个类型")
        #         print("tmodCERNOtest")
        #         tmodCERNOtest(genes[ord], mset=mset, qval=1)
        #     }
        #     if(input$test_type == " tmodUtest"){
        #         print("这是一个类型")
        #         print("tmodUtest")
        #         tmodUtest(genes[ord], mset=mset, qval=1)
        #     }
        # }, simplify=FALSE
        # )
        if(is.null(names(res))) names(res) <- input$files$name
        res <- module_filter(res, input$mset)
        # res <- sapply(res, function(x){
        #     subset(x, startsWith(x$ID, "LI"))
        #     # x[startsWith(x$ID, "LI"), ]
        # }, simplify = FALSE)
        print(head(res[1]))
        return(res)
    })
    
    stat_test1 <- reactive({
        input$run1
        dat <- isolate(loaded_data())
        if(is.null(dat) || length(dat)==0) {
            print("no data yet")
            return(NULL)
        }
        lfcs <- sapply(dat, function(x) {
            as.matrix(x[, "logFC", with=FALSE])
        })
        pvals <- sapply(dat, function(x){
            as.matrix(x[, "qval", with=FALSE])
        })
        
        geneName <- isolate(input$which_col_genename)
        gene <- dat[[1]][, geneName, with=FALSE]
        
        
        ddd <- data.frame(dat[[1]])
        gg <- ddd[[geneName]]
        pie <- tmodDecideTests(g=gg,
                               lfc=lfcs,
                               pval=pvals,
                               lfc.thr=isolate(input$pie.lfc),
                               pval.thr=isolate(input$pie.pval),
                               mset=mset
        )
        return(pie)
    })

    output$plot <- renderPlot({
        input$run
        if(input$run == 0){
            return(NULL)
        }
        if((input$sort_by != "") && (input$inc_dec != "") && (input$abs != "") && (input$test_type != "")){
            withProgress(message = 'Making plot', value = 0, {
                n <- 10
                # Number of times we'll go through the loop
                for (i in 1:n) {
                    # Each time through the loop, add another row of data. This is
                    # a stand-in for a long-running computation.
                    
                    # Increment the progress bar, and update the detail text.
                    incProgress(1/n, detail = paste("Doing part", i))
                    
                    # Pause for 0.1 seconds to simulate a long computation.
                    Sys.sleep(0.1)
                }
                
                
                
                plo <- stat_test()
                if(!is.null(plo))
                    tmodPanelPlot(plo, text.cex = 0.9, legend.style = "auto")
                print("plot done")
                # tryCatch({
                #     plo <- stat_test()
                #     if(!is.null(plo))
                #         tmodPanelPlot(plo, text.cex = 0.9, legend.style = "auto")
                #     print("plot done")
                # },
                # warning = function(w){
                #     print("no correct gene column selected")
                # },
                # error = function(e){
                #     print("gene column is selected, but it is processed by isolated() function")
                #     return(NULL)
                # })
            })
        }
    }, bg="transparent")
    
    
    output$plot1 <- renderPlot({
        input$run1
        if(input$run1 == 0){
            return(NULL)
        }
        if((input$sort_by != "") && (input$inc_dec != "") && (input$abs != "") && (input$test_type != "")){
            withProgress(message = 'Making plot', value = 0, {
                n <- 10
                # Number of times we'll go through the loop
                for (i in 1:n) {
                    # Each time through the loop, add another row of data. This is
                    # a stand-in for a long-running computation.
                    
                    # Increment the progress bar, and update the detail text.
                    incProgress(1/n, detail = paste("Doing part", i))
                    # Pause for 0.1 seconds to simulate a long computation.
                    Sys.sleep(0.1)
                }
                res <- isolate(stat_test())
                pie <- isolate(stat_test1())
                names(pie) <- names(res)
                if(!is.null(res))
                    tmodPanelPlot(res, pie=pie, pie.style="r", grid="b", filter.rows.pval=0.001)
            })
        }
    }, bg="transparent")
    
    # This will show an allert, if the user trys to run without selecting gene column
    observeEvent(input$run,{
        if(input$run == 0){
            return(NULL)
        }
        
        if(is.null(input$which_col_genename))
            return(NULL)
        
        if(input$which_col_genename == "-----------------"){
            session$sendCustomMessage(type = "alert_message",
                                      message = 'Please select gene cloumn!')
        }
    })
    
    observeEvent(input$which_preview_file,{
        Sys.sleep(0.5)
        if(file_num() != 0){
            session$sendCustomMessage(type = "alert_message",
                                      message = "Please select gene cloumn!")
        }
    })
    
    # 以下的内容将会实现january此前tmod enrichment tool 中的功能

    # "2017-08-07 10:05:28: Running tmod in version 0.31" is printed in tab "Logs"
    addLog("Running tmod in version %s", si$otherPkgs$tmod$Version)
    
    # when we click "Plot heatmap-like", it shows message on the page
    observeEvent(input$run,{
        if((input$sort_by != "") && (input$inc_dec != "") && (input$abs != "") && (input$test_type != "")){
            addMsg(
                sprintf("Running test %s whith mset=%s, pleast wait", 
                        isolate(input$test_type),
                        isolate(input$mset)))
        }else{
            addMsg(" Attention! Please, make sure all parameters are set!")
        }
    })
    
    # when we click "Plot rug-like", it shows message on the page
    observeEvent(input$run1,{
        if((input$sort_by != "") && (input$inc_dec != "") && (input$abs != "") && (input$test_type != "")){
            addMsg(
                sprintf("Running test %s whith mset=%s, pleast wait", 
                        isolate(input$test_type),
                        isolate(input$mset)))
        }else{
            addMsg(" Attention! Please, make sure all parameters are set!")
        }
    })
    
    # When "rug-like plot" is clicked, it will show rug-like tab
    observeEvent(input$run1,{
        updateTabsetPanel(session, "inTabset",
                          selected = "rug-like")
    })
        
    # When "headmap-like plot" is clicked, it will show heatmap-like tab
    observeEvent(input$run,{
        updateTabsetPanel(session, "inTabset",
                          selected = "heatmap-like")
    })
    
    # if user wants to test with example data, a new tab will appear in sidebar
    output$example_test <- renderMenu({
        if(input$example != "empty")
            menuSubItem("Example tests", tabName = "example_test")
    })
}


