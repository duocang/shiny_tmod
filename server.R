load("data/annotObject.RData")
load("data/msig.rda")

function(input, output, session) {
    # global variables holding the state of the statistical tests
    fg           <- NULL
    Utest        <- "hg"
    example      <- FALSE
    log          <- ""
    
    # reactive values
    rv <- reactiveValues()
    rv$results <- NULL       # it is used for storing example data
    rv$uploadResults <- NULL # it is used for storing results after processing uploaded files
    rv$headerMessage <- NULL # this is used to show message in header
    
    si <- sessionInfo()
    # load the code
    source("R/all_sessions.R")
    source("R/data_loading.R", local=TRUE)
    source("R/visualizations.R", local=TRUE)
    source("R/helpers.R", local = TRUE)
    
    # this variable will be used for keeping file data
    loadedData <- reactiveVal(value=NULL)
    
    
    # dispaly selectioninput:  which file to preview
    output$choosePreviewFile <- renderUI({
        req(loadedData())
        selectInput("which_preview_file", "File Preview", 
                    as.list(input$files$name), selected = NULL)
    })
    
    # go through all loaded files and find the common columns
    common_columns <- reactive({
        # go through all loaded files and find the common columns
        dat <- loadedData()
        foo <- lapply(dat, colnames)
        Reduce(intersect, foo)# 对列表foo不断做intersect操作，进而得出共同行
    })
    
    # display selection for which column is genename
    output$genename_col <- renderUI({
        req(loadedData())
        selectInput("whichColumnIsGenename", "Select genename column", 
                    choices = c("-----------------", common_columns()))
    })
    
    # this will show the table of file in page
    output$table = DT::renderDataTable( preview8Lines(),options=list(scrollX=TRUE))
    
    # this function will show first 8 rows of selected file to preview
    preview8Lines <- reactive({
        tryCatch({
            n <- 1
            fileSelectedForPreview <- input$which_preview_file# which file selected for preview
            infile <- input$files
            if(is.null(infile)) return(NULL)# user has not uploaded a file yet
            for(i in 1:fileNum())
                if(infile$name[i] == fileSelectedForPreview)
                    n <- i
            a <- loadedData()[[n]]
            head(a, n=8)
        },
        error = function(e){
            print("There is no selection for gene name column.")
            return(NULL)
        })
    })
    
    # this function will get the number of files uploaded
    fileNum <- reactive({length(input$files$size)})
    
    # read data and  put into a list
    loadData <- observe({
        infile <- input$files$datapath
        if(is.null(infile)){
            # User has not uploaded files yet
        } else {
            data <- lapply(infile, function(x) {fread(x, header = TRUE, stringsAsFactors = FALSE) })
            loadedData(data) # load file data into global variable loaded_dtat, which is a reactive value
        }
        data
    })
    
    # below will do many things:
    # 1. sort data by selected column
    # 2. abs
    # 3. increasing or decreasing
    # 4. tmod test 
    tmodTest <- reactive({
        input$run
        input$whichColumnIsGenename
        dat <- isolate(loadedData())
        if(is.null(dat) || length(dat)==0) {
            addMsg("NO DATA! Upload file(s) or select an example.")
            return(NULL)
        }
        sortCol <- isolate(input$sortByWhich)# isolate() is used, we donot want to rerun this code block every time, 
        sortAbs <- isolate(input$abs)    # when we change sorting column or othre choices
        sortDecr <- isolate(input$incOrDec)
        geneName <- isolate(input$whichColumnIsGenename)
        
        if(geneName == "-----------------" )
            addMsg("No task running, because there no is gene column selected yet!")
        if(isolate(input$testType) == "tmodCERNOtest"){
            res <- sapply(dat, function(x) {
                x <- data.frame(x)
                genes <- x[ , geneName ]
                ord   <- x[ , sortCol ]
                if(sortAbs == "YES") ord <- abs(ord)
                ord <- order(ord, decreasing=sortDecr)
                tmodCERNOtest(genes[ord], mset=isolate(getMset()), qval=1)
            }, simplify=FALSE)
        }else{
            res <- sapply(dat, function(x) {
                x <- data.frame(x)
                genes <- x[ , geneName ]
                ord   <- x[ , sortCol ]
                if(sortAbs == "YES") ord <- abs(ord)
                ord <- order(ord, decreasing=sortDecr)
                tmodUtest(genes[ord], mset=isolate(getMset()), qval=1)
            }, simplify=FALSE)
        }
        if(is.null(names(res))) names(res) <- input$files$name
        rv$uploadResults <- res    # it will be used for downloading
        return(res)
    })
    
    tmodTest1 <- reactive({
        input$run
        dat <- isolate(loadedData())
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
        
        geneName <- isolate(input$whichColumnIsGenename)
        gene <- dat[[1]][, geneName, with=FALSE]
        
        ddd <- data.frame(dat[[1]])
        gg <- ddd[[geneName]]
        pie <- tmodDecideTests(g=gg,
                               lfc=lfcs,
                               pval=pvals,
                               lfc.thr=isolate(input$pie.lfc),
                               pval.thr=isolate(input$pie.pval),
                               mset=mset)
        return(pie)
    })
    
    addLog("Run tmod in version %s", si$otherPkgs$tmod$Version)
    
    # use example
    # allows saving of the results in a CSV file
    # note that there is no error handling if no results 
    # have been generated
    output$exampleExport <- downloadHandler(
        filename=function() {
            sprintf("results_%s_%s_%s.csv", Utest, isolate(input$geneModule), Sys.Date() ) 
        },
        content=function(file) {
            if(!is.null(rv$results)) {
                foo <- rv$results
                foo$Genes <- getGenes(rv$results$ID, fg, mset=isolate(input$geneModule))$Genes
                write.csv(foo, file, row.names=FALSE)
            }
        }
    )
    
    # upload files
    # allows saving of the results in a CSV file
    # note that there is no error handling if no results 
    # have been generated
    output$uploadExport <- downloadHandler(
        filename = "data.zip",
        content = function(fname){
            req(loadedData())
            mapply( function(x, y){
                write.csv(x, file = paste0("Result_", y))
            },
            rv$uploadResults,
            names(rv$uploadResults))
            
            zip(zipfile = fname, files =paste0("Result_", names(rv$uploadResults)))
        },
        contentType = "application/zip"
    )
    
    # This will show an allert, if the user trys to run without selecting gene column
    observeEvent(input$run,{
        req(input$run)
        req(input$whichColumnIsGenename)
        if(input$whichColumnIsGenename == "-----------------"){
            session$sendCustomMessage(type = "alert_message", message = 'Please select gene cloumn!')
        }
    })
    
    # when we click "Plot heatmap-like", it shows message on the header
    observeEvent(input$run,{
        if((!is.null(input$files) && input$whichColumnIsGenename != "-----------------") || input$example != "exempty")
            addMsg(sprintf("Run test %s whith mset=%s.", isolate(input$testType),isolate(input$geneModule)))
    })
    
    # When "Run" is clicked, it will show heatmap-like tab
    observeEvent(input$run,{
        updateTabsetPanel(session, "inTabset", selected = "heatmap-like")
    })
    
    # when "Tagcloud" is clicked, it will show table tab
    observeEvent(input$tagcloud,{
        updateTabsetPanel(session, "inTabset", selected = "table")
    })
    
    # an example is selected, corresponding test will run
    # and result will be given to rv$results
    observe({
        if(input$example == "exempty")
            return(NULL)
        mset <- getMset()
        isolate(load.example())
        if(is.null(fg)){
            print("no example data is uploaded")
            addMsg("Please, select example data")
            return(NULL)
        }
        rv$results <- run.stats(fg, Utest, mset=mset)
    })
    
    # when example is useed, disable some selection boxes
    observeEvent(input$example,{
        if(input$example != "exempty"){
            shinyjs::disable("sortByWhich")
            disable("incOrDec")
            disable("abs")
            disable("pie.pval")
            disable("pie.lfc")
            disable("testType")
            disable("files")
            addMsg("Example is ready for running!   <b>Go to Test tap</b>")
            return()
        }
    })
    
    # when example is useed, disable some selection boxes
    observeEvent(input$files,{
        if(!is.null(input$files)){
            disable("example")
            addMsg(" Uploaded file(s) will be used for running!   <b>Go to Test tap</b>")
        }
    })
    
    # refresh page
    observeEvent(input$refresh,{
        session$reload()
    })
    
    observeEvent(input$run,{
        if(is.null(input$files) && input$example == "exempty"){
            addMsg("No task running, there is no data!")
        }
    })
    
    observeEvent(input$run,{
        shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    })
    
    
    observeEvent(input$whichColumnIsGenename, {
        if(input$whichColumnIsGenename != "-----------------" )
            showNotification(
                sprintf("You select %s as GeneName.", input$whichColumnIsGenename),
                duration = 10,
                type = "message")
    })
}