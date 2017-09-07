load("data/annotObject.RData")
load("data/msig.rda")

function(input, output, session) {
    # global variables holding the state of the statistical tests
    # fg           <- NULL
    fg           <<- read.genes(filename="www/data/test.csv", output=output)
    Utest        <- "hg"
    example      <- FALSE
    log          <- ""
    exampleFileNameList <- list("tt_2_S.csv", "tt_3_TBvS.csv")
    # reactive values
    rv <- reactiveValues()
    rv$uploadResults  <- NULL # it is used for storing results after processing uploaded files
    rv$headerMessage  <- NULL # this is used to show message in header
    rv$tabNum         <- 0  # this var is used to track tab, 1: heatmap-like 2: rug-like 3: table
    rv$tomdTestValue  <- NULL
    rv$tmodTest1Value <- NULL
    si                <- sessionInfo()
    # load the code
    source("R/all_sessions.R")
    source("R/data_loading.R", local=TRUE)
    source("R/visualizations.R", local=TRUE)
    source("R/helpers.R", local = TRUE)
    
    # this variable will be used for keeping file data
    loadedData <- reactiveVal(value=NULL)
    
    
    # dispaly selectioninput:  which file to preview
    output$choosePreviewFile <- renderUI({
        if(is.null(input$files) && input$example == "exempty")
            return(NULL)
        if(!is.null(input$files))
            return(selectInput("which_preview_file", "File Preview", as.list(input$files$name), selected = NULL))
        if(input$example != "exempty")
            return(selectInput("which_preview_file", "File Preview", exampleFileNameList, selected = NULL))
    })
    
    # display selection for which column is genename
    output$genename_col <- renderUI({
        if(is.null(input$files) && input$example == "exempty")
            return(NULL)
         if(!is.null(input$files))
             return(selectInput("whichColumnIsGenename", "Select genename column", 
                                choices = c("-----------------", common_columns())))
        if(input$example != "exempty")
            return(selectInput("whichColumnIsGenename", "Select genename column", 
                               choices = c("-----------------", common_columns()),
                               selected = "GeneName"))
    })
    
    # go through all loaded files and find the common columns
    common_columns <- reactive({
        # go through all loaded files and find the common columns
        dat <- loadedData()
        foo <- lapply(dat, colnames)
        Reduce(intersect, foo)# 对列表foo不断做intersect操作，进而得出共同行
    })
    

    
    # this will show the table of file in page
    output$table = DT::renderDataTable( preview8Lines(),options=list(scrollX=TRUE))
    
    # this function will show first 8 rows of selected file to preview
    preview8Lines <- reactive({
        tryCatch({
            if(!is.null(input$files)){
                n <- 1
                fileSelectedForPreview <- input$which_preview_file# which file selected for preview
                infile <- input$files
                for(i in 1:fileNum())
                    if(infile$name[i] == fileSelectedForPreview)
                        n <- i
                a <- loadedData()[[n]]
                return(head(a, n=8))
            }
            if(input$example != "exempty"){
                no <- 1
                fileSelectedForPreview <- input$which_preview_file# which file selected for preview
                for(i in 1:2)
                    if(exampleFileNameList[i] == fileSelectedForPreview)
                        no <- i
                a <- loadedData()[[no]]
                return(head(a, n=8))
            }
        },
        error = function(e){
            #print("There is no selection for gene name column.")
            return(NULL)
        })
    })
    
    # this function will get the number of files uploaded
    fileNum <- reactive({length(input$files$size)})
    
    # read data and  put into a list
    loadData <- observe({
        input$example
        infile <- input$files$datapath
        print("这里是loadData")
        workingDirecotry <- getwd()
        exampleFileListPath <-  paste0(workingDirecotry, "/data/", exampleFileNameList )
        if(is.null(infile) && input$example != "exempty"){
            data <- lapply(exampleFileListPath, function(x) {fread(x, header = TRUE, stringsAsFactors = FALSE) })
            loadedData(data)
        } else {
            data <- lapply(infile, function(x) {fread(x, header = TRUE, stringsAsFactors = FALSE) })
            loadedData(data) # load file data into global variable loaded_dtat, which is a reactive value
        }
    })
    
    # 1. sort data by selected column
    # 2. abs
    # 3. increasing or decreasing
    # 4. tmod test 
    tmodTest <- reactive({
        print("这里是tmodTest")
        dat <- loadedData()
        if(is.null(dat) || length(dat)==0) {
            addMsg("NO DATA! Upload file(s) or select an example.")
            return(NULL)
        }
        sortCol <- isolate(input$sortByWhich)
        sortAbs <- isolate(input$abs)
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
        if(is.null(names(res)) && !is.null(input$files)) 
            names(res) <- input$files$name
        if(is.null(names(res)) && input$example != "exempty")
            names(res) <- exampleFileNameList
        rv$uploadResults <- res    # it will be used for downloading
        return(res)
    })
    # put the value of tmodTest() into a var, to provent run it repeatedly
    observeEvent(input$whichColumnIsGenename,{
        rv$tomdTestValue <- tmodTest()
    })
    
    tmodTest1 <- reactive({
        print("这里是tmodTest1")
        dat <- loadedData()
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
    # put the value of tmodTest1() into a var, to provent run it repeatedly
    observeEvent(input$whichColumnIsGenename,{
        rv$tomdTest1Value <- tmodTest1()
    })
    
    addLog("Run tmod in version %s", si$otherPkgs$tmod$Version)
    
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
    
    # # When "Run" is clicked, it will show heatmap-like tab
    # observeEvent(input$run,{
    #     updateTabsetPanel(session, "inTabset", selected = "heatmap-like")
    # })
    
    # when "Tagcloud" is clicked, it will show table tab
    observeEvent(input$tagcloud,{
        updateTabsetPanel(session, "inTabset", selected = "table")
    })
    
    
    # when example is useed, disable some selection boxes
    observeEvent(input$example,{
        if(input$example != "exempty"){
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
    
    # no data, add message on header
    observeEvent(input$run,{
        if(is.null(input$files) && input$example == "exempty"){
            addMsg("No task running, there is no data!")
        }
    })
    
    
    # sidebar will collapse
    observeEvent(input$run,{
        # when there is no file uploaded or example, not effect
        req(input$whichColumnIsGenename)  # when no file uploaded, gene name selection box does not exit
        if (input$whichColumnIsGenename != "-----------------" && (!is.null(input$files) || input$example == "exempty" ))
            shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    })
    
    
    observeEvent(input$whichColumnIsGenename, {
        if(input$whichColumnIsGenename != "-----------------" )
            showNotification(
                sprintf("You select %s as GeneName.", input$whichColumnIsGenename),
                duration = 5)
    })
}