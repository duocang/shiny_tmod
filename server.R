load("data/annotObject.RData")
load("data/msig.rda")

function(input, output, session) {
    # load the code
    source("R/all_sessions.R")
    source("R/data_loading.R", local=TRUE)
    source("R/visualizations.R", local=TRUE)
    source("R/helpers.R", local = TRUE)
    
    # global variables holding the state of the statistical tests
    fg           <- NULL
    
    Utest        <- "hg"
    example      <- FALSE
    log          <- ""
    exampleFileNameList <- list("tt_2_S.csv", "tt_3_TBvS.csv")
    # reactive values
    rv <- reactiveValues()
    rv$uploadResults  <- NULL # it is used for storing results after processing uploaded files
    rv$headerMessage  <- NULL # this is used to show message in header
    rv$tabNum         <- 0  # this var is used to track tab, 1: heatmap-like 2: rug-like 3: table
    rv$inti           <- 0
    si                <- sessionInfo()

    # this variable will be used for keeping file data
    loadedData <- reactiveVal(value=NULL)
    
    # dispaly selectioninput:  which file to preview
    output$choosePreviewFile <- renderUI({
        if(is.null(input$files) && input$example == "exempty")
            return(NULL)
        if(!is.null(input$files))
            return(selectInput("which_preview_file", "Select file to preview", as.list(input$files$name), selected = NULL))
        if(input$example != "exempty")
            return(selectInput("which_preview_file", "Select file to preview", exampleFileNameList, selected = NULL))
    })
    
    # display selection for which column is genename
    output$genename_col <- renderUI({
        # read.gene() is put here, just to make sure fg has a avlue form begining.
        fg           <<- read.genes(filename="www/data/test.csv", output=output)
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
        Reduce(intersect, foo)
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
    
    
    # according to rv$fileExampleUpdate, loadData will know which file should be loaded
    observeEvent(input$example,{
        rv$fileExampleUpdate <- 0
    })
    observeEvent(input$files,{
        rv$fileExampleUpdate <- 1
    })
    
    # read data and  put into a list
    # Args:
    #   input$example: it is a reactive trigger of this observe.
    #   input$files$datapath: a reactive trigger and uploaded files
    #   rv$fileExampleUpdate: 0-read example, 1-read uploaded files
    # Returns:
    #   a list includes data, example or uploaded files
    loadData <- observe({
        input$example
        infile <- input$files$datapath
        workingDirecotry <- getwd()
        exampleFileListPath <-  paste0(workingDirecotry, "/data/", exampleFileNameList )
        if(rv$fileExampleUpdate == 0){
            data <- lapply(exampleFileListPath, function(x) {fread(x, header = TRUE, stringsAsFactors = FALSE) })
            return(loadedData(data))
        } 
        if(rv$fileExampleUpdate == 1){
            data <- lapply(infile, function(x) {fread(x, header = TRUE, stringsAsFactors = FALSE) })
            return(loadedData(data)) # load file data into global variable loaded_dtat, which is a reactive value
        }
    })
    
    # tmod test  
    # Args:
    #   loadedData(): example data or uploaded files data
    #   input$sortByWhich: sort data by selected column
    #   input$abs: abs
    #   input$incOrDec: increasing or decreasing
    #   input$whichColumnIsGenename: which column is Genename
    #   input$testType: which test will be executed, cerno or utest
    # Returns:
    #   result after by test, cerno or utest
    #   rv$uploadResults is used to keep the result of tmod test for later useage.
    tmodTest <- reactive({
        dat <- loadedData()
        if(is.null(dat) || length(dat)==0) {
            addMsg("NO DATA! Upload file(s) or select an example.")
            return(NULL)
        }
        sortCol <- input$sortByWhich
        sortAbs <- input$abs
        sortDecr <- input$incOrDec
        geneName <- input$whichColumnIsGenename
        if(geneName == "-----------------" )
            addMsg("No task running, because there no is gene column selected yet!")
        if(isolate(input$testType) == "tmodCERNOtest"){
            res <- sapply(dat, function(x) {
                x <- data.frame(x)
                genes <- x[ , geneName ]
                ord   <- x[ , sortCol ]
                if(sortAbs == "YES") ord <- abs(ord)
                ord <- order(ord, decreasing=sortDecr)
                tmodCERNOtest(genes[ord], mset = getMset(), qval=1)
            }, simplify=FALSE)
        }else{
            res <- sapply(dat, function(x) {
                x <- data.frame(x)
                genes <- x[ , geneName ]
                ord   <- x[ , sortCol ]
                if(sortAbs == "YES") ord <- abs(ord)
                ord <- order(ord, decreasing=sortDecr)
                tmodUtest(genes[ord], mset = getMset(), qval=1)
            }, simplify=FALSE)
        }
        if(is.null(names(res)) && !is.null(input$files)) 
            names(res) <- input$files$name
        if(is.null(names(res)) && input$example != "exempty")
            names(res) <- exampleFileNameList
        rv$uploadResults <- res    # it will be used for downloading
        if (is.null(res)){
            rv$uploadResults <- NULL
            return(NULL)
        }
        return(res)
    })

    # tmod test: tmodDecideTests
    # Args:
    #   loadedData(): example data or uploaded files data
    #   input$whichColumnIsGenename: gene name column
    #   input$pie.lfc: parameter of tmodDEcideTests
    #   input$pie.pval: parameter of tmodDEcideTests
    # Returns:
    #   result of tmodDecideTests
    tmodTest1 <- reactive({
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
        geneName <- input$whichColumnIsGenename
        gene <- dat[[1]][, geneName, with=FALSE]
        ddd <- data.frame(dat[[1]])
        gg <- ddd[[geneName]]
        pie <- tmodDecideTests(g=gg,
                               lfc=lfcs,
                               pval=pvals,
                               lfc.thr=input$pie.lfc,
                               pval.thr=input$pie.pval,
                               mset=mset)
        return(pie)
    })
    
    # add Log
    addLog("Run tmod in version %s", si$otherPkgs$tmod$Version)
    
    # upload files
    # allows saving of the results in a CSV file
    # note that there is no error handling if no results 
    # have been generated
    # Args: 
    #   loadedData(): if it is null, return NULL
    #   rv$uploadResults: pack it into csv file.
    # Returns:
    #   click download button, zip file is downloaded into local path.
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
    
    # when we click "Plot heatmap-like", it shows message on the header
    observeEvent(input$runHeatmap,{
        if((!is.null(input$files) && input$whichColumnIsGenename != "-----------------") || input$example != "exempty")
            addMsg(sprintf("Run test %s whith mset=%s.", isolate(input$testType),isolate(input$geneModule)))
    })
    
    # refresh page
    observeEvent(input$refresh,{
        session$reload()
    })
    
    # no data, add message on header
    observe({
        input$runHeatmap
        input$runRug
        input$runTable
        if(is.null(input$files) && input$example == "exempty"){
            addMsg("No task running, there is no data!")
        }
    })
    
    # show a meeage on bottom right, when genename is slected.
    observeEvent(input$whichColumnIsGenename, {
        if(input$whichColumnIsGenename != "-----------------" )
            showNotification(
                sprintf("You select %s as GeneName.", input$whichColumnIsGenename),
                duration = 5)
    })
}