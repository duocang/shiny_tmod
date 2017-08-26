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
    loaded_data <- reactiveVal(value=NULL)
    
    
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
    output$table = DT::renderDataTable( preview_8_lines(),options=list(scrollX=TRUE))
    
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
        for(i in 1:file_num())
            filename <- c(filename, input$files$name[i])
        filename
    }, colnames = FALSE)
    
    # read data and  put into a list
    loadData <- observe({
        infile <- input$files$datapath
        if(is.null(infile)){
            # User has not uploaded files yet
        } else {
            data <- lapply(infile, function(x) {fread(x, header = TRUE, stringsAsFactors = FALSE) })
            loaded_data(data) # load file data into global variable loaded_dtat, which is a reactive value
        }
        data
    })
    

    
    # if file(s) is/are uploaded, the test page shows with two tabs: heatmap-like and rug-like
    # if example is used, the test page shows with three tabs: table, heatmap-like and rug-like
    output$testOrExample_result <- renderUI({
        tabsetPanel(id = "inTabset",
                    #tabPanel("table", dataTableOutput( "example_results" )),
                    tabPanel("table", 
                             # these are required for button to be reactive
                             div(id="glist", class="shiny-input-radiogroup", 
                                 div(id="row", class="shiny-input-radiogroup", 
                                     
                                     # hidden buttons with value 0 
                                     div(class="hidden",
                                         HTML('<input type="radio" name="row" value="0" id="r0" /><label for="r0">Plot</label>'),
                                         HTML('<input type="radio" name="glist" value="0" id="r0" /><label for="r0">Plot</label>')), 
                                     hr(),
                                     dataTableOutput( "example_results" ))),
                             # plot popup panel
                             popupWindow("plotpanelW", 
                                         div(plotOutput("evidencePlot2"))),
                             
                             popupWindow("genelistW",  
                                         div(class="glist",
                                             p(tags$b(textOutput("genelist_title"))),
                                             p(HTML("Genes shown in <b>bold</b> are in the main data set")),
                                             p(uiOutput("genelist")))),
                             popupWindow("tagcloudW",
                                         div(plotOutput( "tagcloudPlot" ), style="width:600px;height:600px;" ))),
                    tabPanel("heatmap-like", plotOutput("plot0", height = "1000px")),
                    tabPanel("rug-like", plotOutput("plot01", height = "1000px")))
        
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
            addMsg("NO DATA! Upload file(s) or select an example.")
            return(NULL)
        }
        sort_col <- isolate(input$sort_by)# isolate() is used, we donot want to rerun this code block every time, 
        sort_abs <- isolate(input$abs)    # when we change sorting column or othre choices
        sort_decr <- isolate(input$inc_dec)
        geneName <- isolate(input$which_col_genename)
        
        if(geneName == "-----------------" )
            addMsg("No task running, because there no is gene column selected yet!")
        validate(
            need(geneName != "-----------------", "No task running, because there no is gene column selected yet!")
        )
        
        if(isolate(input$test_type) == "tmodCERNOtest"){
            res <- sapply(dat, function(x) {
                x <- data.frame(x)
                genes <- x[ , geneName ]
                ord   <- x[ , sort_col ]
                if(sort_abs == "YES") ord <- abs(ord)
                ord <- order(ord, decreasing=sort_decr)
                tmodCERNOtest(genes[ord], mset=isolate(getMset()), qval=1)
            }, simplify=FALSE)
        }else{
            res <- sapply(dat, function(x) {
                x <- data.frame(x)
                genes <- x[ , geneName ]
                ord   <- x[ , sort_col ]
                if(sort_abs == "YES") ord <- abs(ord)
                ord <- order(ord, decreasing=sort_decr)
                
                tmodUtest(genes[ord], mset=isolate(getMset()), qval=1)
            }, simplify=FALSE)
        }
        
        if(is.null(names(res))) names(res) <- input$files$name
        rv$uploadResults <- res    # it will be used for downloading
        print("巴扎黑")
        # print(class(rv$uploadResults))
        # print(class(rv$uploadResults[[1]]))
        # print(head(rv$uploadResults[[1]]))
        # print(names(rv$uploadResults))
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
                               mset=mset)
        return(pie)
    })
    
    
    # This will show an allert, if the user trys to run without selecting gene column
    observeEvent(input$run,{
        req(input$run)
        req(input$which_col_genename)
        if(input$which_col_genename == "-----------------"){
            session$sendCustomMessage(type = "alert_message", message = 'Please select gene cloumn!')
        }
    })
    
    observeEvent(input$which_preview_file,{
        Sys.sleep(0.5)
        if(file_num() != 0)
            session$sendCustomMessage(type = "alert_message",
                                      message = "Please select gene cloumn!")
    })
    
    
    # "2017-08-07 10:05:28: Running tmod in version 0.31" is printed in tab "Logs"
    addLog("Run tmod in version %s", si$otherPkgs$tmod$Version)
    
    # when we click "Plot heatmap-like", it shows message on the page
    observeEvent(input$run,{
        if((!is.null(input$files) && input$which_col_genename != "-----------------") || input$example != "exempty")
            addMsg(sprintf("Run test %s whith mset=%s.", isolate(input$test_type),isolate(input$gene_module)))
    })
    
    # when we click "Plot rug-like", it shows message on the page
    observeEvent(input$run1,{
        if((!is.null(input$files) && input$which_col_genename != "-----------------") || input$example != "exempty")
            addMsg(sprintf("Run test %s whith mset=%s.", isolate(input$test_type), isolate(input$gene_module)))
    })
    
    # When "rug-like plot" is clicked, it will show rug-like tab
    observeEvent(input$run1,{
        updateTabsetPanel(session, "inTabset", selected = "rug-like")
    })
    
    # When "headmap-like plot" is clicked, it will show heatmap-like tab
    observeEvent(input$run,{
        updateTabsetPanel(session, "inTabset", selected = "heatmap-like")
    })
    
    # when "Tagcloud" is clicked, it will show table tab
    observeEvent(input$tagcloud,{
        updateTabsetPanel(session, "inTabset", selected = "table")
    })
    
    # an example is selected, corresponding test will runn 
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
    
    output$example_results <- renderDataTable({
        res <- formatResultsTable(rv$results)
        req(res)
        # if(is.null(res)) return(NULL)
        datatable(res, escape =FALSE)
    })
    
    # when example is useed, disable some selection boxes
    observeEvent(input$example,{
        if(input$example != "exempty"){
            shinyjs::disable("sort_by")
            disable("inc_dec")
            disable("abs")
            disable("pie.pval")
            disable("pie.lfc")
            disable("test_type")
            disable("files")
            addMsg("Example is ready for running!   <b>Go to Test tap</b>")
            return()
        }
        enable("sort_by")
        enable("inc_dec")
        enable("abs")
        enable("pie.pval")
        enable("pie.lfc")
        enable("test_type")
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
    
    # allows saving of the results in a CSV file
    # note that there is no error handling if no results 
    # have been generated
    output$exampleExport <- downloadHandler(
        filename=function() {
            sprintf("results_%s_%s_%s.csv", Utest, isolate(input$gene_module), Sys.Date() ) 
        },
        content=function(file) {
            if(!is.null(rv$results)) {
                foo <- rv$results
                foo$Genes <- getGenes(rv$results$ID, fg, mset=isolate(input$gene_module))$Genes
                write.csv(foo, file, row.names=FALSE)
            }
        }
    )
    
    output$uploadExport <- downloadHandler(
        filename = "data.zip",
        content = function(fname){
            req(loaded_data())
            mapply( function(x, y){
               write.csv(x, file = paste0("干哈啊", y))
            },
            rv$uploadResults,
            names(rv$uploadResults))
            
            zip(zipfile = fname, files =paste0("干哈啊", names(rv$uploadResults)))
        },
        contentType = "application/zip"
    )

    
    
    output$developer <- renderText({
        " 青青子衿，悠悠我心。<br>
          纵我不往，子宁不嗣音？<br>
          青青子佩，悠悠我思。<br>
          纵我不往，子宁不来？<br>
          挑兮达兮，在城阙兮。<br>
          一日不见，如三月兮。
        "
    })
    
    # this block fires each time we receive a message from JavaScript
    output$text <- renderText({
        count <- 0
        paste("you clicked", input$count, "times on the RStudio ball",  input$gene_module)
        
    })
    
    # # this is another way to show message in header
    # observe({
    #     input$run
    #     mess <- "I am worried about you"
    # 
    #     print("I am worried about you")
    #     session$sendCustomMessage(type="header_message", HTML(rv$headerMessage))
    # })
    
}