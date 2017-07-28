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


# below allows file bigger than 25M to be uploaded
options(shiny.maxRequestSize=30*1024^2)

function(input, output, session) {
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
                    choices = c("", common_columns()))
    })
    
 
    # this will show the table of file in page
    output$table = DT::renderDataTable( preview_8_lines(),
                                        options=list(scrollX=TRUE))

    
 
    # this function will show first 8 rows of selected file to preview
    preview_8_lines <- reactive({
        n <- 1
        file_typein <- input$which_preview_file# which file selected for preview
        infile <- input$files
        if(is.null(infile)) return(NULL)# user has not uploaded a file yet
        for(i in 1:file_num())
            if(infile$name[i] == file_typein)
                n <- i
        a <- loaded_data()[[n]]
        head(a, n=8)
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
        input$run# track "plot it" button, onece clicked, run this code bolck
        dat <- isolate(loaded_data())
        if(is.null(dat) || length(dat)==0) {
          print("no data yet")
          return(NULL)
        }
        print("running test")
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
        #if(is.null(names(res))) names(res) <- paste0("N.", 1:length(res))
        if(is.null(names(res))) names(res) <- input$files$name
        return(res)
    })
    
    stat_test1 <- reactive({
        dat <- isolate(loaded_data())

        if(is.null(dat) || length(dat)==0) {
            print("no data yet")
            return(NULL)
        }
        print("running test1")
        
        print("dat[[1]]的数据类型")
        print(class(dat[[1]]))
        print(dim(dat[[1]]))
        
        pie_pval <- 0.05
        pie_lfc <- 1
        
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
                               lfc.thr=1,
                               pval.thr=0.05,
                               mset=mset
        )
        print("333")
        return(pie)
    })
    
    output$plot <- renderPlot({
        input$run
        #input$which_col_genename
        tryCatch({
            print("making the plot")
            plo <- stat_test()
            if(!is.null(plo))
                tmodPanelPlot(plo, text.cex = 0.9, legend.style = "auto")
        },
        warning = function(w){
            print("no correct gene column selected")
        },
        error = function(e){
            print("gene column is selected, but it is processed by isolated() function")
            return(NULL)
        })
    }, bg="transparent")
    
    output$plot1 <- renderPlot({
        input$run1
        
        print("making the plot1")
        res <- isolate(stat_test())
        pie <- isolate(stat_test1())
        names(pie) <- names(res)
        print(names(res))
        print("方程运行到此处1")
        
        if(!is.null(res))
            tmodPanelPlot(res, pie=pie, pie.style="r", grid="b", filter.rows.pval=0.001)
        print("方程运行到此处2")
    }, bg="transparent")
    
    
    
    # 以下的内容将会实现january此前tmod enrichment tool 中的功能
    # global variables holding the state of the statistical tests
    fg           <- NULL
    bg           <- NULL
    Utest        <- "hg"
    example      <- FALSE
    log          <- ""
    
    # reactive values
    rv <- reactiveValues()
    rv$results <- NULL
    
    # load the code
    source("R/helpers.R", local=TRUE)
    source("R/data_loading.R", local=TRUE)
    source("R/visualizations.R", local=TRUE)
    
    si <- sessionInfo()
    addLog("Running tmod in version %s", si$otherPkgs$tmod$Version)
    
    # output$plot1 <- renderText({
    #     pval.col <- "qval"
    #     
    #     ## 13. which column contains the log fold changes
    #     lfc.col <- "logFC"
    #     input$run1
    #     dat <- isolate(loaded_data())
    #     if(is.null(dat) || length(dat)==0) {
    #         print("no data yet")
    #         return(NULL)
    #     }
    #     print("run1也开始运行了")
    #     lfcs <- sapply(dat, function(x) {
    #         x <- data.frame(x)
    #         x[, "logFC"]
    #         },simplify=FALSE
    #     )
    #     pvals <- sapply(dat, function(x){
    #         x <- data.frame(x)
    #         x[, "qval"]
    #         }
    #         ,simplify=FALSE
    #         )
    #     
    #     geneName <- isolate(input$which_col_genename)
    #     pie.pval <- 0.05
    #     pie.lfc <- 1
    # 
    #     pie <- tmodDecideTests(g=dat[[1]][ , geneName ],
    #                            lfc=lfcs,
    #                            pval=pvals,
    #                            lfc.thr=pie.lfc,
    #                            pval.thr=pie.pval,
    #                            mset=mset
    #     )
    # 
    #     tmodPanelPlot(res, pie=pie, pie.style="r", grid="b", filter.rows.pval=0.001)
    #     
    # })

    
    # # 实现参数弹窗
    # dropdown <- function(){
    #     sort_col <- isolate(input$sort_by)# isolate() is used, we donot want to rerun this code block every time, 
    #     sort_abs <- isolate(input$abs)    # when we change sorting column or othre choices
    #     sort_decr <- isolate(input$inc_dec)
    #     geneName <- isolate(input$which_col_genename)
    #     isolate(input$test_type)
    #     isolate(input$drowdownTest)
    #     isolate(input$files)
    #     isolate(input$run1)
    #     modalDialog(
    #         title = "Notice",
    #         #uiOutput("choose_test_file"),
    #         selectInput("sort_by", "which column to use for sorting genes?",
    #                     choices = c("logFC" = "logFC",
    #                                 "t" = "t",
    #                                 "msd" = "msd",
    #                                 "SE" = "SE",
    #                                 "d" = "d",
    #                                 "qval" = "qval"),
    #                     selected = "qval"
    #         ),
    #         selectInput("inc_dec", "Trend",
    #                     choices = c("Increasing" = "False",
    #                                 "Decreasing" = "TRUE"),
    #                     selected = "Decreasing"
    #         ),
    #         selectInput("abs", "Abs",
    #                     choices = c("YES", "NO"),
    #                     selected = "YES"),
    #         selectInput("test_type", "test",
    #                     choices = c("NO SELECT",
    #                                 "tmodCERNOtest" = "tmodCERNOtest",
    #                                 "tmodUtest" = "tmodUtest")
    #         ),
    #         actionButton("run", "Polt it"),
    #         easyClose = TRUE, footer = NULL
    #     )
    # }
    
    # # 显示参数弹窗
    # observeEvent(no_gene_col_selected, {
    #     showModal(dropdown())
    # })
    # # remove modal after clicking "plot it"
    # observeEvent(input$run, {
    #     removeModal()
    # })
    # 
    # # when you click "plot it", it will hide preview talbe
    # observeEvent(input$run, {
    #     hide("table")
    # })
    # # when you select file to preview, it will update and show file content in web page
    # observeEvent(input$which_preview_file,{
    #     show("table")
    # })
    # 
    # observeEvent(input$download,{
    #     hide("table")
    # })
}

# 松 Print version information about R, the OS and attached or loaded packages.
