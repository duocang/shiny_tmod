# it is defined in ui.R, it will show the table of result in 'test' tab.
output$testOrExampleResult <- renderUI({
    tabsetPanel(id = "inTabset",
                tabPanel("heatmap-like", value = "heatmapTab", plotOutput("plot0",height = "1000px")),
                tabPanel("rug-like",  value = "rugTab", plotOutput("plot01", height = "1000px")),
                tabPanel("table", value = "tableTab",
                         # these are required for button to be reactive
                         div(id="glist", class="shiny-input-radiogroup", 
                             div(id="row", class="shiny-input-radiogroup", 
                                 # hidden buttons with value 0 
                                 div(class="hidden",
                                     HTML('<input type="radio" name="row" value="0" id="r0" /><label for="r0">Plot</label>'),
                                     HTML('<input type="radio" name="glist" value="0" id="r0" /><label for="r0">Plot</label>')), 
                                 uiOutput("remindMessage"),
                                 fluidRow(
                                     column(3, uiOutput("selectionBoxOfResult")),
                                     column(1, uiOutput("cloudWordButton"))
                                 ),
                                 dataTableOutput( "resultTable"))),
                         # plot popup panel
                         popupWindow("plotpanelW", 
                                     div(plotOutput("evidencePlot2"))),
                         popupWindow("genelistW",  
                                     div(class="glist",
                                         p(tags$b(textOutput("genelist_title"))),
                                         p(HTML("Genes shown in <b>bold</b> are in the main data set")),
                                         p(uiOutput("genelist")))),
                         popupWindow("tagcloudW",
                                     div(plotOutput( "tagcloudPlot" ), style="width:1100px;height:1000px;" ))))
})

popupWindow <- function(varname, contents) {
    # hooks for reactive elements to show / dismiss the window
    show    <- paste0( "show_", varname)
    dismiss <- paste0( "dismiss_", varname)
    jqui_draggabled(# enable draggable feature
        conditionalPanel(
            condition=sprintf('input.%s == 1', show),
            # variable to keep track of showing the overlay
            div(style="display:none;", textInput(show, "", 0)),
            div(class="overlay", draggable="true",
                p(actionButton(dismiss, label="Dismiss [X]" )), 
                contents)))
}

## -------------------------------------------------------------------
## creates the plot overlay
## depends on: input$row
## side effect: modifies the input value for showplotpanel
## -------------------------------------------------------------------
observe({
    if (is.null(input$row) || input$row == 0)
        return(NULL)
    no <- as.numeric(isolate(input$row))
    mset <- isolate(getMset())
    #fg           <<- read.genes(filename="www/data/test.csv", output=output)
    # first, create ghe graphics
    if (!is.null(rv$uploadResults)){
        output$evidencePlot2 <- renderPlot({
            catf("making evidence plot with %d genes and ID=%s(%d)\n", length(fg), rv$uploadResults[[input$resultOfWhichFile]]$ID[no], no)
            return(evidencePlot(fg, rv$uploadResults[[input$resultOfWhichFile]]$ID[no], mset=mset))
        }, width=600, height=400)
    }
    # second, make it visible by changing variable showplotpanel
    updateTextInput(session, "show_plotpanelW", value=1) # show_plotpanleW gets by var show
})

## hide the plot panel if dismissPlot2 is clicked
## also, the value for the radio buttons is set to the hidden radio button
observe({
    # check whether input$dismiss_plotpanelW is NULL, if it is, do nothing
    req(input$dismiss_plotpanelW)
    if (input$dismiss_plotpanelW > 0) {
        updateTextInput(session, "show_plotpanelW", value=0)
        updateNumericInput(session, "row", value=0)
    }
})

## -------------------------------------------------------------------
## Creates the gene list
## -------------------------------------------------------------------
observe({
    if (is.null(input$glist) || input$glist == 0) 
        return(NULL) 
    no   <- as.numeric(input$glist)
    mset <- getMsetReal(isolate(getMset()))
    #fg           <<- read.genes(filename="www/data/test.csv", output=output)
    if (!is.null(rv$uploadResults))
        mod <- rv$uploadResults[[input$resultOfWhichFile]]$ID[no]
    catf("generating gene list for module %d\n", no)
    glist <- sort(mset$MODULES2GENES[[mod]])
    sel <- glist %in% fg
    glist[sel] <- gsub( "(.*)", "<b>\\1</b>", glist[sel])
    glist <- paste( glist, collapse=", ")
    output$genelist_title <- 
        renderText({ sprintf("Genes in module %s, %s", mod, mset$MODULES[mod, "Title"]) })
    output$genelist <- renderUI({HTML(glist)})
    updateTextInput(session, "show_genelistW", value=1)
})

## hide the plot panel if dismissPlot2 is clicked
## also, the value for the radio buttons is set to the hidden radio button
observe({
    # check whether input$dismiss_genelistW is NULL, if it is, do nothing
    req(input$dismiss_genelistW)
    if (input$dismiss_genelistW > 0) {
        updateTextInput(session, "show_genelistW", value=0)
        updateNumericInput(session, "glist", value=0)
    }
})

# create a tagcloud button if results are generated
# depends on: reactive value rv$uploadResults
output$cloudWordButton <- renderUI({
    req(input$resultOfWhichFile)
    if (!is.null(rv$uploadResults)){# upload files
        catf( "+ generating tagcloud button\n" )
        return(div(class="exportButton",
               actionButton( "tagcloud", label= "",icon = icon("cloud"), class="tmodAct" ),
               div(class = "exportButton-content",
                   p(sprintf("Click me, you will get cloud word of %s", input$resultOfWhichFile)))))
    }
})

## -------------------------------------------------------------------
## Creates a tag cloud 
## -------------------------------------------------------------------
observeEvent(input$tagcloud, {
    if (is.null(rv$uploadResults))
        tmodTest()
        
    res <- rv$uploadResults[[input$resultOfWhichFile]]
    req(res)
    print("+ generating tagcloud")
    w <- -log10(res$P.Value)
    if (!is.null(res$AUC)) 
        v <- res$AUC
    else
        v <- res$E
    c <- smoothPalette(v, min=0.5) # Replace A Vector Of Numbers By A Gradient Of Colors
    tags <- strmultline(gsub("_", " ", res$Title))
    output$tagcloudPlot <- renderPlot({ tagcloud(tags, weights=w, col=c)}, width=1100, height=1000)
    output$plotWordCloud <- renderPlot({ tagcloud(tags, weights=w, col=c)},  height=1000)
    updateTextInput(session, "show_tagcloudW", value=1)
})



## hide the popup window
## also, the value for the radio buttons is set to the hidden radio button
observe({
    req(input$dismiss_tagcloudW)
    if (input$dismiss_tagcloudW > 0)
        updateTextInput(session, "show_tagcloudW", value=0)
})

## -------------------------------------------------------------------
# uiOut("operation") is defined in ui.R
# the "run" button will generated according to which tab is selected, 
# different button will be used to active different task in different tab.
## -------------------------------------------------------------------
output$operation <- renderUI({
    if(!is.null(input$files) || input$example != "exempty"){
        # without following if statement, there is always red error message,
        # when you switch into "test" tab by left side bar click.
        if (is.null(input$inTabset))
            return(actionButton("runHeatmap", "RUN", class="tmodAct"))
        
        if (input$inTabset == "heatmapTab")
            return(actionButton("runHeatmap", "RUN", class="tmodAct"))
        if (input$inTabset == "rugTab")
            return(actionButton("runRug", "RUN", class="tmodAct"))
        if (input$inTabset == "tableTab")
            return(actionButton("runTable", "RUN", class="tmodAct"))
    }
})

# show or hide pie.lfc and pie.pval selection boxes
# args:
#   input$inTabset
# effects:
#   show/hide siderbar
observeEvent(input$inTabset,{
    if (input$inTabset == "rugTab"){
        toggle("pie.lfc")
        toggle("pie.pval")
    }
    else {
        toggle("pie.lfc")
        toggle("pie.pval")
    }
})


## -------------------------------------------------------------------
# Show the heatmap plot
# Args:
#    input$whichColumnIsGenename:
#    tmodTest()
# Returns:
#    heatmap plot
## -------------------------------------------------------------------
observeEvent(input$runHeatmap,{
    req(isolate(input$whichColumnIsGenename))
    geneCol <- isolate(input$whichColumnIsGenename)
    if (geneCol == "-----------------"){
        addMsg("Please select gene name column!")
        shinyjs::addClass(selector = "body", class = "sidebar-collapse")  # show sidebar
        session$sendCustomMessage(type = "alert_message", message = "Please select gene name column!")  # alert message via JS
    }else{
        output$plot0 <- renderPlot({
            # following code chunk is a progress bar
            withProgress(message = 'Making plot', value = 0, {
                n <- 5
                # Number of times we'll go through the loop
                for (i in 1:n) {
                # Each time through the loop, add another row of data. This is
                # a stand-in for a long-running computation.
                # Increment the progress bar, and update the detail text.
                    incProgress(1/n, detail = paste("", ""))
                # Pause for 0.1 seconds to simulate a long computation.
                    Sys.sleep(0.1)
                }
                tryCatch({
                    plo <- NULL
                    plo <- isolate(tmodTest())
                },warning = function(w){
                    addMsg("Wrong gene column selected!")
                    isolate(shinyjs::removeClass(selector = "body", class = "sidebar-collapse"))
                    session$sendCustomMessage(type = "alert_message", message = "Wrong gene column selected! Please select gene cloumn, again!")
                },error = function(w){
                })
                if(is.null(plo))
                    return(NULL)
                tmodPanelPlot(plo, text.cex = 0.9, grid="b",   legend.style = "auto")
            })
        }, bg="#EEEEEE")
    }
})

## -------------------------------------------------------------------
# Show the rug plot
# Args: 
#   input$whichColumnIsGenename: 
#   tmodTest():
# Returns:
#   rug plot
## -------------------------------------------------------------------
observeEvent(input$runRug,{
    req(isolate(input$whichColumnIsGenename))
    geneCol <- isolate(input$whichColumnIsGenename)
    if (geneCol == "-----------------"){
        addMsg("Please select gene name column!")
        shinyjs::addClass(selector = "body", class = "sidebar-collapse")
        session$sendCustomMessage(type = "alert_message", message = "Please select gene name column!")
    }else{
        output$plot01 <- renderPlot({
            # no file or no example, no running
            if (!is.null(isolate(input$files)) || isolate(input$example) != "exempty"){
                # progress bar
                withProgress(message = 'Making plot', value = 0, {
                    n <- 10
                    for (i in 1:n) {
                        incProgress(1/n, detail = paste("", ""))
                        Sys.sleep(0.1)
                    }
                    tryCatch({
                        res <- NULL
                        res <- isolate(tmodTest())
                    },warning = function(w){
                        addMsg("Wrong gene column selected!")
                        isolate(shinyjs::removeClass(selector = "body", class = "sidebar-collapse"))
                        session$sendCustomMessage(type = "alert_message", message = "Wrong gene column selected! Please select gene cloumn, again!")
                    },error = function(w){
                    })
                    
                    sapply(res, function(x){
                        if (nrow(x) == 0){
                            addMsg(sprintf("There is no moudle named %s!", isolate(input$gene_module)))
                            return(NULL)
                        }
                    })
                    pie <- isolate(tmodTest1())
                    names(pie) <- names(res)
                    if (!is.null(res))
                        tmodPanelPlot(res, pie=pie, pie.style="r", grid="b", filter.rows.pval=0.001)
                })
            }
        }, bg = "#EEEEEE")
    }
})

## -------------------------------------------------------------------
## Show the result table
## -------------------------------------------------------------------
observeEvent(input$runTable,{
    output$resultTable <- renderDataTable({
        req(isolate(input$whichColumnIsGenename))
        req(input$resultOfWhichFile)
        withProgress(message = 'Not ready for RUN', value = 0, {
            n <- 10
            for (i in 1:n) {
                incProgress(1/n, detail = paste("", ""))
                Sys.sleep(0.1)
            }
            tryCatch({
                tmodTest()
            },warning = function(w){
                addMsg("Wrong gene column selected!")
                isolate(shinyjs::removeClass(selector = "body", class = "sidebar-collapse"))
                session$sendCustomMessage(type = "alert_message", message = "Wrong gene column selected! Please select gene cloumn, again!")
            },error = function(w){
            })
        })
        res <- formatResultsTable(isolate(rv$uploadResults[[input$resultOfWhichFile]]))
        return(datatable(res, escape = FALSE))
    })
})

## -------------------------------------------------------------------
# under heatmap-like tab
# create a selection box, which is used to display different result of corresponding file uploaded
## -------------------------------------------------------------------
observeEvent(input$runTable,
             output$selectionBoxOfResult <- renderUI({
                 if (is.null(rv$uploadResults))
                     tmodTest()
                 if (!is.null(input$files))
                     choicess <- input$files$name
                 if (input$example != "exempty")
                     choicess <- exampleFileNameList
                 return(selectInput("resultOfWhichFile", label = NULL, choices =  choicess))
             }))

## -------------------------------------------------------------------
## create an export button if results are generated
## depends on: reactive value rv$uploadResults
## -------------------------------------------------------------------
output$uploadExportButton <- renderUI({
    req(rv$uploadResults)
    catf( "+ generating uploaded files export button\n" )
    return(div(class="exportButton",
               tags$a(id = "uploadExport", 
                      class = "btn shiny-download-link headerButton", 
                      href="", 
                      target = "_blank",
                      icon("download"), ""),
               div(class = "exportButton-content",
                   p("Hit me, you can download results of all files."))))
})


## -------------------------------------------------------------------
# disable button/selection
# when example is useed, disable some selection boxes
## -------------------------------------------------------------------
observeEvent(input$example,{
    if (input$example != "exempty"){
        disable("testType")
        disable("files")
        addMsg("Example is ready for running!   <b>Go to Test tab</b>")
        return()
    }
})

## -------------------------------------------------------------------
# disable button/selection
# when files are uploaded, disable some selection boxes
## -------------------------------------------------------------------
observeEvent(input$files,{
    if (!is.null(input$files)){
        disable("example")
        addMsg(" Uploaded file(s) will be used for running!   <b>Go to Test tab</b>")
    }
})

## -------------------------------------------------------------------
# sidebar will collapse
## -------------------------------------------------------------------
observe({
    input$runHeatmap
    input$runRug
    input$runTable
    # when there is no file uploaded or example, not effect
    req(isolate(input$whichColumnIsGenename))  # when no file uploaded, gene name selection box does not exit
    # if (input$whichColumnIsGenename != "-----------------" && (!is.null(input$files) || input$example == "exempty" ))
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
})


