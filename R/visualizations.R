# it is defined in ui.R, it will show the table of result in 'test' tab.
output$testOrExampleResult <- renderUI({
    tabsetPanel(id = "inTabset",
                tabPanel("table", 
                         # these are required for button to be reactive
                         div(id="glist", class="shiny-input-radiogroup", 
                             div(id="row", class="shiny-input-radiogroup", 
                                 # hidden buttons with value 0 
                                 div(class="hidden",
                                     HTML('<input type="radio" name="row" value="0" id="r0" /><label for="r0">Plot</label>'),
                                     HTML('<input type="radio" name="glist" value="0" id="r0" /><label for="r0">Plot</label>')), 
                                 uiOutput("remindMessage"),
                                 uiOutput("resultOfEachFile"),
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
                                     div(plotOutput( "tagcloudPlot" ), style="width:600px;height:600px;" ))),
                tabPanel("heatmap-like", plotOutput("plot0", height = "1500px")),
                tabPanel("rug-like", plotOutput("plot01", height = "1500px")))
})

popupWindow <- function(varname, contents) {
    # hooks for reactive elements to show / dismiss the window
    show    <- paste0( "show_", varname)
    dismiss <- paste0( "dismiss_", varname)
    jqui_draggabled(# enable draggable feature
        conditionalPanel(
            condition=sprintf('input.%s == 1', show),# condition is a javascript expression that will be evaluated repeatedly
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
    if(is.null(input$row) || input$row == 0)
        return(NULL)
    no <- as.numeric(isolate(input$row))
    mset <- isolate(getMset())
    # first, create ghe graphics
    if(!is.null(rv$uploadResults)){
        # fg <<- read.genes(filename="www/data/test.csv", output=output)
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
    if(input$dismiss_plotpanelW > 0) {
        updateTextInput(session, "show_plotpanelW", value=0)
        updateNumericInput(session, "row", value=0)
    }
})

## -------------------------------------------------------------------
## Creates the gene list
## -------------------------------------------------------------------
observe({
    req(input$files)
    fg <<- read.genes(filename="www/data/test.csv", output=output)
    if(is.null(input$glist) || input$glist == 0 || is.null(fg)) { return(NULL) ; }
    no   <- as.numeric(input$glist)
    mset <- getMsetReal(isolate(getMset()))
    if(!is.null(rv$uploadResults))
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
    if(input$dismiss_genelistW > 0) {
        updateTextInput(session, "show_genelistW", value=0)
        updateNumericInput(session, "glist", value=0)
    }
})

# create a tagcloud button if results are generated
# depends on: reactive value rv$uploadResults
output$cloudWordButton <- renderUI({
    if(!is.null(rv$uploadResults)){# upload files
        catf( "+ generating tagcloud button\n" )
        return(actionButton( "tagcloud", label= "",icon = icon("cloud"), class="headerButton" ))
    }
})

## -------------------------------------------------------------------
## Creates a tag cloud 
## -------------------------------------------------------------------
observeEvent(input$tagcloud, {
    res <- isolate(rv$uploadResults[[input$resultOfWhichFile]])
    req(res)
    print("+ generating tagcloud")
    w <- -log10(res$P.Value)
    if(!is.null(res$AUC)) 
        v <- res$AUC
    else
        v <- res$E
    c <- smoothPalette(v, min=0.5) # Replace A Vector Of Numbers By A Gradient Of Colors
    tags <- strmultline(gsub("_", " ", res$Title))
    output$tagcloudPlot <- renderPlot({ tagcloud(tags, weights=w, col=c)}, width=600, height=600)
    updateTextInput(session, "show_tagcloudW", value=1)
})

## hide the popup window
## also, the value for the radio buttons is set to the hidden radio button
observe({
    req(input$dismiss_tagcloudW)
    if(input$dismiss_tagcloudW > 0)
        updateTextInput(session, "show_tagcloudW", value=0)
})

## -------------------------------------------------------------------
## display data of result in table
## -------------------------------------------------------------------
output$resultTable <- renderDataTable({
    input$run
    if(!is.null(input$files)){
        req(input$resultOfWhichFile)
        res <- formatResultsTable(rv$uploadResults[[input$resultOfWhichFile]])
        req(res)
        return(datatable(res, escape = FALSE))
    }
})

## -------------------------------------------------------------------
## Show the heatmap plot
## -------------------------------------------------------------------
output$plot0 <- renderPlot({
    req(input$run)
    if(!is.null(isolate(input$files))){
        plo <- NULL
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
                plo <- isolate(tmodTest())
            },warning=function(w){
                if(input$whichColumnIsGenename != "-----------------"){
                    addMsg("Wrong gene column selected!")
                    updateSelectInput(session, "whichColumnIsGenename", selected = "-----------------")
                    session$sendCustomMessage(type = "alert_message",
                                              message = "Wrong gene column selected! Please select gene cloumn, again猪八戒!")
                }
            },error=function(e){
                if(input$whichColumnIsGenename != "-----------------"){
                    addMsg("Wrong gene column selected!")
                    updateSelectInput(session, "whichColumnIsGenename", selected = "-----------------")
                    session$sendCustomMessage(type = "alert_message",
                                              message = "Wrong gene column selected! Please select gene cloumn, again!")
                }
            })
            req(plo)
            tmodPanelPlot(plo, text.cex = 0.9, legend.style = "auto")
        })
    }
}, bg="transparent")

## -------------------------------------------------------------------
## Show the rug plot
## -------------------------------------------------------------------
output$plot01 <- renderPlot({
    input$run
    if(!is.null(isolate(input$files)))
    {
        withProgress(message = 'Making plot', value = 0, {
            n <- 10
            # Number of times we'll go through the loop
            for (i in 1:n) {
                # Each time through the loop, add another row of data. This is
                # a stand-in for a long-running computation.
                # Increment the progress bar, and update the detail text.
                incProgress(1/n, detail = paste("", ""))
                # Pause for 0.1 seconds to simulate a long computation.
                Sys.sleep(0.1)
            }
            res <- isolate(rv$uploadResults)
            #res <- isolate(tmodTest())
            sapply(res, function(x){
                if(nrow(x) == 0){
                    addMsg(sprintf("There is no moudle named %s!", isolate(input$gene_module)))
                    return(NULL)
                }
            })
            pie <- isolate(tmodTest1())
            names(pie) <- names(res)
            if(!is.null(res))
                tmodPanelPlot(res, pie=pie, pie.style="r", grid="b", filter.rows.pval=0.001)
        })
    }
}, bg="transparent")

## -------------------------------------------------------------------
## export button on top right
## create an export button if results are generated
## depends on: reactive value rv$uploadResults
## -------------------------------------------------------------------

output$uploadExportButton <- renderUI({
    req(rv$uploadResults)
    catf( "+ generating uploaded files export button\n" )
    return(tags$a(id = "uploadExport", class = "btn shiny-download-link headerButton1", href="", target = "_blank",icon("download"), ""))
})

# create a selection box, which is used to display different result of corresponding file uploaded
output$resultOfEachFile <- renderUI({
    req(rv$uploadResults)
    req(input$files)
    if(is.null(rv$uploadResults))
        selectInput("resultOfWhichFile", NULL, NULL)
    else{
        selectInput("resultOfWhichFile", label = NULL, choices = input$files$name)
    }
})