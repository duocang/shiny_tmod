popupWindow <- function(varname, contents) {
    # hooks for reactive elements to show / dismiss the window
    show    <- paste0( "show_", varname)
    dismiss <- paste0( "dismiss_", varname)
    
    conditionalPanel(
        condition=sprintf('input.%s == 1', show),# condition is a javascript expression that will be evaluated repeatedly
        # variable to keep track of showing the overlay
        div(style="display:none;", textInput(show, "", 0)),
        
        div(class="overlay", draggable="true",
            p(actionButton(dismiss, label="Dismiss [X]" )), 
            contents)
    )
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
    output$evidencePlot2 <- renderPlot({
        catf("making evidence plot with %d genes and ID=%s(%d)\n", length(fg), rv$results$ID[no], no)
        evidencePlot(fg, rv$results$ID[no], mset=mset)
    }, width=600, height=400)
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
    if(is.null(input$glist) || input$glist == 0 || is.null(fg)) { return(NULL) ; }
    no   <- as.numeric(isolate(input$glist))
    mset <- getMsetReal(isolate(getMset()))
    print(mset)
    
    mod <- rv$results$ID[no]
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



output$plot0 <- renderPlot({
    req(input$run)
    
    plo <- NULL
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
        tryCatch({
            plo <- isolate(stat_test())
        },warning=function(w){
            addMsg("Wrong gene column selected!")
            updateSelectInput(session, "which_col_genename", selected = "-----------------")
            session$sendCustomMessage(type = "alert_message",
                                      message = "Wrong gene column selected! Please select gene cloumn, again!")
            
        },error=function(e){
            addMsg("Wrong gene column selected!")
            updateSelectInput(session, "which_col_genename", selected = "-----------------")
            session$sendCustomMessage(type = "alert_message",
                                      message = "Wrong gene column selected! Please select gene cloumn, again!")
        }
        )
        
        
        if(!is.null(plo))
            tmodPanelPlot(plo, text.cex = 0.9, legend.style = "auto")
        else
            return(NULL)
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
}, bg="transparent")

output$plot01 <- renderPlot({
    input$run1
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
        res <- isolate(stat_test())
        print("fasdfsadfasdf发多少发松岛枫")
        print(res)
        sapply(res, function(x){
            if(nrow(x) == 0){
                addMsg(sprintf("There is no moudle named %s!", isolate(input$gene_module)))
                return(NULL)
            }
        })
        
        pie <- isolate(stat_test1())
        
        names(pie) <- names(res)
        if(!is.null(res))
            tmodPanelPlot(res, pie=pie, pie.style="r", grid="b", filter.rows.pval=0.001)
    })
}, bg="transparent")

output$plot3 <- renderPlot({
    input$run
    req(isolate(input$example))
    plo <- list(isolate(rv$results))
    names(plo) <- "Example Data"
    tmodPanelPlot(plo, text.cex = 0.9, legend.style = "auto")
}, bg="transparent")

output$plot03 <- renderPlot({
    input$run1
    req(isolate(input$example))
    print(head(fg))
    pie <- tmodDecideTests(g=fg, mset = isolate(getMset()))
    print(head(pie))
    
    plot(1:1000, 1:1000)
}, bg="transparent")