popupWindow <- function(varname, contents) {
    # hooks for reactive elements to show / dismiss the window
    show    <- paste0( "show_", varname)
    dismiss <- paste0( "dismiss_", varname)
    
    print("popupWindow是否在运行")
    print(show)
    print(dismiss)
    
    conditionalPanel(
        condition=sprintf('input.%s == 1', show),# condition is a javascript expression that will be evaluated repeatedly
        
        # variable to keep track of showing the overlay
        div(style="display:none;", textInput(show, "", 0)),
        
        div(class="overlay", draggable="true",
            p(actionButton(dismiss, label="Dismiss [X]" )), contents)
    )

}

## -------------------------------------------------------------------
## creates the plot overlay
## depends on: input$row
## side effect: modifies the input value for showplotpanel
## -------------------------------------------------------------------
observe({
    print("打印input$row")
    print(input$row)
    print(input$glist)
    if(is.null(input$row) || input$row == 0)
        return(NULL)
    no <- as.numeric(isolate(input$row))
    print("打印no的值")
    print(no)
    mset <- isolate(getMset())
    print("打印mset")
    print(mset)
    print("打印fg")
    print(head(fg))
    print("打印rv$results$ID")
    print(head(rv$results$ID))
    # first, create ghe graphics
    output$pppp <- renderPlot({
        catf("making evidence plot with %d genes and ID=%s(%d)\n", length(fg), rv$results$ID[no], no)
        evidencePlot(fg, rv$results$ID[no], mset=mset)
    }, width=600, height=400)
    output$evidencePlot2 <- renderPlot({
        print("这一部分似乎没有工作")
        catf("这一部分似乎没有工作")
        evidencePlot(fg, rv$results$ID[no], mset=mset)
    }, width=600, height=400)
    # second, make it visible by changing variable showplotpanel
    # updateTextInput(seesion, "show_plotpanelW", value=1) # show_plotpanleW gets by var show
})
# 
# ## hide the plot panel if dismissPlot2 is clicked
# ## also, the value for the radio buttons is set to the hidden radio button
# observe({
#     if(input$dismiss_plotpanelW > 0){
#         updateTextInput(seesion, "show_plotpanelW", value=0)
#         updateNumericInput(session, "row", value=0)
#     }
# })

## -------------------------------------------------------------------
## Creates the gene list
## -------------------------------------------------------------------
# observe({
#     if(is.null(input$glist) || input$glist == 0 || is.null(fg)) { return(NULL) ; }
#     no   <- as.numeric(isolate(input$glist))
#     mset <- getMsetReal(isolate(getMset()))
#     
#     mod <- rv$results$ID[no]
#     catf("generating gene list for module %d\n", no)
#     glist <- sort(mset$MODULES2GENES[[mod]])
#     print(head(fg))
#     sel <- glist %in% fg
#     glist[sel] <- gsub( "(.*)", "<b>\\1</b>", glist[sel])
#     glist <- paste( glist, collapse=", ")
#     
#     output$genelist_title <- 
#         renderText({ sprintf("Genes in module %s, %s", mod, mset$MODULES[mod, "Title"]) })
#     
#     output$genelist <- renderUI({HTML(glist)})
#     updateTextInput(session, "show_genelistW", value=1)
# })

## hide the plot panel if dismissPlot2 is clicked
## also, the value for the radio buttons is set to the hidden radio button
# observe({
#     if(input$dismiss_genelistW > 0) {
#         updateTextInput(session, "show_genelistW", value=0)
#         updateNumericInput(session, "glist", value=0)
#     }
# })


## -------------------------------------------------------------------
## Creates a tag cloud 
## -------------------------------------------------------------------
# observe({
#     res <- isolate(rv$results)
#     if(is.null(input$tagcloud) || input$tagcloud == 0 
#        || is.null(res) || nrow(res) == 0) return(NULL) ;
#     
#     print("+ generating tagcloud")
#     w <- -log10(res$P.Value)
#     
#     if(!is.null(res$AUC)) {
#         v <- res$AUC
#     } else {
#         v <- res$E
#     }
#     c <- smoothPalette(v, min=0.5)
#     tags <- strmultline(gsub("_", " ", res$Title))
#     output$tagcloudPlot <- renderPlot({ tagcloud(tags, weights=w, col=c)}, width=600, height=600)
#     updateTextInput(session, "show_tagcloudW", value=1)
# })

## hide the popup window
## also, the value for the radio buttons is set to the hidden radio button
# observe({
#     if(input$dismiss_tagcloudW > 0) {
#         updateTextInput(session, "show_tagcloudW", value=0)
#     }
# })
