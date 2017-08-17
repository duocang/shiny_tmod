popupWindow <- function(varname, contents) {
    # hooks for reactive elements to show / dismiss the window
    show    <- paste0( "show_", varname)
    dismiss <- paste0( "dismiss_", varname)
    
    
    print("打印个锤子")
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