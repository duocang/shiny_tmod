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

