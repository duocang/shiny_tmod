## ===================================================================
## 
##                 Functions related to data loading
##
## ===================================================================



## -------------------------------------------------------------------
## this is hooked to the "choose file" dialogs
## loads the data to be analysed and stores it in fg / bg
## depends on: input$example (via load.example)
##             input$file1
##             input$file2
## side effect: populates fg and bg
##              changes the output message
## -------------------------------------------------------------------
load.data <- reactive({
    e <- load.example()
    catf("load.data here, e=%s\n", e)
    
    # load data from files only if no example is loaded
    if( ! e %in% c( "utest", "cerno", "hg" ) ) {
        fg <<- read.genes( input$file1, output=output )
        bg <<- read.genes( input$file2, output=output )
        msg <- c()
        if( !is.null(fg)) {
            foo <- get.annotations(fg, annotObject)
            msg <- c(msg, paste("set 1:", foo$msg))
            fg <- foo$ret.genes
        }
        
        if( Utest == "hg" && !is.null(bg)) {
            foo <- get.annotations(bg, annotObject)
            msg <- c(msg, paste("set 2:", foo$msg))
            bg <- foo$ret.genes
        }
        
        if(is.null(fg) && is.null(bg)) { msg <- "No data loaded" }
        print(msg)
    }
})

## -------------------------------------------------------------------
## hooked to the example data selection
## depends on: input$example
## side effect: loads the fg and bg for example data
## -------------------------------------------------------------------

load.example <- reactive({
    selection <- input$example
    catf("load.example selection=%s\n", selection)
    if(selection == "exempty"){
        print("No selection")
        return(selection)
    }
    if(selection == "hg") {
        fg <<- read.genes(filename="www/data/foreground.csv", output=output)
        bg <<- read.genes(filename="www/data/bg.csv", output=output)
        Utest <<- "hg"
        updateSelectInput( session, "testType", selected="hg" )
        addMsg( 'Loaded example for the hypergeometric test, click on the "▶ Run tmod" button' )
    }
    if(selection == "cerno") {
        fg <<- read.genes(filename="www/data/test.csv", output=output)
        bg <<- NULL
        Utest <<- "cerno"
        updateSelectInput( session, "testType", selected="cerno" )
        addMsg( 'Loaded example for the CERNO test, click on the "▶ Run tmod" button' )
    }
    if(selection == "utest") {
        fg <<- read.genes(filename="www/data/test.csv", output=output)
        bg <<- NULL
        Utest <<- "utest"
        updateSelectInput( session, "testType", selected="utest" )
        addMsg( 'Loaded example for the U-test, click on the "▶ Run tmod" button' )
    }
    if(selection == "reset") {
        uiReset()
    }
    catf("fg is null=%s\n", is.null(fg))
    
    return(selection)
})