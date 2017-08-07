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