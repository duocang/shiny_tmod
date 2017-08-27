## ===================================================================
## 
##                 Functions related to data loading
##
## ===================================================================

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
        Utest <<- "cerno"
        updateSelectInput( session, "test_type", selected="tmodCERNOtest" )
        addMsg( 'Loaded example for the CERNO test.' )
    }
    if(selection == "utest") {
        fg <<- read.genes(filename="www/data/test.csv", output=output)
        Utest <<- "utest"
        updateSelectInput( session, "test_type", selected="tmodUtest" )
        addMsg( 'Loaded example for the U-test.' )
    }
    return(selection)
})