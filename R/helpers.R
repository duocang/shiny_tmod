 ## a function which updates the log as well as the status message
## side effect: modify the log global variable
addMsg <- function(message, ...) {
    message <- sprintf( message, ... )
    output$messageInHeader <- renderUI({ HTML(paste0( "💡 <b>Message:</b> ", message )) })
    rv$headerMessage <- message
    time <- as.character(Sys.time())
    log <<- paste0( time, ": ", message, "\n", log )
    output$messageLog <- renderUI({ HTML(paste0("<pre>", log, "</pre>")) })
    catf( "MESSAGE: %s\n", message )
    rv$headerMessage <- message
}

## a function which updates the log only
## side effect: modify the log global variable
addLog <- function(message, ...) {
    message <- sprintf( message, ... )
    time <- as.character(Sys.time())
    log <<- paste0( time, ": ", message, "\n", log )
    output$messageLog <- renderText({ paste0("<pre>", log, "</pre>") })
    catf( "MESSAGE: %s\n", message )
}

getMset <- reactive({
    mset <- input$gene_module
    foo <- substr(mset, 1, 4)
    if(foo == "msig"){
        cat <- gsub("msig", "", mset) # gsub() function reeplaces all matches of a string..
        mset <- filterModulesByCategory(msig, cat)
    }
    mset
})
