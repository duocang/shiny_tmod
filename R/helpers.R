## a function which updates the log as well as the status message
## side effect: modify the log global variable
addMsg <- function(message, ...) {
    message <- sprintf( message, ... )
    output$message <- renderUI({ HTML(paste0( "💡 <b>Message:</b> ", message )) })
    time <- as.character(Sys.time())
    log <<- paste0( time, ": ", message, "\n", log )
    output$messageLog <- renderUI({ HTML(paste0("<pre>", log, "</pre>")) })
    catf( "MESSAGE: %s\n", message )
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

# uiReset <- function() {
#     fg <<- NULL
# 
#     updateSelectInput(session, "example", selected = "exempty")
#     output$choose_preview_file <- renderUI(NULL)
#     output$genename_col <- renderUI(NULL)
#     updateSelectInput(session, "gene_module", selected = "LI" )
#     updateSelectInput(session, "sort_by", selected = "logFC")
#     updateSelectInput(session, "inc_dec", selected = "FALSE")
#     updateSelectInput(session, "abs",  selected = "NO")
#     updateSelectInput(session, "test_type", selected = "tmodUtest")
#     output$table <- renderUI(NULL)
#     #updateNumericInput(session, "pie.pval", 0.05)
#     #updatenumericInput(session, "pie.lfc", 1)
#     addMsg("Page is resetted.")
# }