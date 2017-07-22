## a function which updates the log only
## side effect: modify the log global variable
addLog <- function(message, ...) {
    message <- sprintf( message, ... )
    time <- as.character(Sys.time())
    print(class(time))
    m <- paste0(time,"    ", message)
    # log <- paste0( str(Sys.time()), ": ", message, "\n", log )
    output$messageLog <- renderText({
        paste0("<pre>", m, "</pre>") 
        
    })
    # catf( "MESSAGE: %s\n", message )
}