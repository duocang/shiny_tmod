## ======================================================================
## Functions which are independent of session-specific variables
## Mostly plain old style R functions, nothing shiny
## ======================================================================


catf   <- function( ... )   cat( sprintf( ... ) )
printf <- function( ... ) print( sprintf( ... ) )

## nicely format the fold changes
format.logfc <- function( val ) {
    out <- sprintf( "%.3f", val )
    sel <- abs( val ) < 0.5
    out[ sel ] <- sprintf( '<span style="color:grey;">%.2f</span>', val[ sel ] )
    sel <- abs( val ) > 2.0
    out[ sel ] <- sprintf( '<b>%s<b>', out[ sel ] )
    sel <- val > 0.5 
    out[ sel ] <- sprintf( '<span style="color:red;">%s</span>', out[ sel ] )
    sel <- val < 0.5 
    out[ sel ] <- sprintf( '<span style="color:blue;">%s</span>', out[ sel ] )
    out
}

## nicely format the q values
format.qval <- function( val ) {
    out <- sprintf( "%.3f", val )
    sel <- val > 0.05
    out[ sel ] <- sprintf( '<span style="color:grey;">%.2f</span>', val[ sel ] )
    sel <- val < 0.001
    out[ sel ] <- sprintf( '<b>%.3e<b>', val[ sel ] )
    out
}

## format the table for attractive display
format.table <- function( df ) {
    csel <- grep( "qval", colnames( df ) )
    df[ , csel ] <- apply( df[ , csel ], 2, format.qval )
    csel <- grep( "logFC", colnames( df ) )
    df[ , csel ] <- apply( df[ , csel ], 2, format.logfc )
    df
}

## 
formatTablePval <- function(df, cols) {
    for(i in 1:50) {
        tmp <- df[,cols,drop=FALSE]
        sel <- tmp > 10^-i
        tmp[sel] <- round(tmp[sel], digits=i+2)
        df[,cols] <- tmp
    }
    
    df
}


## selects a category from a module set
filterModulesByCategory <- function(mset, cat){
    mset$MODULES <- mset$MODULES[mset$MODULES$Category == cat, , drop=FALSE]
    mset$MODULES2GENES <- mset$MODULES2GENES[ mset$MODULES$ID ]
    mset$GENES <- mset$GENES[ mset$GENES$ID %in% unique(unlist(mset$MODULES2GENES)),, drop=FALSE ]
    return(mset)
}

# stat_run <- function(data_f, test_type_f, ...){
#     if(is.null(data_f) || length(data_f) == 0){
#         print("no data yet")
#         return(NULL)
#     }
#     sort_col <- isolate(input$sort_by)
#     sort_abs <- isolate(input$abs)
#     sort_decr <- isolate(input$inc_dec)
#     geneNmae <- isolate(input$which_col_genename)
#     
#     res <- sapply(data_f, function(x){
#         x <- data.frame(x)
#         genes <- x[, geneName]
#         ord <- x[, sort_cold]
#         if(sort_abs == "YES") ord <- abs(ord)
#         ord <- order(ord, decreasing = sort_decr)
#         if(test_type_f == "tmodCERNOtest")
#             tmodeCERNOtest(genes[ord], mset=mset, qval=1)
#         else
#             tmodUtest(genes[ord], mset = mset, qval=1)
#     })
#     
# }

# according to user's selection, specific module will be used.
module_filter <- function(){
    if(input$mset == ""){
        
    }
}
# module_filter <- function(res, module_name){
#     if(module_name == "all")
#         return(res)
#     else{
#         res <- sapply(res, function(x){
#             subset(x, startsWith(x$ID, module_name))
#         }, simplify = FALSE)
#     }
# }


## read genes from a file object returned by input[[]]
## or from filename
read.genes <- function( filename=NULL, output=NULL){
    if(is.null(filename)) return(NULL)
    
    catf("Reading genes from %s...", filename)
    
    l <- read.csv(filename, stringsAsFactors = FALSE, header=FALSE)[,1]
    catf("%d genes read\n", length(l))
    
    if(!is.null(output)){
        output$message <- renderText(sprintf("Read %d genesd", length(l)))
    }
    return(l)
}

## calls tmod and runs the apropriate statistical test
run.stats <- function(fg, Utest="utest", ...){
    if(Utest == "cerno"){
        catf("running tmodCERNOtest\n")
        res <- tmodCERNOtest(fg, ...)
    }else{
        catf("running tmodUtest\n")
        res <- tmodUtest(fg, ...)
    }
    res
}

## Prepare the result table from tmod for output in shiny
## Add action buttons, format URLs, remove unnecessary columns 
formatResultsTable <- function(res) {
    
    if(is.null(res)) return(NULL)
    if(nrow(res) == 0) return(NULL)
    res$cerno <- NULL
    res$cES <- NULL
    res <- formatTablePval(res, c( "P.Value", "adj.P.Val") )
    if(!is.null(res$AUC)) res$AUC <- round(res$AUC, digits=2)
    if(!is.null(res$E))   res$E <- round(res$E, digits=2)
    
    # need this for MSigDB names
    res$Title <- gsub( "_", " ", res$Title)
    
    nn <- 1:nrow(res)
    buttons <- paste0(
        sprintf( '<input type="radio" name="row"   id="r%d" value="%d" /><label for="r%d">Plot</label>&nbsp;', nn, nn, nn),
        sprintf( '<input type="radio" name="glist" id="l%d" value="%d" /><label for="l%d">List</label>', nn, nn, nn))
    
    if(!is.null(res$URL)) {
        res$Title <- paste0( '<a href="', res$URL, '" target="_blank">', res$Title, '</a>' )
        res$URL <- NULL
    }
    
    res <- cbind(Action=buttons, res)
    res
}