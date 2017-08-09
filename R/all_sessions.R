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
module_filter <- function(res, module_name){
    if(module_name == "all")
        return(res)
    else{
        res <- sapply(res, function(x){
            subset(x, startsWith(x$ID, module_name))
        }, simplify = FALSE)
    }
}


