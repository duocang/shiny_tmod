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


