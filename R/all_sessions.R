## ======================================================================
## Functions which are independent of session-specific variables
## Mostly plain old style R functions, nothing shiny
## ======================================================================


catf   <- function( ... )   cat( sprintf( ... ) )
printf <- function( ... ) print( sprintf( ... ) )

## selects a category from a module set
filterModulesByCategory <- function(mset, cat){
    mset$MODULES <- mset$MODULES[mset$MODULES$Category == cat, , drop=FALSE]
    mset$MODULES2GENES <- mset$MODULES2GENES[ mset$MODULES$ID ]
    mset$GENES <- mset$GENES[ mset$GENES$ID %in% unique(unlist(mset$MODULES2GENES)),, drop=FALSE ]
    return(mset)
}
