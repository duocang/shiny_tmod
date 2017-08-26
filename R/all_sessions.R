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


## get the actual mset as list
## if it is the original tmod, it returns a list with tmod objects
getMsetReal <- function(mset){
    if(is(mset, "list"))
        return(mset)
    ret <- tmod
    
    if(mset != "all") {
        ret$MODULES <- ret$MODULES[ ret$MODULES$SourceID == mset, ]
        ret$MODULES2GENES <- ret$MODULES2GENES[ ret$MODULES$ID ]
    }
    
    return(ret)
}

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
    print("到么大大发第三方")
    print(class(res))
    print(head(res))
    if(!is.null(res$AUC)) res$AUC <- round(res$AUC, digits=2)
    if(!is.null(res$E))   res$E <- round(res$E, digits=2)
    print("极乐空间了")
    # need this for MSigDB names
    res$Title <- gsub( "_", " ", res$Title)
    print(head(res))
    print("UI欧普偶偶")
    nn <- 1:nrow(res)
    # it includes two button in table
    print("没防守打法")
    buttons <- paste0(
        sprintf( '<input type="radio" name="row"   id="r%d" value="%d" /><label for="r%d">Plot</label>&nbsp;', nn, nn, nn),
        sprintf( '<input type="radio" name="glist" id="l%d" value="%d" /><label for="l%d">List</label>', nn, nn, nn))
    if(!is.null(res$URL)) {
        res$Title <- paste0( '<a href="', res$URL, '" target="_blank">', res$Title, '</a>' )
        res$URL <- NULL
    }
    print("则指的是")
    res <- cbind(Action=buttons, res)
    res
}
