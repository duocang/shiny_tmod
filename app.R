shinyApp(
    ui = fluidPage(
        title = 'Radio buttons in a table',
        tags$div(id="C",class='shiny-input-radiogroup',DT::dataTableOutput('foo')),
        verbatimTextOutput("test")
    ),
    server = function(input, output, session) {
        m = matrix(
            c(round(rnorm(24),1), rep(3,12)), nrow = 12, ncol = 3, byrow = F,
            dimnames = list(month.abb, LETTERS[1:3])
        )
        for (i in seq_len(nrow(m))) {
            m[i, 3] = sprintf(
                ifelse(i == 1,
                        '<input type="radio" name="%s" value="%s" checked="checked"/>',
                        '<input type="radio" name="%s" value="%s"/>'),
                "C", month.abb[i]
            )
        }
        m
        output$foo = DT::renderDataTable(
            m, escape = FALSE, selection = 'none', server = FALSE,
            options = list(dom = 't', paging = FALSE, ordering = FALSE)
        )
        output$test <- renderPrint(str(input$C))
    }
)