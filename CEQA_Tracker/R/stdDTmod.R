myDTmodUI <- function(id) {
  ns <- NS(id)
  tagList(
    DTOutput(ns('tbl')) 
  )
}

myDTmodServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      # use the session name space as a prefix for the table
      ns <- session$ns
      output$tbl <- renderDT(
        datatable(data,
                  callback = JS("$.fn.dataTable.ext.errMode = 'none';"),
                  rownames = FALSE,
                  escape = FALSE,
                  options = list(dom = 'Btp',
                                 buttons = list( 
                                   list(extend = 'csv', filename = paste(ns('table'), sep='-')))#,
                         #          list(extend = 'excel', filename =  paste(ns('table'), sep = "-")))
                  ),
                  extensions = c('Buttons'))
      )
    }
  )
}