boxplotServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      data <- c()
      ns <- NS(id)
      observeEvent(
        eventExpr = input$continue,
        handlerExpr = {
          showModal(modalDialog("Processing Data...", footer=NULL))
          data <<- dataFormatter(input$data, input$anno, input$labels)
          output$transformation <- renderUI({
            selectInput(ns("transformation"), "Transformation", choices = c(
              "log2 + 0.001" = "0.001",
              "log2 + 0.01" = "0.01",
              "log2 + 0.1" = "0.1",
              "log2 + 1" = "1",
              "None" = "0"
            ))
          })
          output$dots <- renderUI({
            checkboxInput(ns("dots"), "Show dots?", value = TRUE)
          })
          
          genes <- unique(data$gene_name)
          output$genes <- renderUI({
            selectizeInput(ns("genes"), "Genes to analyze", multiple = TRUE, choices=NULL)
          })
          
          updateSelectizeInput(session = session, inputId = 'genes', choices = genes, server = TRUE)
          
          output$combine <- renderUI({
            selectInput(ns("combine"), "Column(s) to use as groups", multiple = TRUE, colnames(data))
          })
          output$submit <- renderUI({
            actionButton(ns("submit"), label = "Submit")
          })
          removeModal()
        }
      )
      observeEvent(
        eventExpr = input$submit,
        handlerExpr = {
          output$graph <- renderPlotly(boxplotPlotter(data = data, genenames = input$genes, combine = input$combine, transformation = as.numeric(input$transformation), dots = input$dots))
        }
      )
    }
  )
}
