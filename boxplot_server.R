boxplotServer <- function(id, useJson, datapaths=FALSE, params=c()) {
  moduleServer(
    id,
    function(input, output, session) {
      
      d <- c()
      ns <- NS(id)
      params <- params
      datapaths <- datapaths
      if (useJson){
        hide("continue")
        click("continue")
        colourpicker::updateColourInput(session, inputId="color", value = params$color)
      }else{
        output$data <- renderUI({
          fileInput(ns("data"), "Select Data File", accept = c("txt/csv", "text/comma-separated-values,text/plain", ".csv"))
        })
        output$anno <- renderUI({
          fileInput(ns("anno"), "Select Row Annotations (optional)", accept = c("txt/csv", "text/comma-separated-values,text/plain", ".csv"))
        })
        output$labels <- renderUI({
          fileInput(ns("labels"), "Select Data Labels", accept = c("txt/csv", "text/comma-separated-values,text/plain", ".csv"))
        })
        show("continue")
      }
      
      hide("color")
      hide("genes")
      
      observeEvent(
        eventExpr = input$continue,
        handlerExpr = {
          showModal(modalDialog("Processing Data...", footer=NULL))
          if (useJson){
            labels <- read.delim(datapaths$labels, sep = "\t", header = T)
            data <- data.frame(read.delim(datapaths$data, sep = "\t", header = T))
            if (!is.null(datapaths$anno)){
              anno <- data.frame(read.delim(datapaths$anno, sep = "\t", header = T))
            }else{
              anno <- NULL
            }
          }else{
            labels <- read.delim(input$labels$datapath, sep = "\t", header = T)
            data <- data.frame(read.delim(input$data$datapath, sep = "\t", header = T))
            if (!is.null(input$anno$datapath)){
              anno <- data.frame(read.delim(input$anno$datapath, sep = "\t", header = T))
            }else{
              anno <- NULL
            }
          }
          
          d <<- dataFormatter(data, anno, labels)
          output$transformation <- renderUI({
            selectInput(ns("transformation"), "Transformation", choices = c(
              "log2 + 0.001" = "0.001",
              "log2 + 0.01" = "0.01",
              "log2 + 0.1" = "0.1",
              "log2 + 1" = "1",
              "None" = "0"
            ), selected = params$transformation)
          })
          output$dots <- renderUI({
            checkboxInput(ns("dots"), "Show dots?", value = params$dots)
          })
          
          genes <- unique(d$gene_name)
          # output$genes <- renderUI({
          #   selectizeInput(ns("genes"), "Genes to analyze", multiple = TRUE, choices=NULL)
          # })
          #, 
          show("genes")
          updateSelectizeInput(session = session, inputId = 'genes', choices = genes, selected=params$genes, server = TRUE)
          
          output$combine <- renderUI({
            selectInput(ns("combine"), "Column(s) to use as groups", multiple = TRUE, colnames(d), selected=params$combine)
          })
          
          show("color")

          output$submit <- renderUI({
            actionButton(ns("submit"), label = "Submit")
          })
          removeModal()
        }
      )
      observeEvent(
        eventExpr = input$submit,
        handlerExpr = {
          output$graph <- renderPlotly(boxplotPlotter(data = d, genenames = input$genes, combine = input$combine, transformation = as.numeric(input$transformation), dots = input$dots, color = input$color))
        }
      )
    }
  )
}
