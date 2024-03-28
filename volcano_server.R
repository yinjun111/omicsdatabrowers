volcanoServer <- function(id, useJson, datapaths=FALSE, params=c()) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS("volcano")
      params <- params
      if (useJson){
        hide("continue")
        click("continue")
      }else{
        output$data <- renderUI({
          fileInput(ns("data"), "Select Data File", accept = c("txt/csv", "text/comma-separated-values,text/plain", ".csv"))
        })
        show("continue")
      }
      
      observeEvent(
        eventExpr = input$continue,
        handlerExpr = {
          if (useJson){
            data <- read.delim(datapaths$data3, sep = "\t", header = T)
          }else{
            data <- read.delim(input$data$datapath, sep = "\t", header = T)
          }
          # render customization UI elements
          print(params)
          output$gene <- renderUI({
            selectInput(ns("gene"), "Gene", colnames(data), selected = params$gene)
          })
          output$x <- renderUI({
            selectInput(ns("x"), "X", colnames(data), selected = params$x)
          })
          output$y <- renderUI({
            selectInput(ns("y"), "Y", colnames(data), selected = params$y)
          })
          output$colorBy <- renderUI({
            selectInput(ns("colorBy"), "Color By", colnames(data), selected = params$colorBy)
          })
          output$xlab <- renderUI({
            textInput(ns("xlab"), "X Label", value = params$xlab)
          })
          output$ylab <- renderUI({
            textInput(ns("ylab"), "Y Label", value = params$ylab)
          })
          output$xCutoff <- renderUI({
            numericInput(ns("xCutoff"), "X Cutoff", value = params$xCutoff, min = 0)
          })
          output$yCutoff <- renderUI({
            numericInput(ns("yCutoff"), "Y Cutoff", value = params$yCutoff, min = 0)
          })
          output$submit <- renderUI({
            actionButton(ns("submit"), label = "Submit")
          })
          # TODO: Download as PDF button.
        }
      )
      
      # When the "Submit" button is pressed
      observeEvent(
        eventExpr = input$submit,
        handlerExpr = {
          showModal(modalDialog("Generating Plot...", footer=NULL))
          if (useJson){
            data <- read.delim(datapaths$data3, sep = "\t", header = T)
          }else{
            data <- read.delim(input$data$datapath, sep = "\t", header = T)
          }
          plot <- volcano_helper(data=data, gene=input$gene, fc=input$x, q=input$y, sig=input$colorBy,
                                 xlab=input$xlab, ylab=input$ylab, q_cutoff=input$xCutoff, fc_cutoff=input$yCutoff)
          output$graph <- renderPlot(plot)
          #output$graph <- renderPlotly(ggplotly(plot))
          removeModal()
        }
      )
    }
  )
}
