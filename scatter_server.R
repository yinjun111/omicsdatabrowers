scatterServer <- function(id, useJson, datapaths = FALSE, params = c()) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
      if (useJson) {
        hide("continue")
        click("continue")
      } else {
        output$data <- renderUI({
          fileInput(ns("data"), "Select Data File", accept = c("txt/csv", "text/comma-separated-values,text/plain", ".csv"))
        })
        show("continue")
      }

      observeEvent(
        eventExpr = input$continue,
        handlerExpr = {
          if (useJson) {
            data <- read.delim(datapaths$data2, header = T, row.names = 1, sep = "\t")
          } else {
            data <- read.delim(input$data$datapath, header = T, row.names = 1, sep = "\t")
          }
          # render customization UI elements
          output$x <- renderUI({
            selectInput(ns("x"), "X", colnames(data), selected = params$x)
          })
          output$y <- renderUI({
            selectInput(ns("y"), "Y", colnames(data), selected = params$y)
          })
          opt <- c("None", colnames(data))
          output$colorBy <- renderUI({
            selectInput(ns("colorBy"), "Color By",  opt, selected = params$colorBy)
          })
          output$shapeBy <- renderUI({
            selectInput(ns("shapeBy"), "Shape By", opt, selected = params$shapeBy)
          })
          output$sizeBy <- renderUI({
            selectInput(ns("sizeBy"), "Size By", opt, selected = params$sizeBy)
          })
          output$xlim <- renderUI({
            numericInput(ns("xlim"), "X Limit", value = params$xlim, min = 0)
          })
          output$ylim <- renderUI({
            numericInput(ns("ylim"), "Y Limit", value = params$ylim, min = 0)
          })
          output$main <- renderUI({
            textInput(ns("main"), "Main Label", value = params$main)
          })
          output$xlab <- renderUI({
            textInput(ns("xlab"), "X Label", value = params$xlab)
          })
          output$ylab <- renderUI({
            textInput(ns("ylab"), "Y Label", value = params$ylab)
          })
          output$na.val <- renderUI({
            numericInput(ns("na.val"), "NA Default Value", value = params$na.val, min = 0)
          })
          output$na.rm <- renderUI({
            checkboxInput(ns("na.rm"), "Remove NAs?", value = params$na.rm)
          })
          output$regress <- renderUI({
            checkboxInput(ns("regress"), "Regression?", value = params$regress)
          })
          output$cor <- renderUI({
            checkboxInput(ns("cor"), "Correlation?", value = params$cor)
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
          showModal(modalDialog("Generating Plot...", footer = NULL))
          if (useJson) {
            data <- read.delim(datapaths$data2, header = T, row.names = 1, sep = "\t")
          } else {
            data <- read.delim(input$data$datapath, header = T, row.names = 1, sep = "\t")
          }
          if (input$colorBy == "None"){
            colorBy <- NULL
          }else{
            colorBy <- unlist(data[input$colorBy])
          }
          if (input$shapeBy == "None"){
            shapeBy <- NULL
          }else{
            shapeBy <- unlist(data[input$shapeBy])
          }
          if (input$sizeBy == "None"){
            sizeBy <- NULL
          }else{
            sizeBy <- unlist(data[input$sizeBy])
          }
          plot <- scatter_plot(xval=unlist(data[input$x]), yval=unlist(data[input$y]),
            colorby = colorBy, shapeby = shapeBy, sizeby = sizeBy,
            #xlim = input$xlim, ylim = ylim,
            main = input$main, xlab = input$xlab, ylab = input$ylab,
            na.val = input$na.val, na.rm = input$na.rm,
            #colors = input$colors, 
            regress = input$regress, cor = input$cor
          )
          output$graph <- renderPlotly(plot)
          removeModal()
        }
      )
    }
  )
}
