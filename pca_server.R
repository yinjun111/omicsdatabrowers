pcaServer <- function(id, useJson, datapaths=FALSE, params=list(x=1,y=2)) {
  
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
      params <- params
      datapaths <- datapaths
      if (useJson){
        hide("continue")
        click("continue")
      }else{
        output$data <- renderUI({
          fileInput(ns("data"), "Select Data File", accept = c("txt/csv", "text/comma-separated-values,text/plain", ".csv"))
        })
        output$labels <- renderUI({
          fileInput(ns("labels"), "Select Data Labels", accept = c("txt/csv", "text/comma-separated-values,text/plain", ".csv"))
        })
        show("continue")
      }
      
      
      observeEvent(
        eventExpr = input$continue,
        handlerExpr = {
          if (useJson){
            labels <- read.delim(datapaths$labels, sep = "\t", header = T)
          }else{
            labels <- read.delim(input$labels$datapath, sep = "\t", header = T)
          }

          labels <- colnames(labels)
          # render customization UI elements

          output$transformation <- renderUI({
            selectInput(ns("transformation"), "Transformation", choices = c(
              "log2 + 0.001" = "0.001",
              "log2 + 0.01" = "0.01",
              "log2 + 0.1" = "0.1",
              "log2 + 1" = "1",
              "None" = "0"
            ), selected = params$transformation)
          })          
          
          output$x <- renderUI({
            numericInput(ns("x"), "X", value = params$x , min = 1)
          })
          output$y <- renderUI({
            numericInput(ns("y"), "Y", value = params$y , min = 1)
          })
          
          output$colorgroup <- renderUI({
            selectInput(ns("colorgroup"), "Column used to define color", labels, selected=params$colorgroup)
          })
 
          output$shapegroup <- renderUI({
            selectInput(ns("shapegroup"), "Column used to define shape", labels, selected=params$shapegroup)
          })
          
          output$showLabels <- renderUI({
            checkboxInput(ns("showLabels"), "Show labels for each point", value = params$showLabels)
          })
          
          output$shape <- renderUI({
            selectInput(ns("shape"), "Shape of Points", choices = c("Circle", "Square", "ByGroup","Labels Only"), selected = params$shape)
          })
          output$color <- renderUI({
            selectInput(ns("color"), "Color Palette", choices = c("Palette 1", "Palette 2", "Palette 3"), selected = params$color)
          })
          output$useCustom <- renderUI({
            checkboxInput(ns("useCustom"), "Use custom colors and shapes", value = FALSE)
          })
          output$showGroupLabels <- renderUI({
            checkboxInput(ns("showGroupLabels"), "Show labels for color groups", value = params$showGroupLabels)
          })
          output$showEllipse <- renderUI({
            checkboxInput(ns("showEllipse"), "Show ellipses around each color group", value = params$showEllipse)
          })

          output$submit <- renderUI({
            actionButton(ns("submit"), label = "Submit")
          })
          
          # TODO: Download as PDF button.
        }
      )

      # When the "Use custom colors and shapes?" checkbox is clicked
      observeEvent(
        eventExpr = input$useCustom,
        handlerExpr = {
          if (useJson){
            labels <- read.delim(datapaths$labels, sep = "\t", header = T)
          }else{
            labels <- read.delim(input$labels$datapath, sep = "\t", header = T)
          }
          groups <- pca_find_groups(labels, input$colorgroup)
          if (input$useCustom) { # if checked
            appendTab(inputId = "dataInput", tabPanel("Custom", uiOutput("custom"))) # add "Custom" tab at top of sidebar
            for (i in groups) { # for each group name, add a colourInput and a numericInput to control each group's color and shape
              insertUI(
                selector = "#custom",
                where = "afterEnd",
                ui = colourpicker::colourInput(ns(paste("customColor", i, sep = "")), paste("Color for ", i), "black")
              )
              insertUI(
                selector = "#custom",
                where = "afterEnd",
                ui = numericInput(ns(paste("customShape", i, sep = "")), paste("Shape for ", i), value = "16", min = 0, max = 25)
              )
            }
            updateTabsetPanel(session, inputId = "dataInput", selected = "Custom")
          } else { # if unchecked
            removeTab(inputId = "dataInput", target = "Custom") # remove "Custom" tab
            removeUI(selector = "div:has(>[id*=customColor])", multiple = T) # remove elements inside "Custom" tab
            removeUI(selector = "div:has(>[id*=customShape])", multiple = T)
          }
        }
      )

      # When the "Submit" button is pressed
      observeEvent(
        eventExpr = input$submit,
        handlerExpr = {
          if (useJson){
            labels <- read.delim(datapaths$labels, sep = "\t", header = T)
            data <- read.delim(datapaths$data, sep = "\t", header = T, row.names = 1, check.names = F)
          }else{
            labels <- read.delim(input$labels$datapath, sep = "\t", header = T)
            data <- read.delim(input$data$datapath, sep = "\t", header = T, row.names = 1, check.names = F)
          }
          
          colorgroups <- pca_find_groups(labels, input$colorgroup)
          shapegroups <- pca_find_groups(labels, input$colorgroup)
          
          # Run PCA analysis, store the pca dataframe and variance list
          pca <- pca_analysis(data = data, labels = labels, transformation = as.numeric(input$transformation))
          variance <- pca$variance
          pca <- pca$data

          if (input$useCustom) { # If custom shapes and colors are used, extract the values and store them in lists
            customColors <- c()
            for (i in colorgroups) {
              c <- paste("customColor", i, sep = "")
              customColors <- c(customColors, input[[c]])
            }
            customColors <- rev(customColors)
            
            customShapes <- c()
            for (i in colorgroups) {
              c <- paste("customShape", i, sep = "")
              customShapes <- c(customShapes, as.numeric(input[[c]]))
            }
            customShapes <- rev(customShapes)

            # Generate Plotly with customization options
            outputGraph <- isolate(pca_plot(
              data = pca, labels = labels, colorgroup = input$colorgroup,shapegroup = input$shapegroup,
              x = as.numeric(input$x), y = as.numeric(input$y),
              showEllipse = input$showEllipse,
              color = customColors,
              shape = customShapes,
              custom = TRUE,
              showLabels = input$showLabels,
              showGroupLabels = input$showGroupLabels,
              variance = variance
            ))
          } else { # If no custom colors and shapes are provided
            outputGraph <- isolate(pca_plot(
              data = pca, labels = labels, colorgroup = input$colorgroup,shapegroup = input$shapegroup,
              x = as.numeric(input$x), y = as.numeric(input$y),
              showEllipse = input$showEllipse,
              color = input$color,
              shape = input$shape,
              custom = FALSE,
              showLabels = input$showLabels,
              showGroupLabels = input$showGroupLabels,
              variance = variance
            ))
          }
          output$graph <- renderPlotly(outputGraph)

          #download button may not be needed
          #output$downloadPlot <- renderUI({
          #  downloadButton('downloadPlot', 'Download')
          # })
          # output$downloadPlot <- downloadHandler(
          #  filename = "Shinyplot.png",
          #  content = function(file) {
          #    png(file)
          #    plotInput()
          #   dev.off()
          # }) 
          
        }
      )
    }
  )
}
