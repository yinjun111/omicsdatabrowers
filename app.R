library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(colourpicker)
library(RColorBrewer)
library(tidyverse)
library(tidyr)
library(shinyjs)
library(rjson)
library(shinyBS)
library(EnhancedVolcano)

source("metadata_ui.R")
source("metadata_server.R")

source("pca_functions.R")
source("pca_ui.R")
source("pca_server.R")

source("boxplot_functions.R")
source("boxplot_ui.R")
source("boxplot_server.R")

source("example_functions.R")
source("example_ui.R")
source("example_server.R")

source("volcano_functions.R")
source("volcano_ui.R")
source("volcano_server.R")

source("scatter_functions.R")
source("scatter_ui.R")
source("scatter_server.R")


ui <- dashboardPage(
  dashboardHeader(title = "Ferring Data Browser 2.0"),
  ## Sidebar content
  dashboardSidebar(sidebarMenu(
    id = "tabs",
    sidebarMenuOutput("metadata"),
    sidebarMenuOutput("pca"),
    sidebarMenuOutput("boxplot"),
    sidebarMenuOutput("volcano"),
    sidebarMenuOutput("scatter"),
    sidebarMenuOutput("example")
    # menuItem("Information", tabName = "metadata", icon = icon("chart-bar")),
    # menuItem("PCA", tabName = "pca", icon = icon("chart-bar")),
    # menuItem("Volcano", tabName = "volcano", icon = icon("chart-area")),
    # menuItem("Scatter", tabName = "scatter", icon = icon("chart-area")),
    # menuItem("Example", tabName = "example", icon = icon("pizza-slice"))
  )),
  ## Body content
  dashboardBody(
    tags$head(tags$style(".modal-dialog{ width:1000px}")),
    tags$head(tags$style(".modal-body{ min-height:700px}")),
    tabItems(
      metadataUI("metadata"),
      pcaUI("pca"),
      boxplotUI("boxplot"),
      volcanoUI("volcano"),
      scatterUI("scatter"),
      exampleUI("example")
    )
  ),
  useShinyjs()
)

options(shiny.maxRequestSize = 5000 * 1024^2)
server <- function(input, output, session) {
  useJson <- FALSE
  observe({
    query <- parseQueryString(session$clientData$url_search)
    print(query)
    if (!is.null(query[["json"]])) {
      print("yes json")
      useJson <- TRUE
      json <- query[["json"]]
      json <- fromJSON(file = json)
      datapaths <- json$datapaths
      metadataServer("metadata", json$metadata)
      output$metadata <- renderMenu({
        sidebarMenu(
          menuItem("Information", tabName = "metadata", icon = icon("chart-area"))
        )
      })
      updateTabItems(session, "tabs", "metadata")
      for (params in json$modules) {
        switch(params$type,
          # /?json=input.json
          "pca" = {
            pcaServer("pca", useJson, datapaths, params)
            output$pca <- renderMenu({
              sidebarMenu(
                menuItem("PCA", tabName = "pca", icon = icon("chart-area"))
              )
            })
            updateTabItems(session, "tabs", "pca")
          },
          "boxplot" = {
            boxplotServer("boxplot", useJson, datapaths, params)
            output$boxplot <- renderMenu({
              sidebarMenu(
                menuItem("Boxplot", tabName = "boxplot", icon = icon("chart-area"))
              )
            })
            updateTabItems(session, "tabs", "boxplot")
          },
          "volcano" = {
            volcanoServer("volcano", useJson, datapaths, params)
            output$volcano <- renderMenu({
              sidebarMenu(
                menuItem("Volcano", tabName = "volcano", icon = icon("chart-area"))
              )
            })
            updateTabItems(session, "tabs", "volcano")
          },
          "scatter" = {
            scatterServer("scatter", useJson, datapaths, params)
            output$scatter <- renderMenu({
              sidebarMenu(
                menuItem("Scatter", tabName = "scatter", icon = icon("chart-area"))
              )
            })
            updateTabItems(session, "tabs", "scatter")
          }
        )
      }
      
    } else {
      print("no json")
      pcaServer("pca", useJson)
      output$pca <- renderMenu({
        sidebarMenu(
          menuItem("PCA", tabName = "pca", icon = icon("chart-area"))
        )
      })
      updateTabItems(session, "tabs", "pca")

      boxplotServer("boxplot", useJson)
      output$boxplot <- renderMenu({
        sidebarMenu(
          menuItem("Boxplot", tabName = "boxplot", icon = icon("chart-area"))
        )
      })
      updateTabItems(session, "tabs", "boxplot")

      volcanoServer("volcano", useJson)
      output$volcano <- renderMenu({
        sidebarMenu(
          menuItem("Volcano", tabName = "volcano", icon = icon("chart-area"))
        )
      })
      updateTabItems(session, "tabs", "volcano")

      scatterServer("scatter", useJson)
      output$scatter <- renderMenu({
        sidebarMenu(
          menuItem("Scatter", tabName = "scatter", icon = icon("chart-area"))
        )
      })
      updateTabItems(session, "tabs", "scatter")

      exampleServer("example")
      output$example <- renderMenu({
        sidebarMenu(
          menuItem("Example", tabName = "example", icon = icon("chart-area"))
        )
      })
      updateTabItems(session, "tabs", "example")
    }
  })
}


shinyApp(ui, server)
