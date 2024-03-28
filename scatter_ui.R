scatterUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "scatter",
    h2("Scatter"),
    fluidRow(
      box(
        title = "Input",
        tabsetPanel(
          id = ns("dataInput"),
          tabPanel(
            "Data",
            uiOutput(ns("data")),
            #fileInput(ns("labels"), "Select Data Labels", accept = c("txt/csv", "text/comma-separated-values,text/plain", ".csv")),
            actionButton(ns("continue"), label = "Use these Files"),
            uiOutput(ns("x")),
            
   
            uiOutput(ns("y")),
            uiOutput(ns("colorBy")),
            uiOutput(ns("shapeBy")),
            uiOutput(ns("sizeBy")),
            uiOutput(ns("xlim")),
            uiOutput(ns("ylim")),
            uiOutput(ns("main")),
            uiOutput(ns("xlab")),
            uiOutput(ns("ylab")),
            uiOutput(ns("na.val")),
            uiOutput(ns("na.rm")),
            uiOutput(ns("colors")),
            uiOutput(ns("regress")),
            uiOutput(ns("cor")),
            uiOutput(ns("submit"))
          )
        )
      ),
      box(
        title = "Output",
        plotlyOutput(ns("graph"), height="80vh")
      )
    )
  )
}
