boxplotUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "boxplot",
    h2("Boxplot"),
    fluidRow(
      box(
        title = "Input",
        tabsetPanel(
          id = "dataInput",
          tabPanel(
            "Data",
            fileInput(ns("data"), "Select Data File", accept = c("txt/csv", "text/comma-separated-values,text/plain", ".csv")),
            fileInput(ns("anno"), "Select Row Annotations (optional)", accept = c("txt/csv", "text/comma-separated-values,text/plain", ".csv")),
            fileInput(ns("labels"), "Select Data Labels", accept = c("txt/csv", "text/comma-separated-values,text/plain", ".csv")),
            actionButton(ns("continue"), label = "Use these Files"),
            uiOutput(ns("transformation")),
            uiOutput(ns("dots")),
            uiOutput(ns("genes")),
            uiOutput(ns("combine")),
            uiOutput(ns("submit"))
          )
        )
      ),
      box(
        title = "Output",
        plotlyOutput(ns("graph"))
      )
    )
  )
}


