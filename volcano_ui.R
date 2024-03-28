volcanoUI <- function(id) {
  ns <- NS("volcano")
  tabItem(
    tabName = "volcano",
    h2("Volcano"),
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
            uiOutput(ns("gene")),
            uiOutput(ns("x")),
            uiOutput(ns("y")),
            uiOutput(ns("colorBy")),
            uiOutput(ns("xlab")),
            uiOutput(ns("ylab")),
            uiOutput(ns("xCutoff")),
            uiOutput(ns("yCutoff")),
            uiOutput(ns("submit"))
          )
        )
      ),
      box(
        title = "Output",
        #plotlyOutput(ns("graph"))
        plotOutput(ns("graph"), height="80vh")
      )
    )
  )
}
