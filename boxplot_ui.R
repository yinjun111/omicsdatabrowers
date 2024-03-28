boxplotUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "boxplot",
    h2("Boxplot"),
    fluidRow(
      box(
        title = "Input",
        tabsetPanel(
          id = ns("dataInput"),
          tabPanel(
            "Data",
            uiOutput(ns("data")),
            uiOutput(ns("anno")),
            uiOutput(ns("labels")),
            actionButton(ns("continue"), label = "Use these Files"),
            uiOutput(ns("transformation")),
            uiOutput(ns("dots")),
            selectizeInput(ns("genes"), "Genes to analyze", multiple = TRUE, choices=NULL),
            uiOutput(ns("combine")),
            colourpicker::colourInput(ns("color"), "Color", value="blue"),
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


