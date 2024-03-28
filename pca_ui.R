pcaUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "pca",
    h2("PCA"),
    fluidRow(
      box(
        title = "Input",
        tabsetPanel(
          id = ns("dataInput"),
          tabPanel(
            "Data",
            uiOutput(ns("data")),
            uiOutput(ns("labels")),
            actionButton(ns("continue"), label = "Use these files"),
            # uiOutput(ns("continue")),
            uiOutput(ns("transformation")),
            uiOutput(ns("x")),
            uiOutput(ns("y")),
            
            uiOutput(ns("colorgroup")),
            uiOutput(ns("color")),
            
            uiOutput(ns("shapegroup")),
            uiOutput(ns("shape")),

            uiOutput(ns("useCustom")),
            uiOutput(ns("showLabels")),            
            uiOutput(ns("showGroupLabels")),
            uiOutput(ns("showEllipse")),
            uiOutput(ns("submit"))
          )
        )
      ),
      box(
        title = "Output",
        plotlyOutput(ns("graph"), height="80vh"),
        uiOutput(ns("downloadPlot"))
      )
    )
  )
}
