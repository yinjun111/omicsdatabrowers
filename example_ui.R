exampleUI <- function(id) {
  #Create namespace of variables. Required for redundant variable names across functions.
  #For example, by defining the namespace, the "data" input 
  #can be differentiated between the PCA and boxplot functions.
  ns <- NS(id)
  #tabItem as defined by shinydashboard
  tabItem(
    tabName = "example",
    h2("Example"),
    fluidRow(
      box(
        title = "Input",
        tabsetPanel(
          id = ns("dataInput"),
          tabPanel(
            "Data",
            #All variables must be wrapped in ns() to define that variable within that specific namespace.
            numericInput(ns("firstNum"), "First Number", value = 1),
            actionButton(ns("continue"), label = "Choose Second Number"),
            uiOutput(ns("secondNum")),
            uiOutput(ns("submit"))
          )
        )
      ),
      box(
        title = "Output",
        textOutput(ns("solution"))
      )
    )
  )
}
