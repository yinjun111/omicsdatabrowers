metadataUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "metadata",
    h2("Information of Dataset"),
    fluidRow(
      box(
        title = "Information",
        uiOutput(ns("metadata"))
      )
    )
  )
}
