metadataServer <- function(id, metadata) {
  moduleServer(
    id,
    function(input, output, session) {
      # <- fromJSON(metadata)
      print(metadata)
      output$metadata <- renderUI({metadata})
    }
  )
}
