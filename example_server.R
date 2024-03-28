#name of function, id attribute refers to the namespace id, required.
exampleServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      #show example tab in UI
      
      #start server functions in here.
      #namespace (see more info in example_ui.R)
      ns <- NS(id)
      #observeEvent function. Checks for change in eventExpr (in this case, the button with id "continue")
      #then runs code in handlerExpr.
      observeEvent(
        eventExpr = input$continue,
        handlerExpr = {
          print("observed Continue button press")
          firstNum <- input$firstNum
          #connects to the uiOutput in example_ui.R.
          #uiOutput works as a placeholder, and then renderUI generates the UI that appears 
          #where the uiOutput is located.
          output$secondNum <- renderUI({
            #Note that every variable name is surrounded by ns(), this defines the 
            #variable within the namespace (defined by the id at the beginning of this file).
            #This prevents variable name collision across different functions. 
            numericInput(ns("secondNum"), "Second Number", value = 2)
          })
          output$submit <- renderUI({
            actionButton(ns("submit"), label = "Submit")
          })
        }
      )
      #Another observeEvent function, this time checking for change (aka a press of)
      #button with id "submit"
      observeEvent(
        eventExpr = input$submit,
        handlerExpr = {
          print("observed Continue button press")
          #the addTwoNumbers function is defined in example_functions.R
          #Can be used to better modularize large functions. 
          output$solution <- renderText(addTwoNumbers(input$firstNum, input$secondNum))
        }
      )
    }
  )
}
