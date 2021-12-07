
library(shiny)

ui <- fluidPage(

  actionButton("rset", "Reset form"),

  br(),


  selectInput(
    "phys_func_select",
    "Please select difference in physical functioning score",
    phys_func_scores,
    selected = non_select
  ),


  conditionalPanel(
    condition = paste0("input.phys_func_select != '", non_select, "'"),
    textOutput("phys_selected")
  ),

  br(),

  br(),

  conditionalPanel(
    condition = paste0("input.phys_func_select != '", non_select, "'"),
    selectInput(
      "behav_select",
      "Please select the first behaviour you would like to change",
      behavs,
      selected = non_select
    )
  ),

  br(),

  br(),

  verbatimTextOutput("audit")






  # textOutput("txtout", get_vars())
  # textInput("txt", "This is no longer useful")
)




