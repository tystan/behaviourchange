
library(shiny)

ui <- fluidPage(

  actionButton("rset", "Reset form"),

  br(),

  selectInput(
    "phys_input",
    "Please select difference in physical functioning score",
    phys_func_scores,
    selected = non_select
  ),


  conditionalPanel(
    condition = paste0("input.phys_input != '", non_select, "'"),
    textOutput("phys_selected")
  ),

  br(),
  br(),

  conditionalPanel(
    condition = paste0("input.phys_input != '", non_select, "'"),
    selectInput(
      "behav1_input",
      "Please select the first behaviour you would like to change",
      behavs,
      selected = non_select
    )
  ),


  conditionalPanel(
    condition = paste0("input.behav1_input != '", non_select, "'"),
    selectInput(
      "change1_input",
      "",
      non_select,
      selected = non_select
    )
  ),

  br(),
  br(),

  conditionalPanel(
    condition = paste0("input.change1_input != '", non_select, "'"),
    selectInput(
      "behav2_input",
      "",
      non_select,
      selected = non_select
    )
  ),



  conditionalPanel(
    condition = paste0("input.behav2_input != '", non_select, "'"),
    selectInput(
      "change2_input",
      "",
      non_select,
      selected = non_select
    )
  ),

  br(),
  br(),

  conditionalPanel(
    condition = paste0("input.change2_input != '", non_select, "'"),
    selectInput(
      "behav3_input",
      "",
      non_select,
      selected = non_select
    )
  ),



  conditionalPanel(
    condition = paste0("input.behav3_input != '", non_select, "'"),
    selectInput(
      "change3_input",
      "",
      non_select,
      selected = non_select
    )
  ),

  br(),
  br(),
  br(),

  conditionalPanel(
    condition = audit_output_js,
    verbatimTextOutput("audit")
  )






)




