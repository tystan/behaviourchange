
library(shiny)

ui <- fluidPage(


  sidebarLayout(

    sidebarPanel(


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
      br(),

      actionButton("rset", "Reset form")

    ),


    mainPanel(

      fluidRow(
        column(
          width = 6,

          br(),

          conditionalPanel(
            condition = paste0("input.phys_input != '", non_select, "'"),
            selectInput(
              "behav1_input",
              "Please select the 1st behaviour you would like to change",
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
          )


        ),

        column(
          width = 6,

          conditionalPanel(
            condition = audit_output_js,
            br(),
            h3("Audit trail"),
            p("(set ", code("audit_output"), " = ", code("FALSE"), " in global.R to remove)"),
            tableOutput("audit")
          )
        )

      )

    )


  )


)




