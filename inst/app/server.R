


server <- function(input, output, session) {





  phys_select <- reactive({
    as.character(input$phys_func_select)
  })

  output$phys_selected <- renderText({
    paste(
      "You have chosen a",
      phys_select(),
      "difference in Physical functioning score.\n\n"
      )
  })


  behav_select <- reactive({
    as.character(input$behav_select)
  })



  output$audit <- renderPrint({
    cat(paste0(
      "phys_select() = '", paste(phys_select(), collapse = ", "), "'\n",
      "behav_select() = '", paste(behav_select(), collapse = ", "), "'\n"
    ))
  })


  outputOptions(output, "phys_selected", suspendWhenHidden = FALSE)





}



