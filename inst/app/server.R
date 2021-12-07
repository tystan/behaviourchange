


server <- function(input, output, session) {





  phys_select <- reactive({
    as.character(input$phys_input)
  })

  output$phys_selected <- renderText({
    paste(
      "You have chosen a",
      phys_select(),
      "difference in Physical functioning score.\n\n"
      )
  })


  behav1_select <- reactive({
    input$behav1_input
  })


  observe({
    updateSelectInput(
      session,
      "change1_input",
      label =  paste0("By how much do you want to change ", get_behav_nm(behav1_select()), "?"),
      choices = changes_one_filter(behav1_select(), "difference_physical_score", phys_select()),
      selected = non_select
    )
  })



  change1_select <- reactive({
    input$change1_input
  })

  observe({
    updateSelectInput(
      session,
      "behav2_input",
      label =  "Please select the second behaviour you would like to change",
      choices = rm_elements_behav(behav1_select()),
      selected = non_select
    )
  })


  behav2_select <- reactive({
    input$behav2_input
  })

  observe({
    updateSelectInput(
      session,
      "change2_input",
      label =  paste0("By how much do you want to change ", get_behav_nm(behav2_select()), "?"),
      choices = changes_two_filter(behav2_select(), behav1_select(), change1_select(), "difference_physical_score", phys_select()),
      selected = non_select
    )
  })


  change2_select <- reactive({
    input$change2_input
  })

  observe({
    updateSelectInput(
      session,
      "behav3_input",
      label =  "Please select the third behaviour you would like to change",
      choices = rm_elements_behav(c(behav1_select(), behav2_select())),
      selected = non_select
    )
  })


  behav3_select <- reactive({
    input$behav3_input
  })

  observe({
    updateSelectInput(
      session,
      "change3_input",
      label =  paste0("By how much do you want to change ", get_behav_nm(behav3_select()), "?"),
      choices =
        changes_three_filter(
          behav3_select(),
          behav2_select(),
          change2_select(),
          behav1_select(),
          change1_select(),
          "difference_physical_score",
          phys_select()
        ),
      selected = non_select
    )
  })










  output$audit <- renderPrint({
    cat(paste0(
      "phys_select() = \n        '", phys_select(), "'\n",
      "behav1_select() = \n        '", behav1_select(), "'\n",
      "changes_one_filter('", behav1_select(), "', 'difference_physical_score', '", phys_select(), "') = \n        ",
      "'[", paste(changes_one_filter(behav1_select(), "difference_physical_score", phys_select()), collapse = ", "), "]'\n",
      "changes_two_filter('", behav2_select(), "', '", behav1_select(), "', '", change1_select(), "', 'difference_physical_score', '", phys_select(), "') = \n        ",
      "'[", paste(changes_two_filter(behav2_select(), behav1_select(), change1_select(), "difference_physical_score", phys_select()), collapse = ", "), "]'\n",
      "changes_three_filter('", behav3_select(), "', '", behav2_select(), "', '", change2_select(),"', '", behav1_select(), "', '", change1_select(), "', 'difference_physical_score', '", phys_select(), "') = \n        ",
      "'[", paste(changes_three_filter(behav3_select(), behav2_select(), change2_select(), behav1_select(), change1_select(), "difference_physical_score", phys_select()), collapse = ", "), "]'\n"
    ))
  })


  outputOptions(output, "phys_selected", suspendWhenHidden = FALSE)
  outputOptions(output, "audit", suspendWhenHidden = FALSE)


  observeEvent(input$rset, {
    updateSelectInput(session, "change3_input", label = "", choices = non_select, selected = non_select)
    updateSelectInput(session, "behav3_input", label = "", choices = non_select, selected = non_select)
    updateSelectInput(session, "change2_input", label = "", choices = non_select, selected = non_select)
    updateSelectInput(session, "behav2_input", label = "", choices = non_select, selected = non_select)
    updateSelectInput(session, "change1_input", label = "", choices = non_select, selected = non_select)
    updateSelectInput(session, "behav1_input", label = "", choices = behavs, selected = non_select)
    updateSelectInput(session, "phys_input", label = "Please select difference in physical functioning score", choices = phys_func_scores, selected = non_select)
  })


}



