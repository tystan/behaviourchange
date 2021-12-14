


server <- function(input, output, session) {


  phys_select <- reactive({
    input$phys_input
  })


  observeEvent(input$phys_input, {
    if (get_nrow() > 10) {
      # do nothing
      dev_null <- NULL
    } else {
      reintialise_selections_df()
      add_selections_df("difference_physical_score", val = phys_select())
      updateSelectInput(
        session,
        "behav1_input",
        label =  get_behav_label(),
        choices = get_filtered_vars(),
        selected = non_select
      )
    }
  })


  # output$phys_selected <- renderText({
  #   paste(
  #     "You have chosen a",
  #     phys_select(),
  #     "difference in Physical Functioning Score.\n\n"
  #   )
  # })


  behav1_select <- reactive({
    input$behav1_input
  })

  observeEvent(input$behav1_input, {
    b1s <- behav1_select()
    add_selections_df(var = b1s, val = "null")
    updateSelectInput(
      session,
      "change1_input",
      label =  paste0("By how much do you want to change ", get_behav_nms(b1s), "?"),
      choices = get_filtered_values(b1s),
      selected = non_select
    )
  })



  change1_select <- reactive({
    input$change1_input
  })

  observeEvent(input$change1_input, {
    add_selections_df(var = input$behav1_input, val = input$change1_input)
    if (get_nrow() > 10) {
      updateSelectInput(
        session,
        "change1_input",
        label =  "",
        choices = non_select,
        selected = non_select
      )
      updateSelectInput(
        session,
        "behav1_input",
        label =  "",
        choices = non_select,
        selected = non_select
      )
      updateSelectInput(
        session,
        "phys_input",
        label =  "",
        choices = non_select,
        selected = non_select
      )
    } else {
      updateSelectInput(
        session,
        "behav1_input",
        label =  get_behav_label(),
        choices = get_filtered_vars(),
        selected = non_select
      )
    }
  })


  output$audit <- renderTable({
    get_selections_df()
  })

  output$txt_output <- renderUI({
    HTML(mk_html_text_selections())
  })


  observeEvent(input$rset, {

    reintialise_selections_df()

    updateSelectInput(
      session,
      "change1_input",
      label = "",
      choices = non_select,
      selected = non_select
    )
    updateSelectInput(
      session,
      "behav1_input",
      label = get_behav_label(),
      choices = get_filtered_vars(),
      selected = non_select
    )
    updateSelectInput(
      session,
      "phys_input",
      label = "Please select difference in physical functioning score",
      choices = phys_func_scores,
      selected = non_select
    )

  })

  # outputOptions(output, "phys_selected", suspendWhenHidden = FALSE)
  outputOptions(output, "audit", suspendWhenHidden = FALSE)

}



