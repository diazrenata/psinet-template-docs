function(input, output, session) {
  renderSurvey()
  
  observeEvent(input$submit, {
    
    posit_board <- board_connect()
    
    previous <- posit_board |>
      pin_read("renatadiaz/datasheet_responses")
    
    # Obtain and and append submitted results
    response <- getSurveyData(custom_id = input$email,
                              include_dependencies = FALSE)
    
    
    timestamp <- data.frame(
      subject_id = input$email,
      question_id = "timestamp",
      question_type = "time",
      response = as.character(Sys.time())
    )
    
    response <- bind_rows(response, timestamp)
    
    response <- response |>
      mutate(response_ID = max(previous$response_ID) + 1)
    
    updated <- bind_rows(previous, response)
    
    # Write back to pin
    
    posit_board |>
      pin_write(updated, "renatadiaz/datasheet_responses")
    
    # Show submission message
    showModal(
      modalDialog(
        title = "Thank you for submitting data to PSInet!",
        "Please reach out to Jessica Guo (jessicaguo@arizona.edu) or Renata Diaz (renatadiaz@arizona.edu) with any questions. "
      )
    )
    
  })
  
}
