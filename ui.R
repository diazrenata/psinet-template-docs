
fluidPage(
  theme = bs_theme(version = 3),
  surveyOutput(df = df,
               survey_title = "Data submission portal",
               survey_description = h4(HTML("Thank you for submitting data to PSInet! Please use this form to submit a link to your filled-in Google Sheet with data. If you have any questions, please reach out to Jessica Guo (jessicaguo@arizona.edu) and/or Renata Diaz (renatadiaz@arizona.edu).")),
               theme = "#6EC298")
)
