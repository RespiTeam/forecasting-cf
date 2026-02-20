# Module UI function
populationTableCardUI <- function(id, title) {
  ns <- NS(id)
  
  severityChoices <- c('mild', 'moderate', 'severe', 'transplant')
  ageChoices <- c('Pediatrics', 'Adults')
  
  card(
    fill = FALSE,
    full_screen = TRUE,
    card_header(
      layout_column_wrap(
        width = "200px",
        fixed_width = TRUE,
        virtualSelectInput(
          ns("stateFilter"),
          label = "Severity",
          choices = severityChoices,
          multiple = TRUE,
          selected = severityChoices,
          dropboxWrapper = "body"
        ),
        virtualSelectInput(
          ns("ageFilter"),
          label = "Age",
          choices = ageChoices,
          multiple = TRUE,
          selected = ageChoices,
          dropboxWrapper = "body"
        )
      )
    ),
    card_body(
      title,
      dataTableOutput(ns("table2"))
    )
  )
  
}



# Module UI function
populationTableCardServer <- function(id, r, targetVar) {
  
  moduleServer(id, function(input, output, session) {
    
    output$table2 = DT::renderDataTable({
      
      validate(need(r$qtyData, "No scenario has been forecasted yet"))
      
      col_sym <- ensym(targetVar)
      datag= r$qtyData
      
      # Applying filters
      validate(
        need(input$ageFilter, "Select an age group"),
        need(input$stateFilter, "Select a state of disease")
        )
      
      datag = datag |> filter(age_range %in% input$ageFilter)
      datag = datag |> filter(state %in% input$stateFilter)
      
      datag |> filter(state != 'dead') |>
        group_by(group,milestone) |>
        summarise(
          subj=sum({{col_sym}}),
          .groups = "drop"
        ) |> arrange(milestone, group) |> select(
          milestone,
          group,
          Patients=subj,
        ) |> mutate (
          group=case_when(group=="Modulator"~"Modulator", 
                          group=="Non-modulator"~"Non_modulator",
          )
        ) |> pivot_wider(names_from = group, values_from = Patients) |> mutate(
          Total=Modulator+Non_modulator
        )
      
    }, 
    selection = 'none',
    options = list(
      dom = 'pt',
      pageLength = 6
    ),
    rownames = FALSE,
    server = FALSE
    )
    
  })
  
}