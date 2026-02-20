
comorbTableCardUI <- function(id, title) {
  
  ns = NS(id)
  
  severityChoices <- c('mild', 'moderate', 'severe')
  ageRanges <- c('0-10','10-20','20-30','30-40','40-50','50-60','60-70','70+')
  comorbiditiesChoices <- c('hosp_IV_times')
  
  card(
    fill = FALSE,
    full_screen = TRUE,
    card_header(
      layout_column_wrap(
        width = "200px",
        fixed_width = TRUE,
        virtualSelectInput(
          ns("comorbidityFilter"),
          label = "Comorbidity",
          choices = comorbiditiesChoices,
          multiple = FALSE,
          selected = comorbiditiesChoices,
          dropboxWrapper = "body"
        ),
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
          choices = ageRanges,
          multiple = TRUE,
          selected = ageRanges,
          dropboxWrapper = "body"
        )
      )
    ),
    card_body(
      title,
      dataTableOutput(ns("outputTable"))
    )
  )
  
}



comorbTableCardServer <- function(id, r, targetVar) {
  
  moduleServer(id, function(input, output, session) {
  
    output$outputTable = DT::renderDataTable({
      
      validate(need(r$qtyData, "No scenario has been forecasted yet"))
      
      col_sym <- input$comorbidityFilter
      col_sym <- ensym(col_sym)
      datag= r$qtyComorbi
      
      # Applying filters
      validate(
        need(input$ageFilter, "Select an age group"),
        need(input$stateFilter, "Select a state of disease")
      )
      
      datag <- datag |> filter(age_range %in% input$ageFilter)
      datag <- datag |> filter(state %in% input$stateFilter)
      
      
      datag <- datag |> filter(state != 'dead') |>
        group_by(group,age_range, state, milestone) |>
        summarise(
          subj=sum(med),
          .groups = "drop"
        ) |> 
        arrange(milestone, group, state, age_range) |> 
        rename(
          Patients=subj
        ) |> 
        left_join(
          r$comorbiRatios |> select(group, age_range, state, {{col_sym}}),
          by=join_by(group,age_range,state)
        ) |> 
        mutate (
          Patients = Patients*{{col_sym}},
          group=case_when(group=="Modulator"~"Modulator", 
                          group=="Non-modulator"~"Non_modulator"
          ))

      
      print(
        datag |> 
              filter(is.na(Patients))
        )
      
      datag |> 
        group_by(group, milestone) |>
        summarise(
          Patients = round(sum(Patients, na.rm = TRUE),0),
          .groups = "drop"
        ) |> 
        pivot_wider(names_from = group, values_from = Patients) |> 
        mutate(
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