
comorbTableCardUI <- function(id) {
  
  ns = NS(id)
  
  severityChoices <- c('mild', 'moderate', 'severe')
  ageRanges <- c('0-12','12-20','20-45','45-60','60+')
  
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
          search = TRUE,
          choices = NULL,
          multiple = FALSE,
          dropboxWrapper = "body",
          optionsCount=5
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
      dataTableOutput(ns("outputTable"))
    )
  )
  
}



comorbTableCardServer <- function(id, r, targetVar, comorList, comorbidityChoices) {
  
  moduleServer(id, function(input, output, session) {
  
    observe({
      
      updateVirtualSelect(
        inputId = "comorbidityFilter",
        choices = comorbidityChoices,
        selected = "hosp_times"
      )
      
    })
    
    output$outputTable = DT::renderDataTable({
      
      validate(need(r$qtyData, "No scenario has been forecasted yet"))
      
      selComor <- input$comorbidityFilter
      datag <- r$qtyComorbi
      
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
        )
      
      keys <- names(comorList()[[selComor]] |> select(!ratio))
      
      if ("sex" %in% keys) {
        
        datag <- datag |> 
          mutate(
            Female = Patients*0.47,
            Male = Patients*0.53
          ) |> 
          select(!Patients) |> 
          pivot_longer(
            cols = c(Female, Male),
            names_to = "sex",
            values_to = "Patients"
          )
        
      }
      
      
      datag |> 
        left_join(
          comorList()[[selComor]],
          by=keys
        ) |> 
        mutate (
          Patients = Patients*ratio,
          group=case_when(group=="Modulator"~"Modulator", 
                          group=="Non-modulator"~"Non_modulator"
          )) |> 
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