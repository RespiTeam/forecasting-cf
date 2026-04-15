# Module UI function
barGraphCardUI <- function(id, title) {
  ns <- NS(id)
  
  cftrChoices <- c('Trikafta', 'None')
  ageChoices <- c('Pediatrics', 'Adults')
  
  card(
    fill = FALSE,
    full_screen = TRUE,
    card_header(
      layout_column_wrap(
        width = "200px",
        fixed_width = TRUE,
        virtualSelectInput(
          ns("cftrFilter"),
          label = "Modulator",
          choices = cftrChoices,
          multiple = TRUE,
          selected = cftrChoices,
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
      plotOutput(ns("barplot")),
    )
  )
  
}



# Module UI function
barGraphCardServer <- function(id, r, colorPalette, targetVar, labY) {
  
  moduleServer(id, function(input, output, session) {
    
    output$barplot <- renderPlot({
      
      validate(need(r$qtyData, "No scenario has been forecasted yet"))
      
      col_sym <- ensym(targetVar)
      datag <- r$qtyData
      
      #start to plotting
      datag <- datag |> filter(state != 'dead')
      
      validate(
        need(input$ageFilter, "Select an age group"),
        need(input$cftrFilter, "Select a sub population")
      )
      
      # Applying filters
      datag <- datag |> filter(group %in% input$cftrFilter)
      datag <- datag |> filter(age_range %in% input$ageFilter)
      
      datag <- datag |>
        group_by(year, state) |>
        summarise(
          outcome=sum({{col_sym}}),
          .groups="drop"
        )
      
      datag |>
        ggplot2::ggplot(aes(
          x = as.factor(year),
          y = outcome,
          fill = state
        )) +
        ggplot2::geom_bar(position = "stack", stat = "identity") +
        ggplot2::theme_classic()
      labs(y = labY, x = "", fill = "") +
      theme(legend.position = "bottom")+
      theme(
        legend.text = element_text(size = 14),  # Adjust the size as needed
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)
      )+
      scale_fill_manual(values=colorPalette)+
      geom_text(aes(label = round(outcome,0)),
                position = position_stack(vjust = 0.5),  # centers each label
                color = "white",
                size = 4)

    })
    
  })
  
}