survivalGraphCardUI <- function(id, title) {
  ns <- NS(id)
  
  card(
    fill = FALSE,
    full_screen = TRUE,
    card_header(
      "Survival Curves",
    ),
    card_body(
      plotOutput(outputId = ns("kmPlot"))
    )
  )
  
}


survivalGraphCardServer <- function(id, r, colorPalette) {
  
  moduleServer(id, function(input, output, session) {
    
    output$kmPlot <- renderPlot({
      
      validate(need(r$survival_data, "No scenario has been forecasted yet"))
      
      #start to plotting
      r$survival_data |>
        ggplot(aes(x=time, y=survival, colour = group)) +
        geom_step(direction="hv", linewidth = 1.5) +
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray", linewidth = 1)+
        theme_classic() +
        labs(y=paste("Survival Probaility at ", r$toYear,sep=""), x="Time (Age)")+
        theme(legend.position="bottom")+
        theme(
          legend.text = element_text(size = 14),  # Adjust the size as needed
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14)
        )+
        scale_x_continuous(breaks = seq(0, 80, by = 5))+
        scale_color_manual(values=color_palette[4:3])
      
      
      
    })
    
  })
  
}