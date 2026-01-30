
hospiFilter <- popover(
  bs_icon("filter-circle"),
  tagList(
    selectInput(
      "ageFilter_hospi",
      label = "Age group",
      choices = c('All', 'Pediatrics', 'Adults')
    ),
  )
)


# Module UI function
outputUI <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
    tagList(
        
        HTML(
            "<p class='text-body-secondary fw-light'>This cystic fibrosis population forecasting application is designed to help
            CF clinicians and researchers understanding the changing demographics of the
            population and healthcare resources needs given the current knowledge of the
            benefits of CFTR modulators on key clinical outcomes.<p>"
        ),
      
        value_box( 
          title = textOutput("times"),
          value = textOutput("selected_scenario"),
          theme = "text-green",
          full_screen = FALSE,
        ),
        barGraphCardUI(ns("diseaseSeverity"), "Disease Severity"),
        tableCardUI(ns("totalPop"), "Population"),
        card(
          fill = FALSE,
          full_screen = TRUE,
          card_header(
            "Survival Curves",
          ),
          card_body(
            plotOutput(outputId = "kmPlot")
          )
        ),
        barGraphCardUI(ns("exacerbations"),"Pulmonary Exacerbations"),
    ) # End of tags list

}

outputServer <- function(id, r, colorPalette) {
  
  moduleServer(id, function(input, output, session) {
    
    # Modules server
    
    barGraphCardServer("diseaseSeverity", r, colorPalette, "med", "Number of patients")
    tableCardServer("totalPop", r, "med")
    barGraphCardServer("exacerbations", r, colorPalette,  "exac", "Number of exacerbations")
    
  })
  
}
