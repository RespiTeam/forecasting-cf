
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

source("modules/barGraphCard.R")
source("modules/populationTableCard.R")
source("modules/survivalGraphCard.R")
source("modules/comorbTableCard.R")

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
        populationTableCardUI(ns("totalPop"), "Population"),
        survivalGraphCardUI(ns("survivalCurve")),
        barGraphCardUI(ns("exacerbations"),"Pulmonary Exacerbations"),
        comorbTableCardUI(ns("comorbiTable"),"Comorbidities")
    ) # End of tags list

}

outputServer <- function(id, r, colorPalette) {
  
  moduleServer(id, function(input, output, session) {
    
    # Modules server
    
    barGraphCardServer("diseaseSeverity", r, colorPalette, "med", "Number of patients")
    populationTableCardServer("totalPop", r, "med")
    survivalGraphCardServer("survivalCurve", r, colorPalette)
    barGraphCardServer("exacerbations", r, colorPalette,  "exac", "Number of exacerbations")
    comorbTableCardServer("comorbiTable", r, "med")
    
  })
  
}
