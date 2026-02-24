
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
        
        tags$p(
            HTML("<span class='text-body-secondary fw-light'>This cystic fibrosis population forecasting application is designed to help
            CF clinicians and researchers understanding the changing demographics of the
            population and healthcare resources needs given the current knowledge of the
            benefits of CFTR modulators on key clinical outcomes.</span>"),
            tags$p(
              downloadButton(
                outputId = ns("download_data_btn"),
                label = "Download Data"
              )
            )
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
        comorbTableCardUI(ns("comorbiTable"))
    ) # End of tags list

}

outputServer <- function(id, r, colorPalette, comorList, comorbidityChoices) {
  
  moduleServer(id, function(input, output, session) {
    
    output$download_data_btn <- downloadHandler(
      
      # a) `filename` defines the name of the file the user's browser will see.
      filename = function() {
        paste0("simulated-data-", Sys.Date(), ".parquet")
      },
      
      # b) `content` is a function that creates and writes the file.
      content = function(file) {
        fullData <- arrow::read_parquet(r$dataPath)
        # Now, copy our temporary file to the path Shiny expects.
        arrow::write_parquet(fullData, file)
      },
      
    )
    
    
    # Modules server
    
    barGraphCardServer("diseaseSeverity", r, colorPalette, "med", "Number of patients")
    populationTableCardServer("totalPop", r, "med")
    survivalGraphCardServer("survivalCurve", r, colorPalette)
    comorbTableCardServer("comorbiTable", r, "med", comorList, comorbidityChoices)
    
  })
  
}
