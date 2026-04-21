
# Module UI function
transitionRatesTab <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
          tags$div(
              tags$p("This table contains the transition probabilities for the CF population that is treated with CFTR modulators (ETI). 
                   The evidence-based transition probability equations for individuals on ETI were estimated using data from CCFR, 2022 to 2023. 
                   Selecting `custom` scenario, users may apply their own custom changes in the transition rates. For example if they want to apply a reduction of 90% to the evidence-based 
                   probability, then 0.90 can be entered into the custom column.
                   Although, transition probability equations for individual who don't take ETI are not shown, they were estimated using data from CCFR, 2022 to 2023, as well."),
              tags$h4("Transition probabilitiy equations for individuals on ETI")
            ),
      )
    ),
    fluidRow(
      column(12,
        DTOutput("transitions_table")
      )
    ),
    fluidRow(
      column(12,
          tags$div(HTML("<small class='fst-italic'>* Where I = exp(x)/(1-exp(x))</small>"))
      )
    ),
    fluidRow(
      column(12,
        shinyjs::disabled(
          uploadCard("transRate", "Custom Transition Functions", "loadRatesData", "btnLoadRatesData")
        )
      )
    ),
  )
}