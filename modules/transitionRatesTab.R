
# Module UI function
transitionRatesTab <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
          tags$div("This table contains the transition probabilities for the CF population that is treated with CFTR modulators. 
                   The proportion of the population not treated with CFTR modulators is assumed to follow the transition probabilities 
                   observed before CFTR modulators. The pessimistic transition probabilities are based on those estimated in Stanojevic et al., 2021 
                   using conservative estimates of the effectiveness of CFTR modulators. The optimistic column applies the real-world effectiveness 
                   reported in published studies. Users may apply their own custom changes in the transition rates.  For example if the rate of 
                   transplant is reduced by 90% from the pessimistic scenario, then 90 can be entered into the custom % box.",
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