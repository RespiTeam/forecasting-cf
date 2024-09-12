
# Module UI function
transitionRatesTab <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
        tagList(
          shinyjs::disabled(
            selectInput("genotype",
                      label = tooltip(
                        trigger = list(
                          "Genotype:",
                          bs_icon("info-circle")
                        ),
                        "Currently it is only allowed to modify transition probabilities of Delta 508 group"
                      ),
                      choices = c('Delta 508'),
                      width=200
                      )
          ),
          tags$div("The percentages in the table below will multiply the transition probabilities for each state. The actual transition probabilities are calculated by functions that were estimated using ",
            tags$strong("Canadian Data from 2012 to 2017"), ". You can only edit values in the Custom column, the other columns are predefined scenarios."
            ),
          tags$br(),
          selectInput("scenarios",
                      label = "Use percentages from",
                      choices = c('Optimistic', 'Custom'),
          ),
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
        shinyjs::disabled(
          uploadCard("transRate", "Custom Transition Functions", "loadRatesData", "btnLoadRatesData")
        )
      )
    ),
  )
}