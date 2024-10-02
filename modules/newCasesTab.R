
# Module UI function
newCasesTab <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(fluidRow(column(
    5,
    tags$p("Set the parameters to simulate new cases of CF", ),
  ), column(
    7,
    card(
      fill = FALSE,
      full_screen = FALSE,
      id = "generate_imm_card",
      card_header("Generation of New Cases"),
      card_body(tagList(
        layout_columns(
          col_widths = c(10, 2),
          "N° of new diagnosis anticipated per year: ",
          numericInput(
            inputId = "newCases",
            label = NULL,
            value = 87,
            min = 0,
            max = 150,
            step = 1,
            width = 90
          )
        ),
        DTOutput("newcases_groups"),
        layout_columns(
          col_widths = c(10, 2),
          "% of population eligible for CFTR modulator: ",
          numericInput(
            inputId = "prop508",
            label = NULL,
            value = 97.6,
            step = 0.1,
            min = 0,
            max = 100,
            width = 90
          )
        )
      ))
    )
  )), # fluidRow(
  #   column(12,
  #          radioButtons("rb_new_cases",NULL,
  #                       choiceNames = list(
  #                         HTML("<span class='py-3 pe-5'>
  #                       <strong class='fw-semibold'>Set parameters to simulate new cases</strong>
  #                       <span class='d-block small opacity-75'>...</span>
  #                     </span>"),
  #                         HTML("<span class='py-3 pe-5'>
  #                       <strong class='fw-semibold'>Use your pre-simulated data</strong>
  #                       <span class='d-block small opacity-75'>.....</span>
  #                     </span>")
  #                       ),
  #                       choiceValues = list(
  #                         "simulate", "my_simulated_data"
  #                       )
  #          ),
  #   )
  # ),
  fluidRow(column(
    5,
    tags$p("Change the exacerbations ratio used to compute the hospitalizations of the population", ),
  ), column(
    7,
    card(
      fill = FALSE,
      full_screen = FALSE,
      id = "exacerbations_rates",
      card_header("Exacerbations rates"),
      card_body(DTOutput("exacerbations_table")),
      card_footer(actionButton(
        inputId = "btnDefaultExaRatios" ,
        label = "Use default",
        icon = NULL,
        width = NULL
      ))
    )
  )))
}