
barplot1Filter <- popover(
  bs_icon("filter-circle"),
  selectInput(
    "genotypeFilter",
    label = "Genotype",
    choices = c('All', 'Delta 508', 'Non-delta 508')
  ),
  selectInput(
    "ageFilter_barplot1",
    label = "Age group",
    choices = c('All', 'Pediatrics', 'Adults')
  ),
)

barplot2Filter <- popover(
  bs_icon("filter-circle"),
  tagList(
    selectInput(
      "stateFilter",
      label = "Lung Function Severity",
      choices = c('All', 'mild', 'moderate', 'severe', 'transplant')
    ),
    selectInput(
      "ageFilter_barplot2",
      label = "Age group",
      choices = c('All', 'Pediatrics', 'Adults')
    ),
  )
)

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
outputTab <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
    # layout_columns(
    #   fill=FALSE,
    #   col_widths = c(4, 4, 4),
    #   selectInput(
    #     "genotypeFilterD",
    #     label = "Genotype",
    #     choices = c('All', 'Delta 508', 'Non-delta 508')
    #   ),
    #   selectInput(
    #     "ageFilter",
    #     label = "Age group",
    #     choices = c('All', 'Pediatrics', 'Adults')
    #   ),
    #   selectInput(
    #     "stateFilter",
    #     label = "Lung Function Severity",
    #     choices = c('All', 'mild', 'moderate', 'severe', 'transplant')
    #   ),
    # ), 
    tagList(
        value_box( 
          title = textOutput("times"),
          value = textOutput("selected_scenario"),
          theme = "text-green",
          full_screen = FALSE,
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            fill = TRUE,
            full_screen = TRUE,
            card_header(
              barplot1Filter,
            ),
            card_body(
              plotOutput(outputId = "barplot1"),
            )
          ),
          card(
            fill = TRUE,
            full_screen = TRUE,
            card_header(
              barplot2Filter,
            ),
            card_body(
              #plotOutput(outputId = "barplot2"),
              # gt_output("table2") 
              dataTableOutput("table2")
            )
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            fill = TRUE,
            full_screen = TRUE,
            card_header(
            ),
            card_body(
              plotOutput(outputId = "kmPlot")
            )
          ),
          card(
            fill = TRUE,
            full_screen = TRUE,
            card_header(
              hospiFilter,
            ),
            card_body(
              plotOutput(outputId = "hospPlot")
            )
          ),
        )
    ) # End of tags list
    # selectInput(
    #   "ageFilter",
    #   label = "Age group",
    #   choices = c('All', 'Pediatrics', 'Adults')
    # ),
}