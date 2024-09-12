

# Module UI function
uploadCard <- function(id, title, id_load, id_button) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  card(
    fill = FALSE,
    full_screen = FALSE,
    card_header(
      tags$h5(title)
    ),
    card_body(
      layout_columns(
        col_widths = c(8, 4),
        fileInput(
          ns(id_load),
          label=NULL,
          multiple = FALSE,
          buttonLabel = "Browse...",
          placeholder = "No file selected (.csv)"
        ),
        actionButton(ns(id_button), label= "Load", width=100)
      ),
    ),
    card_footer(
      "(This function is not currently available) You can customize the transitions uploading a file with the parameters for each transition function. You can download the template here",
    )
  )
    
}