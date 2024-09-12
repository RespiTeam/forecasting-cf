modelGraph <- function(id, title, id_load, id_button) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  colsLayout=c(3, 1, 4, 1, 3)
  
  
  tagList(
    layout_columns(
      col_widths = colsLayout,
      fill=FALSE,
      tags$img(
        src="newDiagnosisSq.png",
        width="150px"
      ),
      "",
      "",
      tagList(
        numericInput("txtSevereToMild", label=NULL, value=100, width="70px"),
        numericInput("txtMildToSevere", label=NULL, value=100, width="70px"),
      ),
      "",
      ""
    ),
    layout_columns(
      col_widths = colsLayout,
      fill=FALSE,
      tags$img(
        src="mildSq.png",
        width="150px"
      ),
      tagList(
        numericInput("txtMildToModerate", label=NULL, value=100, width="70px"),
        numericInput("txtModerateToMild", label=NULL, value=100, width="70px"),
      ),
      tags$img(
        src="moderateSq.png",
        width="150px"
      ),
      tagList(
        numericInput("txtModerateToSevere", label=NULL, value=100, width="70px"),
        numericInput("txtSevereToModerate", label=NULL, value=100, width="70px"),
      ),
      tags$img(
        src="severeSq.png",
        width="150px"
      ),
    ),
    layout_columns(
      col_widths = colsLayout,
      fill=FALSE,
      numericInput("txtMildToLFU", label=NULL, value=100, width="70px"),
      numericInput("txtMildToDead", label=NULL, value=100, width="70px"),
      numericInput("txtModerateToDead", label=NULL, value=100, width="70px"),
      numericInput("txtSevereToDead", label=NULL, value=100, width="70px"),
      numericInput("txtSevereToTransplant", label=NULL, value=100, width="70px"),
    ),
    layout_columns(
      col_widths = colsLayout,
      fill=FALSE,
      tags$img(
        src="lossToFollowSq.png",
        width="150px"
      ),
      "",
      tags$img(
        src="deadSq.png",
        width="150px"
      ),
      numericInput("txtTransplantToDead", label=NULL, value=100, width="70px"),
      tags$img(
        src="transplantSq.png",
        width="150px"
      ),
    )
  )
}