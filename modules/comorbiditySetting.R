

comorbiditySettingUI <- function(id) {

  ns <- NS(id)
  
  layout_columns(
      col_widths = c(5, 7),
      tags$p(sprintf("Set the ratios for %s by group",id)),
      card(
        fill = FALSE,
        full_screen = FALSE,
        id = ns("comorbidity_card"),
        card_header(sprintf("Ratios for %s", id)),
        card_body(
          DTOutput(
            ns("comorbidityRatiosTable")
          )
        )
      )
  )
}


comorbiditySettingServer <- function(id, rComorList) {
  
  moduleServer(id, function(input, output, session) {
    
    # Rates table
    output$comorbidityRatiosTable=renderDT(
      rComorList()[[id]], 
      editable = list(target = "cell", disable = list(columns = c(0))),
      rownames = FALSE,
      selection = 'none',
      options = list(
        dom = 't'
      ),
      server = TRUE
    )
    
    # Observe cell edits
    observeEvent(input$comorbidityRatiosTable_cell_edit, {
      info <- input$comorbidityRatiosTable_cell_edit
      
      if (!is.na(as.numeric(info$value))) {
        
        # Update the reactive data with the new value
        rComorList()[[id]][info$row, info$col + 1] <- as.numeric(info$value)
        
      } else {
        
        showModal(modalDialog(
          title = "Invalid proportion",
          "You should enter a number",
          easyClose = TRUE,
          footer = NULL
        ))
        
      }
      
      # Revert the change
      if (is.na(as.numeric(info$value))) {
        
        output$comorbidityRatiosTable=renderDT(
          rComorList()[[id]],
          editable = list(target = "cell", disable = list(columns = c(0))),
          rownames = FALSE,
          selection = 'none',
          options = list(
            dom = 't'
          ),
          server = TRUE
        )
        
      }
      
    })
    
    observe ({
      
      print("go")
      print(rComorList()[[id]])
        
    })
    
    
    # # Go back to default values
    # observe({
    #   
    #   r$exacerbations_ratios=exacerbations_ratios
    #   
    # }) |> bindEvent(input$btnDefaultExaRatios)
    
  })
  
  
}