
source("modules/comorbiditySetting.R")

# Module UI function
moreSettingsUI <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  layout_column_wrap(
    width = 1,
    heights_equal = "row",
    fillable = FALSE,
    layout_column_wrap(
      fillable = FALSE,
      width=1/2,
      tags$p("Set the parameters to simulate new cases of CF", ),
      card(
        full_screen = FALSE,
        id = ns("generate_imm_card"),
        card_header("Generation of New Cases"),
        card_body(
          fill = FALSE,
          layout_column_wrap(
            width = 1,
            heights_equal = "row",
            layout_column_wrap(
              width = 1/2,
              "N° of new diagnosis anticipated per year: ",
              numericInput(
                inputId = ns("newCases"),
                label = NULL,
                value = 142,
                min = 0,
                max = 150,
                step = 1,
                width = 90
              )
            ),
            DTOutput(
              ns("newcases_groups")
            ),
            layout_columns(
              col_widths = c(10, 2),
              "% of population on modulator: ",
              numericInput(
                inputId = ns("prop508"),
                label = NULL,
                value = 64.4,
                step = 0.1,
                min = 0,
                max = 100,
                width = 90
              )
            )
          )
        )
      )
    ),
    layout_column_wrap(
      fillable = FALSE,
      width=1/2,
      tags$p("Set proportion (%) of females in the CF population"),
      numericInput(
        inputId = ns("femaleProp"),
        label = NULL,
        value = 47,
        step = 1,
        min = 0,
        max = 100,
        width = 90
      )
    ),
    comorbiditySettingUI(ns("hosp_times"), "Hospitalization"),
    comorbiditySettingUI(ns("homeIV_times"),"Home IV"),
    comorbiditySettingUI(ns("pancreatic_status"),"Pancreatic insufficiency"),
    comorbiditySettingUI(ns("cfrd_status"),"CFRD"),
    comorbiditySettingUI(ns("compl_resp"),"Respiratory related complication"),
    comorbiditySettingUI(ns("compl_digest"),"Digestive related complication"),
    comorbiditySettingUI(ns("compl_liver"),"Liver related complication"),
    comorbiditySettingUI(ns("compl_mental"),"Mental health related complication"),
    comorbiditySettingUI(ns("compl_bone"),"Bone related complication"),
    comorbiditySettingUI(ns("compl_malignancy"),"Malignancy"),
    comorbiditySettingUI(ns("pregnancy_status"),"Pregnancy")
  )
}

moreSettingsServer <- function(id, r, rComorList) {
  
  moduleServer(id, function(input, output, session) {
    
    observe({
      prop508 <- input$prop508/100
      r$new_F508 <- as.integer(input$newCases*prop508)
      r$new_0F508 <- as.integer(input$newCases*(1-prop508))  
    })
    
    observe({
      r$femaleProp <- as.numeric(input$femaleProp/100)
    })
    
    # New cases age groups table
    output$newcases_groups=renderDT(
      r$age_proportions_newcases, 
      editable = list(target = "cell", disable = list(columns = c(0))),
      colnames = c("Age distribution of new diagnoses", "%"),
      rownames = FALSE,
      selection = 'none',
      options = list(
        dom = 't'
      ),
      server = TRUE
    )
    
    # Observe cell edits in New cases age groups table
    observeEvent(input$newcases_groups_cell_edit, {
      info <- input$newcases_groups_cell_edit
      
      if (as.numeric(info$value)<=1 & as.numeric(info$value)>=0 ) {
        
        # Update the reactive data with the new value
        r$age_proportions_newcases[info$row, info$col + 1] <- as.numeric(info$value)
        
      } else {
        
        showModal(modalDialog(
          title = "Invalid proportion",
          "Proportions should be values between 0 and 1",
          easyClose = TRUE,
          footer = NULL
        ))
        
      }
      
      # Revert the change
      if (r$age_proportions_newcases[info$row, info$col + 1] != as.numeric(info$value)) {
        
        output$newcases_groups=renderDT(
          r$age_proportions_newcases,
          editable = list(target = "cell", disable = list(columns = c(0))),
          colnames = c("Age Group", "Proportion"),
          rownames = FALSE,
          selection = 'none',
          options = list(
            dom = 't'
          ),
          server = TRUE
        )
        
      }
      
    })
    
    # Modules Server -------
    
    comorbiditySettingServer("hosp_times",rComorList)
    comorbiditySettingServer("homeIV_times",rComorList)
    comorbiditySettingServer("pancreatic_status",rComorList)
    comorbiditySettingServer("cfrd_status",rComorList)
    comorbiditySettingServer("compl_resp",rComorList)
    comorbiditySettingServer("compl_digest",rComorList)
    comorbiditySettingServer("compl_liver",rComorList)
    comorbiditySettingServer("compl_mental",rComorList)
    comorbiditySettingServer("compl_bone",rComorList)
    comorbiditySettingServer("compl_malignancy",rComorList)
    comorbiditySettingServer("pregnancy_status",rComorList)
    
    # Adding comorbidities dinamycally
    # observe({
    # 
    #   # Isolate the dependency so it runs only once
    #   comorbidities <- isolate(names(rComorList()))
    # 
    #   print(comorbidities)
    #   
    #   purrr::walk(comorbidities, function(x) {
    # 
    #     insertUI(
    #       selector = "#moreSettings-btnDefaultExaRatios",
    #       where = "afterEnd",
    #       ui = comorbiditySettingUI(x)
    #     )
    # 
    #   })
    # 
    # })
    
    
  })
  
}
