#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
# library(ggsurvfit)
library(shinybusy)
library(future)
library(promises)
library(RColorBrewer)
library(ggplot2)
library(tibble)
library(rlang)
library(DBI)

server <- auth0_server(function(input, output, session) {

# server <- function(input, output, session) {  
  
    w <- Waiter$new()
    w$show()
    
    # Loading data and creating initial variables ------
    
    readRenviron(".Renviron")
    
    pg_conn <- dbConnect(
      RPostgres::Postgres(),
      dbname = Sys.getenv("DB_NAME"),
      host = Sys.getenv("DB_HOST"),
      port = Sys.getenv("DB_PORT"),
      user = Sys.getenv("DB_USER"),
      password = Sys.getenv("DB_PASS")
    )
    
    inital_data_default <- dbGetQuery(pg_conn, "SELECT * FROM cf_app_init_pop")
    comorbiRatios <- dbGetQuery(pg_conn, "SELECT * FROM cf_app_comor_ratios")
    comorbiDescription <- dbGetQuery(pg_conn, "SELECT * FROM cf_app_comor_desc")
    
    comorbiditiesNamesValues <- as.list(comorbiDescription$Variable) %>% set_names(comorbiDescription$Description)
    
    transition_data_default <- tibble::tibble(
      from = c("Mild", "Mild","Mild","Moderate","Moderate","Moderate", "Severe", "Severe","Severe","Severe", "Transplant"),
      to = c("Moderate", "Severe","Dead","Mild", "Severe","Dead", "Mild", "Moderate", "Dead","Transplant","Dead"),
      assumptions=c("","","Limited to age > 16 years",
                    "","","Limited to age > 16 years",
                    "","","Limited to age > 16 years","Limited to age > 16 years",
                    "Limited to age > 16 years"
      ),
      Coeff = c("I (-3.675 + 0.028 * age - 0.839 * tShort + 0.184 * tLong)",
                "I (-7.719)",
                "I (-9.23 + 0.087 * age)",
                "I (-0.161 - 0.041 * age + 1.324 * tShort -0.890 * tLong )",
                "I (-2.105 -3.001 * tShort - 0.842 * tLong)",
                "I (-21.84 + 0.25 * age)",
                "I (-15.315 -0.161 * age + 18.062 * tShort + 15.371 * tLong)",
                "I (-1.126 + 1.034 * tShort - 0.387 * tLong)",
                "I (-4.69 + 0.0368 * age)",
                "I (-3.061 + 0.008 * age - 19.820 * tShort - 19.809 * tLong)",
                "I (-3.308 + 0.013 * age)"),
      Custom = rep(0.5,11)
    )
    
    newcases_ages_data <- tibble(
      age_range=c('0-1','1-2','2-18','18-40'), 
      # prob=c(0.672,0.103,0.155,0.07)
      prob=c(0.677,0.062,0.1405,0.1205)
    )
    
    exacerbations_ratios_default <- tibble(
      state=c('mild','moderate','severe'),
      # cftr=c(0.055,0.55,1.35),
      cftr=c(0.12,0.29,0.49),
      non_cftr=c(0.17,0.73,1.12)
    )
    
    #Loading the simulation core functions
    source('r/sim_functions.R')
    source('r/micSim.r')
    source('r/auxFctMicSim.r')
    source('r/myFuns.R')
    
    plan(multisession)
    
    # Creating gray scale palette
    grey_palette = brewer.pal(9, 'Greys')
    
    # Creating color palette
    r=c(2, 17, 111, 253)
    g=c(75, 129, 166, 127)
    b=c(112, 153, 179, 228)
    
    color_palette = rgb(r,g,b, maxColorValue = 255)
    
    
    # Defining reactive values ------
    comorList <- reactiveVal(
      groupingComorbiditiesRatios(comorbiRatios, comorbiDescription)
      )
  
    rv <- reactiveValues(qtyData=NULL, survival_data=NULL, erDF508=rep(0,10),
                         qtyComorbi=NULL, transition_data=transition_data_default,
                         new_F508=NULL, new_0F508=NULL,
                         age_proportions_newcases=newcases_ages_data,
                         exacerbations_ratios=exacerbations_ratios_default,
                         initial_pop=inital_data_default,
                         forecasted_scenario=NULL, forecasted_times=NULL, 
                         toYear=NULL, comorbiRatios = comorbiRatios,
                         dataPath=NULL, femaleProp=NULL)
    
    observe({

       if (input$rb_initial_population=="canada") {
         rv$initial_pop = inital_data_default
       } else {
         rv$initial_pop = NULL
       }

    })
    
    observe({

      # Check if the file is uploaded
      req(input$initial_data)

      # Read the uploaded file using read.csv or another suitable function
      df <- read.csv(input$initial_data$datapath)

      validation = initial_data_validation(df)

      if (validation$result) {

        rv$initial_pop = df

      } else {

        showModal(modalDialog(
          title = "Error in initial data",
          validation$msg,
          easyClose = TRUE,
          footer = NULL
        ))

      }

    })
    
    # Running simulation  ------
    
    observe({
        
      show_modal_gif(
        #https://media.giphy.com/media/7d8tndK1hVRNYtOWTJ/giphy.gif?cid=ecf05e47jbrq1m0cnl5zswwwanw9x1820sd19cj67mnhgih0&ep=v1_gifs_search&rid=giphy.gif&ct=g
        src = "https://i.giphy.com/media/v1.Y2lkPTc5MGI3NjExdzNvYTA2MnkyZzNuZ2c1anZ6czRqdzRxdWYwbm1rcjZnZWYzZDBxMCZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/jozzZO5vdRZWFECNdO/giphy.gif",
        width = "300px", height = "300px",
        modal_size = "m",
        text = "Please wait..."
      )
      
      nIter <- input$nSim
      period_length <- input$breaks
      start_date <- as.Date(paste(as.integer(input$from)+1,"-01-01",sep=""))
      end_date <- as.Date(paste(input$to,"-12-31",sep=""))
      
      initial_data <- rv$initial_pop
      verDF508 <- 1-rv$erDF508
      dist_ages_newcases <- rv$age_proportions_newcases
      
      newCasesF508 <- rv$new_F508
      newCases0F508 <- rv$new_0F508
      
      future({
        
        start_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        dataList <- iteratingSimulations2(initial_data, start_date, end_date, nIter, period_length, newCasesF508, newCases0F508, verDF508, dist_ages_newcases)
        end_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        
        dataList$times <- paste("Simulation computed from ",start_time," to ",end_time)
        
        return(dataList)
        
      }, seed=TRUE) %...>% (
      function(result) {
          dataList=result
          
          # Preprocessing simulation results
          dataList$qty <- dataList$qty |> mutate(
            group=case_when(group=="cftr"~"Trikafta", 
                            group=="non_cftr"~"None",
                            .default = NA) 
          ) |> 
            rename(year=milestone)
          
          dataList$qty2 <- dataList$qty2 |> mutate(
            group=case_when(group=="cftr"~"Trikafta", 
                            group=="non_cftr"~"None",
                            .default = NA)
          ) |> 
            rename(year=milestone)
          
          dataList$km <- dataList$km |> mutate(
            group=case_when(group=="cftr"~"Trikafta", 
                            group=="non_cftr"~"None",
                            .default = NA)
          )
          
          # Saving some data of the simulated scenario in the result
          rv$forecasted_scenario <- paste(input$scenarios," scenario",sep="")
          rv$forecasted_times <- dataList$times
          rv$toYear <- input$to
          
          # ratios_tb <-  rv$exacerbations_ratios
          #   
          # rv$qtyData <- dataList$qty |> 
          #   mutate(
          #     exac=case_when(state=="mild" & group=="Non-modulator"~round(med*ratios_tb |> filter(state=='mild') |> pull(non_cftr),0),
          #                    state=="moderate" & group=="Non-modulator"~round(med*ratios_tb |> filter(state=='moderate') |> pull(non_cftr),0),
          #                    state=="severe" & group=="Non-modulator"~round(med*ratios_tb |> filter(state=='severe') |> pull(non_cftr),0),
          #                    state=="mild" & group=="Modulator"~round(med*ratios_tb |> filter(state=='mild') |> pull(cftr),0),
          #                    state=="moderate" & group=="Modulator"~round(med*ratios_tb |> filter(state=='moderate') |> pull(cftr),0),
          #                    state=="severe" & group=="Modulator"~round(med*ratios_tb |> filter(state=='severe') |> pull(cftr),0))
          #   )
          
          rv$qtyData <- dataList$qty
          rv$qtyComorbi <- dataList$qty2
          
          rv$survival_data <- dataList$km
          
          rv$dataPath <- dataList$dataPath
          
          remove_modal_gif()
          
        }
      ) %...!% (
        function(error) {
          remove_modal_gif()
          stop(error)
        }
      )
      
    }) |> 
      bindEvent(input$runSim)
    
    
    observe({
      
      choices = c('Evidence-based', 'Custom')
      selected_col <- match(input$scenarios, choices)
      
      if (selected_col==1) rv$erDF508=rep(0,10)
      else  rv$erDF508=as.numeric(rv$transition_data[,selected_col+3] |> pull())

    
    }) |> bindEvent(input$scenarios)
    
    # Transitions table
    output$transitions_table=renderDT(
      rv$transition_data, 
      editable = list(target = "cell", disable = list(columns = c(1, 2, 3, 4))),
      colnames = c("From", "To", "Assumptions",
                   "Transition equation", 
                   "Custom reduction"),
      rownames = FALSE,
      selection = 'none',
      options = list(
        dom = 't'
      ),
      server = TRUE
    )
    
    # Observe cell edits in transitions table
    observeEvent(input$transitions_table_cell_edit, {
      info <- input$transitions_table_cell_edit
      
      if (as.numeric(info$value)<=1 & as.numeric(info$value)>=0 ) {
        # Update the reactive data with the new value
        rv$transition_data[info$row, info$col + 1] <- as.numeric(info$value)
      } else {
       
        showModal(modalDialog(
          title = "Invalid percentage",
          "Reductions are expressed in ratios, they should be values between 0 and 1",
          easyClose = TRUE,
          footer = NULL
        ))
        
        # Revert the change
        output$transitions_table=renderDT(
          rv$transition_data, 
          editable = list(target = "cell", disable = list(columns = c(1, 2, 3, 4))),
          colnames = c("From", "To","Assumptions",
                       "Transition equation",
                       "Custom reduction"),
          rownames = FALSE,
          selection = 'none',
          options = list(dom = 't'),
          server = TRUE
        )
        
      }
    })
    
    # Modules server -----
    
    outputServer("simResults", rv, color_palette, comorList, comorbiditiesNamesValues)
    moreSettingsServer("moreSettings", rv, comorList)
 
    w$hide()
       
},
info = auth0::auth0_info())



