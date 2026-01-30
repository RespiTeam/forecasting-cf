#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggsurvfit)
library(shinybusy)
library(dplyr)
library(future)
library(promises)
library(RColorBrewer)

transition_data_default=tibble(
  from = c("Mild", "Mild","Mild","Moderate","Moderate","Moderate", "Severe", "Severe","Severe","Severe", "Transplant"),
  to = c("Moderate", "Severe","Dead","Mild", "Severe","Dead", "Mild", "Moderate", "Dead","Transplant","Dead"),
  assumptions=c("","","Limited to age > 16 years",
                "","","Limited to age > 16 years",
                "","","Limited to age > 16 years","Limited to age > 16 years",
                "Limited to age > 16 years"
                ),
  Coeff = c("I (−3.870427 + 0.0130938 * age)","0","I (−6.26)",
            "I (−1.323925 − 0.0409952 * age)", "I (−2.259841 - 0.0345258 * age)", "I (−4.477337)",
            "0","I (−2.599317)","I (−2.766158)","I (−2.0)",
            "I (−3.218876)"),
  Optimistic = c(1, 1, 1, 0, 1, 1, 0, 0, 0.7, 0.9, 0),
  Custom = rep(0.5,11)
)

newcases_ages_data=tibble(
  age_range=c('0-1','1-2','2-18','18-40'), 
  # prob=c(0.672,0.103,0.155,0.07)
  prob=c(0.677,0.062,0.1405,0.1205)
)

exacerbations_ratios_default=tibble(
  state=c('mild','moderate','severe'),
  # cftr=c(0.055,0.55,1.35),
  cftr=c(0.12,0.29,0.49),
  non_cftr=c(0.17,0.73,1.12)
)

inital_data_default <- read.csv(file = "data/initPop.csv")

#Loading the simulation core functions
source('r/sim_functions.R')
source('r/micSim.r')
source('r/auxFctMicSim.r')

plan(multisession)

# Creating gray scale palette
grey_palette = brewer.pal(9, 'Greys')

# Creating color palette
r=c(2, 17, 111, 253)
g=c(75, 129, 166, 127)
b=c(112, 153, 179, 228)

color_palette = rgb(r,g,b, maxColorValue = 255)

# Define server logic required to draw a histogram
server <- auth0_server(function(input, output, session) {
  
    #Defining reactive values
    rv <- reactiveValues(qtyData=NULL, survival_data=NULL, erDF508=rep(0,10),
                         transition_data=transition_data_default,
                         age_proportions_newcases=newcases_ages_data,
                         exacerbations_ratios=exacerbations_ratios_default,
                         initial_pop=inital_data_default,
                         forecasted_scenario=NULL, forecasted_times=NULL, toYear=NULL)
    
    rvSeverity <- reactiveValues(data=NULL)
    rvPopulation <- reactiveValues(data=NULL)
    rvSurvival <- reactiveValues(data=NULL)
    rvExacerbations <- reactiveValues(data=NULL)
    
    
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
      
      nIter=input$nSim
      period_length=input$breaks
      start_date=as.Date(paste(as.integer(input$from)+1,"-01-01",sep=""))
      end_date=as.Date(paste(input$to,"-12-31",sep=""))
      
      prop508=input$prop508/100
      new_F508=as.integer(input$newCases*prop508)
      new_0F508=as.integer(input$newCases*(1-prop508))
      
      initial_data=rv$initial_pop
      verDF508=1-rv$erDF508
      dist_ages_newcases=rv$age_proportions_newcases
      
      future({
        
        start_time=format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        dataList=iteratingSimulations2(initial_data, start_date, end_date, nIter, period_length, new_F508, new_0F508, verDF508, dist_ages_newcases)
        end_time=format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        
        dataList$times=paste("From ",start_time," to ",end_time)
        
        return(dataList)
        
      }, seed=TRUE) %...>% (
        function(result) {
          dataList=result
          
          # Preprocessing simulation results
          dataList$qty <- dataList$qty |> mutate(
            group=case_when(group=="cftr"~"Modulator", 
                            group=="non_cftr"~"Non-modulator",
                            .default = NA)
          )
          
          dataList$km <- dataList$km |> mutate(
            group=case_when(group=="cftr"~"Modulator", 
                            group=="non_cftr"~"Non-modulator",
                            .default = NA)
          )
          
          # Saving some data of the simulated scenario in the result
          rv$forecasted_scenario = paste("Results for ",input$scenarios," scenario",sep="")
          rv$forecasted_times=dataList$times
          rv$toYear=input$to
          
          # rv$qtyData=dataList$qty
          ratios_tb <-  rv$exacerbations_ratios
            
          rv$qtyData <- dataList$qty |> 
            mutate(
              exac=case_when(state=="mild" & group=="Non-modulator"~round(med*ratios_tb |> filter(state=='mild') |> pull(non_cftr),0),
                             state=="moderate" & group=="Non-modulator"~round(med*ratios_tb |> filter(state=='moderate') |> pull(non_cftr),0),
                             state=="severe" & group=="Non-modulator"~round(med*ratios_tb |> filter(state=='severe') |> pull(non_cftr),0),
                             state=="mild" & group=="Modulator"~round(med*ratios_tb |> filter(state=='mild') |> pull(cftr),0),
                             state=="moderate" & group=="Modulator"~round(med*ratios_tb |> filter(state=='moderate') |> pull(cftr),0),
                             state=="severe" & group=="Modulator"~round(med*ratios_tb |> filter(state=='severe') |> pull(cftr),0))
            )
          
          rv$survival_data = dataList$km
          
          remove_modal_gif()
          
        }
      ) %...!% (
        function(error) {
          remove_modal_gif()
          stop(error)
        }
      )
        
      # } Validation if
      
    }) |> 
      bindEvent(input$runSim)
    
    
    observeEvent(input$scenarios, {
      
      choices = c('Pessimistic','Optimistic', 'Custom')
      selected_col <- match(input$scenarios, choices)
      
      if (selected_col==1) rv$erDF508=rep(0,10)
      else  rv$erDF508=as.numeric(rv$transition_data[,selected_col+3] |> pull())

    
    })
    
    # Transitions table
    output$transitions_table=renderDT(
      rv$transition_data, 
      editable = list(target = "cell", disable = list(columns = c(1, 2, 3,4))),
      colnames = c("From", "To", "Assumptions", 
                   "Transition coefficients estimated in 2021 based on conservative effectiveness of CFTR modulators", 
                   "Optimistic reduction in transition probability based con contemporary evidence", 
                   "Custom reduction in transition probability"),
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
          colnames = c("From", "To", "Assumptions", 
                       "Transition coefficients estimated in 2021 based on conservative effectiveness of CFTR modulators", 
                       "Optimistic reduction in transition probability based con contemporary evidence", 
                       "Custom reduction in transition probability"),
          rownames = FALSE,
          selection = 'none',
          options = list(dom = 't'),
          server = TRUE
        )
        
      }
    })
    
    # New cases age groups table
    output$newcases_groups=renderDT(
      rv$age_proportions_newcases, 
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
        rv$age_proportions_newcases[info$row, info$col + 1] <- as.numeric(info$value)

      } else {

        showModal(modalDialog(
          title = "Invalid proportion",
          "Proportions should be values between 0 and 1",
          easyClose = TRUE,
          footer = NULL
        ))

      }

      # Revert the change
      if (rv$age_proportions_newcases[info$row, info$col + 1] != as.numeric(info$value)) {

        output$newcases_groups=renderDT(
          rv$age_proportions_newcases,
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
    
    # Exacerbations Rates table
    output$exacerbations_table=renderDT(
      rv$exacerbations_ratios, 
      editable = list(target = "cell", disable = list(columns = c(0))),
      colnames = c("State", "Modulator", "Non-Modulator"),
      rownames = FALSE,
      selection = 'none',
      options = list(
        dom = 't'
      ),
      server = TRUE
    )
    
    # Observe cell edits in Exacerbations Rates table
    observeEvent(input$exacerbations_table_cell_edit, {
      info <- input$exacerbations_table_cell_edit
      
      if (!is.na(as.numeric(info$value))) {
        
        # Update the reactive data with the new value
        rv$exacerbations_ratios[info$row, info$col + 1] <- as.numeric(info$value)
        
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
        
        output$exacerbations_table=renderDT(
          rv$exacerbations_ratios, 
          editable = list(target = "cell", disable = list(columns = c(0))),
          colnames = c("State", "Modulator", "Non-modulator"),
          rownames = FALSE,
          selection = 'none',
          options = list(
            dom = 't'
          ),
          server = TRUE
        )
        
      }
      
    })
    
    # Go back to default values
    observeEvent(input$btnDefaultExaRatios, {
      
      rv$exacerbations_ratios=exacerbations_ratios
      
    })
    
    # Modules server -----
    
    outputServer("simResults", rv, color_palette)
    
    # OUTPUT PLOTS --------
    
    output$selected_scenario <- renderText({
      
      validate(need(rv$forecasted_scenario, ""))
      rv$forecasted_scenario
      
    })
    
    output$times <- renderText({
      
      validate(need(rv$forecasted_times, "No scenario has been forecasted yet"))
      rv$forecasted_times
    })
    
    
    output$kmPlot <- renderPlot({
      
      validate(need(rv$survival_data, "No scenario has been forecasted yet"))
      
      #start to plotting
      rv$survival_data |>
        ggplot(aes(x=time, y=survival, colour = group)) +
        geom_step(direction="hv", linewidth = 1.5) +
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray", linewidth = 1)+
        theme_classic() +
        labs(y=paste("Survival Probaility at ", rv$toYear,sep=""), x="Time (Age)")+
        theme(legend.position="bottom")+
        theme(
          legend.text = element_text(size = 14),  # Adjust the size as needed
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14)
        )+
        scale_x_continuous(breaks = seq(0, 80, by = 5))+
        scale_color_manual(values=color_palette[4:3])
        
    })
    
    # output$hospPlot <- renderPlot({
    #   
    #   validate(need(rv$hospiplot_data, "No scenario has been forecasted yet"))
    #     
    #   datag= rv$hospiplot_data
    #   
    #   ratios_tb = rv$exacerbations_ratios
    #   
    #   #start to plotting
    #   datag = datag |> filter(state != 'dead' & state!="transplant")
    #   
    #   # Applying filters to databp3
    #   if (input$ageFilter_hospi!="All") {
    #     datag = datag |> filter(age_range==input$ageFilter_hospi)
    #   }
    #   
    #   datag = datag |> mutate(
    #       hosp=case_when(state=="mild" & group=="Non-modulator"~round(med*ratios_tb |> filter(state=='mild') |> pull(non_cftr),0),
    #                      state=="moderate" & group=="Non-modulator"~round(med*ratios_tb |> filter(state=='moderate') |> pull(non_cftr),0),
    #                      state=="severe" & group=="Non-modulator"~round(med*ratios_tb |> filter(state=='severe') |> pull(non_cftr),0),
    #                      state=="mild" & group=="Modulator"~round(med*ratios_tb |> filter(state=='mild') |> pull(cftr),0),
    #                      state=="moderate" & group=="Modulator"~round(med*ratios_tb |> filter(state=='moderate') |> pull(cftr),0),
    #                      state=="severe" & group=="Modulator"~round(med*ratios_tb |> filter(state=='severe') |> pull(cftr),0))
    #     ) |>
    #     group_by(milestone, state) |>
    #     summarise(
    #       hosp=sum(hosp),
    #       .groups="drop"
    #     )
    #   
    #   datag |> 
    #   ggplot(aes(
    #     x = as.factor(milestone),
    #     y = hosp,
    #     fill = state
    #   )) +
    #     geom_bar(position = "stack", stat = "identity") +
    #     theme_classic() +
    #     labs(y = "Number of exacerbations", x = "", fill = "") +
    #     theme(legend.position = "bottom")+
    #     theme(
    #       legend.text = element_text(size = 14),  # Adjust the size as needed
    #       axis.text.x = element_text(size = 14),
    #       axis.text.y = element_text(size = 14)
    #     )+
    #     scale_fill_manual(values=color_palette[3:1])+
    #     geom_text(aes(label = round(hosp,0)),
    #               position = position_stack(vjust = 0.5),  # centers each label
    #               color = "white",
    #               size = 4)
    #     
    # })

}
)


