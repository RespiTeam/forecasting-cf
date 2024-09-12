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
library(tidyverse)
library(future)
library(promises)

transition_data=tibble(
  from = c("Mild", "Mild","Mild","Moderate","Moderate","Moderate", "Severe", "Severe","Severe","Severe", "Transplant"),
  to = c("Moderate", "Severe","Dead","Mild", "Severe","Dead", "Mild", "Moderate", "Transplant", "Dead","Dead"),
  assumptions=c("","","Limited to age > 16 years",
                "","","Limited to age > 16 years",
                "","","Limited to age > 16 years","Limited to age > 16 years",
                "Limited to age > 16 years"
                ),
  Coeff = c("I (−3.870427 + 0.0130938 * age)","0","I (−6.26)",
            "I (−1.323925 − 0.0409952 * age)", "I (−2.259841 - 0.0345258 * age)", "I (−4.477337)",
            "0","I (−2.599317)","I (−2.0)","I (−2.766158)",
            "I (−3.218876)"),
  Optimistic = c(0, 0, 0, 100, 0, 0, 100, 100, 90, 70, 100),
  Custom = rep(100,11)
)

newcases_ages_data=tibble(
  age_range=c('0-1','1-2','2-18','18-40'), 
  prob=c(0.672,0.103,0.155,0.07)
)

data <- read.csv(file = "data/defaultData.csv")

#Loading the simulation core functions
source('r/sim_functions.R')
source('r/micSim.r')
source('r/auxFctMicSim.r')

plan(multisession)

# Define server logic required to draw a histogram
function(input, output, session) {
  
    #Defining reactive values
    simOutput <- reactiveValues(data = NULL)
    barplot1 <- reactiveValues(data = NULL)
    barplot2 <- reactiveValues(data = NULL)
    hospiplot <- reactiveValues(data = NULL)
    erDF508 <- reactiveVal(rep(1,10))
    rv <- reactiveValues(data = transition_data)
    rv2 <- reactiveValues(data = newcases_ages_data)
    initial_pop <- reactiveValues(data = data)
    scenario <- reactiveVal("")
    toYear <- reactiveVal("")
    times <- reactiveVal("")
    
    #Events definition
    observeEvent(input$rb_initial_population, {
      
       if (input$rb_initial_population=="canada") {
         initial_pop$data = data
       } else {
         initial_pop$data = NULL
       }
      
    })
    
    observeEvent(input$btnInitialData, {
      
      # Check if the file is uploaded
      req(input$initial_data)
      
      # Read the uploaded file using read.csv or another suitable function
      df <- read.csv(input$initial_data$datapath)
  
      validation = initial_data_validation(df)
      
      if (validation$result) {
        
        initial_pop$data = df
        
      } else {
        
        showModal(modalDialog(
          title = "Error in initial data",
          validation$msg,
          easyClose = TRUE,
          footer = NULL
        ))
        
      }
      
    })
    
    observeEvent(input$runSim, {
      
      # Validations
      validated=TRUE
      sum_prop=sum(rv2$data[, 2] |> pull())
      
      # Perform validation
      if(sum_prop!=1) {
        validated=FALSE
        showModal(modalDialog(
          title = "Error in input data",
          "The sum of proportions for new cases should be 1",
          easyClose = TRUE,
          footer = NULL
        ))
      }
      
      if(is.null(initial_pop$data)) {
        validated=FALSE
        showModal(modalDialog(
          title = "Error in input data",
          "You haven't uploaded the initial population dataset",
          easyClose = TRUE,
          footer = NULL
        ))
      }
      
      if (validated) {
        
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
        
        initial_data=initial_pop$data
        verDF508=erDF508()
        dist_ages_newcases=rv2$data
        
        future({
          
          start_time=format(Sys.time(), "%Y-%m-%d %H:%M:%S")
          dataList=iteratingSimulations2(initial_data, start_date, end_date, nIter, period_length, new_F508, new_0F508, verDF508, dist_ages_newcases)
          end_time=format(Sys.time(), "%Y-%m-%d %H:%M:%S")
          
          dataList$times=paste("From ",start_time," to ",end_time)
          
          return(dataList)
        
        }, seed=TRUE) %...>% (
        function(result) {
        dataList=result
            dataList$qty = dataList$qty |> mutate(
              group=case_when(group=="cftr"~"CFTR modulator", 
                              group=="non_cftr"~"Non-CFTR modulator",
                              .default = NA)
            )
            
            dataList$km = dataList$km |> mutate(
              group=case_when(group=="cftr"~"CFTR modulator", 
                              group=="non_cftr"~"Non-CFTR modulator",
                              .default = NA)
            )
            
            simOutput$data=dataList
            barplot1$data=dataList$qty
            barplot2$data=dataList$qty
            hospiplot$data=dataList$qty
            times(dataList$times)
            scenario(paste("Results for ",input$scenarios," scenario",sep=""))
            
            remove_modal_gif()
            
          }
        ) %...!% (
          function(error) {
            remove_modal_gif()
            stop(error)
          }
        )
        
      }
      
    })
    
    
    observeEvent({
      input$genotypeFilter
      input$ageFilter_barplot1
      
      input$ageFilter_barplot2
      input$stateFilter
      
      input$ageFilter_hospi
      }, {
      
      databp1 = simOutput$data$qty
      databp2 = simOutput$data$qty
      databp3 = simOutput$data$qty
      # simOutputQty$data=applyFilters(datag, input$genotypeFilter, input$ageFilter, input$statusFilter)
      
      # Applying filters to databp1
      if (input$genotypeFilter!="All") {
        databp1 = databp1 |> filter(group==input$genotypeFilter)
      }
      
      if (input$ageFilter_barplot1!="All") {
        databp1 = databp1 |> filter(age_range==input$ageFilter_barplot1)
      }
      
      # Applying filters to databp2
      if (input$ageFilter_barplot2!="All") {
        databp2 = databp2 |> filter(age_range==input$ageFilter_barplot2)
      }
      
      if (input$stateFilter!="All") {
        databp2 = databp2 |> filter(state==input$stateFilter)
      }
      
      # Applying filters to databp3
      if (input$ageFilter_hospi!="All") {
        databp3 = databp3 |> filter(age_range==input$ageFilter_hospi)
      }
      
      barplot1$data=databp1
      barplot2$data=databp2
      hospiplot$data=databp3
      
    })
    
    observeEvent(input$scenarios, {
      
      eRs=erDF508()
      
      choices = c('Optimistic', 'Custom')
      selected_col <- match(input$scenarios, choices)
        
      eRs=as.numeric(rv$data[,selected_col+4] |> pull())/100

      erDF508(eRs)

    
    })
    
    # Transitions table
    output$transitions_table=renderDT(
      rv$data, 
      editable = list(target = "cell", disable = list(columns = c(1, 2, 3,4))),
      colnames = c("From", "To", "Assumptions","Coefficients", "Optimistic (%)", "Custom (%)"),
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
      
      if (as.numeric(info$value)<=100 & as.numeric(info$value)>=0 ) {
        # Update the reactive data with the new value
        rv$data[info$row, info$col + 1] <- as.numeric(info$value)
      } else {
       
        showModal(modalDialog(
          title = "Invalid percentage",
          "Percentages should be values between 0 and 100",
          easyClose = TRUE,
          footer = NULL
        ))
        
        # Revert the change
        output$transitions_table=renderDT(
          rv$data, 
          editable = list(target = "cell", disable = list(columns = c(1, 2, 3, 4))),
          colnames = c("From", "To", "Assumptions", "Coefficients", "Optimistic (%)", "Custom (%)"),
          rownames = FALSE,
          selection = 'none',
          options = list(dom = 't'),
          server = TRUE
        )
        
      }
    })
    
    # New cases age groups table
    output$newcases_groups=renderDT(
      rv2$data, 
      editable = list(target = "cell", disable = list(columns = c(0))),
      colnames = c("Age at Diagnosis Group", "%"),
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
        rv2$data[info$row, info$col + 1] <- as.numeric(info$value)
      
      } else {
        
        showModal(modalDialog(
          title = "Invalid proportion",
          "Proportions should be values between 0 and 1",
          easyClose = TRUE,
          footer = NULL
        ))
      
      }
      
      # Revert the change
      if (rv2$data[info$row, info$col + 1] != as.numeric(info$value)) {
      
        output$newcases_groups=renderDT(
          rv2$data, 
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
    
    # OUTPUT PLOTS
    
    output$selected_scenario <- renderText({
      
      scenario()
    })
    
    output$times <- renderText({
      
      times()
    })
    
    output$barplot1 <- renderPlot({
      
      if (!is.null(barplot1$data)) {
        
        datag= barplot1$data
        
        #start to plotting
        datag = datag |> filter(state != 'dead')
        
        datag = datag |>
          group_by(milestone, state) |>
          summarise(
            med=sum(med),
            .groups="drop"
          )
        
        p = ggplot(data= datag,aes(
          x = as.factor(milestone),
          y = med,
          fill = state
        )) +
          geom_bar(position = "stack", stat = "identity") +
          theme_classic() +
          labs(y = "Number of patients", x = "", fill = "") +
          theme(legend.position = "bottom")+
          theme(
            legend.text = element_text(size = 14),  # Adjust the size as needed
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14)
          )
        
        print(p)
        
      }
    })
    
    output$table2 = DT::renderDataTable({
      
      if (!is.null(barplot2$data)) {
        
        datag= barplot2$data
        
          datag |> filter(state != 'dead') |>
            group_by(group,milestone) |>
            summarise(
              subj=sum(med),
              .groups = "drop"
            ) |> arrange(milestone, group) |> select(
          milestone,
          group,
          Patients=subj,
        ) |> mutate (
          group=case_when(group=="CFTR modulator"~"CFTR", 
                          group=="Non-CFTR modulator"~"Non_CFTR",
                          )
        ) |> pivot_wider(names_from = group, values_from = Patients) |> mutate(
          Total=CFTR+Non_CFTR
        )
            
      }
      
      }, 
      selection = 'none',
      options = list(
        dom = 'pt',
        pageLength = 6
      ),
      rownames = FALSE,
      server = FALSE
    )
    
    
    output$kmPlot <- renderPlot({
      
      if (!is.null(simOutput$data)) {
        
        #start to plotting
        simOutput$data$km |> 
          ggplot(aes(x=time, y=survival, colour = group)) +
          geom_step(direction="hv") +
          geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray")+
          theme_classic() + 
          labs(y=paste("Survival Probaility at ",toYear(),sep=""), x="Time (Age)")+
          scale_colour_brewer(
            palette = "Set1",
            name=""
          )+
          theme(legend.position="bottom")+
          theme(
            legend.text = element_text(size = 14),  # Adjust the size as needed
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14)
          )+
          scale_x_continuous(limits = c(0, 80))
        
      }
        
    })
    
    output$hospPlot <- renderPlot({
      
      if (!is.null(hospiplot$data)) {
        
        datag= hospiplot$data
        
        #start to plotting
        datag = datag |> filter(state != 'dead' & state!="transplant")
        
        datag = datag |> mutate(
            hosp=case_when(state=="mild" & group=="Non-CFTR modulator"~round(med*0.09,0),
                           state=="moderate" & group=="Non-CFTR modulator"~round(med*0.9,0),
                           state=="severe" & group=="Non-CFTR modulator"~round(med*2.2,0),
                           state=="mild" & group=="CFTR modulator"~round(med*0.055,0),
                           state=="moderate" & group=="CFTR modulator"~round(med*0.55,0),
                           state=="severe" & group=="CFTR modulator"~round(med*1.35,0))
          ) |>
          group_by(milestone, state) |>
          summarise(
            hosp=sum(hosp),
            .groups="drop"
          )
        
        p = ggplot(data= datag,aes(
          x = as.factor(milestone),
          y = hosp,
          fill = state
        )) +
          geom_bar(position = "stack", stat = "identity") +
          theme_classic() +
          labs(y = "Number of exacerbations", x = "", fill = "") +
          theme(legend.position = "bottom")+
          theme(
            legend.text = element_text(size = 14),  # Adjust the size as needed
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14)
          )
        
        print(p)
        
      }
    })

}


