#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinyjs)
library(bslib)
library(ggplot2)
library(bsicons)
library(htmltools)
library(DT)
library(gt)

source("modules/transitionRatesTab.R")
source("modules/outputTab.R")
source("modules/uploadFileCard.R")
source("modules/modelGraph.R")


# Custom JavaScript to send a blur event back to Shiny
# Shiny.setInputValue('text_input_blur', Math.random());
js <- "
$(document).on('shiny:connected', function(event) {
  $('#to').on('blur', function() {
       if ($('#to').val()-$('#from').val()>25 | $('#to').val()-$('#from').val()<0) {
          $('#valTo').modal('show');
          $('#to').val(parseFloat($('#from').val())+5);
       }
  });
  $('#newCases').on('blur', function() {
       if ($('#newCases').val()>150 | $('#newCases').val()<0) {
          $('#valNewCases').modal('show');
          $('#newCases').val('87');
       }
  });
  $('#prop508').on('blur', function() {
       if ($('#prop508').val()>100 | $('#prop508').val()<0) {
          $('#valPct').modal('show');
          $('#prop508').val('96.7');
       }
  });
  $('#breaks').on('blur', function() {
       if ($('#breaks').val()>($('#to').val()-$('#from').val()) | $('#breaks').val()<=0) {
          $('#valBreaks').modal('show');
          $('#breaks').val('2');
       }
  });
  $('#nSim').on('blur', function() {
       if ($('#nSim').val()>1000 | $('#nSim').val()<=0) {
          $('#valIters').modal('show');
          $('#nSim').val('10');
       }
  });
  
  
  // Initially hide the card if 'Hide Card' is selected by default
  if ($('input[name=rb_initial_population]:checked').val() === 'canada') {
      $('#initial_data_card').hide()
  }
  
  $('input[name=rb_initial_population]').on('change', function() {
       if ($(this).val() ==='canada') {
          $('#initial_data_card').hide()
       } else {
          $('#initial_data_card').show()
       }
  });
  
  $('#runSim').on('click', function() {

    const tabPaneDivs = $('div.tab-pane');
    output=tabPaneDivs.eq(0)
    rates=tabPaneDivs.eq(1)

    if (rates.hasClass('active')) {
      output.addClass('active show')
      rates.removeClass('active show')
    }
    
    const tabLinks = $('a.nav-link');
    outputLink=tabLinks.eq(0)
    ratesLink=tabLinks.eq(1)
    
    if (ratesLink.hasClass('active')) {
      outputLink.addClass('active')
      ratesLink.removeClass('active')
    }
    
  });
  
});
"

validation_modal <- function(id,msg) {
  
  return (
    paste("
      <div id='",id,"' class='modal fade' style='display: none;' aria-hidden='true'>
      	<div class='modal-dialog modal-confirm'>
      		<div class='modal-content'>
      			<div class='modal-body text-center'>
      				<h4>Ooops!</h4>	
      				<p>",msg,"</p>
      			</div>
      		</div>
      	</div>
      </div>
    ",sep="")
  )
}

foot <- popover(
  bs_icon("question-square-fill"),
  "Predetermined initial data come from Canadian CF Registry. You can 
  change the year and the initial population uploading a file with the 
  required information. You can download the template ", a("here", href = "resources/template_initial_data.csv")
)

app_details <- popover(
  HTML(
    "Cystic Fibrosis population forecasting")
  ,
  "Many more to say"
)

opencollective <-
  HTML(
    '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-piggy-bank" viewBox="0 0 16 16">
  <path d="M5 6.25a.75.75 0 1 1-1.5 0 .75.75 0 0 1 1.5 0m1.138-1.496A6.6 6.6 0 0 1 7.964 4.5c.666 0 1.303.097 1.893.273a.5.5 0 0 0 .286-.958A7.6 7.6 0 0 0 7.964 3.5c-.734 0-1.441.103-2.102.292a.5.5 0 1 0 .276.962"/>
  <path fill-rule="evenodd" d="M7.964 1.527c-2.977 0-5.571 1.704-6.32 4.125h-.55A1 1 0 0 0 .11 6.824l.254 1.46a1.5 1.5 0 0 0 1.478 1.243h.263c.3.513.688.978 1.145 1.382l-.729 2.477a.5.5 0 0 0 .48.641h2a.5.5 0 0 0 .471-.332l.482-1.351c.635.173 1.31.267 2.011.267.707 0 1.388-.095 2.028-.272l.543 1.372a.5.5 0 0 0 .465.316h2a.5.5 0 0 0 .478-.645l-.761-2.506C13.81 9.895 14.5 8.559 14.5 7.069q0-.218-.02-.431c.261-.11.508-.266.705-.444.315.306.815.306.815-.417 0 .223-.5.223-.461-.026a1 1 0 0 0 .09-.255.7.7 0 0 0-.202-.645.58.58 0 0 0-.707-.098.74.74 0 0 0-.375.562c-.024.243.082.48.32.654a2 2 0 0 1-.259.153c-.534-2.664-3.284-4.595-6.442-4.595M2.516 6.26c.455-2.066 2.667-3.733 5.448-3.733 3.146 0 5.536 2.114 5.536 4.542 0 1.254-.624 2.41-1.67 3.248a.5.5 0 0 0-.165.535l.66 2.175h-.985l-.59-1.487a.5.5 0 0 0-.629-.288c-.661.23-1.39.359-2.157.359a6.6 6.6 0 0 1-2.157-.359.5.5 0 0 0-.635.304l-.525 1.471h-.979l.633-2.15a.5.5 0 0 0-.17-.534 4.65 4.65 0 0 1-1.284-1.541.5.5 0 0 0-.446-.275h-.56a.5.5 0 0 1-.492-.414l-.254-1.46h.933a.5.5 0 0 0 .488-.393m12.621-.857a.6.6 0 0 1-.098.21l-.044-.025c-.146-.09-.157-.175-.152-.223a.24.24 0 0 1 .117-.173c.049-.027.08-.021.113.012a.2.2 0 0 1 .064.199"/>
</svg>'
  )


# Define UI for application that draws a histogram
page_sidebar(
  useShinyjs(),
  # theme=my_theme,
  # Include the custom CSS file
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$link(rel="stylesheet", href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css", integrity="sha384-QWTKZyjpPEjISv5WaRU9OFeRpok6YctnYmDr5pNlyT2bRjXh0JMhjY6hW+ALEwIH", crossorigin="anonymous"),
    tags$script(HTML(js)),
  ),
  
  tags$body(
    tags$div(
      HTML(validation_modal("valTo","Forecast cannot be longer than 25 years")),
      HTML(validation_modal("valNewCases","It should be a value between 1 and 150")),
      HTML(validation_modal("valPct","It should be a value between 0 and 100")),
      HTML(validation_modal("valBreaks","It should be a value less than the length of the forecast and it cannot be negative")),
      HTML(validation_modal("valIters","Tha maximum number of iterations is 1000 and it should be greater than 0")),
      ),
    tags$script(src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js", integrity="sha384-YvpcrYf0tY3lHB60NNkmXc5s9fDVZLESaAA55NDzOxhy9GkcIdslK1eN7N6jIeHz", crossorigin="anonymous")
  ),
  
  title= span(
           app_details
         ),
  
  # helpText("Brief explanation"),
  
  # Inputs: Select variables to plot
  sidebar = sidebar(
    #Properties of sidebar"
    #title = "Input data",

    width = 400,
    #Controls
    tags$h5("Population Settings"),
    radioButtons("rb_initial_population",NULL,
                 choiceNames = list(
                   HTML("<span class='py-3 pe-5'>
                          <strong class='fw-semibold'>Use 2021 Canadian CF Population</strong>
                          <span class='d-block small opacity-75'>Some other text goes here</span>
                        </span>"),
                   HTML("<span class='py-3 pe-5'>
                          <strong class='fw-semibold'>Use your own data</strong>
                          <span class='d-block small opacity-75'>Some other text goes here</span>
                        </span>")
                 ),
                 choiceValues = list(
                   "canada", "own"
                 )
    ),
    
    card(
      fill = FALSE,
      full_screen = FALSE,
      id="initial_data_card",
      card_header(
        tags$h5(
          span(
            "Load Initial Population",
            foot
          )
        )
      ),
      card_body(
        layout_columns(
          col_widths = c(8, 4),
          fileInput(
            inputId="initial_data",
            label=NULL,
            multiple = FALSE,
            buttonLabel = "Browse...",
            placeholder = "No file selected (.csv)",
            accept = c(
              "csv",
              "comma-separated-values,plain",
              ".csv")
          ),
          actionButton(inputId="btnInitialData" ,label = "Load", icon = NULL, width = NULL)
        )
      )
    ),
    
    tags$h5("New cases"),
    
    layout_columns(
      col_widths = c(8, 4),
      "New diagnosis: ",
      numericInput(
        inputId = "newCases",
        label = NULL,
        value = 87,
        min = 0, 
        max = 150,
        step = 1,
        width=90
      )
    ),
    layout_columns(
      col_widths = c(8, 4),
      "% CFTR modulator eligible: ",
      numericInput(
        inputId = "prop508",
        label = NULL,
        value = 97.6,
        step = 0.1,
        min = 0, 
        max = 100,
        width=90
      )
    ),
    
    fluidRow(
      column(12,
         DTOutput("newcases_groups")
      )
    ),
    
    
    tags$h5("Simulation Details"),
    
    layout_columns(
      shinyjs::disabled(
        numericInput(
          inputId = "from",
          label =  tooltip(
            trigger = list(
              "From:",
              bs_icon("info-circle")
            ),
            "To change start date you need to load initial data"
          ),
          value = 2021,
          step = 1,
        )
      ), 
      numericInput(
        inputId = "to",
        label =  tooltip(
          trigger = list(
            "To:",
            bs_icon("info-circle")
          ),
          "Up to 25+"
        ),
        value = as.integer(format(Sys.Date(), "%Y"))+1,
        step = 1,
      )
    ),
    
    span(
      "Number of simulations: ",
      tags$div(
        numericInput(
          inputId = "nSim",
          label = NULL,
          value = 5,
          step = 10,
          width=100
        ),
        style="display:inline-block"
      )
    ),
    
    span(
      "Display results by ",
      tags$div(
        numericInput(
          inputId = "breaks",
          label = NULL,
          value = 2,
          step = 1,
          width=55
          ),
        style="display:inline-block"
      ),
      " years intervals"
    ),
    
    # selectInput(
    #   inputId="genotypeFilter2",
    #   label = "Genotype",
    #   choices = c('All', 'Delta 508', 'Non-delta 508'),
    # ),
    # bslib::input_action_button("runSim", "Run Simulations"),
    actionButton("runSim", "Run Simulations"),
    tags$div(
      tags$img(src="icons8-github-24.png"),
      tags$br(),
      "Respiratory Epidemiology Research Team",
      tags$br(),
      "Dalhousie University",
      tags$br(),
      HTML("<a target='_blank' href='https://icons8.com/icon/106562/github'>GitHub</a> icon by <a target='_blank' href='https://icons8.com'>Icons8</a>"),
      class="respi-footer"
    ),
  ),
  # Output: Show scatterplot
  navset_card_underline(
    id= 'tabs',
    title = "",
    nav_panel("Output", outputTab("outputTab")),
    nav_panel("Transition Rates", transitionRatesTab("transitionTab"))
  )
)
