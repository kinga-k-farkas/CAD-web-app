
shinyUI(fluidPage(
      
      titlePanel(tags$div("CAD Anomaly Detection",id="main_title")),
      sidebarLayout(
            sidebarPanel(
                  # uiOutput('resetable_input'),
                  # tags$hr(),
                  radioButtons("upload_or_example", label = "Select an option:",choices = list("Upload a data set" = 1, "Use example data set" = 2), selected = 2),  
                  conditionalPanel("input.upload_or_example == 1",
                                   fileInput('the_file', 'Upload the csv file containing the time series  ',accept=c('.csv')),
                                   actionButton("enter_upload", "Process Upload"),
                                   conditionalPanel("input.enter_upload",
                                                    numericInput("col_number", "Enter the column number of the column containing the time series:", 1)
                                   ),
                                   conditionalPanel("input.enter_upload",
                                                    radioButtons("radio", label = "Choose the unit of time:",choices = list("Generic" = 1, "Days" = 2), selected = 1)           
                                   ),
                                   conditionalPanel("input.radio == 2 ",
                                                    dateInput('date',label = 'Enter beginning date',value = Sys.Date())
                                   ),
                                   
                                   conditionalPanel("input.enter_upload",
                                                    radioButtons("radio2", label = "Enter the type of anomaly:",choices = list("Upper" = 1, "Lower" = 2), selected = 1),
                                                    sliderInput("lambda", label = "Choose the parameter lambda", min = 1, 
                                                                max = 100, value = 5),
                                                    sliderInput("delta", label = "Choose the parameter delta", min = 0, 
                                                                max = 20, value = 3),
                                                    actionButton("goButton", "Find the Anomalies"),
                                                    actionButton("reset", "Reset")
                                   )
                  ),
                  conditionalPanel("input.upload_or_example == 2",
                                   actionButton("nextButton", "Next"),
                                   conditionalPanel("input.nextButton",
                                          numericInput("col_number_panel2", "Enter the column number of the column containing the time series:", 1)
                                   ),
                                   conditionalPanel("input.nextButton",
                                          radioButtons("radio_panel2", label = "Choose the unit of time:",choices = list("Generic" = 1, "Days" = 2), selected = 1)           
                                   ),
                                   conditionalPanel("input.radio_panel2 == 2",
                                          dateInput('date_panel2',label = 'Enter beginning date',value = Sys.Date())
                                   ),
                                   
                                   conditionalPanel("input.nextButton",
                                          radioButtons("radio2_panel2", label = "Enter the type of anomaly:",choices = list("Upper" = 1, "Lower" = 2), selected = 1),
                                          sliderInput("lambda_panel2", label = "Choose the parameter lambda", min = 1, 
                                                                max = 100, value = 5),
                                          sliderInput("delta_panel2", label = "Choose the parameter delta", min = 0, 
                                                                max = 20, value = 3),
                                                    actionButton("goButton_panel2", "Find the Anomalies"),
                                                    actionButton("reset_panel2", "Reset")
                                   )
                  )
            ),
            

            mainPanel(
                  tags$head(
                        tags$link(
                              rel = "stylesheet", 
                              href="http://fonts.googleapis.com/css?family=Monda"
                        ),
                 tags$style(type="text/css", "
                        #loadmessage {
                         position: fixed;
                         top: 100px;
                         left: 350px;
                         width: 50%;
                        padding: 5px 0px 5px 0px;
                        text-align: center;
                        font-weight: Bold;
                        font-size: 200%;
                        color: #2e73b8;
                        # background-color: #f2db0d;
                        z-index: 105;
             }, 
                        #main_title {
                         position: fixed;
                         top: 100px;
                         left: 350px;
                         width: 50%;
                        padding: 5px 0px 5px 0px;
                        text-align: center;
                        font-weight: Bold;
                        font-size: 200%;
                        color: #2e73b8;
                        # background-color: #f2db0d;
                        z-index: 105;
             }
          ")),
                  
                  h4(textOutput("error", container = span)), 
                  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                   tags$div("This might take a while...",id="loadmessage")),
                  p(textOutput("elapsed_time")),
                  p(textOutput("ts_length")),
                  plotOutput('p1'),
                  # tableOutput('the_df'),
                  h4(textOutput("no_solution", container=span)),
                  h4(textOutput("anomalies", container = span)),
                  p(textOutput("anomalies_list", container = span)),
                  # h4(textOutput("lower_anomalies1", container = span)),
                  # p(textOutput("lower_anomalies2", container = span)),
                  h2(textOutput("tester", container=span))
                  
            )
      )


))