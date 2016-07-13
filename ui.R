
shinyUI(fluidPage(
      
      titlePanel("CAD Anomaly Detection"),
      sidebarLayout(
            sidebarPanel(
                   

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
            

            mainPanel(
                  h4(textOutput("error", container = span)), 
                  p(textOutput("ts_entries")),
                  plotOutput('p1'),
                  # tableOutput('the_df'),
    
                  h4(textOutput("upper_anomalies1", container = span)),
                  p(textOutput("upper_anomalies2", container = span)),
                  h4(textOutput("lower_anomalies1", container = span)),
                  p(textOutput("lower_anomalies2", container = span))
                  # h2(textOutput("tester", container=span))
                  
            )
      )


))