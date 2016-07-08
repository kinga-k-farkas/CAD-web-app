
# dateInput('date',label = 'Date input: yyyy-mm-dd',value = Sys.Date()),
# textInput("caption", "Graph's Caption:", "X Time Series Anomaly Detection"),
# radioButtons("time_r", label = h3("Unit of time"),
#              choices = list("Days" = 1, "Generic" = 2), 
#              selected = 1),
# 
#fileInput('the_file', 'Upload csv file',accept=c('.csv'))

shinyUI(fluidPage(
      
      titlePanel("CAD Anomaly Detection"),
      sidebarLayout(
            sidebarPanel(
                   
                  # textInput("caption", "Enter the Graph's Title:", "X Time Series Anomaly Detection"),

                  fileInput('the_file', 'Upload the csv file containing the time series  ',accept=c('.csv')),
                  # conditionalPanel("!is.null(output.the_df)", 
                  #                  numericInput("col_number", "Enter the column number of the column containing the time series:", 1)
                  #                  )
                  numericInput("col_number", "Enter the column number of the column containing the time series:", 1),
                  radioButtons("radio", label = "Choose the unit of time:",choices = list("Generic" = 1, "Days" = 2), selected = 1),
                  conditionalPanel("input.radio == 2 ",
                                   dateInput('date',label = 'Enter beginning date',value = Sys.Date())
                                   # textOutput('radio_value')
                  ),
                  radioButtons("radio2", label = "Enter the type of anomaly:",choices = list("Upper" = 1, "Lower" = 2), selected = 1),
                  sliderInput("lambda", label = "Choose the parameter lambda", min = 1, 
                              max = 100, value = 5),
                  sliderInput("delta", label = "Choose the parameter delta", min = 0, 
                              max = 20, value = 3),
                  actionButton("goButton", "Enter")
                  
                  
            ),
            

            mainPanel(
                  # h3(textOutput("caption", container = span)),
                  h4(textOutput("error1", container = span)), 
                  h4(textOutput("error2", container = span)), 
                  # tableOutput('the_df')
                  p(textOutput("ts_entries")),
                  h4(textOutput("error3", container = span)),
                  p(textOutput('added1')),
                  h2(textOutput("tester", container=span)),
                  plotOutput('p1')
            )
      )

      # column(5,
      #        "The plot below will be not displayed when the slider value",
      #        "is less than 50.",
      #        
      #        # With the conditionalPanel, the condition is a JavaScript
      #        # expression. In these expressions, input values like
      #        # input$n are accessed with dots, as in input.n
      #        conditionalPanel("input.n >= 50",
      #                         plotOutput("scatterPlot", height = 300)
      #        )
      # )
))