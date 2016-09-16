source("plotter1_Function.R")
source("cad_function.R")
source("anomaly_finder_function.R")
source("result_plotter_function_generic.R")
source("normality_finder_function.R")
source("checker_function.R")
source("parameter_finder_function.R")
source("show_trainer_normality_function.R")
source("training_data_finder_function.R")
source("evaluate_cusum_results_function.R")
source("find_turning_points_function.R")
source("find_intervals_function.R")
source("df_maker_function3.R")
source("plotter_g1_function.R")
beginning<-0
example_df<-data.frame(read.csv("ExampleDF.csv"))
shinyServer( 
  function(input, output, session) {

        v <- reactiveValues(go = NULL, the_df=NULL, the_ts = NULL, 
                            the_ts_name=NULL, ready=0, error="blank", t = NULL)
        
        # observeEvent(input$enter_upload, {
        #       if (input$upload_or_example == 1){
        #            if(is.null(input$the_file$datapath)) {
        #                  v$error<-"no data"
        #            } else {
        #                  v$the_df<-data.frame(read.csv(input$the_file$datapath)) 
        #            }
        #       }
        #       if (input$upload_or_example == 2) {
        #             v$the_df<-data.frame(example_df)
        #             v$t <- "valami megy"
        #       }
        # })
        
        ##################### OBSERVE EVENT UPLOAD AND ITS EVENT REACTIVES ####################
        observeEvent(input$enter_upload, {
              if(is.null(input$the_file$datapath)) {
                   v$error<-"no data"
            } else {
                   v$the_df<-data.frame(read.csv(input$the_file$datapath))
            }
        })
        
        
        observeEvent(input$goButton, {
              v$go <- 1
              # v$t <- "tovabb megy"
              if  (!(is.null(v$the_df)) ) {
                    if ( (input$col_number <= ncol(v$the_df)) & input$col_number > 0  ) {
                          v$the_ts<-v$the_df[,input$col_number]
                          v$the_ts_name<-names(v$the_df)[input$col_number]
                          if (is.numeric(v$the_ts) 
                              & (length (v$the_ts) >= 100)
                              & (length(v$the_ts) - sum(is.null(v$the_ts)) >= 100)){
                                v$error<-"no problem"
                                v$ready<-1
                          } else {
                                v$ready<-0
                                v$error<-"fewer than 100 entries"}
                    } else  {
                          v$error <- "invalid column number"
                          v$ready <- 0}
              }else{ 
                    v$error<-"no data"
                    v$ready <-0
              } 
        })
        
        observeEvent(input$reset,{
              v$go<-NULL
              v$the_df<-NULL
              v$the_ts <- NULL
              v$the_ts_name<-NULL
              v$error<-"blank"
              v$ready<-0
        })
        
        testing<-eventReactive(input$goButton, {
              if (input$upload_or_example == 2) return("two")
              return("one")
        })
        
        d<-eventReactive(input$goButton, {
              input$delta
        })
        l<-eventReactive(input$goButton, {
              input$lambda
        })
        tp<-eventReactive(input$goButton, {
              if (input$radio2 == 1) return("upper")
              "lower"
              
        })
        date_type<-eventReactive(input$goButton,{
              if (input$radio == 1) return("generic")
              "days"
        })
        the_date <- eventReactive(input$goButton, {
              if (input$radio == 1) return(NULL)
              return(as.character(input$date))
        })
        the_year <- eventReactive(input$goButton, {
              if (input$radio == 1) return(2012)
              return(substr(as.character(input$date), 1, 4))
        })
        the_month <- eventReactive(input$goButton, {
              if (input$radio == 1) return(11)
              return(substr(as.character(input$date), 6, 7))
        })
        the_day <- eventReactive(input$goButton, {
              if (input$radio == 1) return(11)
              return(substr(as.character(input$date), 9, 10))
        })
        

    
    
        
      #################### OBSERVE EVENT AND USE EXAMPLE DATA EVENT REACTIVES ############################
        
        observeEvent(input$nextButton,{
              v$the_df<-data.frame(example_df)
              v$t<-"megy"
        } )
            
        observeEvent(input$goButton_panel2, {
              v$go <- 1
              v$t <- "tovabb megy"
              if  (!(is.null(v$the_df)) ) {
                    if ( (input$col_number_panel2 <= ncol(v$the_df)) & input$col_number_panel2 > 0  ) {
                          v$the_ts<-v$the_df[,input$col_number_panel2]
                          v$the_ts_name<-names(v$the_df)[input$col_number_panel2]
                          if (is.numeric(v$the_ts) 
                              & (length (v$the_ts) >= 100)
                              & (length(v$the_ts) - sum(is.null(v$the_ts)) >= 100)){
                                v$error<-"no problem"
                                v$ready<-1
                          } else {
                                v$ready<-0
                                v$error<-"fewer than 100 entries"}
                    } else  {
                          v$error <- "invalid column number"
                          v$ready <- 0}
              }else{ 
                    v$error<-"no data"
                    v$ready <-0
              } 
        })
        
        observeEvent(input$reset_panel2,{
              v$go<-NULL
              v$the_df<-NULL
              v$the_ts <- NULL
              v$the_ts_name<-NULL
              v$error<-"blank"
              v$ready<-0
        })
        
        testing<-eventReactive(input$goButton_panel2, {
              if (input$upload_or_example == 2) return("two")
              return("one")
        })
        
        d<-eventReactive(input$goButton_panel2, {
              input$delta_panel2
        })
        l<-eventReactive(input$goButton_panel2, {
              input$lambda_panel2
        })
        tp<-eventReactive(input$goButton_panel2, {
              if (input$radio2_panel2 == 1) return("upper")
              "lower"
              
        })
        date_type<-eventReactive(input$goButton_panel2,{
              if (input$radio_panel2 == 1) return("generic")
              "days"
        })
        the_date <- eventReactive(input$goButton_panel2, {
              if (input$radio_panel2 == 1) return(NULL)
              return(as.character(input$date_panel2))
        })
        the_year <- eventReactive(input$goButton_panel2, {
              if (input$radio_panel2 == 1) return(2012)
              return(substr(as.character(input$date_panel2), 1, 4))
        })
        the_month <- eventReactive(input$goButton_panel2, {
              if (input$radio_panel2 == 1) return(11)
              return(substr(as.character(input$date_panel2), 6, 7))
        })
        the_day <- eventReactive(input$goButton_panel2, {
              if (input$radio_panel2 == 1) return(11)
              return(substr(as.character(input$date_panel2), 9, 10))
        })
        
        

        
       
   ########################### REACTIVES #############################     
      the_results<-reactive({
            if (is.null(v$go)) return()
            if (v$ready==0) return()
            # withProgress(message = 'Searching for the anomalies    ', detail = 'This may take a while...', max = 100, value = 0, {
            #       for (i in 1:35) {
            #             # incProgress(1/35, detail = paste("progress", i))
            #             setProgress(1)
            #             Sys.sleep(0.1)
            #       }
            # })
            # 

            Sys.sleep(2)
            #ptm <<- proc.time()

            # if (v$ready!=0) v$t<-"running CAD"
            cad(v$the_ts, type=tp(),delta=d(), lambda=l(), year=the_year(),mo=the_month(),day=the_day(),
                main_title="The Time Series with Anomalies in Red")
            
            
      })
      the_anomalies<-reactive({
            if (is.null(v$go)) return()
            if (v$ready==0) return()
            if (is.null(the_results)) return(NULL)
            return(unique(c(the_results()$x4,the_results()$x5)))
      })
      the_length<-reactive({
            if (is.null(v$go)) return()
            if (v$ready==0) return()
            paste("The time series length is: ",length(v$the_ts))
      })
      # elapsed_time<-reactive({
      #       if (is.null(v$go)) return()
      #       if (v$ready==0) return()
      #       Sys.time()
      # })
      output$elapsed_time <- renderText({
            if (is.null(v$go)) return()
            if (v$ready==0) return()
            if (the_results()$no_solution) return()
            # invalidateLater(1000, session)
            paste("The elapsed time is:", the_results()$elapsed_time  )
      })
      
      output$ts_length<-renderText({
            if (is.null(v$go)) return()
            if (v$ready==0) return()
            the_length()
      })

      # output$df<-renderTable({
      #       if (is.null(v$the_df)) return(NULL)
      #       if (nrow(v$the_df)>50) return(head(v$the_df, 20))
      #       head(v$the_df, nrow(v$the_df))
      # })
      output$caption <- renderText({
            input$caption
      })
      output$radio_value<-renderText({
            input$radio
      })

      output$error<-renderText({
            if (v$error == "no problem") return(NULL)
            if (v$error == "no data") return("Error, no data!")
            if(v$error == "invalid column number") return("Error, invalid column number.")
            if (v$error == "fewer than 100 entries"){
                  return("Error. The time series either has fewer than 100 entries or its entries are non-numeric. Here are its first few values:")   
            } 
            return(NULL)
            
      })

      output$p1 <- renderPlot({
            if (is.null(v$go)) return()
            if (v$ready==0) return()
            #if (the_results()$no_solution) return()
            # withProgress(message = 'Creating plot', value = 0, {
            #       Sys.sleep(0.04)
            #       incProgress(0.003)
            #       })
            plotter_g1(the_results()$x1, caption=v$the_ts_name, x_axis=date_type())

 

            # plotter1(v$the_ts, d=d(), l=l(),type1=tp(), type2= date_type(), caption=v$the_ts_name)
      })
      output$the_df<-renderTable({
            if (is.null(v$the_df)) return(NULL)
            if (v$ready==0) return(NULL)
            if (is.null(the_results)) return(NULL)
            return(head(the_results()$x1, 100))
            
      })
      output$ts_entries<-renderText({
            if (v$error == "no problem" & v$ready==1) return(NULL)
            if (v$error == "no data") return(NULL)
            if (v$error == "invalid column number") return(NULL)
            if (v$error == "fewer than 100 entries"){
                  return(as.character(v$the_df[1:10,input$col_number]))   
            } 
            return(NULL)

      })
      output$tester <-renderText({
            # if (v$ready==0 ) return()
            return(v$t)
      })
      output$no_solution <-renderText({
            if (v$ready==0 ) return()
            if (is.null(v$go)) return()
            if (the_results()$no_solution) return("No anomalies were found by CAD.")
            return()
            
      })
      output$anomalies<-renderText({
            if (is.null(v$the_df)) return(NULL)
            if (v$ready==0) return(NULL)
            if (is.null(the_results)) return(NULL)
            if (is.null(the_results()$x4) & is.null(the_results()$x5)) return(NULL)
            return(paste("The ", tp(),  " anomalies occur at: "))
            
      })
      output$anomalies_list<-renderText({
            if (is.null(v$the_df)) return(NULL)
            if (v$ready==0) return(NULL)
            if (is.null(the_results)) return(NULL)
            if (is.null(the_results()$x4) & is.null(the_results()$x5)) return(NULL)
            return(the_anomalies())
            
      })
      
      
      output$resetable_input <- renderUI({
            actionButton("button2","Reset text and this button")
            # times <- input$reset
            # cat(times)
            # div(id=letters[(times %% length(letters)) + 1],
            #     numericInput("mynumber", "Enter a number", 20),
            #     textInput("mytext", "Enter a text", "test"))
      })
      # output$upper_anomalies2<-renderText({
      #       if (is.null(v$the_df)) return(NULL)
      #       if (v$ready==0) return(NULL)
      #       if (is.null(the_results)) return(NULL)
      #       if (is.null(the_results()$x4)) return(NULL)
      #       return(the_results()$x4)
      # 
      # })
      # 
      # output$lower_anomalies1<-renderText({
      #       if (is.null(v$the_df)) return(NULL)
      #       if (v$ready==0) return(NULL)
      #       if (is.null(the_results)) return(NULL)
      #       if (is.null(the_results()$x5)) return(NULL)
      #       return("The lower anomalies occur at: ")
      #       
      # })
      # output$lower_anomalies2<-renderText({
      #       if (is.null(v$the_df)) return(NULL)
      #       if (v$ready==0) return(NULL)
      #       if (is.null(the_results)) return(NULL)
      #       if (is.null(the_results()$x5)) return(NULL)
      #       return(the_results()$x5)
      # 
      # 
      # })
})