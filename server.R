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
shinyServer( 
  function(input, output, session) {

        v <- reactiveValues(go = NULL, the_df=NULL, the_ts = NULL, 
                            the_ts_name=NULL, ready=0, error="blank")
        
        observeEvent(input$enter_upload, {
              if(is.null(input$the_file$datapath)) {
                    v$error<-"no data"
              } else v$the_df<-data.frame(read.csv(input$the_file$datapath))
        })

        
        observeEvent(input$goButton, {
              v$go <- 1
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

        d<-eventReactive(input$goButton, {
              input$delta
        })
        l<-eventReactive(input$goButton, {
              input$lambda
        })
        tp<-eventReactive(input$goButton, {
              input$radio2
        })
        date_type<-eventReactive(input$goButton,{
              if (input$radio == 1) return("generic")
              "days"
        })
        the_date <- eventReactive(input$goButton, {
              if (input$radio == 1) return(NULL)
              return(as.character(input$date))
        })
        
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
            beginning<-Sys.time()
            Sys.sleep(2)
            cad(v$the_ts, type="upper",delta=d(), lambda=l(),  main_title="The Time Series with Anomalies in Red")
      })
      
      output$current_time <- renderText({
            if (is.null(v$go)) return()
            if (v$ready==0) return()
            # invalidateLater(1000, session)
            paste("The elapsed time is:",  Sys.time() - beginning )
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
            if (the_results()$no_solution) return()
            # withProgress(message = 'Creating plot', value = 0, {
            #       Sys.sleep(0.04)
            #       incProgress(0.003)
            #       })
            plotter_g1(the_results()$x1, caption=v$the_ts_name)

 

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
            if (v$ready==0 ) return()
            as.character(the_results()$no_solution)
      })
      output$no_solution <-renderText({
            if (v$ready==0 ) return()
            if (is.null(v$go)) return()
            if (the_results()$no_solution) return("No training set could be found, so CAD could not do a search for the anomalies.")
            return( "There should be a solution.")
            
      })
      output$upper_anomalies1<-renderText({
            if (is.null(v$the_df)) return(NULL)
            if (v$ready==0) return(NULL)
            if (is.null(the_results)) return(NULL)
            if (is.null(the_results()$x4)) return(NULL)
            return("The upper anomalies occur at: ")
            
      })
      output$upper_anomalies2<-renderText({
            if (is.null(v$the_df)) return(NULL)
            if (v$ready==0) return(NULL)
            if (is.null(the_results)) return(NULL)
            if (is.null(the_results()$x4)) return(NULL)
            return(the_results()$x4)

      })

      output$lower_anomalies1<-renderText({
            if (is.null(v$the_df)) return(NULL)
            if (v$ready==0) return(NULL)
            if (is.null(the_results)) return(NULL)
            if (is.null(the_results()$x5)) return(NULL)
            return("The lower anomalies occur at: ")
            
      })
      output$lower_anomalies2<-renderText({
            if (is.null(v$the_df)) return(NULL)
            if (v$ready==0) return(NULL)
            if (is.null(the_results)) return(NULL)
            if (is.null(the_results()$x5)) return(NULL)
            return(the_results()$x5)


      })
})