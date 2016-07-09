source("plotter1_Function.R")
push_count<-1
shinyServer( 
  function(input, output) {
        # check_basic<-reactive({
        #       if (is.null(input$the_file$datapath)) return(100)
        #       0
        # })

 
        v <- reactiveValues(go = NULL, the_df=NULL, the_ts = NULL, the_ts_name=NULL, ready=0, error="blank")
        # df <- eventReactive(input$enter_upload, {
        #       if(is.null(input$the_file$datapath)) {
        #             return(NULL) 
        #       }    
        #       data.frame(read.csv(input$the_file$datapath))
        # })
      # df<-reactive({
      #       if(is.null(input$the_file$datapath))     return(NULL) 
      #       data.frame(read.csv(input$the_file$datapath))
      # })
        

        
        observeEvent(input$enter_upload, {
              if(is.null(input$the_file$datapath)) {
                    v$error<-"no data"
              } else v$the_df<-data.frame(read.csv(input$the_file$datapath))
        })
        # check<-reactive({
        #       #checks whether an invalid column number was entered
        #       if (is.null(input$the_file$datapath)) return(100)
        #       # if (input$col_number > ncol(v$the_df)) return(1) 
        #       # if (input$col_number <= 0) return(2)
        #       0
        # })
        
        observeEvent(input$goButton, {
              v$go <- 1
              if  (!(is.null(v$the_df)) ) {
                    if ( (input$col_number <= ncol(v$the_df)) & input$col_number > 0  ) {
                          v$the_ts<-v$the_df[,input$col_number]
                          v$the_ts_name<-names(v$the_df)[input$col_number]
                          if (is.numeric(v$the_ts) 
                              & (length (v$the_ts) >= 300)
                              & (length(v$the_ts) - sum(is.null(v$the_ts)) >= 300)){
                               v$error<-"no problem"
                               v$ready<-1
                               } else {
                                     v$ready<-0
                                     v$error<-"fewer than 300 entries"}
                    } else  {
                          v$error <- "invalid column number"
                          v$ready <- 0}
              }else{ 
                    v$error<-"no data"
                    v$ready <-0
              
                   } 
             
        })
        
        # v$ready<-reactive({
        #       if (v$error == "no problem") {
        #             if (is.numeric(v$the_ts) 
        #                 & (length (v$the_ts) >= 300)
        #                 & (length(v$the_ts) - sum(is.null(v$the_ts)) >= 300)) {
        #                   return(1)
        #             } else { 
        #                  v$error<-"fewer than 300 entries"
        #                  return(0)
        #              }
        #             } else  return(0)
        #       })
        
        observeEvent(input$reset,{
              v$go<-NULL
              v$the_df<-NULL
              v$the_ts <- NULL
              v$the_ts_name<-NULL
              v$error<-"blank"
              v$ready<-0
              
              
        })
        # observeEvent(input$goButton, {
        #       if (is.null(v$the_df)) { v$ready <-0
        #       # } else if (check() != 0) {v$ready <-0
        #       } else if (!is.numeric(v$the_ts)) {v$ready <-0
        #       } else if ( length (v$the_ts) < 300) {v$ready <-0
        #       } else if (length(v$the_ts) - sum(is.na(v$the_ts)) < 300) {v$ready<-0
        #       } else v$ready <- 1
        #       
        # })
        
        # ts_name <- reactive({
        #       if ( check() != 0 ) return(NULL)
        #       names(v$the_df)[input$col_number]
        # 
        # })
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
        # observeEvent(input$reset, {
        #       v$data <- NULL
        # }) 


      # ts <- reactive({
      #       if ( check() != 0 | check2() != 0 | check3() != 0) return(NULL)
      #       v$the_df[, input$col_number]
      #       
      # })
      # observeEvent(input$rnorm, {
      #       v$data <- rnorm(100)
      # })  
        # check2<-reactive({
        #       #checks whether the selected column contains non-numeric values
        #       if (is.null(input$the_file$datapath)) return(100)
        #       if (check()!=0 ) return(100)
        #       if (!is.numeric(v$the_ts)) return(1) 
        #       # if (input$col_number <= 0) return(2)
        #       0
        # })
        
        # check3<-reactive({
        #       #checks whether the selected column contains enough values and NA's
        #       if (is.null(input$the_file$datapath)) return(100)
        #       if (check()!=0 ) return(100)
        #       if (length (v$the_ts) < 300) return(1) 
        #       if (length(v$the_ts) - sum(is.na(v$the_ts)) < 300){
        #             return(2)  
        #       }
        #       0
        # })
      output$df<-renderTable({
            if (is.null(v$the_df)) return(NULL)
            if (nrow(v$the_df)>50) return(head(v$the_df, 20))
            head(v$the_df, nrow(v$the_df))
      })
      output$caption <- renderText({
            input$caption
      })
      output$radio_value<-renderText({
            input$radio
      })
      # output$col_count<-renderText({
      #       if (is.null(v$the_df)) return(NULL)
      #       ncol(v$the_df)
      # })
      output$error<-renderText({
            if (v$error == "no problem") return(NULL)
            if (v$error == "no data") return("Error, no data!")
            if(v$error == "invalid column number") return("Error, invalid column number.")
            if (v$error == "fewer than 300 entries"){
                  return("Error. The time series either has fewer than 300 entries or its entries are non-numeric. Here are its first few values:")   
            } 
            return(NULL)
            
      })
      # output$error2<-renderText({
      #       if (is.null(v$the_df)) return(NULL)
      #       if (check() == 0 & !is.numeric(v$the_ts)){
      #             return("Error,your column contains non-numeric values. Here are its first few values:")   
      #       } 
      #             
      #       if (check() == 0 & check2() == 0){
      #             return("OK, here are the first few of the time series' values:") 
      #       } 
      #       NULL
      #       
      #       
      # })
      # output$error3<-renderText({
      #       if (is.null(v$the_df)) return(NULL)
      #       if (check() != 0 | check2() != 0){
      #             return("NULL")   
      #       } 
      #       
      #       if ( check3() == 1 ){
      #             return("Error. The uploaded time series contains fewer than 300 entries.") 
      #       } 
      #       
      #       if (check3()==2) {
      #             return("Error. There were too many missing entries in the time series.")
      #       }
      #       NULL
      #       
      # 
      # })
      
      # output$added1<-renderText({
      #       if (is.null(v$the_df)) return(NULL)
      #       if (check() != 0 | check2() != 0 | check3() != 0) return(NULL)
      #       adder(ts())
      #       
      # })
      # output$p1 <- eventReactive(input$goButton,{
      #       renderPlot({
      #             
      #             if (is.null(v$the_df)) return(NULL)
      #             if (check() != 0 | check2() != 0 | check3() != 0){
      #                   g<-plot(c(1:100))
      #                   return(g) 
      #             } 
      #             plotter1(ts(), input$delta, input$lambda)
      #             # input$goButton
      #             # isolate(plotter1(ts())) 
      #       })      
      # })
     
      # output$p1<-renderPlot({
      # 
      #       if (is.null(v$the_df)) return(NULL)
      #       if (check() != 0 | check2() != 0 | check3() != 0){
      #             g<-plot(c(1:100))
      #             return(g)
      #       }
      #       plotter1(ts(), input$delta, input$lambda)
      #       # input$goButton
      #       # isolate(plotter1(ts()))
      # })
      
      output$p1 <- renderPlot({
            if (is.null(v$go)) return()
            if (v$ready==0) return()
            # push_count <<- push_count + 1
            # cat(push_count)
            plotter1(v$the_ts, d=d(), l=l(),type1=tp(), type2= date_type(), caption=v$the_ts_name)
      })
      output$the_df<-renderTable({
            if (is.null(v$the_df)) return(NULL)
            if (v$ready==0) return(NULL)
            
            head(v$the_df[,input$col_number],10)
      })
      output$ts_entries<-renderText({
            if (v$error == "no problem" & v$ready==1) return(as.character(v$the_df[1:10,input$col_number]))
            if (v$error == "no data") return(NULL)
            if (v$error == "invalid column number") return(NULL)
            if (v$error == "fewer than 300 entries"){
                  return(as.character(v$the_df[1:10,input$col_number]))   
            } 
            return(NULL)

      })
      output$tester <-renderText({
            if (v$ready==0 ) return()
            v$error
      })
})