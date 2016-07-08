source("plotter1_Function.R")
source("adder_Function.R")
push_count<-1
shinyServer( 
  function(input, output) {
      df<-reactive({
            if(is.null(input$the_file$datapath))     return(NULL) 
            data.frame(read.csv(input$the_file$datapath))
      })
      check1<-reactive({
            #checks whether an invalid column number was entered
            if (is.null(df())) return(100)
            if (input$col_number > ncol(df())) return(1) 
            if (input$col_number <= 0) return(2)
            0
      })
      check2<-reactive({
            #checks whether the selected column contains non-numeric values
            if (is.null(df())) return(100)
            if (check1()!=0 ) return(100)
            if (!is.numeric(df()[,input$col_number ])) return(1) 
            # if (input$col_number <= 0) return(2)
            0
      })
      
      check3<-reactive({
            #checks whether the selected column contains enough values and NA's
            if (is.null(df())) return(100)
            if (check1()!=0 ) return(100)
            if (length (df()[,input$col_number ]) < 300) return(1) 
            if (length(df()[,input$col_number ]) - sum(is.na(df()[,input$col_number ])) < 300){
                return(2)  
            }
            0
      })
      ts <- reactive({
            if ( check1() != 0 | check2() != 0 | check3() != 0) return(NULL)
            df()[, input$col_number]
            
      })
      
      v <- reactiveValues(go = 0)
      
      observeEvent(input$goButton, {
            v$go <- v$go+1
      })
      
      # observeEvent(input$rnorm, {
      #       v$data <- rnorm(100)
      # })  
      
      output$the_df<-renderTable({
            if (is.null(df())) return(NULL)
            if (nrow(df())>50) return(head(df(), 20))
            head(df(), nrow(df()))
      })
      output$caption <- renderText({
            input$caption
      })
      output$radio_value<-renderText({
            input$radio
      })
      output$col_count<-renderText({
            if (is.null(df())) return(NULL)
            ncol(df())
      })
      output$error1<-renderText({
            if (is.null(df())) return(NULL)
            if (check1() == 1) return("Error, you have entered an invalid column number.")
            if(check1()==2) return("Please enter a positive value for your column number.")
            return(NULL)
            
      })
      output$error2<-renderText({
            if (is.null(df())) return(NULL)
            if (check1() == 0 & check2() == 1){
                  return("Error,your column contains non-numeric values. Here are its first few values:")   
            } 
                  
            if (check1() == 0 & check2() == 0){
                  return("OK, here are the first few of the time series' values:") 
            } 
            NULL
            
            
      })
      output$error3<-renderText({
            if (is.null(df())) return(NULL)
            if (check1() != 0 | check2() != 0){
                  return("NULL")   
            } 
            
            if ( check3() == 1 ){
                  return("Error. The uploaded time series contains fewer than 300 entries.") 
            } 
            
            if (check3()==2) {
                  return("Error. There were too many missing entries in the time series.")
            }
            NULL
            
    
      })
      
      output$added1<-renderText({
            if (is.null(df())) return(NULL)
            if (check1() != 0 | check2() != 0 | check3() != 0) return(NULL)
            adder(ts())
            
      })
      # output$p1 <- eventReactive(input$goButton,{
      #       renderPlot({
      #             
      #             if (is.null(df())) return(NULL)
      #             if (check1() != 0 | check2() != 0 | check3() != 0){
      #                   g<-plot(c(1:100))
      #                   return(g) 
      #             } 
      #             plotter1(ts(), input$delta, input$lambda)
      #             # input$goButton
      #             # isolate(plotter1(ts())) 
      #       })      
      # })
      d<-eventReactive(input$goButton, {
            input$delta
      })
      l<-eventReactive(input$goButton, {
            input$lambda
      })
      # output$p1<-renderPlot({
      # 
      #       if (is.null(df())) return(NULL)
      #       if (check1() != 0 | check2() != 0 | check3() != 0){
      #             g<-plot(c(1:100))
      #             return(g)
      #       }
      #       plotter1(ts(), input$delta, input$lambda)
      #       # input$goButton
      #       # isolate(plotter1(ts()))
      # })
      
      output$p1 <- renderPlot({
            if (v$go != push_count) return()
            if (check1() != 0 | check2() != 0 | check3() != 0) return(NULL)
            push_count <<- push_count + 1
            # cat(push_count)
            plotter1(ts(), d=input$delta, l=input$lambda,type1= )
      })
      output$the_df<-renderTable({
            if (is.null(df())) return(NULL)
            if (check1()!=0) return(NULL)
            
            head(df()[,input$col_number],10)
      })
      output$ts_entries<-renderText({
            if (is.null(df())) return(NULL)
            if (check1() != 0) return(NULL)
            as.character(df()[1:10,input$col_number])
            
      })
      output$tester <-renderText({
         
            v$go
      })
})