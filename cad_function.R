#this function takes a daily time series with length greater than 
#365, the year, month, and day of its first entry
#the type of anomaly to be searching for: lower, upper
#with the default value being lower
#delta the value to be added to the value to the CUSUM parameter k
#its default value is 3
#the minimum length of the anomalous subsequence
#the code should detect, with the defalut being 5
#the title of the output graph, with the default being 
#"The Time Series with Anomalies in Red"
#and looks for anomalies using moving windows
#

cad<-function(ts, year=2012,mo=01,day=01,type="upper",delta=3, lambda=5, 
              main_title="The Time Series with Anomalies in Red"){
      source("anomaly_finder_function.R")
      #source("result_plotter_function.R")
      #source("result_plotter_function_interconn_png.R")
      #source("result_plotter_function_iran.R")
      #source("result_plotter_function_interconn.R")
      #source("result_plotter_function_generic.R")
      n<-length(ts)
      window_length<-n %/% 3
      w<-window_length
#       print(paste("Window length is", w))
      year<-as.character(year)
      mo<-as.character(mo)
      day<-as.character(day)
      the_date<-paste(year,"/",mo,"/",day, collapse="")
      the_date<-gsub(" ","", the_date)
      dates<-seq(as.Date(the_date), by = "day",length.out = n) 
      # print(dates)
      plotter_df<-data.frame(index=1:n, y=ts, date=dates)
#       print(head(plotter_df))
      for (i in 1:(n-w)){
            plotter_df[[i+3]]<-rep(NA, n)
      }
      upper_dates<-c()
      lower_dates<-c()
      for (i in 1:(n-w)){
      #for (i in 47:47){
            x<-ts[i:(i+w-1)]
            print(paste("window",i))
            #obj<-anomaly_finder(x,type, lambda)
            obj<-anomaly_finder(x,type,delta, lambda,i) 
            #adding the i so I can reference the windownumber in graph
            # print(paste("number of upper anomalies:", length(obj$upper)))
            # print(paste("number of lower anomalies:", length(obj$lower)))
            if (length(obj$upper) != 0){
                  plotter_df[[i+3]][i:(i+w-1)][obj$upper]<-
                        plotter_df$y[i:(i+w-1)][obj$upper]
#                   print(paste("This window contains anomalies:",i))
                  # print("upper anomaly dates:")
                  # print(plotter_df$date[i:(i+w-1)][obj$upper])
#                   print(str(plotter_df$date[i:(i+w-1)][obj$upper]))
                    upper_dates<-c(upper_dates,as.character(plotter_df$date[i:(i+w-1)][obj$upper] ))
#                   print("THIS IS upper_dates:")
#                   print(upper_dates)
#                   if ("2013-02-24" %in% as.character(plotter_df$date[i:(i+w-1)][obj$upper]) ){
#                         print(paste("Feb 24 appears in window number:", i))
#                   }
                  
                  
            }
            if (length(obj$lower) != 0){
                  plotter_df[[i+3]][i:(i+w-1)][obj$lower]<-
                        plotter_df$y[i:(i+w-1)][obj$lower]
#                   print(paste("This window contains anomalies:",i))
#                 
#                   print("lower anomaly dates:")
#                   print(plotter_df$date[i:(i+w-1)][obj$lower])
                  lower_dates<-c(lower_dates, as.character(plotter_df$date[i:(i+w-1)][obj$lower]))
            }
            
            
      }
#       print("PLOTTER_DF index")
#       print(head(plotter_df$index[249:(248+w)],100))
#       print("plotter_df date")
#       print(head(plotter_df$date[249:(248+w)],100))
      print("lower_dates=")
      print(unique(lower_dates))
      print("upper_dates=")
      print(unique(upper_dates))
      # result_plotter(plotter_df,w,main_title)
      # print(lower_dates)
      # print(upper_dates)

      return(list(x1=plotter_df, x2=w, x3=main_title, x4=unique(upper_dates), x5=unique(lower_dates)))


      
}