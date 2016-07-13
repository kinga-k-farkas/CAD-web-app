#this function takes a time series of length of at least 75
#and the type of anomaly to look for: "lower" or "upper"
#and looks for anomalies using the statistical process control chart, the CUSUM chart
#TEMPORARY: the input should not be much longer than 365 
#FINAL VERSION: the input can be any length longer than 74
# Added an i as an input indicating the window number the detector is being applied
#to. It is used to save the graphs to separate files.

anomaly_finder <- function(time_series,type,delta,lambda,i){
      #temporary version
      require(qcc)
      require(ggplot2)
      require(zoo)
      n<-length(time_series)
      #stopifnot(n>=75)
      source("training_data_finder_function.R")
      source("evaluate_cusum_results_function.R")
      #source("cusum_interpreter_function2.R")
      training_data_object <- training_data_finder(time_series)
      if (length(training_data_object$training_data) == 0) {
            print("This method cannot be applied to this particular time series.")
      }
      else {
            
            training_data<-training_data_object$training_data
            #plotting the time_series and the training set by
            #creating a data frame that is to be plotted using ggplot2
            start_val<-training_data_object$segment_starting_point
            segment_length<-training_data_object$segment_length
            end_val<-segment_length + start_val-1
            training_in_ts<-rep(NA, n)
            training_in_ts[start_val:end_val]<-time_series[start_val:end_val]
            #creating the CUSUM chart
            x_bar<-mean(training_data)
            #print(paste("the mean is:", x_bar))
            sigma<-sd(training_data)
            #print(paste("the standard dev. is:", sigma))
            k<-training_data_object$k+delta
            H<-training_data_object$H
            #print(paste("H is:",H))
            anomaly_indices<-
            evaluate_cusum_results(time_series,x_bar,sigma,H,k,type, lambda)
            #print(anomaly_indices)
            #anomaly_indices2<-cusum_interpreter(rollmean(time_series,3),x_bar,sigma,H,k)
            #print(anomaly_indices2)
            
            df <- data.frame(
                  
                  index = 1:n,
                  y = time_series,
                  col = as.character(c(rep("black", (start_val-1)), 
                          rep("green", segment_length), 
                          rep("black", (n-end_val)))),
                  training_set=training_in_ts,
                  anomaly=rep(NA, n),
                          stringsAsFactors = FALSE)
            lower_anomaly<-NULL
            upper_anomaly<-NULL
           # if (length(anomaly_indices2$upper_anomaly)!= 0){
                  #df$col[anomaly_indices2$upper_anomaly]="yellow"
           # }
           # if (length(anomaly_indices2$lower_anomaly) != 0){
                  #df$col[anomaly_indices2$lower_anomaly] ="yellow"
           # }
            if (length(anomaly_indices$upper_anomaly)!= 0){
                  df$col[anomaly_indices$upper_anomaly]="red"
                  upper_anomaly<-anomaly_indices$upper_anomaly
                  df$anomaly[anomaly_indices$upper_anomaly]<-
                        df$y[anomaly_indices$upper_anomaly]
            }
           
            if (length(anomaly_indices$lower_anomaly) != 0){
                  df$col[anomaly_indices$lower_anomaly] ="red"
                  lower_anomaly<-anomaly_indices$lower_anomaly
                  df$anomaly[anomaly_indices$lower_anomaly]<-
                        df$y[anomaly_indices$lower_anomaly]
                  
                 
            }
           
           #PRINT TO FILE TIME SERIES INPUT, ITS ANOMALY AND TRAINING SET GRAPH:
#          file_name<-paste("detector3ResultsForWindowNumber",i,".pdf", sep="")
#            pdf( file_name, width = 20, height = 8 )
#            
#            cusum(time_series,sizes=1, center=x_bar, std.dev=sigma, 
#                  decision.interval=H, se.shift=k,restore.par = TRUE)
#            step_size<-n %/% 9
#            tick_marks<-seq(df$index[1],df$index[n],by=step_size)
#            labs<-seq(1,n, by=step_size)
#            plot(df$index, df$y, type="l", ylab="Download Throughput (Mbps)", xlab="Date", 
#                 main=file_name, xaxt = 'n',
#                 col=rgb(0,0,0,alpha=0.7))
#            
#            points(df$index, df$y, col=rgb(0,0,0,alpha=0.6), cex=0.7, pch=1)
#            points(df$index, df$anomaly, col=rgb(1,0.1,0), pch=19)
#            points(df$index, df$training_set, col="green")
#            
#            
#            axis(1, at=tick_marks, labels=df$index[labs])
#            dev.off()
            return(list(upper=upper_anomaly, lower=lower_anomaly))
            
      }
}