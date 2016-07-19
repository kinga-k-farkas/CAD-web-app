#this function takes a vector of daily measurements
#of length of at least 75 
#and finds a stretch of most normally distributed sub-segment 

training_data_finder <- function(sequence){
      
      source("normality_finder_function.R")
      source("checker_function.R")
      source("parameter_finder_function.R")
      source("show_trainer_normality_function.R")
      n <-length(sequence)
      #stopifnot(n>=75)
      segment_found<-FALSE
      no_solution<-FALSE
      #TEMPORARY:creating a data frame that will keep track of the statistics for each segment
      stats_df<-data.frame()  
      
      row<-c(-1, rep(1000000,7)) #non-sensical dummy values
    
      segment_length <- n %/% 3
     
      while(((!segment_found ) & (!no_solution))){
            
            for (i in (1:(n-segment_length+1))){
                  x<-sequence[i: (i+segment_length-1)]
                  if (normality_finder(x)$p_value >= 0.05){
                        skew<-normality_finder(x)$skew
                        kurt<-normality_finder(x)$kurt
                        p_value<-normality_finder(x)$p_value
                        cusum_parameters<-parameter_finder(x)
                        repr <- abs(skew) + abs(3-kurt)+abs(1-p_value)+
                              cusum_parameters$H*100 + cusum_parameters$k*100
                        new_row<-c(i,segment_length,skew,kurt,p_value, 
                                   cusum_parameters$k, cusum_parameters$H,repr)
                        stats_df<-rbind(stats_df,new_row)
                        if (repr < row[8]) row<-new_row
                        
                        
                  }
            }
            if (checker(row)) {
                  segment_found<-TRUE
            } 
            else if (segment_length > 25) segment_length<-segment_length-1
            else {
                  print("Couldn't find a usable training set, cannot proceed with searching for anomalies.")
                  no_solution<-TRUE
                  print(no_solution)
                  stats_df<-data.frame("segment_starting_point"=c(NA), "segment_length"=c(NA), "skewness"=c(NA),
                                        "kurtosis"=c(NA), "Shapiro_test_p_value"=c(NA), "cusum_k"=c(NA), "cusum_H"=c(NA),
                                        "summarizing_value"=c(NA))
            }     
      }
      names(stats_df)<-c("segment_starting_point", "segment_length", "skewness",
                         "kurtosis", "Shapiro_test_p_value", "cusum_k", "cusum_H",
                         "summarizing_value")
      # print(names(stats_df))
      if (no_solution) {
            print("in if in training data finder function")
            training_data<-NULL
            k<-NULL
            H<-NULL
            segment_starting_point<-NULL
            segment_length<-NULL
            summarizing_value<-NULL
      }
      else {  
            
            training_data<-sequence[row[1]:(row[1]+row[2]-1)]
            k<-row[6]
            H<-row[7]
            segment_starting_point<-row[1]
            segment_length<-row[2]
            summarizing_value<-row[8]
            start_val<-segment_starting_point
            end_val<-segment_length + start_val-1
            #two_color_plotter(sequence, segment_length, start_value, end_value)
            temp<-sequence[start_val:end_val]
           
            #show_trainer_normality(temp)
      }
      #plot(training_data, type="l", main="Traning Set", ylab="Daiy Median Throughput (Mbps)")
      print(no_solution)
      return(list(df=stats_df, training_data=training_data, k=k, H=H,
                  segment_starting_point= segment_starting_point, segment_length=segment_length,
                  summarizing_value=summarizing_value, no_solution=no_solution))
      
      
}