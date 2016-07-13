#this function takes a time series, a mean, a standard dev., a k value, an H value
#and the type of anomaly to be identified: "lower" or "upper"
#and returns the indices of the anomalies

evaluate_cusum_results<-function(seq,xbar,sigma, H, k, type, subseq_length){
      options(expressions=10000)
      require(qcc)
      require(dplyr)
      #source("find_turning_pts_function.R")
      source("find_turning_points_function.R")
      source("find_intervals_function.R")
      object<-cusum(seq,sizes=1, center=xbar, std.dev=sigma, 
                        decision.interval=H, se.shift=k,restore.par = TRUE, 
                        plot=FALSE)
      low_sums<-object$neg
      hi_sums<-object$pos
#       print("hi_sums=")
#       print(hi_sums)
      upper_viol_index<-object$violations$upper
      lower_viol_index<-object$violations$lower
      lower_viol<-low_sums[lower_viol_index]  #seq. of lower sum out of bound values
      upper_viol<-hi_sums[upper_viol_index] #seq. of upper sum out of bound values
      interval_points_finder<-function(x,y){
            return(c(x:y))
      }
      if (length(upper_viol_index)>0){
#             print("upper violatons index=")
#             print(upper_viol_index)
#             print("violation values of the hi sum =")
#             print(upper_viol)
#             upper_turning_pts<-find_turning_pts(upper_viol)
            high_sum_turning_pts<-find_turning_pts(hi_sums)
#             print("upper turning point indices=")
#             print(upper_turning_pts)
#             print("high_sum_turning_points")
#             print(high_sum_turning_pts)
#             upper_df<-find_intervals(upper_viol, upper_turning_pts, type) 
#             print("upper df")
#             print(upper_df)
            if (high_sum_turning_pts[1]!= 1) high_sum_turning_pts<-c(1,high_sum_turning_pts)
           

            hi_sum_df<-find_intervals(hi_sums,high_sum_turning_pts, type)
#             print("hi_sum_df")
#             print(hi_sum_df)
      }
      if (length(lower_viol_index)>0){
#             lower_turning_pts<-find_turning_pts(lower_viol)
            low_sum_turning_pts<-find_turning_pts(low_sums)
#             lower_df<-find_intervals(lower_viol, lower_turning_pts,type)
            if (low_sum_turning_pts[1]!= 1) low_sum_turning_pts<-c(1,low_sum_turning_pts)
            low_sum_df<-find_intervals(low_sums,low_sum_turning_pts, type)
      }
      hi_sum_anomaly_indices<-c()
      low_sum_anomaly_indices <-c()
      if (type=="lower"){
       #finding the indices of the decreasing terms for the upper violations seq.
            if (length(upper_viol_index)>0){
#                   upper_df<-filter(upper_df, sign=="decreasing" & (left_index-right_index)>subseq_length)
#                   print("UPPER DF")
#                   print(upper_df)
                  hi_sum_df<-filter(hi_sum_df, sign=="decreasing" & (left_index-right_index)>subseq_length)
#                   print("HI_SUM_DF")
#                   print(hi_sum_df)
#                   print(nrow(hi_sum_df))
                  if (nrow(hi_sum_df)>0){
                        for (i in (1:nrow(hi_sum_df))){
#                               print(paste("the ith row is",i))
#                               print(paste("the left endpoint:",hi_sum_df[[1]][i] ))
#                               print(paste("the right endpoint:",hi_sum_df[[2]][i] ))
                              dummy_interval<-c(hi_sum_df[[1]][i]:hi_sum_df[[2]][i])
                              dummy_interval<-dummy_interval[dummy_interval %in% upper_viol_index]
                              if (length(dummy_interval) > subseq_length){
                                    hi_sum_anomaly_indices<-c(hi_sum_anomaly_indices,dummy_interval)
                              }
                        }
                        
                  }
                  else hi_sum_anomaly_indices<-NULL
                 
#                   potential_hi_sum_anomaly_indices <- 
#                         unlist(mapply(interval_points_finder, 
#                         hi_sum_df$right_index, hi_sum_df$left_index))
                  
#                   upper_anomaly_indices_in_upper_viol<-unlist(mapply(interval_points_finder, 
#                                                                      upper_df$right_index, upper_df$left_index))
#                   print("upper anom indices in upper viol")
#                   print(upper_anomaly_indices_in_upper_viol)
#                   print("potential_anomaly_indices_in_hi_sum")
#                   print(potential_hi_sum_anomaly_indices)
#                   print('hi sum anomaly indices')
#                   print(hi_sum_anomaly_indices)
#                   upper_anomaly_indices_in_hi_sums<-
#                         upper_viol_index[upper_anomaly_indices_in_upper_viol] 
#                   print("upper anom. indices in high sums=")
#                   print(upper_anomaly_indices_in_hi_sums)
            }
            else {
                  hi_sum_anomaly_indices<-NULL
#                   upper_anomaly_indices_in_hi_sums<-NULL
            }
            
            #finding the indices of the decreasing terms for the lower violations seq.
            if (length(lower_viol_index)>0){
#                   lower_df<-filter(lower_df, sign=="decreasing" & (left_index-right_index)>subseq_length)
                  low_sum_df<-filter(low_sum_df, sign=="decreasing" & (left_index-right_index)>subseq_length)
                  #print(low_sum_df)
                  if (nrow(low_sum_df)>0){
                        for (i in (1:nrow(low_sum_df))){
                              dummy_interval<-c(low_sum_df[[1]][i]:low_sum_df[[2]][i])
                              dummy_interval<-dummy_interval[dummy_interval %in% lower_viol_index]
                              if (length(dummy_interval) > subseq_length){
                                    low_sum_anomaly_indices<-c(low_sum_anomaly_indices,dummy_interval)
                              }
                        }
                        
                  }
                  else low_sum_anomaly_indices<-NULL
                
              
#                   lower_anomaly_indices_in_lower_viol<-unlist(mapply(interval_points_finder, 
#                                                                      lower_df$right_index, lower_df$left_index))
#                   lower_anomaly_indices_in_low_sums<-
#                         lower_viol_index[lower_anomaly_indices_in_lower_viol]      
            }
            else {
                  low_sum_anomaly_indices<-NULL
#                   lower_anomaly_indices_in_low_sums<-NULL
            }
      }
      
      if (type == "upper") {
            #finding the indices of the increasing terms for the upper violations seq.
            if (length(upper_viol_index)>0){
#                   upper_df<-filter(upper_df, sign=="increasing" & (left_index-right_index)>subseq_length)
                  hi_sum_df<-filter(hi_sum_df, sign=="increasing" & (left_index-right_index)>subseq_length)
                  if (nrow(hi_sum_df)>0){
                        for (i in (1:nrow(hi_sum_df))){
                              dummy_interval<-c(hi_sum_df[[1]][i]:hi_sum_df[[2]][i])
                              dummy_interval<-dummy_interval[dummy_interval %in% upper_viol_index]
                              if (length(dummy_interval) > subseq_length){
                                    hi_sum_anomaly_indices<-c(hi_sum_anomaly_indices,dummy_interval)
                              }
                        }
                        
                  }
                  else hi_sum_anomaly_indices<-NULL
                
                  
                  
#                   upper_anomaly_indices_in_upper_viol<-unlist(mapply(interval_points_finder, 
#                                                                      upper_df$right_index, upper_df$left_index))
#                   upper_anomaly_indices_in_hi_sums<-
#                         upper_viol_index[upper_anomaly_indices_in_upper_viol]
            }
            else {
                  hi_sum_anomaly_indices<-NULL
#                   upper_anomaly_indices_in_hi_sums<-NULL
            }
            
            #finding the indices of the increasing terms for the lower violations seq.
            if (length(lower_viol_index)>0){
#                   lower_df<-filter(lower_df, sign=="increasing" & (left_index-right_index)>subseq_length)
#                   lower_anomaly_indices_in_lower_viol<-unlist(mapply(interval_points_finder, 
#                                                                      lower_df$right_index, lower_df$left_index))
#                   lower_anomaly_indices_in_low_sums<-
#                         lower_viol_index[lower_anomaly_indices_in_lower_viol]  
                  low_sum_df<-filter(low_sum_df, sign=="increasing" & (left_index-right_index)>subseq_length)
                  if (nrow(low_sum_df)>0){
                        for (i in (1:nrow(low_sum_df))){
                              dummy_interval<-c(low_sum_df[[1]][i]:low_sum_df[[2]][i])
                              dummy_interval<-dummy_interval[dummy_interval %in% lower_viol_index]
                              if (length(dummy_interval) > subseq_length){
                                    low_sum_anomaly_indices<-c(low_sum_anomaly_indices,dummy_interval)
                              }
                        }      
                  }
                  else low_sum_anomaly_indices<-NULL
                
            }
            else {
                  low_sum_anomaly_indices<-NULL
#                   lower_anomaly_indices_in_low_sums<-NULL
            }
            
      }
      if (type!="lower" & type!="upper") stop("ERROR: type has to equal 'lower' or 'upper'")
      
      #return(list(upper_anomaly=upper_anomaly_indices_in_hi_sums, lower_anomaly=lower_anomaly_indices_in_low_sums))
      return(list(upper_anomaly=hi_sum_anomaly_indices, lower_anomaly=low_sum_anomaly_indices))
}