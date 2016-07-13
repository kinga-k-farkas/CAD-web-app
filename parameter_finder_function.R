#This function takes a time series - that has (hopefully) a nearly normal distribution
#and selects the smallest parameters k and H for which the time series is in statistical 
#control
#TEMPORARY VERSION:For now the value of H will remain a constant, H=5,
# only the value of k will increase from its starting value of 2 until the 
#time_series is in statistical control
#FINAL VERSION: either allow H to change its value as well (if it is needed) 
#or remove it 
parameter_finder <- function(time_series){
      require(qcc)
      xbar<-mean(time_series)
      sigma<-sd(time_series)
      k<-3
      H<-5
      parameters_found<-FALSE
      while (!parameters_found){
            object<-cusum(time_series,sizes=1, center=xbar, std.dev=sigma, 
                          decision.interval=H, se.shift=k, plot=FALSE)
            #print(object)
            lower_count<-length(object$violations$lower)
            upper_count<-length(object$violations$upper)
            if ((lower_count==0) & (upper_count==0)) {
                  parameters_found<-TRUE
            }
            else k<-k+1
            
      }
      return(list(k=k, H=H))
}