#given an input of the sequence and the turning points 
#returns a data frame containing: 
#right_index: rigth indices of the intervals
#left_index: left indices of the intervals
#right_value:right endpoint values
#left_value: left endpoint values
#sign: "increasing", "decreasing", "constant"

#NOTE: the function will return an error message if the the intervals were found to 
# be neither monotone increasing/decreasing nor constant


find_intervals<-function(seq,turningPt_indices,type){
      #double checking signs for intervals, can be changed later to single check
      if ((length(seq) <= 1) | (length(turningPt_indices)<=1) ){
            df<-data.frame()
            return(df)
      }
      
      n<-length(turningPt_indices)
      x1<-turningPt_indices[1:(n-1)]
      x2<-turningPt_indices[2:n]
      all_intervals<-data.frame(cbind(x1,x2))
      all_intervals$turningPts1<-seq[x1]
      all_intervals$turningPts2<-seq[x2]
      all_intervals$pts_on_right <- seq[x1+1]
      all_intervals$pts_on_left<-seq[(x2-1)]
      m<-nrow(all_intervals)
     
      sign_finder<-function(a1,a2){
            #given two values, it returns "increasing" if a2-a1>0 
            #returns "decreasing" if a2-a1<0
            #return "constant" if a2=a1
            if ((a2-a1) > 0) return("increasing")
            else if ((a2-a1)<  0) return("decreasing")
            else return("constant")
                 
      }
      all_intervals$sign<-rep(NA,m)
      #double checking signs for intervals, can be changed later to single check
      dummy1<-mapply(sign_finder, all_intervals$turningPts1, 
                     all_intervals$pts_on_right)
      dummy2<-mapply(sign_finder, all_intervals$pts_on_left, all_intervals$turningPts2)
      if (sum(dummy1==dummy2)==m) all_intervals$sign<-dummy1
      else stop("the sign was not constant on one of the monotone intervals!")
      #all_intervals$sign<-dummy1
      df<-data.frame(all_intervals$x1,all_intervals$x2, all_intervals$turningPts1, 
                     all_intervals$turningPts2, all_intervals$sign)
      names(df)<-c("right_index", "left_index","right_end_value", "left_end_value", "sign")
      return(df)      
}