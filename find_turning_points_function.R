
#THIS IS THE SECOND AND CORRECT VERSION.  USE THIS ONE.
#this function takes a sequence of numbers and finds the indices of 
#the values at which the sequence turns from increasing to decreasing or vice-versa


find_turning_pts<-function(seq,turning_points=NULL, sgn=0, index=1){
      n<-length(turning_points)
      if (length(seq)==1 | length(seq)==0){
            if (length(turning_points)>0) turning_points<-c(turning_points,index)
            return(turning_points)
      }
      diff<-seq[2]-seq[1]
      if (sign(diff) == 0) { 
            if (sgn != 0) { 
                  turning_points<-c(turning_points,index)      
            }
            sgn<-0
            index<-index+1
            return(find_turning_pts(seq[2:length(seq)], turning_points,sgn,index))
      }
      else if (sign(diff)==sgn){
            index<-index+1
            return(find_turning_pts(seq[2:length(seq)], turning_points, sgn, index))
      }
      else {
            sgn<-sign(diff)
            turning_points<-c(turning_points, index)
            index<-index+1
            return(find_turning_pts(seq[2:length(seq)], turning_points, sgn, index))
            
      }
}


