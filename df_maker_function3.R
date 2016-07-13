#creates the data frame that is used to plot the votes

df_maker3<-function(df){
      n<-nrow(df)
      p_df<-data.frame()
      for (row in (1:n)){
            count<-df[row,2]
            if (count==0) {
                  p_df<-rbind(p_df, c(row, NA))
            }
            else {
                  for (i in (1:count)){
                        p_df<-rbind(p_df, c(row,i))
                  }
            }
      }
      return(p_df)

}