plotter_g1 <- function(df, caption="Time Series"){
      l<-nrow(df)
      step_size<-l %/% 9
      tick_marks<-seq(df$index[1],df$index[l],by=step_size)
      labs<-seq(1,l, by=step_size)
      plot(df$index, df$y, type="l", ylab=caption, xlab="Date", 
           main=paste("Plot of ", caption, "With Anomalies in Red"), xaxt = 'n',
           col=rgb(0,0,0,alpha=0.7))
      
      points(df$index, df$y, col=rgb(0,0,0,alpha=0.6), cex=0.7, pch=1)
      
      for( i in (4: (ncol(df)-1))){
            if (sum(is.na(df[[i]]))!=nrow(df)){
                  points(df$index, df[[i]], col=rgb(1,0.1,0, alpha=0.3), pch=19)
            }
      }
      
      axis(1, at=tick_marks, labels=df$date[labs])
      
      
}