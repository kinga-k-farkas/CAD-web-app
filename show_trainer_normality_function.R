#this function applies several normality tests to the input
#and displays the results

show_trainer_normality<-function(y){
      
      hist(y, freq=FALSE,  main="Histogram and Density Plot of the Training Set Plus
      the Normal Distribution Curve of Corresponding Mean and Stand. Dev.", 
           xlab="Training Set Data")
      curve(dnorm(x, mean=mean(y), sd=sd(y)), add=TRUE, lwd=2, col="red")
      lines(density(y), col = "blue", lwd = 2)
      plot.new()
      frame()
     # par(mfrow=c(1,1))
     # acf(y)
      #qqnorm(y, main="Normal QQ Plot for the Training Set"); qqline(y, col = 2)
}