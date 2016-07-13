##Given a vector x, this function finds its Skewness, Kurtosis
##and Shapiro-Wilk normality test p-value

normality_finder<-function(x){
      require(moments)
      return(list(skew=skewness(x), kurt=kurtosis(x), p_value=shapiro.test(x)$p.value))
}