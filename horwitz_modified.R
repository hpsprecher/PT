#modified Horwitz estimate of reproducibility standard deviation

horwitz_modifided<- function(x, scale=10^-6, na.rm=TRUE) {
  library(metRology)
  if(na.rm==TRUE)
    x<- na.omit(x)
    c<- algA(x)$mu*scale
  
  if (c<1.2*10^-7)
    s<- 0.22*c
  if(1.2*10^-7<=c & c<=0.138)
    s<- 0.02*c^0.8495
  if (c>0.138)
    s<- 0.01*c^0.5
return(list(s= s/scale))
}


#Example
x<- c(10:1 %o% 10^-(1:10))
ho_fun<-sapply(x, horwitz_short) 
y<- unlist(ho_fun)
plot(log10(x), log10(y), type = "l", ylab = "log10 of s estimate", xlab= "log10 of analyte concentration", main = "s estimate by modified Horwitz estimator")
