horwitz_short<- function(x, na.rm=TRUE, scale=10^-6) {
  library(metRology)
  if(na.rm==TRUE)
    x<- na.omit(x)
  c<- x*scale
  
  if (c<1.2*10^-7)
    s<- 0.22*c
  if(1.2*10^-7<=c & c<=0.138)
    s<- 0.02*c^0.8495
  if (c>0.138)
    s<- 0.01*c^0.5
  return(list(s= s/scale))
}