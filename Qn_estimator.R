#Q estimate
qn_est<-function(x, na.rm=TRUE){
  if (na.rm==TRUE)
    x<- na.omit(x)
  p<- length(x)
  d_ij<-outer(x, x, "-")
  d_ij<- abs(as.vector(d_ij[upper.tri(d_ij, diag = FALSE)]))
  d_ij<- sort(d_ij)
  h<- floor(p/2)
  k<- h*(h-1)/2
  if (p<=12)
    b<- c(0.9937, 0.9937, 0.5132, 0.8440, 0.6122, 0.8588, 0.6699, 0.8734, 0.7201, 0.8891, 0.7574)[p]
  if (p>12 & p %%2 == 0)
    b<- 1/((1/p)*(3.6756 +(1/p)*(1.965+(1/p)*(6.987-77/p)))+1)
  if(p>12 & p %%2 !=0)
    b<- 1/(1/p*(1.6019+1/p*(-2.128-5.172/p))+1)
  
  
  
  1 / (sqrt(2) * qnorm(5/8))* d_ij[k]*b
  
}
