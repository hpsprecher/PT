#Q estimate
qn_est<-function(x){
  p<- length(x)
  d_ij<-outer(x, x, "-")
  d_ij<- abs(as.vector(d_ij[upper.tri(d_ij, diag = FALSE)]))
d_ij<- sort(d_ij)
h<- floor(p/2)
k<- h*(h-1)/2
if (p<4)
  r<- 0.9937
if (p==4)
  r<- 0.5132
if (p==5)
  r<- 0.8440
if (p==6)
  r<- 0.6122
if (p==7)
  r<- 0.6699
if (p==8)
  r<- 0.6699
if (p==9)
  r<- 0.8734
if (p==10)
  r<- 0.7201
if (p==11)
  r<- 0.8891
if (p==12)
  r<- 0.7574
if (p>12 & p %%2 == 0)
  r<- (1/p)*(3.6756 +(1/p)*(1.965+(1/p)*(6.987-77/p)))
if(p>12 & p %%2 !=0)
  r<- 1/p*(1.6019+1/p*(-2.128-5.172/p))
b<- 1/(r+1)

2.2219* d_ij[k]*b

}