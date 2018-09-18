hampl<- function(x, maxiter= 25) {
  x_star<- median(x)
  s_star<- mad(x)
  dif<- max(x)-min(x)
  iter<- 0
  while ( dif >=s_star*0.01/sqrt(length(x)) && iter < maxiter) {
  iter<- iter+1
  q_i<- abs((x-x_star)/s_star)
  w_4.5<- which(q_i>4.5)
  w_3<- which(4.5>=q_i & q_i>3)
  w_1.5<- which(3>=q_i & q_i>1.5)
  w_0<- which(1.5>=q_i)
  w_i<- q_i
  w_i[w_4.5]<- q_i[w_4.5]*0
  w_i[w_3]<- (4.5-q_i[w_3])/q_i[w_3]
  w_i[w_1.5]<- (1.5/q_i[w_1.5])
  w_i[w_0]<- 1
  
  x_star_new<-sum(w_i*x)/sum(w_i)
  dif<- abs(x_star_new-x_star)
  x_star<- x_star_new}
  if (iter >= maxiter) 
    warning("Maximum iterations reached; Hampl/Q estimator may not have converged")
  return(list(mu= x_star, s= s_star, iterations=iter))
}