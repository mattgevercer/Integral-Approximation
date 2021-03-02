#use variable 'func' to define the function with which you would like to find the integral
func<-function(x){
  1/((x^3)+1)  
}
#Trapezoidal Rule
trap_rule<-function(int1='Lower Limit',int2='Upper Limit',n='Subintervals'){
  delta_x<-(int2-int1)/n
  f<-seq(int1,int2,delta_x)
  f_last=f[length(f)]
  fn=f_last-1
  delta_x/2*(func(f[1])+func(f[f_last])+
               sum(sapply(f[2:fn],FUN = func))*2)
}

#midpoint
midpnt_rule<-function(int1='Lower Limit',int2='Upper Limit',n='Subintervals'){
  delta_x<-(int2-int1)/n
  f<-seq(int1,int2,delta_x)
  midpnts<-(f[-length(f)]+diff(f)/2)
  f_midpnts<-sapply(midpnts,func)
  delta_x*sum(f_midpnts)
}
#simpsons rule
simps_rule<-function(int1='Lower Limit',int2='Upper Limit',n='Subintervals'){
  delta_x=(int2-int1)/n
  f<-seq(int1,int2,delta_x)
  f_last=length(f)
  fn=f_last-1
  outside<-c(f[1],f[f_last])
  inside<-f[2:fn]
  odds<-inside[c(TRUE,FALSE)]
  evens<-inside[c(FALSE,TRUE)]
  delta_x/3*(4*sum(sapply(odds,func))+
               2*sum(sapply(evens,func))+sum(sapply(outside,func)))
}