#Problem 1 on Exercise 11 11/7/2020

#N(t+1)=N(t)+r*N(t)*(1-N(t)+M(t)/k) is the formula for Normal cnacer growth
#M(t+1)=M(t)+r*M(t)*(1-N(t)+M(t)/k) is the formula for Mutant cnacer growth

#First I will define the growth rate and the carrying capacity
r=0.1
k=1000000

#Generate an arbitrary time step that shows the carrying capacity being reached
t<-1:1000

#store output as a data frame, has one column for time, one for mutant population, one for normal population
output<-matrix(data=NA, nrow=length(t), ncol=(3))
output[,1]=t
output[1,2]=1
output[1,3]=99
#for loop running the model of each populations growth, that stores the pop size in the matrix
for(i in t[-1]){
  if(t[i]<300){
    output[i,2]=output[(i-1),2]+r*output[(i-1),2]*(1-(output[(i-1),2]+output[(i-1),3])/k)
    output[i,3]=output[(i-1),3]+r*output[(i-1),3]*(1-(output[(i-1),3]+output[(i-1),3])/k)
  }else{
    output[i,2]=output[(i-1),2]+(r/2)*output[(i-1),2]*(1-(output[(i-1),2]+output[(i-1),3])/k)
    output[i,3]=output[(i-1),3]+(-1*r)*output[(i-1),3]*(1-(output[(i-1),3]+output[(i-1),3])/k)
  }
}

#plot results, need to make dataframe first to use ggplot
outputDF<-data.frame(time=output[,1], MutantPop=output[,2], NormalPop=output[,3])
ggplot(data=outputDF, aes(x=time))+
  geom_line(aes(y=MutantPop))+
  geom_line(aes(y=NormalPop))+
  ylab("Total pop")+
  theme_classic()




























