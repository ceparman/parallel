

library(parallel)

thermal<-function(iter=100,size=100,bound=sample(1:4,1))
{
#  initialize matrix
  d<-matrix(1,size,size)

# set boundary conditions   
     

    if(bound ==1){
         d[1,]<-size
         d[,1]<-size 
      }  else if( bound == 2) {
         d[1,]<-100
         d[,100]<-100:1
         d[,1]<-100:1 
      }  else if (bound ==3 ){
        d[1,]<-100*sin((1:100)/10)
           
      } else if (bound ==4) {
        d[,1]<-100*sin((1:100)/10)
        d[1,]<-100*sin((1:100)/10)
      } else {
        d[,1]<-100:1 
        
      }
      
      
   

#run the simulation

  for(k in 1: iter) {
  
      for(i in 2:(size-1))  {  
    
        for(j in 2:(size-1)) {
      
          d[i,j] <- (d[i-1,j]+d[i+1,j]+d[i,j-1]+d[i,j+1])/4
      
        }
    
    
      }
   
   }

#write image file

pdf(tempfile(pattern="image",tmpdir =".", fileext=".pdf"))
image(d)
dev.off()


  
}  #end thermal


cl<-makeCluster(detectCores())
#t<-clusterCall(cl,rnorm,10000000)
#mc.cores <- detectCores()
#makeCluster

t<-clusterCall(cl,thermal)

stopCluster(cl)

#results<- mclapply(rep(100,10),thermal,mc.cores=mc.cores)






