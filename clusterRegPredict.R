clustreg.predict=function(results,newdat){
  
  yhat=rep(NA,nrow(newdat))
  resid=pred=matrix(0,nrow(newdat),length(table(results$cluster)))
  
  for(j in 1:length(table(results$cluster))){			
    pred[,j]=predict(glm(results$data[results$cluster==j,],family="gaussian"),newdata=newdat)		
    resid[,j] = (pred[,j]-newdat[,1])^2
  }
  
  c = apply(resid,1,fun.index.rowmin)
  for(m in 1:nrow(newdat)) {yhat[m]=pred[m,c[m]]}
  rsq = cor(newdat[,1],yhat)^2	
  
  return(list(results=results,newdata=newdat,cluster=c,yhat=yhat,rsq=rsq))
  
}