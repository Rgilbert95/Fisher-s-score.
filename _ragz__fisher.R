##############RUN HERE
mu_i=function(xf){
  cmean=as.numeric() #variable to store the mean of each column
  n_col=ncol(xf)-1
  for (i in (1:n_col)) {
    cmean[i]= mean(xf[,i])
    mu_i=cmean
  }
  return(mu_i)
}

fisher=function(xf){
  labels= xf[,ncol(xf)]
  reps=as.data.frame(table(labels))
  n_j= reps[,2] #fraction of points belonging to class j
  nclass= length(as.vector(unique(labels)))
  features= ncol(xf)-1
  names(xf)[ncol(xf)]= "labels"
  #mean and standard deviation of the data points belonging to a label 
  mu_ij= aggregate.data.frame(xf[, 1:features], list(xf$labels), mean)
  rho_ij= aggregate.data.frame(xf[, 1:features], list(xf$labels), sd)
  for (i in (1:features)) {
    o1= numeric()
    o2= numeric()
    den= numeric()
    num=numeric()
    for (j in (1:nclass)) {
      o1[j]= n_j[j]*((mu_ij[j,i]-mu_i(xf)[i])^2)
      o2[j]= n_j[j]*(rho_ij[j,i]^2)
    }
    #den= sum(o2) 
    #num= sum(o1)
    num=o1
    den=o2
    fscore= num/den
  }
  return(fscore)
}

#Generate the data, using 2 gaussians
#read.csv("C:\\Users\\Admin\\Downloads\\driver-data.csv")
load(file="C:\\Users\\47406\\Downloads\\practice_04\\practice_04\\2gaussiandata.RData")
plot(x[,1], x[,2])
labels=x[,3]
x=x[,1:2]
xf= as.data.frame(cbind(x,labels))
mu_i(xf) ###Mean of each respective column, belonging to a feature
fisher(xf) #fisher score

#Generate the data, using blobs 
cluster=2
features=2
library(synthesis)
blobs=data.gen.blobs(
  nobs = 1000,
  features = features,
  centers = cluster,
  sd = 1,
  bbox = c(-10, 10),
  do.plot = TRUE)


#x1= blobs$x
#labels1= as.vector(unlist(kmeans(x1, cluster)[1]))
#xf1=as.data.frame(cbind(x1,labels))
#mu_i(xf1) ###Mean of each respective column, belonging to a feature
#fisher(xf1) #fisher score


