permutation.test=function(x1,x2,testStatistic,alt) {
  library(combinat)
  
  x=c(x1,x2)
  n1=length(x1)
  n2=length(x2)
  n=n1+n2
  t.ob=sum(x1)-sum(x2)
  x1.perm.indexes.array = combn(seq(1,n), n1)
  nperm = ncol(x1.perm.indexes.array)
  t.perm=vector(,nperm)
  
  for (i in 1:nperm) {
    x1.perm.indexes = x1.perm.indexes.array[,i]
    x2.perm.indexes = setdiff(seq(1,n), x1.perm.indexes)
    x1.perm = x[x1.perm.indexes]
    x2.perm = x[x2.perm.indexes]
    t.perm[i]=testStatistic(x1.perm,x2.perm)
  }
  
  if (alt=="greater")   pv.t=length(t.perm[t.perm>=t.ob])/nperm
  if (alt=="less")      pv.t=length(t.perm[t.perm<=t.ob])/nperm
  if (alt=="two.sided") pv.t=length(abs(t.perm)[abs(t.perm)>=abs(t.ob)])/nperm
  
  return(pv.t)
}

statistics.differenceOfSums = function(x1,x2){
  return(sum(x1)-sum(x2))
}

x1=rnorm(10)
x2=rnorm(10, mean = 1)
print(permutation.test(x1,x2,statistics.differenceOfSums,"two.sided"))