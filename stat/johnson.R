
johnson_cor = function(data){   
  
  # data = corr matrix
  
  rxx=data[-1,-1]  # correlations between predictors (X)
  rxy=data[-1,1]  # correlations between predictos and target (Y)
  
  ev=eigen(rxx)  # derive eigenvectors and eigenvalues from X
  d=sqrt(diag(abs(ev$values)) )  # matrix with sqrt(eigen values) on main diag and zeros
  evec=ev$vectors  # eigen vectors
  
  lam=evec%*%d%*%t(evec)  # L = V * D^(1/2) * t(V)
  lamsq=lam^2  # L ^ 2
  
  beta=solve(lam)%*%rxy  # inv(L) * correlation vector Y
  
  raw_w=lamsq%*%(beta^2)  # L^2 * B^2
  w=100*raw_w/sum(raw_w)  # result weights
   
  rsq=sum(beta^2)  # R2
  rownames(w)=colnames(data[,-1])
  w=w[,1]
  out=list(w=w,rsq=rsq)
  
}

df = read.csv2("data/correl_js.csv", header = F, stringsAsFactors=F)
df = as.data.frame(apply(df, 1, as.numeric))

js = johnson_cor(df)
write.csv2(js, "data/jonson.csv")


