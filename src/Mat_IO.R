require(readxl)


##Cobb Dogulas productorial 
cobb_douglas = function(vec_k, L, alpha, A){
  A*prod(vec_k^alpha)*L^(1-sum(alpha))
}

prod_IO = function(matio, caliblist){
  vl <- mapply( function(x,y) do.call(x, y), matio, caliblist, 
                SIMPLIFY = FALSE)
  matrix(unlist(vl), ncol = 2, byrow = TRUE)
}


Mat_pr = lapply(na.omit(jp_matriz_local_transacciones_cinco_sectores_2002), as.numeric)
Mat_pr = data.frame(Mat_pr)

props = NULL
for(i in 1:nrow(Mat_pr)){
  elas = Mat_pr[i, ncol(Mat_pr)]/sum(Mat_pr[, ncol(Mat_pr)])
  props = data.frame(rbind(props, elas))
}

new_demand = function(commodities, L, A, new_d_total){
  props = c(commodities, L)/sum(commodities,L)
  elasts = c(commodities + new_d_total, L)/sum(commoditites, new_d_total, L)
  vec_k1 = new_d_total*props
  cobb_douglas(vec_k1, L, elasts, A)
}

##Random numer generator for the log-normal dist. is rlnorm(n, mean, sd)

est_elast <- function(input, output, commat, returnmean = TRUE){
  dfs_input <- exp(diff(log(input))) - 1
  dfs_output <- exp(diff(log(output))) - 1
  base <- t(apply( dfs_input, 1, function(x) sort(dfs_output / x,
  decreasing = FALSE)))
  
  if(returnmean == FALSE) 
  {
    iq_range <- base[,3] - base[,2]
    cor(datamat[ ,names(input)]) * (iq_range %*% t(iq_range))
  }
  
  rowMeans(base)
}

rmvlognorm <- function(n, mean, covariance)
{
  val0 <- MASS::mvrnorm(n,mean,covariance)
  exp(val0)
}

### NASDAQ Time series daily (1982-2007) convert to PR fiscal year
### Deseason (X13-ARIMA-SEATS)


