## Function assigning field values to spatial raster object:
assign_values <- function(field, spatial_raster) {
    values(spatial_raster) <- t(field[,ncol(field):1])  ## Flip column order, tranpose matrix
    return(spatial_raster)
}


# generate data from exponential function and illustrate bias-variance trade-off:
generate.data.poly <- function(N, seed, sd.noise = 300) {
  # set.seed(seed = seed)
  # X = rnorm(N)
  # set.seed(seed = seed+1)
  # Y = exp(X-offset) + rnorm(n = length(X), sd = sd.noise)
  #Y = X + rnorm(n = length(X), sd = sd.noise)
  # taken from: https://www.r-bloggers.com/fitting-polynomial-regression-in-r/
  set.seed(seed)
  q <- seq(from=0, to=30, length.out = N)
  y <- 500 + 0.7 * (q-15)^3 + 100 * (q-15)
  noise <- rnorm(length(q), mean=15, sd = sd.noise)
  noisy.y <- y + noise
  
  #plot(X, Y)
  return(data.frame(X=q, Y=noisy.y))
}


knnreg.fit <- function(train.data, test.data, k) {
  # fit a model WITHOUT cross-validation:
  knn.fits.train.data = kknn(formula = Y ~ X, train = train.data, k = k, test = train.data, kernel="rectangular")$fitted.values
  test.fit=kknn(formula = Y ~ X, train = train.data, k = k, test = train.data, kernel="rectangular")
  knn.fits.test.data = predict(object = test.fit, new.data=test.data)
  ret.list = data.frame(Y.train = knn.fits.train.data, Y.test = knn.fits.test.data)
  return(ret.list)
}


poly.fit <- function(train.data, test.data, degree = 2) {
  
  # fit polynomial model WITHOUT cross-validation:
  test=lm(formula = c(train.data$Y) ~ poly(train.data$X, degree = degree))
  poly.fit.train.data = test$fitted.values
  poly.fit.test.data = predict(test, newdata = poly(test.data$X, degree=degree))
  
  ret.list = data.frame(Y.train = poly.fit.train.data, Y.test = poly.fit.test.data)
  return(ret.list)
}



rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}
rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}



MSE <- function(obs,pred){
  mean((obs-pred)^2)
}



# poly.fit.4TOTPREC(data = train.data, degree = 3, holdout.subset = indvalidate)

poly.fit.4TOTPREC <- function(data, degree, holdout.subset) {
  
  # define new data frame with polynomial:
  PSL.df = poly(data$PSL, degree = degree)
  PSL_Nshift.df = poly(data$PSL_Nshift, degree = degree)
  PSL_Wshift.df = poly(data$PSL_Wshift, degree = degree)
  train.df = data.frame(Y = data$TOTPREC, PSL.df, PSL_Nshift.df, PSL_Wshift.df)
  
  # cur.formula = as.formula(paste("fapar ~ ", paste(names(test.df)[2:length(names(test.df))], collapse = ' + ')))
  # fit model and predict holdout set:
  test.model = lm(formula = Y ~ ., data = train.df[-holdout.subset,])
  return(predict(test.model, newdata = train.df[holdout.subset,]))
}



