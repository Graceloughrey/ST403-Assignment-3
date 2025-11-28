#Exercise 3.3

#Part 1: Construct a data.frame where y is to be treated as the response and the remaining variables are to be used as predictors as follows:df <- with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))
df<-with(mtcars,data.frame(y=mpg,x1=disp,x2=hp,x3=wt))
head(df)

#Part 2: Write a function nll_lm() which implements the negative log-likelihood for the linear regression model in terms of the residuals εi.Your function should take only two arguments: data (for df) and par (for the parameter vector). (Hint: you may use dnorm() inside the body of your function and don't need a for loop.)
nll_lm<-function(par,data) {
  beta<-par[1:4];sigma<-par[5]
  mu<-beta[1]+beta[2]*data$x1+beta[3]*data$x2+beta[4]*data$x3
  -sum(dnorm(data$y,mean=mu,sd=sigma,log=TRUE))
}

#Part 3: Use optim() in conjunction with nll_lm() to find the maximum likelihood estimates of β^and σ^(not σ^2!) using the response and predictors outlined in part 1
start<-c(mean(df$y),0,0,0,sd(df$y))
fit<-optim(
  par=start,
  fn=nll_lm,
  data=df,
  method="L-BFGS-B",
  lower=c(-Inf,-Inf,-Inf,-Inf, 1e-8)
)
fit$par

#Part 4: Why was it necessary to implement the negative log-likelihood function?
#We minimise the negative log-likelihood because optim() can only minimise functions and minimising -ℓ(θ) is equivelent to maximising ℓ(θ). Therefore in order to perform MLE we have to minimise the -log likelihood.

#Part 5:Compare your answers for β^ with those obtained using matrix operations in base R without the use of lm().
X<-as.matrix(cbind(1,df[,c("x1","x2","x3")]))
y<-df$y
beta_matrix<-solve(t(X)%*%X)%*%(t(X)%*%y)
beta_matrix
fit$par[1:4]

#Part 6:Compare your answers for σ^with those obtained using matrix operations in base R without the use of lm().
res<-y-X%*%beta_matrix
sigma_mle_matrix<-sqrt(sum(res^2)/nrow(df))
sigma_mle_matrix
fit$par[5]

#Part 7:Your answers in part 5. should have been reasonably close, but your answers in part 6. should be quite different. How do you account for this difference?
#The σ estimates are different because optim() returns the MLE, which divides by n while the OLS returns the unbiased estimator, which divides by (n-p). Because n-p<n, the OLS σ^ will be larger.

#Part 8:Report the standard errors of the regression coefficients only, using optim() rather than any manual matrix operations in base R. You may need to modify your call to optim() from part 3.
fit2<-optim(par=start,fn=nll_lm,
            data=df,method="L-BFGS-B",
            lower=c(-Inf,-Inf,-Inf,-Inf,1e-8),
            hessian = TRUE
)
H<-fit2$hessian
cov_mat<-solve(H)
se_betas<-sqrt(diag(cov_mat)[1:4])
se_betas

#Question 4
lm_fit<-lm(y~x1+x2+x3,data=df)
beta_hat_lm<-coef(lm_fit)
sigma_hat_lm<-sigma(lm_fit)
beta_hat_lm
sigma_hat_lm