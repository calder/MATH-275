pdf("6-31.pdf", height=2)
par(mfrow=c(1,4))
f = function (n) {
  curve(x*(1-x)/n, from=0, to=1, xlab="p", ylab="MSE", main=sprintf("N=%d",n))
  curve(n*(1-x)*x/(n+2)^2 + (1-2*x)^2/(n+2)^2, add=TRUE, col="blue", lty=2)
}
f(30); f(50); f(100); f(500)