theta_hat_prior <- 0.524
se_prior <- 0.041

n <- 400
y <- 190

theta_hat_data <- y/n
se_data <- sqrt((y/n)*(1-y/n)/n)

theta_hat_bayes <- (theta_hat_prior/se_prior^2 + theta_hat_data/se_data^2) / (1/se_prior^2 + 1/se_data^2)
se_bayes <- sqrt(1/(1/se_prior^2 + 1/se_data^2))

par(mar=c(3,1,1,1), mgp=c(1.5, 0.5, 0), tck=-.02)
plot(0, 0, xlim=c(0.37, 0.67), ylim=c(0, 20), xlab=expression(theta), xaxt="n", ylab="", yaxs="i", yaxt="n", bty="n", cex.lab=1.2)
axis(1, seq(0.3, 0.7, 0.1))
curve(dnorm(x, theta_hat_prior, se_prior), n=1000, add=TRUE)
text(0.588, 5, "Prior")
curve(dnorm(x, theta_hat_data, se_data), n=1000, add=TRUE)
text(0.420, 8, "Likelihood")

par(mar=c(3,1,1,1), mgp=c(1.5, 0.5, 0), tck=-.02)
plot(0, 0, xlim=c(0.37, 0.67), ylim=c(0, 20), xlab=expression(theta), xaxt="n", ylab="", yaxs="i", yaxt="n", bty="n", cex.lab=1.2)
axis(1, seq(0.3, 0.7, 0.1))
curve(dnorm(x, theta_hat_prior, se_prior), n=1000, add=TRUE, col="gray30")
text(0.588, 5, "Prior")
curve(dnorm(x, theta_hat_data, se_data), n=1000, add=TRUE, col="gray30")
text(0.420, 8, "Likelihood")
curve(dnorm(x, theta_hat_bayes, se_bayes), n=1000, add=TRUE)
text(0.525, 15, "Posterior")
