library(MASS)
library(mvtnorm)
library(mvnmle)

source("format.R")

get_sigma <- function(x, l=3)
{
    Sigma <- matrix(rep(0, length(x)^2), nrow = length(x))
    for (i in 1:nrow(Sigma))
    {
        for (j in i:nrow(Sigma))
        {
            Sigma[i,j] <- Sigma[j,i] <- exp(-0.5 * (abs(x[i] - x[j])/l)^2)
        }
    }
    return(Sigma)
}

compute_cluster_posterior <-
    function(Y, assignments, Sigma_0, mu_0, Sigma)
{
    Sigma.post <- list()
    mu.post <- list()
    for(k in 1:length(unique(assignments)))
    {
        indices <- which(assignments == k)
        n <- length(indices)
        post.projection <- solve(Sigma_0 + Sigma / n)
        yk <-
            matrix(
                Y[indices,],
                nrow = length(indices),
                ncol = ncol(Y)
                )
        xbar <- apply(yk, MARGIN = 2, FUN = mean)
        mu.post[[k]] <-
            Sigma_0 %*% post.projection %*% xbar +
                (1/n)*Sigma %*% post.projection %*% mu_0
        Sigma.post[[k]] <- Sigma_0 %*% post.projection %*% ((1/n) * Sigma)
    }
    return(
        list(
            mu = mu.post,
            Sigma = Sigma.post
            )
        )
}

sample_ll <- function(y, labels, post.params, Sigma, prior.params)
{
    ll <- 0
    prior.density <- 0
    post.density <- 0
    centers <- list()
    for(j in 1:length(post.params$mu))
    {
        indices <- which(labels == j)
        centers[[j]] <- mvrnorm(1, post.params$mu[[j]], post.params$Sigma[[j]])
        prior.density <-
            prior.density +
                log(dmvnorm(centers[[j]], prior.params$mu, prior.params$Sigma))
        post.density <-
            post.density +
                log(dmvnorm(centers[[j]], post.params$mu[[j]], post.params$Sigma[[j]]))
        for(i in indices)
        {
            ll <- ll + log(dmvnorm(y[i,], centers[[j]], Sigma))
        }
    }
    return(list(centers = centers, ml = ll + prior.density - post.density))
}

plot_raw_data <- function(x, y, colors = NULL)
{
    set_pars()
    par(mgp = c(1.5, 1, 0))
    plot(
        NULL, NULL,
        xlim = range(x), ylim = c(-1, 2),
        xaxt = "n", yaxt = "n",
        xlab = "Time", ylab = "Position"
        )
    for(j in 1:nrow(y))
    {
        lines(x, y[j,], lty = 1, type = "l",
              col = ifelse(is.null(colors), par("fg"), colors[j]))
    }
}


set.seed(1)

### Define/generate parameters
xmesh <- seq(-1, 1, 0.01)
xsubmesh <- seq(1, 201, by = 40)
Sigma_0 <- get_sigma(xmesh, 1)
mu_0 <- rep(0, length(xmesh))
mu <- mvrnorm(3, mu_0, Sigma_0)
Sigma <- 1/30 * Sigma_0

### Generate data
y1 <- mvrnorm(3, mu[1,], Sigma)
y2 <- mvrnorm(2, mu[2,], Sigma)
y3 <- mvrnorm(4, mu[3,], Sigma)
y <- rbind(y1, y2, y3)

### Define cluster hypotheses
assignments <-
    list(
        rep(1, 9),
        rep(c(1,2), times = c(5,4)),
        rep(c(1,2,3), times = c(3,2,4)),
        rep(c(1,2,3,4), times = c(3,2,3,1)),
        1:9
        )
num.clusters <- sapply(assignments, function(x){length(unique(x))})

### Set plotting parameters
traj.colors <-
    rep(solar[c("green", "orange", "cyan", "blue", "red", "magenta", "violet",
            "yellow", "base02")], 3)
sample_and_plot <-
    function(
        iters, y, labels,
        Sigma_0, mu_0, Sigma, x,
        n_to_plot
        )
{
    centers <- list()
    ## lik <- 0
    marg.lik <- 0
    params <-
        compute_cluster_posterior(
            y[,x],
            labels,
            Sigma_0[x, x],
            mu_0[x],
            Sigma[x, x]
            )
    plot_raw_data(xmesh, y)
    for(it in 1:iters)
    {
        samp <-
            sample_ll(
                y[,x],
                labels,
                params,
                Sigma[x, x],
                list(mu = mu_0[x], Sigma = Sigma_0[x,x])
                )
        centers <- append(centers, samp$centers)
        ## lik <- lik + exp(samp$lik)
        marg.lik <- marg.lik + samp$ml
        if(it <= n_to_plot)
        {
            for(g in 1:length(samp$centers))
            {
                lines(
                    xmesh[x], samp$centers[[g]], lty = 2, lwd = 1.5,
                    col = traj.colors[g]
                    )
            }
        }
    }
    ## return(list(ll = log(lik / iters), ml = marg.lik / iters))
    return(marg.lik / iters)
}

run_it_all <- function(l, n_to_plot)
{
   ll <-
       sample_and_plot(
          10, y, assignments[[l]],
           Sigma_0, mu_0, Sigma, xsubmesh, n_to_plot
           )
   return(ll)
}

pdf("../img/GP_raw_data.pdf", width = 5, height = 5)
plot_raw_data(xmesh, y)
dev.off()

pdf("../img/GP_ground_truth.pdf", width = 5, height = 5)
plot_raw_data(xmesh, y)
for(j in unique(assignments[[3]]))
{
    lines(xmesh, mu[j,], lty = 1, lwd = 2, col = traj.colors[j])
}
dev.off()

results <- numeric(0)
for(j in 1:length(assignments))
{
    pdf(paste("../img/GP_fit_", j, ".pdf", sep = ""), width = 9, height = 5)
    set_pars()
    par(mfrow = c(1,2))
    results[[j]] <- run_it_all(j, 2)
    plot(NULL, xlab = "Number of clusters", ylab = "Log Marginal Likelihood",
         xaxt = "n", yaxt = "n",
         xlim = c(1, 10), ylim = c(-500, 200)
         )
    points(num.clusters[1:j], results[1:j], pch = 15, col = solar["red"], type = "b")
    axis(side = 1, labels = num.clusters[1:j], at = num.clusters[1:j])
    dev.off()
}

