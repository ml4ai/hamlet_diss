source("format.R")

### Define different prevalences

n <- 10000
k <- 1000
p.y <- seq(0.000001, 1, length = n)
p.x <- rep(1 / k, n)

### Define priors
prior.broad <- rep(1/2, n)
prior.narrow <- 1/2 * p.x

### Define likelihoods
l.narrow <- p.x
l.broad <- p.x * p.y

### Likelihood ratio
l.ratio <- l.narrow / l.broad

### Posterior ratio
odds.ratio <- (l.narrow * prior.narrow) / (l.broad * prior.broad)

### Plot results
plot.arrows <- function()
{
    arrows(
        x0 = -log10(1/300), x1 = -log10(1/300), y0 = -4, y1 = -5,
        length = 0.1, lwd = 2, col = solar["blue"])
    arrows(
        x0 = -log10(1/20000), x1 = -log10(1/20000), y0 = -4, y1 = -5,
        length = 0.1, lwd = 2, col = solar["magenta"])
    text(-log10(1/300), -3.4, "di", col = solar["blue"], cex = 1.2)
    text(-log10(1/20000), -3.4, "zhi", col = solar["magenta"], cex = 1.2)
}


pdf("../img/likelihood-ratios.pdf", width = 6, height = 4)
set_pars()
plot(
    -log10(p.y), log10(l.ratio),
    type = "l", xlim = c(0, 5), ylim = c(-5, 5),
    xaxt = "n",
    col = solar["orange"], lwd = 2,
    xlab = paste("Relative Frequency of",
                 expression(Y), "Syllable"),
    ylab = "Log LR (+ favors narrow)"
    )
axis(1, at = 0:6, labels = 10^(-(0:6)))
abline(h = 0, lty = 3)
plot.arrows()
dev.off()

pdf("../img/odds-ratios.pdf", width = 6, height = 4)
set_pars()
plot(
    -log10(p.y), log10(odds.ratio),
    type = "l", xlim = c(0, 5), ylim = c(-5, 5),
    xaxt = "n",
    col = solar["green"], lwd = 2,
    xlab = paste("Relative Frequency of",
                 expression(Y), "Syllable"),
    ylab = "Log Posterior Odds (+ favors narrow)"
    )
options(scipen = 4)
axis(1, at = 0:6, labels = 10^(-(0:6)))
abline(h = 0, lty = 3)
plot.arrows()
dev.off()

pdf("../img/prior-grid.pdf", width = 6, height = 4)
set_pars()
par(mar = c(1,1,1,1))
plot(NULL, xaxt = "n", yaxt = "n",
     xaxs = "i", yaxs = "i",
     xlim = c(-1, 1), ylim = c(-1, 1),
     xlab = "", ylab = "", main = "")
abline(h = seq(-1, 1, 1/3), lty = 1)
abline(v = seq(0, 1, 1/3), lty = 1)
polygon(c(-1, 0, 0, -1, -1), c(-1, -1, 1, 1, -1), col = solar["bg"])
text(-0.5, 0, "XXY", cex = 2, col = solar["violet"])
text(0.5, 0.5, "XXzhi", cex = 2, col = solar["orange"])
text(5/6, 0.5, "XXba", cex = 2, col = solar["fg"])
text(1/6, 0.5, "XXdi", cex = 2, col = solar["fg"])
dev.off()
