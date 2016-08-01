library(car)
library(ggplot2)

load("bayesrep.RData")

source("format.R")



### Music Intervals

p <- c(0,pnorm(seq(-6.5,6.5,1), 0, 2), 1)

pdf("../img/music-intervals.pdf", width = 6, height = 4)
set_pars()
plot(
    -7:7, diff(p), type = 'h', lwd = 4,
    col = solar["cyan"],
    xlab = "Interval Size (scale steps)",
    ylab = "Probability")
dev.off()

### Music Intervals (Semitone)

pdf("../img/music-intervals-semitones.pdf", width = 6, height = 4)
set_pars()
p <- c(0.00001, 0.0001, 0.001, 0.005,
       0.01, 0.025, 0.001, 0.035,
       0.09, 0.10, 0.125, 0.045,
       0.22, 0.03, 0.11, 0.08,
       0.05, 0.05, 0.0001, 0.02,
       0.015, 0.01, 0.001, 0.0001,
       0.008)
plot(
    -12:12, p, type = 'h', lwd = 4,
    col = solar["violet"],
    xlab = "Interval Size (semitones)",
    ylab = "Probability")
dev.off()

### Bayes Rep Plots

makeBRPlot <-
    function(
        means,
        ses,
        o,
        g,
        ylim,
        ylab,
        chance,
        main = ""
        )
{
    barplot(
        matrix(c(0,0,0)[o],length(o),1),
        ylim = ylim,
        beside = TRUE,
        names.arg = c("Repetition", "Smooth" , "Uniform")[o],
        ylab = ylab,
        main = main
        )

    points(
        seq(1.5, 3.5, 1)[1:length(o)],
        means[o,g],
        col = solar[c("blue", "green", "orange")][o],
        pch = (17:15)[o]
        )

    arrows(
        x0 = seq(1.5, 3.5, 1)[1:length(o)],
        x1 = seq(1.5, 3.5, 1)[1:length(o)],
        y0 = means[o,g] - ses[o,g],
        y1 = means[o,g] + ses[o,g],
        col = solar[c("blue", "green", "orange")][o],
        code = 3, angle = 90, length = 0.1
        )

    arrows(
        x0 = 1.5, x1 = 2.5, y0 = 85, y1 = 85,
        code = 3, angle = 90, length = 0.05, lwd = 2)

    if(length(o) == 3)
    {
        arrows(
            x = 2.0, x1 = 3.5, y0 = 95, y1 = 95,
            code = 3, angle = 90, length = 0.05, lwd = 2)
    }


    text(2.0, 87.5, ifelse(length(o) == 3, "n.s.", "* (p < 0.05)"))
    text(2.75, 98, ifelse(length(o) == 3, "*", ""))

    abline(h = chance, lty = 2)

    abline(h = seq(40,100,5), lty = 3, lwd = 0.5)
    if(length(o) == 3)
    {
        abline(v = c(2,3), lty = 3, lwd = 0.5)
    } else
    {
        abline(v = c(2), lty = 3, lwd = 0.5)
    }
}


### Bayes Rep Plot (2 groups)

pdf("../img/brplot.pdf", width = 4, height = 4)
set_pars()
makeBRPlot(
    means = RCmeans.N.med / .96,
    ses = RCse.N.med / 0.96,
    o = c(3,2),
    g = 1,
    ylim = c(40,100),
    ylab = "% Correct",
    chance = 50)
dev.off()


### Bayes Rep Plot (3 groups)

pdf("../img/brplot-3.pdf", width = 6, height = 4)
set_pars()
makeBRPlot(
    means = RCmeans.N.med / .96,
    ses = RCse.N.med / 0.96,
    o = c(1,3,2),
    g = 1,
    ylim = c(40,100),
    ylab = "% Correct",
    chance = 50)
dev.off()

### Bayes Rep Contour Plot (2 groups)

pdf("../img/br-contour-plot.pdf", width = 4, height = 4)
set_pars()
makeBRPlot(
    means = RCmeans.N.med / .96,
    ses = RCse.N.med / 0.96,
    o = c(3,2),
    g = 2,
    ylim = c(40,70),
    ylab = "% Correct",
    chance = 50)
dev.off()


### Transition Probabilities

p_star <- diff(c(pnorm(seq(0.5, 7.5, 1), 3, 2.4)))
p <- p_star / sum(p_star)

pdf("../img/repetition-transitional-probabilities.pdf", width = 7, height = 4)
set_pars()
par(mfrow = c(1,2))
plot(
    1:7, rep(1/7, 7),
    type = "h", lwd = 3, col = solar["orange"],
    xlab = "Next Tone Following Tone 3", ylab = "Probability",
    main = "Uniform Condition",
    ylim = c(0, 0.25))
plot(
    1:7, p,
    type = "h", lwd = 3, col = solar["green"],
    xlab = "Next Tone Following Tone 3", ylab = "Probability",
    main = "Smooth Condition",
    ylim = c(0, 0.25))
dev.off()

### Transition Probabilities (3 groups)

remainder <- (1 - p[3]) / 6

pdf("../img/repetition-transitional-probabilities-3.pdf", width = 6, height = 2.5)
set_pars()
par(mfrow = c(1,3))
plot(
    1:7, c(remainder, remainder, p[3], rep(remainder, 4)),
    type = "h", lwd = 3, col = solar["blue"],
    xlab = "Next Tone Following Tone 3", ylab = "Probability",
    main = "Repetition Condition",
    ylim = c(0, 0.25))
plot(
    1:7, rep(1/7, 7),
    type = "h", lwd = 3, col = solar["orange"],
    xlab = "Next Tone Following Tone 3", ylab = "Probability",
    main = "Uniform Condition",
    ylim = c(0, 0.25))
plot(
    1:7, p,
    type = "h", lwd = 3, col = solar["green"],
    xlab = "Next Tone Following Tone 3", ylab = "Probability",
    main = "Smooth Condition",
    ylim = c(0, 0.25))
dev.off()


### Baby Data

babybr.means <-
    matrix(
        c(2.8, -2.2, 5.8,
          1.8, -0.2, 3.2),
        3,2)

babybr.ses <-
    matrix(
        c(2.2, 2.3, 1.8,
          1.2, 1.9, 1.4),
        3,2)

rclabels <-
    list(
        c("Rep", "Smooth", "Unif"),
        c("4m", "7m")
        )

dimnames(babybr.means) <- rclabels
dimnames(babybr.ses) <- rclabels


### Baby Plot
pdf("../img/br-baby-plot.pdf", width = 8, height = 4.5)
set_pars()
par(mfrow = c(1,2))
makeBRPlot(
    means = babybr.means,
    ses = babybr.ses,
    o = c(1,3,2),
    g = 1,
    ylim = c(-6, 12),
    ylab = "Mean Preference (sec.)",
    chance = 0,
    main = "4-month-olds")
arrows(
    x0 = 1.5, x1 = 2.5, y0 = 8.25, y1 = 8.25,
    code = 3, angle = 90, length = 0.05, lwd = 2)
text(2.0, 9, "n.s.")
arrows(
    x0 = 2.0, x1 = 3.5, y0 = 10, y1 = 10,
    code = 3, angle = 90, length = 0.05, lwd = 2)
text(2.75, 10.75, "*")
makeBRPlot(
    means = babybr.means,
    ses = babybr.ses,
    o = c(1, 3, 2),
    g = 2,
    ylim = c(-6, 12),
    ylab = "Mean Preference (sec.)",
    chance = 0,
    main = "7-month-olds")
arrows(
    x0 = 1.5, x1 = 2.5, y0 = 6, y1 = 6,
    code = 3, angle = 90, length = 0.05, lwd = 2)
text(2.0, 6.75, "n.s.")
arrows(
    x0 = 2.0, x1 = 3.5, y0 = 7.75, y1 = 7.75,
    code = 3, angle = 90, length = 0.05, lwd = 2)
text(2.75, 8.5, "*")
ellipse(
    c(2, 2.5),
    matrix(c(0.2, 0.1, 0.1, 1), 2, 2),
    radius = 2.5,
    col = solar["violet"],
    center.pch = FALSE
    )
dev.off()



### BR Model Plots
source("~/research/psych/BR Model/getData.R")
source("~/research/psych/BR Model/makePlot.R")

pdf("../img/br-model-plots.pdf", width = 7, height = 4.5)
set_pars()
makePlots(
    subset(
        modelData,
        Context %in% c("U","RLV","SLV")
        & S == 0.1 & N == 20
        )
    )
dev.off()


### clustering diagram

pdf("../img/cluster-diagram.pdf", width = 5, height = 5)
set_pars()
x1 <- mvrnorm(30, c(-1, 1), diag(c(0.01, 1)))
x2 <- mvrnorm(60, c(1, -1), diag(c(1, 0.01)))
x3 <- mvrnorm(12, c(1, 1), matrix(c(1, 0.95, 0.95, 1), 2, 2))
plot(
    NULL,
    xlim = c(-2, 2),
    ylim = c(-2, 2),
    xaxt = "n",
    yaxt = "n",
    xlab = "", ylab = "", main = "")
points(x1, col = solar["orange"], pch = 15)
points(x2, col = solar["green"], pch = 16)
points(x3, col = solar["violet"], pch = 17)
dev.off()
