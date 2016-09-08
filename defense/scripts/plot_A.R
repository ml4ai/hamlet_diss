plot.A <- function(path1, path2, iteration1 = 10000, iteration2 = 10000)
{
    A.noLT <- as.matrix(read.table(paste(path1, "/G/block_A/", iteration, ".txt", sep = "")))
    A.LT <- as.matrix(read.table(paste(path2, "/G/block_A/", iteration, ".txt", sep = "")))
    J.noLT <- nrow(A.noLT)
    J.LT <- nrow(A.LT)
    par(mfrow = c(1,2))
    image(t(A.noLT[J.noLT:1,]), col = heat.colors(100), xaxt = "n", yaxt = "n")
    axis(side = 1, at = seq(0,1,length=J.noLT), labels = 1:J.noLT)
    axis(side = 2, at = seq(0,1,length=J.noLT), labels = 1:J.noLT)
    image(t(A.LT[J.LT:1,]]), col = heat.colors(100), xaxt = "n", yaxt = "n")
    axis(side = 1, at = seq(0,1,length=J.LT), labels = 1:J.LT)
    axis(side = 2, at = seq(0,1,length=J.LT), labels = 1:J.LT)
}
