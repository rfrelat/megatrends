panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  #usr <- par("usr")
  usr <- par()$usr
  on.exit(par(usr = usr))
  par(usr = c(0, 1, 0, 1))
  if (sum(complete.cases(cbind(x, y))) > 3) {
    r <- cor(x, y, use = "pairwise.complete.obs")
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if (missing(cex.cor)) {
      cex <- 0.5 / strwidth(txt)
    }

    test <- cor.test(x, y, use = "pairwise.complete.obs")
    # borrowed from printCoefmat
    Signif <- symnum(
      test$p.value,
      corr = FALSE,
      na = FALSE,
      cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
      symbols = c("***", "**", "*", ".", " ")
    )

    xtxt <- ifelse(par()$xlog, 10^0.5, 0.5)
    ytxt <- ifelse(par()$ylog, 10^0.5, 0.5)
    xstar <- ifelse(par()$xlog, 10^0.8, 0.8)
    ystar <- ifelse(par()$ylog, 10^0.8, 0.8)
    text(xtxt, ytxt, txt, cex = cex * abs(r))
    text(xstar, ystar, Signif, cex = cex, col = 2)
  }
}
