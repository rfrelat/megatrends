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

panel.smooth.max <- function(
  x,
  y,
  col = par("col"),
  bg = NA,
  pch = par("pch"),
  cex = 1,
  col.smooth = 2,
  span = 2 / 3,
  iter = 3,
  maxN = 50000,
  ...
) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) > maxN) {
    ok <- sample(which(ok), size = maxN, replace = FALSE)
  }
  points(x[ok], y[ok], pch = pch, col = col, bg = bg, cex = cex)
  if (any(ok)) {
    lines(
      stats::lowess(x[ok], y[ok], f = span, iter = iter),
      col = col.smooth,
      ...
    )
  }
}

panel.eqline <- function(
  x,
  y,
  col = par("col"),
  bg = NA,
  pch = par("pch"),
  cex = 1,
  col.smooth = 2,
  span = 2 / 3,
  iter = 3,
  ...
) {
  col <- ifelse(x > y, "#1b9e77", "#7570b3")
  points(x, y, pch = pch, col = col, bg = col, cex = cex)
  abline(a = 0, b = 1, lty = 3)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) {
    lines(
      stats::lowess(x[ok], y[ok], f = span, iter = iter),
      col = col.smooth,
      ...
    )
  }
}

# function to remove some column
# and handle single column output
rm_col <- function(x, col) {
  sel <- !names(x) %in% col
  y <- data.frame(x[, sel])
  names(y) <- names(x)[sel]
  return(y)
}
