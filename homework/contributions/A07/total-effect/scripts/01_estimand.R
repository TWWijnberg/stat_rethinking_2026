# A07 step 01 — Estimand
#
# Step 01 has no model and no data. What it does have is the DAG, the adjustment set each
# estimand needs, and the claim each arrow makes. Those are the assumptions everything
# downstream rests on, so they get drawn.
#
# NOTE: rethinking must not be attached. See docs/adr/0001-brms-over-rethinking.md.

library(dagitty)
library(here)

OUT <- here("homework", "contributions", "A07", "total-effect", "outputs")

fox_dag <- dagitty("dag{ A -> F  F -> G  F -> W  G -> W }")

cat("\n================ STEP 01 ================\n")
cat("A = area   F = avgfood   G = groupsize   W = weight\n\n")
for (e in c("total", "direct")) {
  cat(sprintf("%-6s effect of F on W, minimal adjustment set: ", e))
  print(adjustmentSets(fox_dag, "F", "W", effect = e))
}

cat("\nevery path from F to W:\n")
p <- paths(fox_dag, "F", "W")
for (i in seq_along(p$paths))
  cat(sprintf("  %-20s %s\n", p$paths[i], ifelse(p$open[i], "open", "closed")))
cat("\nNo path leaves F through an incoming arrow. There is no backdoor path, so the\n")
cat("total effect needs no adjustment. G is a descendant of F, so conditioning on it\n")
cat("gives the direct effect instead.\n")


# ---- Draw it -----------------------------------------------------------------

INK <- "#17211F"; ACC <- "#A8501F"; STR <- "#2C6455"; GREY <- "#AFB9B3"
pos <- list(A = c(0, 2), F = c(-1, 1), G = c(1, 1), W = c(0, 0))

arrow_between <- function(from, to, col, lwd = 2, lty = 1, s_from = 0.22, s_to = 0.22) {
  a <- pos[[from]]; b <- pos[[to]]
  d <- b - a; u <- d / sqrt(sum(d^2))
  arrows(a[1] + u[1] * s_from, a[2] + u[2] * s_from,
         b[1] - u[1] * s_to,   b[2] - u[2] * s_to,
         length = 0.11, lwd = lwd, col = col, lty = lty)
}

labels <- c(A = "area", F = "avgfood", G = "groupsize", W = "weight")

node <- function(n, conditioned = FALSE) {
  q <- pos[[n]]
  if (conditioned) {
    rect(q[1] - 0.19, q[2] - 0.15, q[1] + 0.19, q[2] + 0.15,
         col = INK, border = INK, lwd = 2)
    text(q[1], q[2], n, col = "white", font = 2, cex = 1.4)
  } else {
    text(q[1], q[2], n, col = INK, font = 2, cex = 1.5)
  }
  text(q[1], q[2] - 0.3, labels[[n]], col = "#7A857F", cex = 0.72)
}

panel <- function(main, sub, hold_G) {
  plot(NA, xlim = c(-1.7, 1.7), ylim = c(-0.9, 2.4), axes = FALSE,
       xlab = "", ylab = "", main = main, font.main = 1, cex.main = 1.15, col.main = INK)
  mtext(sub, side = 3, line = -0.4, cex = 0.76, col = "#7A857F")
  ind <- if (hold_G) GREY else STR          # the indirect path F -> G -> W
  ilt <- if (hold_G) 2 else 1
  gs <- if (hold_G) 0.33 else 0.22          # clear the box when G is conditioned on
  arrow_between("F", "G", ind, 2.5, ilt, s_to = gs)
  arrow_between("G", "W", ind, 2.5, ilt, s_from = gs)
  arrow_between("F", "W", ACC, 3)           # the direct path, always live
  arrow_between("A", "F", GREY, 2)
  for (n in names(pos)) node(n, hold_G && n == "G")
}

png(file.path(OUT, "01_estimand_dag.png"), width = 1150, height = 480, res = 120)
op <- par(mfrow = c(1, 2), mar = c(1, 1, 3.4, 1))

panel("TOTAL effect of food on weight", "adjustment set: none", FALSE)
legend("bottom", c("direct path  F to W", "indirect path  F to G to W  (open)"),
       col = c(ACC, STR), lwd = c(3, 2.5), bty = "n", cex = 0.72, y.intersp = 1.3)

panel("DIRECT effect of food on weight", "adjustment set: {G}", TRUE)
legend("bottom", c("direct path  F to W", "indirect path closed by conditioning on G"),
       col = c(ACC, GREY), lwd = c(3, 2.5), lty = c(1, 2), bty = "n",
       cex = 0.72, y.intersp = 1.3)

par(op); invisible(dev.off())
cat("\nwrote", file.path(OUT, "01_estimand_dag.png"), "\n")
