# A07 step 03 — Data
#
# Load data(foxes) unmodified, describe it, build the analysis frame, and test the frame
# against what the step 02 generative model produces. Shared with ../../direct-effect/.
#
# The descriptive statistics live here rather than in a separate script: describing the
# data is part of loading it, not a step of its own.
#
# NOTE: rethinking must not be attached. See docs/adr/0001-brms-over-rethinking.md.

library(here)
source(here("homework", "contributions", "A07", "total-effect",
            "scripts", "02_generative_model.R"))

OUT  <- here("homework", "contributions", "A07", "total-effect", "outputs")   # figures, reports
DATA <- here("homework", "contributions", "A07", "total-effect", "data")      # rds, raw data


# ---- Raw data, unmodified ----------------------------------------------------

e <- new.env(); data("foxes", package = "rethinking", envir = e)
raw <- e$foxes

cat("\n\n================ STEP 03 ================\n")
cat("raw:", nrow(raw), "rows,", ncol(raw), "columns:", paste(names(raw), collapse = ", "), "\n")
cat("missing values:", sum(is.na(raw)), "\n")
cat("complete rows:", sum(complete.cases(raw)), "of", nrow(raw), "\n")
cat("duplicated rows:", sum(duplicated(raw)), "\n")


# ---- Inclusion and exclusion -------------------------------------------------
# No exclusions. Every one of the 116 foxes has all four variables recorded, none is
# out of range, and the DAG applies to all of them equally. N stays 116.

dat_raw <- raw


# ---- Describe the data -------------------------------------------------------
# Plain descriptive statistics, before anything is transformed or compared to the
# generative model. Univariate for all four variables; bivariate for the four
# relationships the DAG asserts, and no others.

grp <- raw[!duplicated(raw$group), ]   # one row per territory

cat(nrow(raw), "foxes in", length(unique(raw$group)), "groups.",
    "No missing values, no duplicate rows.\n")
cat("Group sizes range", min(grp$groupsize), "to", max(grp$groupsize),
    "| foxes per group in the file:", min(table(raw$group)), "to", max(table(raw$group)), "\n")

# ---- Univariate --------------------------------------------------------------
uni <- function(x) c(n = length(x), mean = mean(x), sd = sd(x), min = min(x),
                     q25 = quantile(x, .25), median = median(x),
                     q75 = quantile(x, .75), max = max(x))

cat("\n-- per fox (n = 116) --\n")
print(round(t(sapply(raw[, c("area", "avgfood", "groupsize", "weight")], uni)), 3))

cat("\n-- per territory (n = 30) — area, avgfood and groupsize are territory-level --\n")
print(round(t(sapply(grp[, c("area", "avgfood", "groupsize")], uni)), 3))

cat("\ngroup size distribution (territories):\n"); print(table(grp$groupsize))
cat("group size distribution (foxes):\n");        print(table(raw$groupsize))

# ---- Bivariate ---------------------------------------------------------------
vars <- c("area", "avgfood", "groupsize", "weight")
cat("\n-- correlations, per fox --\n"); print(round(cor(raw[, vars]), 3))

cat("\n-- the four relationships the DAG asserts --\n")
edges <- list(c("area","avgfood"), c("avgfood","groupsize"),
              c("avgfood","weight"), c("groupsize","weight"))
tab <- do.call(rbind, lapply(edges, function(p) {
  m <- lm(raw[[p[2]]] ~ raw[[p[1]]])
  data.frame(edge = paste(p[1], "->", p[2]),
             r = cor(raw[[p[1]]], raw[[p[2]]]),
             slope = coef(m)[2],
             r2 = summary(m)$r.squared)
}))
print(round_df <- data.frame(edge = tab$edge, r = round(tab$r, 3),
                             slope = round(tab$slope, 3), r2 = round(tab$r2, 3)))

cat("\nmean weight by group size:\n")
print(round(tapply(raw$weight, raw$groupsize, mean), 2))

# ---- Figures -----------------------------------------------------------------
# Two panels rather than one: distributions, then the four relationships the DAG asserts.

INK <- "#17211F"; FILL <- "#D6DDD8"; ACC <- "#A8501F"; STR <- "#2C6455"

png(file.path(OUT, "03_eda_hist.png"), width = 1300, height = 380, res = 120)
op <- par(mfrow = c(1, 4), mar = c(4.4, 4.2, 3.2, 1), col.axis = INK, col.lab = INK,
          fg = "#9AA6A0", cex.main = 1.15, font.main = 1)
hist(raw$area, breaks = 14, col = FILL, border = "white", main = "area", xlab = "km2")
hist(raw$avgfood, breaks = 14, col = FILL, border = "white", main = "avgfood", xlab = "index")
barplot(table(raw$groupsize), col = FILL, border = "white", main = "groupsize", xlab = "foxes")
hist(raw$weight, breaks = 14, col = FILL, border = "white", main = "weight", xlab = "kg")
par(op); invisible(dev.off())

png(file.path(OUT, "03_eda_scatter.png"), width = 1300, height = 380, res = 120)
op <- par(mfrow = c(1, 4), mar = c(4.4, 4.2, 3.2, 1), col.axis = INK, col.lab = INK,
          fg = "#9AA6A0", cex.main = 1.15, font.main = 1)
sc <- function(x, y, xl, yl, main, jit = FALSE, col = ACC) {
  xx <- if (jit) jitter(x, 0.8) else x
  plot(xx, y, pch = 16, col = "#17211F55", xlab = xl, ylab = yl, main = main)
  abline(lm(y ~ x), lwd = 2.5, col = col)
  legend("topleft", sprintf("r = %+.2f", cor(x, y)), bty = "n",
         cex = 1.05, text.col = col, text.font = 2)
}
sc(raw$area, raw$avgfood, "area (km2)", "avgfood", "A -> F", col = STR)
sc(raw$avgfood, raw$groupsize, "avgfood", "groupsize", "F -> G", col = STR)
sc(raw$avgfood, raw$weight, "avgfood", "weight (kg)", "F -> W")
sc(raw$groupsize, raw$weight, "groupsize", "weight (kg)", "G -> W", jit = TRUE)
par(op); invisible(dev.off())

cat("
wrote 03_eda_hist.png and 03_eda_scatter.png
")


# ---- Transformations ---------------------------------------------------------
# Standardise avgfood, groupsize and weight (step 01 M2). Standardising is a linear
# rescaling: it changes the units the coefficients are stated in, not what the estimand
# means. The scaling constants are kept so step 09 can convert back to kg.

scales <- list(
  F = c(mean = mean(raw$avgfood),   sd = sd(raw$avgfood)),
  G = c(mean = mean(raw$groupsize), sd = sd(raw$groupsize)),
  W = c(mean = mean(raw$weight),    sd = sd(raw$weight)),
  A = c(mean = mean(raw$area),      sd = sd(raw$area))
)

dat <- data.frame(
  group = raw$group,
  Fs = (raw$avgfood   - scales$F["mean"]) / scales$F["sd"],
  Gs = (raw$groupsize - scales$G["mean"]) / scales$G["sd"],
  Ws = (raw$weight    - scales$W["mean"]) / scales$W["sd"],
  As = (raw$area      - scales$A["mean"]) / scales$A["sd"]
)

cat("\nanalysis frame:", nrow(dat), "rows.", "scaling constants (mean, sd):\n")
print(round(do.call(rbind, scales), 3))


# ---- Data testing: real against simulated ------------------------------------
# Simulate a dataset the size of the real one and put them side by side.

sim116 <- simulate_foxes(n = nrow(dat))
names(sim116) <- c("As", "Fs", "Gs", "Ws")

summ <- function(x) c(mean = mean(x), sd = sd(x), min = min(x), max = max(x))
vars <- c("As", "Fs", "Gs", "Ws")

cat("\nmarginals — real vs simulated (n = 116 each):\n")
cmp <- do.call(rbind, lapply(vars, function(v)
  c(real = summ(dat[[v]]), sim = summ(sim116[[v]]))))
rownames(cmp) <- vars
print(round(cmp, 2))

pair_cor <- function(d) {
  m <- cor(d[, vars]); m[upper.tri(m)]
}
labs <- c("A-F", "A-G", "F-G", "A-W", "F-W", "G-W")
big <- simulate_foxes(1e5); names(big) <- vars
cc <- rbind(real = pair_cor(dat), sim_116 = pair_cor(sim116), sim_1e5 = pair_cor(big))
colnames(cc) <- labs
cat("\npairwise correlations:\n"); print(round(cc, 3))

cat("\ndistinct values (real data is territory-level, repeated within group):\n")
print(rbind(real = sapply(dat[, vars], function(x) length(unique(x))),
            sim  = sapply(sim116[, vars], function(x) length(unique(x)))))

png(file.path(OUT, "03_data_check.png"), width = 1150, height = 760, res = 110)
op <- par(mfrow = c(2, 4), mar = c(4, 4, 3, 1))
for (v in vars) {
  plot(density(sim116[[v]]), main = paste("marginal:", v), xlab = v, lwd = 2,
       col = "grey30", ylim = c(0, max(density(dat[[v]])$y) * 1.15))
  lines(density(dat[[v]]), lwd = 2, col = "firebrick")
  legend("topright", c("simulated", "real"), col = c("grey30", "firebrick"),
         lwd = 2, bty = "n", cex = 0.75)
}
for (p in list(c("Fs","Ws"), c("Gs","Ws"), c("Fs","Gs"), c("As","Fs"))) {
  plot(sim116[[p[1]]], sim116[[p[2]]], pch = 16, col = "#00000030",
       main = paste(p[1], "vs", p[2]), xlab = p[1], ylab = p[2])
  points(dat[[p[1]]], dat[[p[2]]], pch = 16, col = "#B2222299")
}
par(op); invisible(dev.off())
cat("\nwrote", file.path(OUT, "03_data_check.png"), "\n")

dir.create(file.path(DATA, "raw"), showWarnings = FALSE, recursive = TRUE)
write.csv(raw, file.path(DATA, "raw", "foxes.csv"), row.names = FALSE)   # immutable snapshot
saveRDS(list(dat = dat, scales = scales), file.path(DATA, "03_analysis_frame.rds"))
