# A07 step 07 — Fit (direct-effect)
#
# Fit Ws ~ Fs + Gs to the real 116 foxes. Diagnostics are read BEFORE any coefficient:
# a coefficient from a sampler that did not converge is a number with nothing attached.
#
# NOTE: rethinking must not be attached. See docs/adr/0001-brms-over-rethinking.md.

library(brms)
library(bayesplot)
library(posterior)
library(here)

options(brms.backend = "cmdstanr", mc.cores = parallel::detectCores())
MD   <- here(".stan_models")
TE   <- here("homework", "contributions", "A07", "total-effect", "data")
OUT  <- here("homework", "contributions", "A07", "direct-effect", "outputs")
DATA <- here("homework", "contributions", "A07", "direct-effect", "data")

spec   <- readRDS(file.path(DATA, "04_model_spec.rds"))
priors <- readRDS(file.path(TE, "05_priors.rds"))

# ---- Fit ---------------------------------------------------------------------
# NUTS, not Laplace: this is the fit we report. ADR 0001.

fit <- brm(spec$formula, data = spec$dat, family = spec$family, prior = priors,
           chains = 4, iter = 2000, warmup = 1000, seed = 7,
           refresh = 0, silent = 2, stan_model_args = list(dir = MD),
           file = file.path(DATA, "07_fit"))

# ---- Diagnostics, before anything else ---------------------------------------

cat("\n\n================ STEP 07 — direct-effect ================\n")
np <- nuts_params(fit)
div <- sum(subset(np, Parameter == "divergent__")$Value)
td  <- subset(np, Parameter == "treedepth__")$Value
maxtd <- attr(fit$fit@sim, "control")$max_treedepth
maxtd <- if (is.null(maxtd)) 10 else maxtd

cat(sprintf("divergent transitions : %d\n", div))
cat(sprintf("max treedepth hits    : %d (limit %d)\n", sum(td >= maxtd), maxtd))

s <- summarise_draws(as_draws_df(fit), "mean", "sd", "rhat", "ess_bulk", "ess_tail")
s <- s[!s$variable %in% c("lprior", "lp__", "Intercept"), ]
cat("\nper-parameter diagnostics:\n")
print(as.data.frame(lapply(s, function(x) if (is.numeric(x)) round(x, 4) else x)),
      row.names = FALSE)

cat(sprintf("\nworst R-hat: %.4f   lowest ess_bulk: %.0f   lowest ess_tail: %.0f\n",
            max(s$rhat), min(s$ess_bulk), min(s$ess_tail)))
ok <- max(s$rhat) < 1.01 && min(s$ess_bulk) > 400 && min(s$ess_tail) > 400 && div == 0
cat(if (ok) "all four diagnostics clean\n" else "SOMETHING NEEDS A RESPONSE\n")

# ---- Traceplot ---------------------------------------------------------------
png(file.path(OUT, "07_trace.png"), width = 1150, height = 420, res = 118)
print(mcmc_trace(as_draws_df(fit), pars = s$variable) +
        ggplot2::theme_minimal(base_size = 10))
invisible(dev.off())
cat("wrote", file.path(OUT, "07_trace.png"), "\n")
