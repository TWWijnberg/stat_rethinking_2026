# A07 step 04 — Statistical model (direct effect)
#
# Specifies what gets fitted. Nothing is fitted here.
# Steps 02 and 03 are inherited from ../../total-effect/.
#
# NOTE: rethinking must not be attached. See docs/adr/0001-brms-over-rethinking.md.

library(brms)
library(here)

TE  <- here("homework", "contributions", "A07", "total-effect", "data")
DATA <- here("homework", "contributions", "A07", "direct-effect", "data")
dir.create(DATA, showWarnings = FALSE, recursive = TRUE)

dat <- readRDS(file.path(TE, "03_analysis_frame.rds"))$dat


# ---- Formula -----------------------------------------------------------------
# Ws ~ Fs + Gs     the adjustment set for the direct effect is {G}
#
# Term sources:
#   Fs         generative model (F -> W) and the exposure the estimand intervenes on
#   Gs         adjustment set — conditioning on the mediator closes F -> G -> W,
#              which is what makes the remaining Fs coefficient the DIRECT effect
#   Intercept  generative model
#
# As is deliberately ABSENT, for the same reason as in the total-effect model.
# No term here was added from looking at the data.

f_direct <- bf(Ws ~ Fs + Gs)

fam <- gaussian()

cat("\n================ STEP 04 (direct) ================\n")
cat("formula: "); print(f_direct)
cat("\nparameters requiring priors:\n")
print(get_prior(f_direct, data = dat, family = fam)[, c("prior", "class", "coef", "source")])

saveRDS(list(formula = f_direct, family = fam, dat = dat), file.path(DATA, "04_model_spec.rds"))
