## mlmodels_texreg_bridge.R -------------------------------------------------
## Make fits from the `mlmodels` package (class "mlmodel": ml_lm, ml_logit,
## ml_probit, ml_poisson, ml_negbin, ...) display in texreg's
## screenreg() / texreg() / htmlreg() regression tables.
##
## WHY IT'S NEEDED
##   texreg builds every table by calling its S4 generic extract() on each
##   model to harvest coefficients, SEs, p-values and goodness-of-fit stats.
##   There is no extract() method for "mlmodel", so texreg errors out with
##   "unable to find an inherited method for 'extract' for 'mlmodel'".
##
## THE APPROACH  ("read the package's own reliable outputs")
##   Unlike the sjPlot bridge -- which has to smuggle numbers through a
##   surrogate lm/glm scaffold because tab_model() delegates to
##   parameters::model_parameters() -- texreg's contract is simple and
##   self-contained: an extract() method just returns a `texreg` object built
##   by createTexreg(). So we read mlmodels' OWN summary()$coefficients and
##   confint(), and its logLik/AIC/BIC/nobs, and hand them straight to
##   createTexreg(). No scaffold, no surrogate.
##
## USAGE
##   source(here::here("code/mlmodels_texreg_bridge.R"))
##   library(texreg)
##   fit1 <- ml_lm(value = mpg ~ wt + hp, data = mtcars)
##   fit2 <- ml_logit(value = am ~ wt + hp, data = mtcars)
##   screenreg(list(fit1, fit2))                 # side-by-side, mixed families
##   texreg(list(fit1))                          # LaTeX
##   htmlreg(list(fit1, fit2), file = "tab.html")
##
##   # Confidence intervals instead of SE/p (uses mlmodels' own confint(),
##   # so it is correct even for non-normal vcov like robust/boot):
##   screenreg(fit1, include.ci = TRUE, ci.level = 0.90)
##
##   # Robust / clustered / bootstrap SEs -- pass vcov.type straight through;
##   # texreg forwards extra args to extract(). cl_var etc. ride along in `...`.
##   screenreg(fit2, vcov.type = "robust")
##   screenreg(fit2, vcov.type = "cluster", cl_var = mtcars$cyl)
##   # ...or set it once as an option (mirrors the sjPlot bridge):
##   options(mlmodels_vcov_type = "robust"); screenreg(fit2)
##   options(mlmodels_vcov_type = NULL)
##
## NOTES
##   * Always pass models inside list( ) -- the texreg idiom. A single BARE
##     model, e.g. screenreg(fit1), prints the right numbers but labels the
##     column "maximum": mlmodels fits carry "list" in their class and expose
##     element names (maximum, estimate, ...), which trips texreg's
##     model-naming heuristic (it outranks our object's model.name slot, so it
##     can't be fixed here). screenreg(list(fit1)) -> "Model 1"; or set
##     custom.model.names.
##   * The nuisance scale parameter (scale::lnsigma, ...) is dropped by
##     default; pass include.scale = TRUE to keep it.
##   * `value::` prefixes are stripped for display.
##   * The R^2 row reports mlmodels' own measure, labelled by family: ordinary
##     R^2 / Adj. R^2 for ml_lm; McFadden's R^2 for logit/poisson/negbin;
##     McKelvey-Zavoina R^2 for probit (a squared-correlation fallback is
##     labelled "Squared corr.", never "R^2").

suppressMessages({
  requireNamespace("texreg", quietly = TRUE)
  requireNamespace("mlmodels", quietly = TRUE)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- helper: coerce a possibly-list summary field to one finite number ----
# mlmodels stores adj.r.squared as a list($effective, $sample); scalars pass
# through. Return NA if nothing usable is found.
.mlm_first_num <- function(x) {
  if (is.list(x)) x <- x[["sample"]] %||% x[[1]]
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x)) x[1] else NA_real_
}

# ---- helper: read mlmodels' own R^2 / pseudo-R^2 with the right label -----
# summary()$r.squared is a plain scalar for ml_lm (ordinary R^2) but a named
# list for the others, whose members vary by family:
#   logit/poisson/negbin -> $cor + $mcfadden ;  probit -> $cor + $mczav.
# We surface the most standard measure available and label it honestly (a
# squared-correlation "cor" fallback is labelled as such, never as "R^2").
# Returns a list of (name, value) rows or an empty list.
.mlm_rsq_rows <- function(sm) {
  rs <- sm$r.squared
  if (is.null(rs)) return(list())
  if (!is.list(rs)) {                                   # ml_lm: ordinary R^2
    return(list(c("R$^2$",      .mlm_first_num(rs)),
                c("Adj. R$^2$", .mlm_first_num(sm$adj.r.squared))))
  }
  if (!is.null(rs$mcfadden))                            # logit / poisson / NB
    return(list(c("McFadden's R$^2$", .mlm_first_num(rs$mcfadden))))
  if (!is.null(rs$mczav))                               # probit
    return(list(c("McKelvey-Zavoina R$^2$", .mlm_first_num(rs$mczav))))
  if (!is.null(rs$cor))
    return(list(c("Squared corr.", .mlm_first_num(rs$cor))))
  list()
}

# ---- the extract() method -------------------------------------------------
# NB: vcov.type / include.* come AFTER `...` so that texreg's own forwarded
# args cannot partial-match them; args after `...` must be named in full.
# `...` (e.g. cl_var, repetitions, seed) is forwarded to summary()/confint().
extract.mlmodel <- function(model,
                            ...,
                            include.ci      = FALSE,
                            ci.level        = 0.95,
                            include.scale   = FALSE,
                            include.rsquared = TRUE,
                            include.nobs    = TRUE,
                            include.loglik  = TRUE,
                            include.aic     = TRUE,
                            include.bic     = TRUE,
                            vcov.type       = getOption("mlmodels_vcov_type", "oim")) {

  # 1. coefficients from mlmodels' own summary ------------------------------
  sm <- summary(model, vcov.type = vcov.type, ...)
  co <- sm$coefficients
  keep <- if (isTRUE(include.scale)) rep(TRUE, nrow(co)) else !grepl("^scale::", rownames(co))
  co <- co[keep, , drop = FALSE]

  coef.names <- sub("^value::", "", rownames(co))   # drop the value:: prefix
  coef       <- co[, "Estimate"]
  se         <- co[, "Std. Error"]
  pvalues    <- co[, "Pr(>|z|)"]

  ci.low <- ci.up <- numeric(0)
  if (isTRUE(include.ci)) {
    ci <- confint(model, level = ci.level, vcov.type = vcov.type, ...)
    ci <- ci[keep, , drop = FALSE]
    ci.low <- ci[, 1]
    ci.up  <- ci[, 2]
  }

  # 2. goodness-of-fit ------------------------------------------------------
  gof.names <- character(0); gof <- numeric(0); gof.decimal <- logical(0)
  add_gof <- function(name, value, decimal = TRUE) {
    if (length(value) && is.finite(value)) {
      gof.names   <<- c(gof.names, name)
      gof         <<- c(gof, as.numeric(value))
      gof.decimal <<- c(gof.decimal, decimal)
    }
  }

  if (isTRUE(include.rsquared)) {
    for (row in .mlm_rsq_rows(sm)) add_gof(row[[1]], as.numeric(row[[2]]))
  }
  if (isTRUE(include.nobs))   add_gof("Num. obs.",      stats::nobs(model),   decimal = FALSE)
  if (isTRUE(include.loglik)) add_gof("Log Likelihood", as.numeric(stats::logLik(model)))
  if (isTRUE(include.aic))    add_gof("AIC",            stats::AIC(model))
  if (isTRUE(include.bic))    add_gof("BIC",            stats::BIC(model))

  texreg::createTexreg(
    coef.names  = coef.names,
    coef        = as.numeric(coef),
    se          = as.numeric(se),
    pvalues     = as.numeric(pvalues),
    ci.low      = as.numeric(ci.low),
    ci.up       = as.numeric(ci.up),
    gof.names   = gof.names,
    gof         = gof,
    gof.decimal = gof.decimal
  )
}

# ---- register the S4 method ----------------------------------------------
# Declare each concrete mlmodels class as an S3 class that extends "mlmodel",
# so S4 dispatch on the generic finds the single "mlmodel" method for all of
# them (the fitted objects carry class c("ml_lm", "mlmodel", "maxLik", ...)).
for (.cl in c("ml_lm", "ml_logit", "ml_probit", "ml_poisson",
              "ml_negbin", "ml_gamma", "ml_beta", "ml_trunc_lm")) {
  setOldClass(c(.cl, "mlmodel"))
}
rm(.cl)
setMethod("extract", signature = "mlmodel",
          definition = function(model, ...) extract.mlmodel(model, ...))
