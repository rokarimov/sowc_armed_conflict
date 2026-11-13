## mlmodels_sjplot_bridge.R --------------------------------------------------
## Make fits from the `mlmodels` package (class "mlmodel": ml_lm, ml_logit,
## ml_probit, ml_poisson, ml_negbin, ...) display in sjPlot::tab_model().
##
## WHY IT'S NEEDED
##   tab_model() has no idea about mlmodels; it delegates entirely to
##   parameters::model_parameters(), which errors on these objects
##   ("class 'ml_lm' not supported"). broom::tidy() returns an empty tibble.
##
## THE APPROACH  ("force the standard outputs through")
##   We give mlmodel objects their own model_parameters() method that reads
##   the package's OWN reliable outputs -- summary(fit)$coefficients and
##   confint(fit) -- and splices them into the exact `parameters_model`
##   scaffold that tab_model already trusts. The scaffold (row set, pretty
##   names, and the log/logit-link flags that drive exponentiation to
##   OR/IRR) is borrowed from a throwaway lm()/glm() fit on the same
##   formula+data; only its STRUCTURE is used -- every number is overwritten
##   with mlmodels' estimates. Small companion methods restore the
##   Observations and R^2 footer rows.
##
## USAGE
##   source(here::here("code/mlmodels_sjplot_bridge.R"))
##   fit <- ml_logit(inlf ~ age + educ + kidslt6, data = mroz)
##   sjPlot::tab_model(fit)                       # coefficients, SE, CI, p, N
##   sjPlot::tab_model(fit, transform = "exp")    # odds ratios (IRR for count)
##   sjPlot::tab_model(fit1, fit2, fit3)          # side-by-side, mixed families
##   sjPlot::tab_model(fit, drop = "factor")      # hide fixed-effect dummies
##   sjPlot::tab_model(fit, keep = "conflict|gdp")# show only matching terms
##
##   # Robust / clustered / bootstrap SEs. Through tab_model, route them via
##   # options (tab_model would otherwise intercept or drop the arguments):
##   options(mlmodels_vcov_type = "robust")            # or "opg","boot","jack"
##   options(mlmodels_vcov_type = "robust",            # clustered by a variable
##           mlmodels_cl_var = "iso3")
##   sjPlot::tab_model(fit)
##   options(mlmodels_vcov_type = NULL, mlmodels_cl_var = NULL)  # reset
##   # ...or call the method directly with the args:
##   parameters::model_parameters(fit, vcov.type = "robust", cl_var = "iso3")
##
## NOTES
##   * keep=/drop= take a regex (or vector of regexes) matched against term
##     names, exactly like parameters::model_parameters(). sjPlot forwards
##     them here, so tab_model(fit, drop=...) works too. sjPlot's own
##     terms=/rm.terms= selectors also work.
##   * The nuisance scale parameter (scale::lnsigma, ...) is dropped by
##     default; pass include_scale = TRUE to model_parameters() to keep it.
##   * `value::` prefixes are stripped for display.
##   * Large fixed-effects models render fine: the surrogate scaffold uses
##     Wald CIs (ci_method = "wald"), avoiding the profile-likelihood path
##     that would otherwise hang on hundreds of parameters.
##   * If mlmodels itself warns that the outcome is fractional (QMLE) -- e.g.
##     a probit on a rate -- heed it: the default "oim" SEs are not valid.
##     Use options(mlmodels_vcov_type = "robust") before tab_model().
##   * ml_gamma / ml_beta are class-generic here but currently fail to *fit*
##     in mlmodels 0.1.2 itself, so they are untested.

# ---- helper: fit a throwaway lm/glm to borrow parameters' scaffolding ----
# Only the STRUCTURE is used (row set, pretty names, link flags that drive
# exponentiation); every number is overwritten with mlmodels' own output.
.mlm_surrogate <- function(object, form, dat) {
  environment(form) <- environment()          # detach R_EmptyEnv
  switch(class(object)[1],
    ml_lm     = lm(form, data = dat),
    ml_logit  = glm(form, family = binomial("logit"),  data = dat),
    ml_probit = glm(form, family = binomial("probit"), data = dat),
    ml_poisson= glm(form, family = poisson(),          data = dat),
    ml_negbin = glm(form, family = poisson(),          data = dat), # scaffold only
    ml_gamma  = glm(form, family = Gamma(link = "log"),data = dat),
    ml_beta   = glm(form, family = binomial(),         data = dat), # scaffold only
    lm(form, data = dat)
  )
}

# NB: `keep`/`drop`/`vcov.type`/`include_scale` come AFTER `...` on purpose,
# so that sjPlot's internal calls (which pass `keep=`, `vcov=`, ...) match
# them exactly and cannot partial-match. Args after `...` must be named in full.
model_parameters.mlmodel <- function(model,
                                     ci = 0.95,
                                     ...,
                                     keep = NULL,
                                     drop = NULL,
                                     vcov.type = getOption("mlmodels_vcov_type", "oim"),
                                     cl_var = getOption("mlmodels_cl_var", NULL),
                                     repetitions = 999,
                                     seed = getOption("mlmodels_seed", NULL),
                                     include_scale = FALSE) {
  keep_scale <- isTRUE(include_scale)
  # 1. mlmodels' standard outputs -----------------------------------------
  #    vcov.type/cl_var/seed/repetitions are forwarded to mlmodels' own
  #    summary()/confint(), so robust, clustered ("robust" + cl_var), and
  #    bootstrap SEs all flow through unchanged.
  sm  <- summary(model, vcov.type = vcov.type, cl_var = cl_var,
                 repetitions = repetitions, seed = seed)$coefficients
  colnames(sm) <- c("Estimate", "SE", "Stat", "p")
  ci_m <- confint(model, level = ci, vcov.type = vcov.type, cl_var = cl_var,
                  repetitions = repetitions, seed = seed)

  keep_rows <- if (keep_scale) rep(TRUE, nrow(sm)) else !grepl("^scale::", rownames(sm))
  sm   <- sm[keep_rows, , drop = FALSE]
  ci_m <- ci_m[keep_rows, , drop = FALSE]

  raw_names   <- rownames(sm)
  clean_names <- sub("^value::", "", raw_names)   # display names

  # 2. build scaffolding from a surrogate lm/glm --------------------------
  # Only the structure/attributes are used, so ci_method = "wald" -- the
  # default "profile" path would try to profile every coefficient, which
  # hangs on large fixed-effects models (hundreds of parameters).
  form <- stats::formula(model)
  dat  <- insight::get_data(model)
  surr <- suppressWarnings(.mlm_surrogate(model, form, dat))
  tmpl <- parameters::model_parameters(surr, ci = ci, ci_method = "wald")

  # align surrogate rows to mlmodels rows by parameter name. scale:: params
  # (kept only when include_scale = TRUE) have no counterpart in the lm/glm
  # scaffold, so they borrow the first row as a structural filler -- every
  # number is overwritten below. A non-scale mismatch is a real error.
  idx <- match(clean_names, tmpl$Parameter)
  bad <- is.na(idx) & !grepl("^scale::", clean_names)
  if (any(bad)) {
    stop("Could not align mlmodels parameters to surrogate scaffold: ",
         paste(clean_names[bad], collapse = ", "))
  }
  idx[is.na(idx)] <- 1L
  out <- tmpl[idx, , drop = FALSE]
  rownames(out) <- NULL

  # 3. splice mlmodels' numbers into the scaffold -------------------------
  out$Parameter   <- clean_names
  out$Coefficient <- sm[, "Estimate"]
  out$SE          <- sm[, "SE"]
  out$CI_low      <- ci_m[, 1]
  out$CI_high     <- ci_m[, 2]
  out$p           <- sm[, "p"]
  stat_col <- intersect(c("t", "z", "Statistic"), names(out))[1]
  if (!is.na(stat_col)) out[[stat_col]] <- sm[, "Stat"]

  # 4. honour keep/drop (regex on parameter names), as model_parameters does.
  #    We must apply these ourselves: without a supported model class,
  #    parameters/sjPlot never gets a chance to filter, so keep=/drop= (and
  #    hence sjPlot's forwarding of them) would otherwise be silently ignored.
  if (!is.null(keep) || !is.null(drop)) {
    sel <- rep(TRUE, nrow(out))
    # perl = TRUE so PCRE features (e.g. negative lookahead "^(?!x).*")
    # work, matching parameters::model_parameters()'s own behaviour.
    if (!is.null(keep)) sel <- sel &  grepl(paste(keep, collapse = "|"), out$Parameter, perl = TRUE)
    if (!is.null(drop)) sel <- sel & !grepl(paste(drop, collapse = "|"), out$Parameter, perl = TRUE)
    out         <- out[sel, , drop = FALSE]
    clean_names <- clean_names[sel]
    rownames(out) <- NULL
  }

  # 5. fix up attributes that depend on the (correct) row set -------------
  pn <- clean_names; names(pn) <- clean_names
  attr(out, "pretty_names") <- pn
  attr(out, "coefficient_name") <- attr(tmpl, "coefficient_name")
  attr(out, "ci")           <- ci
  attr(out, "n_obs")        <- insight::n_obs(model)
  attr(out, "model_class")  <- class(model)[1]
  attr(out, "object_name")  <- deparse(substitute(model))
  class(out) <- class(tmpl)
  out
}

# --------------------------------------------------------------------------
# r2() support -> populates the R^2 footer row in tab_model and silences the
# "r2() does not support models of class ..." warning. Values come straight
# from mlmodels' own summary(), so they match the package:
#   * ml_lm            -> R^2 / adjusted R^2
#   * ml_probit        -> McKelvey-Zavoina R^2 (summary()$r.squared$mczav)
#   * ml_logit         -> McFadden's R^2       (summary()$r.squared$mcfadden)
#   * count models     -> McFadden's pseudo-R^2
r2.ml_lm <- function(model, ...) {
  s <- summary(model)
  out <- list(R2 = c(R2 = unname(s$r.squared)))
  adj <- tryCatch(as.numeric(s$adj.r.squared[[1]]), error = function(e) NA_real_)
  if (length(adj)) out$R2_adjusted <- c(`adjusted R2` = adj)
  structure(out, class = "r2_generic")
}

r2.mlmodel <- function(model, ...) {
  # mlmodels' own summary already reports the right pseudo-R2 per family.
  # For probit/logit, summary()$r.squared is a list with $mczav = the
  # McKelvey-Zavoina R2 (and $cor = squared correlation); surface mczav.
  # summary()$r.squared is a list whose members depend on the family:
  # probit -> $mczav (McKelvey-Zavoina), logit -> $mcfadden, etc. Surface
  # whichever the package reports. The outer list name becomes the tab_model
  # footer-row label (it maps e.g. "R2_Tjur" -> "R<sup>2</sup> Tjur").
  rs <- tryCatch(suppressWarnings(summary(model))$r.squared, error = function(e) NULL)
  if (is.list(rs)) {
    if (is.numeric(rs$mczav))
      return(structure(list(`McKelvey-Zavoina_R2` = c(`McKelvey-Zavoina's R2` = as.numeric(rs$mczav))),
                       class = "r2_generic"))
    if (is.numeric(rs$mcfadden))
      return(structure(list(`McFadden_R2` = c(`McFadden's R2` = as.numeric(rs$mcfadden))),
                       class = "r2_generic"))
  }
  # Last-resort analytic McFadden = 1 - loglik(full)/loglik(null). mlmodels
  # forbids an explicit `~ 1`, so the intercept-only log-likelihood is
  # computed analytically (its MLE is just the response mean).
  y <- tryCatch(as.numeric(insight::get_response(model)), error = function(e) NULL)
  if (is.null(y)) return(NULL)
  ll0 <- switch(class(model)[1],
    ml_poisson = sum(stats::dpois(y, mean(y), log = TRUE)),
    NULL)
  if (is.null(ll0) || !is.finite(ll0)) return(NULL)
  mcf <- 1 - as.numeric(stats::logLik(model)) / ll0
  structure(list(`McFadden_R2` = c(`McFadden's R2` = mcf)), class = "r2_generic")
}

# insight::n_obs() comes back empty for mlmodel objects, which drops the
# "Observations" footer row from tab_model. Delegate to the package's nobs().
n_obs.mlmodel <- function(x, ...) stats::nobs(x)

# --------------------------------------------------------------------------
# register the S3 methods with their generics' namespaces
registerS3method("model_parameters", "mlmodel", model_parameters.mlmodel,
                 envir = asNamespace("parameters"))
registerS3method("r2",    "ml_lm",   r2.ml_lm,    envir = asNamespace("performance"))
registerS3method("r2",    "mlmodel", r2.mlmodel,  envir = asNamespace("performance"))
registerS3method("n_obs", "mlmodel", n_obs.mlmodel, envir = asNamespace("insight"))
