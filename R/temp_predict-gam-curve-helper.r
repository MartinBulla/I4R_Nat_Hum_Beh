# ---- helper ---------------------------------------------------------------
predict_gam_curves <- function(m, data, year = "year", grade = "holc_grade",
                               offset_var = NULL, area_at = 1,
                               k_year = 200,
                               exclude = c("s(state)","s(city_state)","s(id2)","s(city_state):year_s")) {
  stopifnot(all(c(year, grade) %in% names(data)))
  # reuse the same scaling
  sc  <- scale(data[[year]])
  c0  <- attr(sc, "scaled:center"); s0 <- attr(sc, "scaled:scale")
  yrs <- seq(min(data[[year]], na.rm=TRUE), max(data[[year]], na.rm=TRUE), length.out = k_year)
  yrs_s <- (yrs - c0)/s0

  # pick any valid factor levels for REs present in model (safe defaults)
  lvl1 <- function(f) factor(levels(f)[1], levels = levels(f))
  nd <- expand.grid(
    holc_grade = levels(data[[grade]]),
    year       = yrs
  )
  nd$year_s     <- (nd$year - c0)/s0
  # add grouping vars if model has them
  for (v in c("state","city_state","id2")) {
    if (v %in% names(model.frame(m))) nd[[v]] <- lvl1(data[[v]])
  }
  # offset handling (per-km² predictions)
  if (!is.null(offset_var)) {
    nd[[offset_var]] <- area_at
  }

  pr <- predict(m, newdata = nd, type = "link", se.fit = TRUE, exclude = exclude)
  nd$fit <- pr$fit; nd$se <- pr$se.fit
  nd$lwr <- nd$fit - 1.96*nd$se
  nd$upr <- nd$fit + 1.96*nd$se

  # back-transform if log link present (Poisson/NB): per-km² on original scale
  fam <- family(m)$family
  has_loglink <- grepl("log", family(m)$link)
  if (has_loglink) {
    nd$fit_orig <- exp(nd$fit); nd$lwr_orig <- exp(nd$lwr); nd$upr_orig <- exp(nd$upr)
  } else {
    nd$fit_orig <- nd$fit; nd$lwr_orig <- nd$lwr; nd$upr_orig <- nd$upr
  }
  nd
}
# ---- examples -------------------------------------------------------------
# A) your current density model (gaussian on log-density): offset_var = NULL
curves_den <- predict_gam_curves(m_bam, d00)

# B) counts with area offset (per-km² predictions with area=1)
m_cnt <- bam(
  n_obs ~ holc_grade + s(year_s, k=10, bs="cr") + s(year_s, by=holc_grade, k=10, bs="cr") +
    s(state, bs="re") + s(city_state, bs="re") + s(id2, bs="re") +
    s(city_state, by = year_s, bs="re"),
  offset = log(area_km2),
  family = poisson(),               # or nb()
  data = d00, method = "fREML", discrete = TRUE, select = TRUE, gc.level = 2
)
curves_cnt <- predict_gam_curves(m_cnt, d00, offset_var = "area_km2", area_at = 1)

# ---- quick ggplot for curves_cnt (original scale already per km²) --------
library(ggplot2)
ggplot(curves_cnt, aes(year, fit_orig, colour = holc_grade, fill = holc_grade)) +
  geom_ribbon(aes(ymin = lwr_orig, ymax = upr_orig), alpha = 0.15, colour = NA) +
  geom_line() +
  labs(x = "Year", y = "Predicted sampling density per km²") +
  theme_minimal(base_size = 9)
