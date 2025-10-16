### B. alternatives (TODO: ADD )#+ F_Z2b, fig.width = 25/2.5, fig.height = 15/2.5
I am plotting multiple models and wonder how come when using log10 to transform the response gives way smaller effect sizes then when using log() (ln-transformation). See attached plots

library(mgcv)
# center/scale year like in lmer
d00$year_s <- as.numeric(scale(d00$year))
sc <- scale(d00$year)                
c0 <- attr(sc, "scaled:center")       # <-- center
s0 <- attr(sc, "scaled:scale")        # <-- scale

# prepare data
d00$year_s     <- as.numeric(scale(d00$year))   # one scaling, reused
d00$state      <- factor(d00$state)
d00$city_state <- factor(d00$city_state)
d00$id2        <- factor(d00$id2)
d00$holc_grade <- factor(d00$holc_grade)

tab <- table(d00$id2)
d00$has2 <- d00$id2 %in% names(tab[tab >= 2])

# trying something that runs
m_bam <- bam(
  log(sampling_density) ~ 
    holc_grade +          # parametric main effect
    s(year_s, k = 10, bs = "cr") +   # global nonlinear year
    s(year_s, by = holc_grade, k = 10, bs = "cr") +  # grade-specific deviations
    # REs approximating nesting
    s(state, bs = "re") +  
    s(city_state, bs = "re") +
    s(id2, bs = "re") +
    # random slope per id2 (≈ quadratic max); runs out of memmory ti(year_s, id2, bs = c("cr","re"), k = c(3, NA)), 
    # random *linear* slope for year_s per id2 (cheap!)
    s(city_state, by = year_s, bs = "re"), 
  data = d00,
  method = "fREML",
  discrete =TRUE,   # speed for large n
  select = TRUE
)

save(file='Data/Dat_bam_output.Rdata', m_bam)
summary(m_bam)

summary(m_bam)                                                                                                                       #### diagnostics
library(mgcv); library(gratia)

# 1) Basis dimension/k-index checks (cheap)
gam.check(m_bam, k.rep = 20)   # look for k-index << 1 on the year smooths

# 2) Concurvity (cheap)
concurvity(m_bam, full = FALSE)

# 3) Residual patterns (cheap)
appraise(m_bam, method = "normal")   # QQ, resid vs fitted
# If you want simulation-based, keep it light:
appraise(m_bam, method = "simulate", nsim = 50)  # slower   

#if f k-index < ~1, increase k a bit for the affected smooth(s), e.g. k = 12.

####

#### COMPARE A and D (with pointwise 95%CI)
yr  <- seq(min(d00$year), max(d00$year), by = 1)
yr_s <- (yr - c0)/s0

# constants for RE columns (they’ll be excluded anyway)
lev <- function(x) levels(x)[1]
const <- list(
  state      = lev(d00$state),
  city_state = lev(d00$city_state),
  id2        = lev(d00$id2)
)

excl <- c("s(state)", "s(city_state)", "s(id2)", "s(city_state):year_s")

# newdata for A and D with ALL factor levels retained
ndA <- data.frame(year_s = yr_s, holc_grade = factor("A", levels = levels(d00$holc_grade)),
                  state = const$state, city_state = const$city_state, id2 = const$id2)
ndD <- data.frame(year_s = yr_s, holc_grade = factor("D", levels = levels(d00$holc_grade)),
                  state = const$state, city_state = const$city_state, id2 = const$id2)

XpA <- predict(m_bam, newdata = ndA, type = "lpmatrix", exclude = excl)
XpD <- predict(m_bam, newdata = ndD, type = "lpmatrix", exclude = excl)

beta <- coef(m_bam)
V    <- vcov(m_bam)

Xd   <- XpA - XpD
fit  <- as.vector(Xd %*% beta)
se   <- sqrt(rowSums((Xd %*% V) * Xd))

out <- data.table(
  year = yr,
  diff = fit,
  lwr  = fit - 1.96*se,
  upr  = fit + 1.96*se
)

g_t1 = 
ggplot(out, aes(year, diff)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.15) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(y = "A − D (ln scale)", x = "Year") +
  theme_minimal(base_size = 9)

# plot as ratio (A:D)
out[, `:=`(
  ratio = exp(diff),          # A / D
  lo    = exp(lwr),
  hi    = exp(upr),
  pct   = (exp(diff) - 1) * 100
)]

g_t2 = 
ggplot(out, aes(year, ratio)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = 2) +
  labs(y = "A / D (original scale)", x = "Year") +
  theme_minimal(base_size = 9)

# plot as percent difference
g_t3 = 
ggplot(out, aes(year, pct)) +
  geom_ribbon(aes(ymin = (lo-1)*100, ymax = (hi-1)*100), alpha = 0.15) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(y = "A − D (% difference)", x = "Year") +
  theme_minimal(base_size = 9)

g_t1 / g_t2 / g_t3 + plot_layout(axis_title = 'collect', axes = "collect")  

ggsave('Output/bam_diffAD.png', width = 7, height = 14, units = 'cm')

#### COMPARE A and D - does not run
library(gratia)
gratia::smooths(m_bam)
# name patterns usually look like "s(year_s)" and "s(year_s):holc_gradeA"
# Compare A vs D across year_s:
diff_AD <- difference_smooths(
  m_bam,
  smooth = "s(year_s):holc_gradeA",
  comp   = "s(year_s):holc_gradeD",
  n = 200,
  unconditional = TRUE   # include smoothing parameter uncertainty
)

autoplot(diff_AD) + 
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    labs(y = "A − D (on log scale)", x = "Scaled year")

# in years
diff_AD$year <- diff_AD$x * s0 + c0
ggplot(diff_AD, aes(year, difference)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15) +
  geom_line() + geom_hline(yintercept = 0, linetype = 2) +
  labs(y = "A − D (log scale)", x = "Year") +
  theme_minimal(base_size = 9)

####

#### plot per-grade curves (marginal over REs) ####
library(data.table); library(ggplot2)

# Rebuild scaling used for year_s
sc <- scale(d00$year); c0 <- attr(sc,"scaled:center"); s0 <- attr(sc,"scaled:scale")
yr_grid   <- seq(min(d00$year), max(d00$year), by = 1)
yr_grid_s <- (yr_grid - c0)/s0

nd <- CJ(
  holc_grade = levels(d00$holc_grade),
  year      = yr_grid
)
nd[, year_s := (year - c0)/s0]

# Exclude RE terms for clean marginal effects
excl <- c("s(state)", "s(city_state)", "s(id2)", "s(city_state):year_s")

pr  <- predict(m_bam, newdata = nd, se.fit = TRUE, type = "link", exclude = excl)

nd[, `:=`(fit = pr$fit, se = pr$se.fit,
          lwr = fit - 1.96*se, upr = fit + 1.96*se)]

# Plot on log scale
ggplot(nd, aes(year, fit, colour = holc_grade, fill = holc_grade)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.15, colour = NA) +
  geom_line() +
  labs(y = "log(sampling_density)", x = "Year") +
  theme_minimal(base_size = 9)

# (optional) original scale
nd[, `:=`(fit_exp = exp(fit), lwr_exp = exp(lwr), upr_exp = exp(upr))]



#### other plotting ####
# 4) Safe grid: pick actual year range (finite only)
yr_grid  <- seq(min(d00$year, na.rm=TRUE), max(d00$year, na.rm=TRUE), length.out=100)
yr_grid_s <- (yr_grid - yr_center) / yr_scale

# 5) Newdata across grades (marginal curves; exclude RE terms in prediction)
nd <- CJ(
  holc_grade = levels(d00$holc_grade),
  year_s     = yr_grid_s
)
# add raw year back for plotting
nd[, year := yr_grid[ match(year_s, yr_grid_s) ] ]

excl <- c("s(state)","s(city_state)","s(id2)","s(year_s,id2)")

pr <- predict(m_bam, newdata = nd, se.fit = TRUE, type = "link", exclude = excl)
nd[, `:=`(fit = pr$fit, se = pr$se.fit,
          lwr = fit - 1.96*se, upr = fit + 1.96*se)]

# 6) Plot
library(ggplot2)
ggplot(nd, aes(year, fit, colour = holc_grade)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = holc_grade), alpha = 0.15, colour = NA) +
  geom_line() +
  labs(x = "Year", y = "log10(sampling_density)") +
  theme_bw()

####

#### summary output ####

Family: gaussian 
Link function: identity 

Formula:
log(sampling_density) ~ holc_grade + s(year_s, k = 10, bs = "cr") + 
    s(year_s, by = holc_grade, k = 10, bs = "cr") + s(state, 
    bs = "re") + s(city_state, bs = "re") + s(id2, bs = "re") + 
    s(city_state, by = year_s, bs = "re")

Parametric coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  2.65195    0.10093  26.275  < 2e-16 ***
holc_gradeB -0.20090    0.07964  -2.523   0.0117 *  
holc_gradeC -0.30191    0.07653  -3.945 7.99e-05 ***
holc_gradeD -0.52090    0.08297  -6.278 3.45e-10 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                            edf Ref.df        F  p-value    
s(year_s)                8.5245      9  509.982  < 2e-16 ***
s(year_s):holc_gradeA    4.2187      9   75.752  < 2e-16 ***
s(year_s):holc_gradeB    0.7676      9    1.767   0.0237 *  
s(year_s):holc_gradeC    0.5067      9    0.387   0.1286    
s(year_s):holc_gradeD    3.5797      9    9.291  < 2e-16 ***
s(state)                12.3942     37 2848.228   0.2086    
s(city_state)          124.2122    187 1918.462 4.11e-06 ***
s(id2)                6294.8920   7494   25.684  < 2e-16 ***
s(city_state):year_s   158.8236    188  128.414  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.643   Deviance explained = 68.5%
fREML = 1.0732e+05  Scale est. = 1.984     n = 55916

r$>  
r$>  
r$>  




# memory issues
m_bam <- bam(
  log(sampling_density) ~ 
    holc_grade +          # parametric main effect
    s(year_s, k = 10, bs = "cr") +   # global nonlinear year
    s(year_s, by = holc_grade, k = 10, bs = "cr") +  # grade-specific deviations
    # REs approximating nesting
    s(state, bs = "re") +  
    s(city_state, bs = "re") +
    s(id2, bs = "re") +
    # random slope per id2 (≈ quadratic max); runs out of memmory ti(year_s, id2, bs = c("cr","re"), k = c(3, NA)), 
    # random *linear* slope for year_s per id2 (cheap!)
    s(id2, by = year_s, bs = "re"), 
  data = d00,
  method = "fREML",
  discrete =TRUE,   # speed for large n
  select = TRUE
)

# possibly replace with 

m_bam <- bam(
  log(sampling_density) ~ 
    holc_grade +          # parametric main effect
    s(year_s, k = 10, bs = "cr") +   # global nonlinear year
    s(year_s, by = holc_grade, k = 10, bs = "cr") +  # grade-specific deviations
    # REs approximating nesting
    s(state, bs = "re") +  
    s(city_state, bs = "re") +
    s(id2, bs = "re") +
    # random *linear* slope for year_s per id2 (cheap!)
    s(id2, by = year_s, bs = "re"), 
  data = d00,
  method = "fREML",
  discrete =TRUE,   # speed for large n
  select = TRUE
)

    s(id2, bs = "re") +       # random intercept per id2                 
    s(year_s, id2, bs = "fs", m = 1, k = 6),     # random smooth (≈ slope) per id2