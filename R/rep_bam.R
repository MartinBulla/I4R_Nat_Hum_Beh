Write in the text “Pre-~2009 absolute gaps are tiny though the relative (ln-ratio) difference is already >0; after ~2010 both relative and absolute gaps increase sharply.”

Add to the description of ACF function(
    
ACF = descriptive “what pattern do we see?”
DHARMa = null-calibrated “is this more than expected under the fitted NB/Poisson GLMM?”)It accounts for the mean–variance link and random effects, which the plain ACF does not.

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
d00[, off := log(area_holc_km2)]
d00[, off := off - mean(off)]

# this run
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
load(file='Data/Dat_bam_output.Rdata')
#summary(m_bam) # runs for too long

# fit pois
    m_pois <- bam(sum_bird_obs ~ 
                holc_grade + 
                s(year_s,k=10,bs="cr") +
                s(year_s,by=holc_grade,k=10,bs="cr") +
                s(state,bs="re")+s(city_state,bs="re")+s(id2,bs="re") +
                s(city_state, by=year_s, bs="re"),
              offset = off, family = poisson(),
              data=d00, method="fREML", 
              discrete=TRUE, select=TRUE, gc.level=2)
    save(file='Data/Dat_bam-pois_output.Rdata', m_pois)          
    load(file='Data/Dat_bam-pois_output.Rdata')   
    m_ass(file_name = 'test_bam-pois_1', mo = m_pois, dat = d00, offset = TRUE, cont = c("year", "area_holc_km2"), categ = 'holc_grade',show_binned = TRUE, show_temporal_grouped = 'year', PNG = TRUE)         

# fit negative binomial (preferred if disp of pois >~ 1.2)
    m_nb <- bam(formula(m_pois), offset = off,
            family = nb(),  # mgcv estimates theta
            data=d00, method="fREML", discrete=TRUE, select=TRUE, gc.level=2)
    save(file='Data/Dat_bam-nb_output.Rdata', m_nb)  
    load(file='Data/Dat_bam-nb_output.Rdata') 
                                                                                    m_ass(file_name = 'test_bam-nb_1', mo = m_nb, dat = d00, offset = TRUE, cont = c("year", "area_holc_km2"), categ = 'holc_grade',show_binned = TRUE, show_temporal_grouped = 'year', PNG = TRUE) # slight spatial pattern → either real biological clustering or residual heterogeneity (city/state-level not fully captured); that’s a small, interpretable residual pattern, not a modeling flaw; the model assumptions look nearly identical to ooks nearly identical to the mas1D_nb with nbinom2(), dispformula = ~ holc_grade_D + scale(log(area_holc_km2))           

#### diagnostics
library(gratia)
library(DHARMa)

plot(m_bam, pages=1)

# 1) Residual patterns
gratia::appraise(m_bam, method = "normal")   # QQ, resid vs fitted # same as gam.check
#gam.check(m_bam, k.rep = 20)   # look for k-index << 1 on the year smooths


# quick partial-residual look
gratia::draw(gratia::smooth_estimates(m_bam, smooth = "s(year_s):holc_gradeA"))

# 2) Concurvity runs for too long
#concurvity(m_bam, full = FALSE) - fails due to memory issues
# (i) term-wise concurvity for the year terms only (cheap proxy)
#concurvity(update(m_bam, . ~ . - s(id2) - s(city_state) - s(state)), full=FALSE) 
# (ii) fit a tiny copy (subset rows) just to inspect concurvity of fixed smooths (runs for too long)
#set.seed(1)
#idx <- sample(nrow(d00), 20000)
#m_small <- update(m_bam, data = d00[idx, ])
#concurvity(m_small, full = FALSE)

# 3)  simulation-based (slow)
appraise(m_bam, method = "simulate", nsim = 25)  # slower 
qq_plot(m_bam, method="simulate", nsim=25)
residuals_hist(m_bam)
residuals_fitted(m_bam)

#### NB check

# a) residual patterns
gratia::appraise(m_nb, method = "normal") # same: gam.check(m_nb, k.rep = 20)

# b)  zero inflation check
m_nb$family$getTheta(TRUE) #NB is warranted

# c) RE diagnostics
plot(m_nb, pages = 1)   # QQ of RE terms ~ straight ≈ OK

# d) Temporal autocorrelation
res <- residuals(m_nb, type = "pearson")
by_year <- aggregate(res, list(year = d00$year), mean)  # yearly mean residual
acf(by_year$x, main = "ACF of yearly mean residuals") # strong ACF would argue for adding a simple AR term (mgcv doesn’t do AR in bam; you’d move to gamm() or handle via block bootstrap for CIs).
acf(resid(m_nb), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))

#### e) MARGINAL per grade curves
# reuse your c0/s0
c0 <- attr(scale(d00$year), "scaled:center"); s0 <- attr(scale(d00$year), "scaled:scale")
yr  <- seq(min(d00$year), max(d00$year), by = 1)
yr_s <- (yr - c0)/s0

lev <- function(x) levels(x)[1]
const <- list(
  state      = lev(d00$state),
  city_state = lev(d00$city_state),
  id2        = lev(d00$id2)
)
excl <- c("s(state)","s(city_state)","s(id2)","s(city_state):year_s")

ndA <- data.frame(
  year_s = yr_s, year = yr,
  holc_grade = factor("A", levels = levels(d00$holc_grade)),
  state = const$state, city_state = const$city_state, id2 = const$id2,
  area_holc_km2 = 1
)
ndD <- ndA; ndD$holc_grade <- factor("D", levels = levels(d00$holc_grade))

# design matrices (chunked to avoid OOM)
XpA <- predict(m_nb, newdata = ndA, type = "lpmatrix",
               exclude = excl, newdata.guaranteed = TRUE, block.size = 1000)
XpD <- predict(m_nb, newdata = ndD, type = "lpmatrix",
               exclude = excl, newdata.guaranteed = TRUE, block.size = 1000)

beta <- coef(m_nb); V <-vcov(m_nb) # vcov(m_nb, unconditional = TRUE) 

# ln-diff
Xd   <- XpA - XpD
diff <- as.vector(Xd %*% beta)
se   <- sqrt(rowSums((Xd %*% V) * Xd))

# ratio & %
ratio <- exp(diff); loR <- exp(diff - 1.96*se); hiR <- exp(diff + 1.96*se)
pct   <- (ratio - 1) * 100

# absolute gap via delta method (fast)
etaA <- as.vector(XpA %*% beta); muA <- exp(etaA)
etaD <- as.vector(XpD %*% beta); muD <- exp(etaD)
G    <- (muA * XpA) - (muD * XpD)
seAD <- sqrt(rowSums((G %*% V) * G))
abs  <- muA - muD; abs_lo <- abs - 1.96*seAD; abs_hi <- abs + 1.96*seAD

out_nb <- data.table(year = yr,
  diff = diff, lwr = diff - 1.96*se, upr = diff + 1.96*se,
  ratio = ratio, lo = loR, hi = hiR, pct = pct,
  abs = abs, abs_lwr = abs_lo, abs_upr = abs_hi)

# plot on ln-scale
g_t1_nb = 
ggplot(out_nb, aes(year, diff)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.15) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(y = "A - D differences", x = "Year", subtitle = 'ln-scale') +
  theme_minimal(base_size = 9)+
  theme(plot.subtitle = element_text(colour = "grey40"))

# plot as ratio (A:D)
g_t2_nb = 
ggplot(out_nb, aes(year, ratio)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = 2) +
  labs(y = "A - D differences", x = "Year", subtitle = "relative, based on original scale") +
  theme_minimal(base_size = 9)+
  theme(plot.subtitle = element_text(colour = "grey40"))

# plot as percent difference
g_t3_nb = 
ggplot(out_nb, aes(year, pct)) +
  geom_ribbon(aes(ymin = (lo-1)*100, ymax = (hi-1)*100), alpha = 0.15) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(y = "A - D differences", x = "Year",  subtitle = "% based on original scale") +
  theme_minimal(base_size = 9)+
  theme(plot.subtitle = element_text(colour = "grey40"))

# plot as absolute differences
g_t4_nb = 
ggplot(out_nb, aes(year, abs)) +
  geom_ribbon(aes(ymin = abs_lwr, ymax = abs_upr), alpha = 0.15) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(y = "A − D differences", x = "Year",  subtitle = "absolute based on original scale") +
  theme_minimal(base_size = 9)+
  theme(plot.subtitle = element_text(colour = "grey40"))


g_t1_nb / g_t2_nb / g_t3_nb / g_t4_nb + plot_layout(axis_title = 'collect', axes = "collect")  

ggsave('Output/bam-nb_diffAD.png', width = 7, height = 16, units = 'cm')

#### f) Per-grade marginal curves (per km²), with CIs
yrs <- yr; yrs_s <- yr_s
lvl1 <- function(f) factor(levels(f)[1], levels = levels(f))

make_nd <- function(g) data.frame(
  holc_grade = factor(g, levels = levels(d00$holc_grade)),
  year = yrs, year_s = yrs_s,
  state = lvl1(d00$state),
  city_state = lvl1(d00$city_state),
  id2 = lvl1(d00$id2),
  area_holc_km2 = 1
)

pred_one <- function(g){
  nd <- make_nd(g)
  pr <- predict(m_nb, newdata = nd, type = "link", se.fit = TRUE,
                exclude = excl, newdata.guaranteed = TRUE, block.size = 1000)
  cbind(nd,
        fit = pr$fit,
        lwr = pr$fit - 1.96*pr$se.fit,
        upr = pr$fit + 1.96*pr$se.fit)
}

curves_nb <- rbindlist(lapply(levels(d00$holc_grade), pred_one))

# original scale (per km²)
curves_nb[, `:=`(fit_orig = exp(fit), lwr_orig = exp(lwr), upr_orig = exp(upr))]


# Plot on log scale
g_bam1_nb = 
ggplot(curves_nb, aes(year, fit, colour = holc_grade, fill = holc_grade)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.15, colour = NA) +
  geom_line() +
  labs(y = "Sampling density / km²", x = "Year", subtitle = 'ln scale') +
  scale_fill_manual(values = holc_pal, name = 'HOLC grade') + 
  scale_colour_manual(values = holc_pal, name = 'HOLC grade') + 
  theme_minimal(base_size = 9) +
  theme(plot.subtitle = element_text(colour = "grey40"))


g_bam2_nb = 
ggplot(curves_nb, aes(year, fit_orig, colour = holc_grade, fill = holc_grade)) +
  geom_ribbon(aes(ymin = lwr_orig, ymax = upr_orig), alpha = 0.15, colour = NA) +
  geom_line() +
  labs(y = "Sampling density / km²", x = "Year", subtitle = 'original scale') +
  scale_fill_manual(values = holc_pal) + 
  scale_colour_manual(values = holc_pal) + 
  theme_minimal(base_size = 9) +
  theme(legend.position = "none",
        plot.subtitle = element_text(colour = "grey40"))


g_bam1_nb / g_bam2_nb  + plot_layout(axis_title = 'collect', axes = "collect")  

ggsave('Output/bam-nb_ABCD.png', width = 8, height = 9, units = 'cm')
####

#### COMPARE A and D (relative; marginal with pointwise 95%CI)
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

# add ration and %
out[, `:=`(
  ratio = exp(diff),          # A / D
  lo    = exp(lwr),
  hi    = exp(upr),
  pct   = (exp(diff) - 1) * 100
)]

# add absolute differences
etaA <- as.vector(XpA %*% beta)
etaD <- as.vector(XpD %*% beta)
muA  <- exp(etaA);  muD  <- exp(etaD)

    # gradient of (exp(etaA) - exp(etaD)) wrt beta:
    G    <- (muA * XpA) - (muD * XpD)     # n × p
    seAD <- sqrt(rowSums((G %*% V) * G))  # fast

absDiff <- muA - muD
abs_lwr     <- absDiff - 1.96 * seAD
abs_upr     <- absDiff + 1.96 * seAD

out[, `:=`(
  abs = absDiff,          # A / D
  abs_lwr    = abs_lwr,
  abs_upr    = abs_upr
)]


# plot on ln-scale
g_t1 = 
ggplot(out, aes(year, diff)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.15) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(y = "A - D differences", x = "Year", subtitle = 'ln-scale') +
  theme_minimal(base_size = 9)+
  theme(plot.subtitle = element_text(colour = "grey40"))

# plot as ratio (A:D)
g_t2 = 
ggplot(out, aes(year, ratio)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = 2) +
  labs(y = "A - D differences", x = "Year", subtitle = "relative, based on original scale") +
  theme_minimal(base_size = 9)+
  theme(plot.subtitle = element_text(colour = "grey40"))

# plot as percent difference
g_t3 = 
ggplot(out, aes(year, pct)) +
  geom_ribbon(aes(ymin = (lo-1)*100, ymax = (hi-1)*100), alpha = 0.15) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(y = "A - D differences", x = "Year",  subtitle = "% based on original scale") +
  theme_minimal(base_size = 9)+
  theme(plot.subtitle = element_text(colour = "grey40"))

# plot as absolute differences
g_t4 = 
ggplot(out, aes(year, abs)) +
  geom_ribbon(aes(ymin = abs_lwr, ymax = abs_upr), alpha = 0.15) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(y = "A − D differences", x = "Year",  subtitle = "absolute based on original scale") +
  theme_minimal(base_size = 9)+
  theme(plot.subtitle = element_text(colour = "grey40"))


g_t1 / g_t2 / g_t3 / g_t4 + plot_layout(axis_title = 'collect', axes = "collect")  

ggsave('Output/bam_diffAD_v2.png', width = 7, height = 16, units = 'cm')


#### plot per-grade curves (marginal and pointwise) ####
# reuse your center/scale
c0 <- attr(scale(d00$year), "scaled:center")
s0 <- attr(scale(d00$year), "scaled:scale")

yr  <- seq(min(d00$year), max(d00$year), by = 1)

# helpers: first level of a factor, kept on original levels
lvl1 <- function(f) factor(levels(f)[1], levels = levels(f))

nd <- CJ(
  holc_grade = levels(d00$holc_grade),
  year       = yr
)[
  , year_s := (year - c0)/s0
][
  , `:=`(
    state      = lvl1(d00$state),
    city_state = lvl1(d00$city_state),
    id2        = lvl1(d00$id2)
  )
]

excl <- c("s(state)", "s(city_state)", "s(id2)", "s(city_state):year_s")

pr <- predict(m_bam, newdata = nd, se.fit = TRUE, type = "link", exclude = excl)
nd[, `:=`(fit = pr$fit, se = pr$se.fit,
          lwr = pr$fit - 1.96*pr$se.fit, upr = pr$fit + 1.96*pr$se.fit)]


# Plot on log scale
g_bam1 = 
ggplot(nd, aes(year, fit, colour = holc_grade, fill = holc_grade)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.15, colour = NA) +
  geom_line() +
  labs(y = "Sampling density / km²", x = "Year", subtitle = 'ln scale') +
  scale_fill_manual(values = holc_pal, name = 'HOLC grade') + 
  scale_colour_manual(values = holc_pal, name = 'HOLC grade') + 
  theme_minimal(base_size = 9) +
  theme(plot.subtitle = element_text(colour = "grey40"))


# (optional) original scale
nd[, `:=`(fit_exp = exp(fit), lwr_exp = exp(lwr), upr_exp = exp(upr))]

g_bam2 = 
ggplot(nd, aes(year, fit_exp, colour = holc_grade, fill = holc_grade)) +
  geom_ribbon(aes(ymin = lwr_exp, ymax = upr_exp), alpha = 0.15, colour = NA) +
  geom_line() +
  labs(y = "Sampling density / km²", x = "Year", subtitle = 'original scale') +
  scale_fill_manual(values = holc_pal) + 
  scale_colour_manual(values = holc_pal) + 
  theme_minimal(base_size = 9) +
  theme(legend.position = "none",
        plot.subtitle = element_text(colour = "grey40"))


g_bam1 / g_bam2  + plot_layout(axis_title = 'collect', axes = "collect")  

ggsave('Output/bam_ABCD.png', width = 8, height = 9, units = 'cm')



#### Confirm absolute vs relative gaps ####

# build X for A and D (as you did), excluding REs:
XpA <- predict(m_bam, newdata = ndA, type="lpmatrix", exclude=excl)
XpD <- predict(m_bam, newdata = ndD, type="lpmatrix", exclude=excl)
beta <- coef(m_bam); V <- vcov(m_bam)

# log-diff (analytic)
Xd   <- XpA - XpD
diff <- as.vector(Xd %*% beta)
se   <- sqrt(rowSums((Xd %*% V) * Xd))

# ratio and % (analytic)
ratio <- exp(diff); loR <- exp(diff - 1.96*se); hiR <- exp(diff + 1.96*se)

# absolute difference on original scale via simulation (delta is messy here)
B <- MASS::mvrnorm(1000, mu = beta, Sigma = V)
fA <- XpA %*% t(B); fD <- XpD %*% t(B)
absDiff <- exp(fA) - exp(fD)
abs_lo  <- apply(absDiff, 1, quantile, 0.025)
abs_hi  <- apply(absDiff, 1, quantile, 0.975)
abs_med <- apply(absDiff, 1, median)

out <- data.table(year = ndA$year,
                  log_diff = diff, log_lo = diff-1.96*se, log_hi = diff+1.96*se,
                  ratio = ratio, ratio_lo = loR, ratio_hi = hiR,
                  abs_med = abs_med, abs_lo = abs_lo, abs_hi = abs_hi)

ggplot(out, aes(year, abs_med)) +
  geom_ribbon(aes(ymin = abs_lo, ymax = abs_hi), alpha=.15) +
  geom_line() + geom_hline(yintercept=0, linetype=2) +
  labs(y = "A − D (original scale)", x = "Year") +
  theme_minimal(base_size = 9)


#### TODO other plotting ####
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