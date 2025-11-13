# packages
require('DHARMa') 

# new speed function 2
m_ass_test_speed <- function(
  file_name = "diagnostics",
  mo, dat,
  # lists of variables to visualize against residuals
  cont  = NULL,              # e.g. c("year","area_holc_km2")
  categ = NULL,              # e.g. "holc_grade"
  trans = NULL,              # transformations for 'cont' (recycled): "none","log","ln","abs","sin","cos"
  # toggles
  show_base     = TRUE,      # fitted-vs-resid, sqrt|res|
  show_dharma   = FALSE,     # DHARMa composite + tests (+ optional temporal/spatial)  # <-- disabled
  show_reqq     = TRUE,      # random-effects Q–Q
  show_calib    = TRUE,      # binned observed vs predicted
  show_temporal = TRUE,      # PACF(residuals)
  show_spatial  = TRUE,      # residual maps
  # optional columns used for temporal/spatial diagnostics (only if present)
  # (a panel-wise serial correlation)
  time_var  = NULL,          # e.g., "year" (only used to label, ACF doesn’t need it)
  group_var = NULL,          # (unused when DHARMa is off)
  lat_var   = NULL,          # e.g., "lat"
  lon_var   = NULL,          # e.g., "lon"
  # device
  PNG = TRUE, width_ = 11, height_ = 6,
  n_cols_render = 2,         # we render panels in paired columns
  wrap_title = FALSE, wrap_width = 100,
  outdir = "Output/Model_ass/"
) {
  # ---- helpers ----
  have     <- function(x) !is.null(x) && length(x) && all(x %in% names(dat))
  safe_num <- function(x) as.numeric(x)
  .model_id <- function(mo) {
    cls <- class(mo)[1]
    fam <- tryCatch(stats::family(mo), error = function(e) NULL)
    if (!is.null(fam)) paste0(cls, " [family: ", fam$family, ", link: ", fam$link) else cls
  }
  .binned_cal <- function(y, mu, nbins = 10, main = "Binned observed vs predicted") {
    ok <- is.finite(y) & is.finite(mu); y <- y[ok]; mu <- mu[ok]
    uy <- sort(unique(y)); is_binary <- length(uy) <= 2 && all(uy %in% c(0, 1))
    qs <- stats::quantile(mu, probs = seq(0, 1, length.out = nbins + 1), na.rm = TRUE, type = 7)
    while (any(duplicated(qs))) { qs[which(duplicated(qs))] <- qs[which(duplicated(qs))] + .Machine$double.eps }
    bins  <- cut(mu, breaks = qs, include.lowest = TRUE, labels = FALSE)
    mu_bar <- tapply(mu, bins, mean); y_bar <- tapply(y, bins, mean); n <- tapply(y, bins, length)
    if (is_binary) { se <- sqrt(pmax(y_bar * (1 - y_bar) / n, 0)); ylab <- "Observed proportion"
    } else { se <- tapply(y, bins, function(z) stats::sd(z)/sqrt(length(z))); ylab <- "Observed mean" }
    plot(mu_bar, y_bar, pch = 16, col = "orange", xlab = "Predicted mean (response)", ylab = ylab, main = main)
    segments(mu_bar, y_bar - 2*se, mu_bar, y_bar + 2*se, col = "orange", lwd = 2)
    abline(0, 1, lty = 2, col = "grey40")
  }
  # SPEED: sampling helper for heavy plots
  .pick_idx <- function(n, cap = 5e4) if (n > cap) sample.int(n, cap) else seq_len(n)

  # colors once (correct rgb usage)
  col_neg <- rgb(83, 95, 124, alpha = 100, maxColorValue = 255)
  col_pos <- rgb(253, 184, 19, alpha = 100, maxColorValue = 255)

  # ---- model-derived ----
  fit <- tryCatch(stats::fitted(mo, type="response"), error=function(e) stats::fitted(mo))
  res <- tryCatch(stats::residuals(mo, type="pearson"),  error=function(e) stats::residuals(mo))
  fit <- safe_num(fit); res <- safe_num(res)

  # Random effects
  re_list <- NULL
  if (show_reqq) {
    re_list <- tryCatch({
      if (inherits(mo, "glmmTMB")) lapply(glmmTMB::ranef(mo, condVar=FALSE), function(x) x$cond)
      else if (inherits(mo, "merMod")) lme4::ranef(mo) else NULL
    }, error = function(e) NULL)
  }

  # ---- panels ----
  panels <- list()

  if (show_base) {
    panels[[length(panels)+1]] <- function() {
      # SPEED: plot a sampled cloud + fast LOWESS on <=10k points
      idx <- .pick_idx(length(res), cap = 5e4)
      plot(fit[idx], res[idx], pch = 16, cex = 0.4, col = "grey",
           main = "Pearson residuals vs fitted",
           xlab = "Fitted (response)", ylab = "Pearson residuals")
      abline(h=0, lty=2, col="red")
      idx2 <- if (length(idx) > 1e4) sample(idx, 1e4) else idx
      lw <- lowess(fit[idx2], res[idx2], f = 2/3, iter = 1)
      lines(lw, col = "black")
    }
  }

  if (show_base) {
    panels[[length(panels)+1]] <- function() {
      # SPEED: sample + LOWESS
      idx <- .pick_idx(length(res), cap = 5e4)
      plot(fit[idx], sqrt(abs(res[idx])), pch = 16, cex = 0.4, col = "grey",
           main="Sqrt(|res|) vs fitted", xlab="Fitted", ylab="Sqrt(|res|)")
      abline(h=0, lty=2, col="red")
      idx2 <- if (length(idx) > 1e4) sample(idx, 1e4) else idx
      lw <- lowess(fit[idx2], sqrt(abs(res[idx2])), f = 2/3, iter = 1)
      lines(lw, col = "black")
    }
  }
  if (show_calib) {
    panels[[length(panels)+1]] <- function() {
      y_obs <- stats::model.response(stats::model.frame(mo))
      .binned_cal(y_obs, fit, main="Binned observed vs predicted")
    }
  }

  if (length(cont)) {
    if (is.null(trans) || !length(trans)) trans <- rep("none", length(cont))
    if (length(trans) < length(cont))     trans <- rep_len(trans, length(cont))
    for (i in seq_along(cont)) {
      vname <- cont[i]; if (!vname %in% names(dat)) next
      v <- dat[[vname]]; xlab_ <- vname; tr <- trans[i]
      if (tr=="log") { v <- log10(v); xlab_ <- paste0("log10(", vname, ")") }
      if (tr=="ln")  { v <- log(v);   xlab_ <- paste0("ln(",    vname, ")") }
      if (tr=="abs") { v <- abs(v);   xlab_ <- paste0("abs(",   vname, ")") }
      if (tr=="sin") { v <- sin(v);   xlab_ <- paste0("sin(",   vname, ")") }
      if (tr=="cos") { v <- cos(v);   xlab_ <- paste0("cos(",   vname, ")") }
      panels[[length(panels)+1]] <- local({ vx <- v; xl <- xlab_; vn <- vname
        function() {
          # SPEED: sample + LOWESS
          idx <- .pick_idx(length(res), cap = 5e4)
          plot(vx[idx], res[idx], xlab=xl, ylab="Pearson residuals", col="grey", pch=16, cex=0.4,
               main=paste("Residuals vs", vn))
          abline(h=0, lty=2, col="red")
          idx2 <- if (length(idx) > 1e4) sample(idx, 1e4) else idx
          lw <- lowess(vx[idx2], res[idx2], f = 2/3, iter = 1)
          lines(lw, col = "black")
        }
      })
    }
  }

  if (length(categ)) {
    for (cat_var in categ) {
      if (!cat_var %in% names(dat)) next
      panels[[length(panels)+1]] <- local({ cv <- cat_var
        function() { boxplot(res ~ dat[[cv]], col="grey", ylab="Pearson residuals",
                             main=paste("Residuals by", cv)); abline(h=0, lty=2, col="red") }
      })
    }
  }

  if (!is.null(re_list) && length(re_list)) {
    for (grp in names(re_list)) {
      rr <- as.data.frame(re_list[[grp]])
      num_cols <- setdiff(names(rr), c("grpvar","condVar"))
      for (colname in num_cols) {
        panels[[length(panels)+1]] <- local({ x <- rr[[colname]]; g <- grp; cn <- colname
          function() { qqnorm(x, main=paste(g, cn), col="grey"); qqline(x, col="red") }
        })
      }
      if (length(num_cols) %% 2 == 1) panels[[length(panels)+1]] <- function() plot.new()
    }
  }

  # Temporal ACF (PACF of residuals)
  if (show_temporal) {
    panels[[length(panels)+1]] <- function() {
      stats::acf(res, type="p", main="Partial ACF (Pearson residuals)")
    }
  }

  # Spatial (map only; no DHARMa test)
  if (show_spatial && have(c(lon_var, lat_var))) {
    panels[[length(panels)+1]] <- function() {
      # SPEED: sample points for map
      idx <- .pick_idx(length(res), cap = 5e4)
      spx <- dat[[lon_var]][idx]; spy <- dat[[lat_var]][idx]
      r   <- res[idx]
      cols <- ifelse(r < 0, col_neg, col_pos)
      cex_vals <- c(1,1.5,2,2.5,3); cexpt <- as.numeric(cut(abs(r), 5, labels = cex_vals))
      plot(spx, spy, col=cols, cex=cexpt, pch=16, main="Spatial distribution of residuals",
           xlab="longitude", ylab="latitude")
      legend("topleft", pch=16, cex=.8, legend=c("<0", ">=0"),
             col=c(col_neg, col_pos))
    }
  }

  # ---- device + render ----
  if (PNG) { dir.create(outdir, showWarnings=FALSE, recursive=TRUE)
    png(file.path(outdir, paste0(file_name, ".png")), width=width_, height=height_, units="in", res=300)
    on.exit(dev.off(), add=TRUE)
  } else { dev.new(width=width_, height=height_) }
  n_panels <- length(panels); n_rows <- ceiling(n_panels / n_cols_render)
  op <- par(mfrow=c(n_rows, n_cols_render), tcl=-0.08, cex=0.55, cex.main=0.95,
            mar=c(2,2,2,1), mgp=c(1,0.25,0), oma=c(1,1,4,1))
  on.exit(par(op), add=TRUE)
  for (p in panels) p()

  call_txt <- tryCatch(deparse(stats::getCall(mo)), error=function(e) "model")
  ttl <- paste0("Model check: ", .model_id(mo), "\n", call_txt)
  if (wrap_title) ttl <- paste(strwrap(ttl, width=wrap_width), collapse="\n")
  mtext(ttl, side=3, line=1, cex=0.55, outer=TRUE)

  invisible(list(sim = NULL))
}



# new function
m_ass_new <- function(
  file_name = "diagnostics",
  mo, dat,
  # lists of variables to visualize against residuals
  cont  = NULL,              # e.g. c("year","area_holc_km2")
  categ = NULL,              # e.g. "holc_grade"
  trans = NULL,              # transformations for 'cont' (recycled): "none","log","ln","abs","sin","cos"
  # toggles
  show_base     = TRUE,      # fitted-vs-resid, sqrt|res|
  show_dharma   = TRUE,      # DHARMa composite + tests (+ optional temporal/spatial)
  show_reqq     = TRUE,      # random-effects Q–Q
  show_calib    = TRUE,      # binned observed vs predicted
  show_temporal = TRUE,      # PACF(residuals) and (optional) DHARMa temporal test
  show_spatial  = TRUE,      # residual maps and (optional) DHARMa spatial test
  # optional columns used for DHARMa temporal/spatial tests (only if present)
  # (a panel-wise serial correlation)
  time_var  = NULL,          # e.g., "year" # time variable for DHARMa temporal test; if null DHARMa skips this temporal test
  group_var = NULL,          # e.g., "city_state" (panel for temporal test); if null DHARMa treats the entire dataset as one series
  lat_var   = lat,          # e.g., "lat"
  lon_var   = lon,          # e.g., "lon"
  # device
  PNG = TRUE, width_ = 11, height_ = 6,
  n_cols_render = 2,         # we render panels in paired columns
  wrap_title = FALSE, wrap_width = 100,
  outdir = "Output/Model_ass/"
) {
  # ---- helpers ----
  have     <- function(x) !is.null(x) && length(x) && all(x %in% names(dat))
  safe_num <- function(x) as.numeric(x)
  col_neg <- rgb(83, 95, 124, alpha = 100, maxColorValue = 255)
  col_pos <- rgb(253, 184, 19, alpha = 100, maxColorValue = 255)

  .model_id <- function(mo) {
    cls <- class(mo)[1]
    fam <- tryCatch(stats::family(mo), error = function(e) NULL)
    if (!is.null(fam)) paste0(cls, " [family: ", fam$family, ", link: ", fam$link) else cls
  }
  .binned_cal <- function(y, mu, nbins = 10, main = "Binned observed vs predicted") {
    ok <- is.finite(y) & is.finite(mu); y <- y[ok]; mu <- mu[ok]
    uy <- sort(unique(y)); is_binary <- length(uy) <= 2 && all(uy %in% c(0, 1))
    qs <- stats::quantile(mu, probs = seq(0, 1, length.out = nbins + 1), na.rm = TRUE, type = 7)
    while (any(duplicated(qs))) { qs[which(duplicated(qs))] <- qs[which(duplicated(qs))] + .Machine$double.eps }
    bins  <- cut(mu, breaks = qs, include.lowest = TRUE, labels = FALSE)
    mu_bar <- tapply(mu, bins, mean); y_bar <- tapply(y, bins, mean); n <- tapply(y, bins, length)
    if (is_binary) { se <- sqrt(pmax(y_bar * (1 - y_bar) / n, 0)); ylab <- "Observed proportion"
    } else { se <- tapply(y, bins, function(z) stats::sd(z)/sqrt(length(z))); ylab <- "Observed mean" }
    plot(mu_bar, y_bar, pch = 16, col = "orange", xlab = "Predicted mean (response)", ylab = ylab, main = main)
    segments(mu_bar, y_bar - 2*se, mu_bar, y_bar + 2*se, col = "orange", lwd = 2)
    abline(0, 1, lty = 2, col = "grey40")
  }

  # ---- model-derived ----
  fit <- tryCatch(stats::fitted(mo, type="response"), error=function(e) stats::fitted(mo))
  res <- tryCatch(stats::residuals(mo, type="pearson"),  error=function(e) stats::residuals(mo))
  fit <- safe_num(fit); res <- safe_num(res)

  # DHARMa (simple + safe)
  sim <- NULL; dh_ok <- FALSE
  if (show_dharma && requireNamespace("DHARMa", quietly = TRUE)) {
    sim <- tryCatch(
      DHARMa::simulateResiduals(mo, n = 300, refit = FALSE, plot = FALSE),
      error = function(e) NULL
    )
    dh_ok <- !is.null(sim) && inherits(sim, "DHARMa") && length(sim$scaledResiduals) == length(res)
  }

  # Random effects
  re_list <- NULL
  if (show_reqq) {
    re_list <- tryCatch({
      if (inherits(mo, "glmmTMB")) lapply(glmmTMB::ranef(mo, condVar=FALSE), function(x) x$cond)
      else if (inherits(mo, "merMod")) lme4::ranef(mo) else NULL
    }, error = function(e) NULL)
  }

  # ---- panels ----
  panels <- list()

  if (show_base) {
    panels[[length(panels)+1]] <- function() {
      scatter.smooth(fit, res, col="grey", main="Pearson residuals vs fitted",
                     xlab="Fitted (response)", ylab="Pearson residuals")
      abline(h=0, lty=2, col="red")
    }
  }
  if (show_dharma && dh_ok) {
    panels[[length(panels)+1]] <- function() suppressMessages(DHARMa::plotQQunif(sim)) #  uniformity QQ
    panels[[length(panels)+1]] <- function() suppressMessages(DHARMa::plotResiduals(sim)) # residuals vs predicted 
    panels[[length(panels)+1]] <- function() suppressMessages(DHARMa::plotResiduals(sim, rank = TRUE))  # scaled residuals vs predicted (ranks)
  }

  if (show_base) {
    panels[[length(panels)+1]] <- function() {
      scatter.smooth(fit, sqrt(abs(res)), col="grey", main="Sqrt(|res|) vs fitted",
                     xlab="Fitted", ylab="Sqrt(|res|)"); abline(h=0, lty=2, col="red")
    }
  }
  if (show_calib) {
    panels[[length(panels)+1]] <- function() {
      y_obs <- stats::model.response(stats::model.frame(mo))
      .binned_cal(y_obs, fit, main="Binned observed vs predicted")
    }
  }

  if (length(cont)) {
    if (is.null(trans) || !length(trans)) trans <- rep("none", length(cont))
    if (length(trans) < length(cont)) trans <- rep_len(trans, length(cont))
    for (i in seq_along(cont)) {
      vname <- cont[i]; if (!vname %in% names(dat)) next
      v <- dat[[vname]]; xlab_ <- vname; tr <- trans[i]
      if (tr=="log") { v <- log10(v); xlab_ <- paste0("log10(", vname, ")") }
      if (tr=="ln")  { v <- log(v);   xlab_ <- paste0("ln(",    vname, ")") }
      if (tr=="abs") { v <- abs(v);   xlab_ <- paste0("abs(",   vname, ")") }
      if (tr=="sin") { v <- sin(v);   xlab_ <- paste0("sin(",   vname, ")") }
      if (tr=="cos") { v <- cos(v);   xlab_ <- paste0("cos(",   vname, ")") }
      panels[[length(panels)+1]] <- local({ vx <- v; xl <- xlab_; vn <- vname
        function() { scatter.smooth(vx, res, xlab=xl, ylab="Pearson residuals", col="grey",
                                    main=paste("Residuals vs", vn)); abline(h=0, lty=2, col="red") }
      })
    }
  }

  if (length(categ)) {
    for (cat_var in categ) {
      if (!cat_var %in% names(dat)) next
      panels[[length(panels)+1]] <- local({ cv <- cat_var
        function() { boxplot(res ~ dat[[cv]], col="grey", ylab="Pearson residuals",
                             main=paste("Residuals by", cv)); abline(h=0, lty=2, col="red") }
      })
    }
  }

  if (!is.null(re_list) && length(re_list)) {
    for (grp in names(re_list)) {
      rr <- as.data.frame(re_list[[grp]])
      num_cols <- setdiff(names(rr), c("grpvar","condVar"))
      for (colname in num_cols) {
        panels[[length(panels)+1]] <- local({ x <- rr[[colname]]; g <- grp; cn <- colname
          function() { qqnorm(x, main=paste(g, cn), col="grey"); qqline(x, col="red") }
        })
      }
      if (length(num_cols) %% 2 == 1) panels[[length(panels)+1]] <- function() plot.new()
    }
  }

  # Temporal ACF + DHARMa temporal (auto, no new args)
  if (show_temporal) {
    panels[[length(panels)+1]] <- function() {
      stats::acf(res, type="p", main="Partial ACF (Pearson residuals)")
    }
    if (dh_ok && !is.null(time_var) && time_var %in% names(dat)) {
      dh_has_group <- "group" %in% names(formals(DHARMa::testTemporalAutocorrelation))
      if (dh_has_group && !is.null(group_var) && group_var %in% names(dat)) {
        panels[[length(panels)+1]] <- function() {
          DHARMa::testTemporalAutocorrelation(sim, time = dat[[time_var]],
                                              group = dat[[group_var]], plot = TRUE)
          mtext("DHARMa temporal test (grouped)", side=3, line=-1, cex=0.7)
        }
      } else {
        # emulate grouping lightly with fixed internal defaults
        panels[[length(panels)+1]] <- function() {
          gvar <- if (!is.null(group_var) && group_var %in% names(dat)) dat[[group_var]] else factor(1)
          sp   <- split(seq_len(NROW(dat)), gvar)
          sp   <- Filter(function(idx) length(idx) >= 5, sp)          # min length = 5
          if (length(sp) > 200) { set.seed(123); sp <- sp[sample(seq_along(sp), 200)] }  # cap = 200
          pvals <- vapply(sp, function(idx) {
            sim_g <- DHARMa::recalculateResiduals(simulationOutput = sim, sel = idx)
            out <- tryCatch(DHARMa::testTemporalAutocorrelation(sim_g, time = dat[[time_var]][idx], plot = FALSE),
                            error = function(e) list(p.value = NA_real_))
            as.numeric(out$p.value)
          }, numeric(1))
          hist(pvals, breaks = 20, main = "DHARMa temporal test p-values (by group)", xlab = "p-value")
          abline(v = 0.05, lty = 2, col = "red")
          mtext(sprintf("groups tested=%d, median p=%.3f, %%<0.05 = %.1f%%",
                        length(pvals), stats::median(pvals, na.rm=TRUE),
                        100*mean(pvals < 0.05, na.rm=TRUE)), side=3, line=-1, cex=0.7)
        }
      }
    } else if (show_dharma && !dh_ok) {
      panels[[length(panels)+1]] <- function() {
        plot.new(); title("DHARMa temporal test skipped (simulateResiduals failed)")
      }
    }
  }

  # Spatial
  if (show_spatial && have(c(lon_var, lat_var))) {
    panels[[length(panels)+1]] <- function() {
      spx <- dat[[lon_var]]; spy <- dat[[lat_var]]
      cols <- ifelse(res < 0, col_neg, col_pos)
      cex_vals <- c(1,1.5,2,2.5,3); cexpt <- as.numeric(cut(abs(res), 5, labels = cex_vals))
      plot(spx, spy, col=cols, cex=cexpt, pch=16, main="Spatial distribution of residuals",
           xlab="longitude", ylab="latitude")
      legend("topleft", pch=16, cex=.8, legend=c("<0", ">=0"),
             col=c(col_neg, col_pos))
    }
    if (dh_ok) {
      panels[[length(panels)+1]] <- function() {
        DHARMa::testSpatialAutocorrelation(simulationOutput = sim,
                                           x = dat[[lon_var]], y = dat[[lat_var]], plot = TRUE)
        mtext("DHARMa spatial test", side=3, line=-1, cex=0.7)
      }
    }
  }

  # ---- device + render ----
  if (PNG) { dir.create(outdir, showWarnings=FALSE, recursive=TRUE)
    png(file.path(outdir, paste0(file_name, ".png")), width=width_, height=height_, units="in", res=300)
    on.exit(dev.off(), add=TRUE)
  } else { dev.new(width=width_, height=height_) }
  n_panels <- length(panels); n_rows <- ceiling(n_panels / n_cols_render)
  op <- par(mfrow=c(n_rows, n_cols_render), tcl=-0.08, cex=0.55, cex.main=0.95,
            mar=c(2,2,2,1), mgp=c(1,0.25,0), oma=c(1,1,4,1))
  on.exit(par(op), add=TRUE)
  for (p in panels) p()

  call_txt <- tryCatch(deparse(stats::getCall(mo)), error=function(e) "model")
  ttl <- paste0("Model check: ", .model_id(mo), "\n", call_txt)
  if (wrap_title) ttl <- paste(strwrap(ttl, width=wrap_width), collapse="\n")
  mtext(ttl, side=3, line=1, cex=0.55, outer=TRUE)

  invisible(list(sim = if (dh_ok) sim else NULL))
}


# old function
m_ass = function(
    file_name = 'define', 
    mo = m0, # mo: model
    dat = d,  # dat: data used in the model
    cont = NULL, # vector of variable names used as continues fixed effects
    categ = NULL, # vector of variable names used as categorical fixed effects
    trans = "none", # vector of transformations used for each fixed effect
    #nested = FALSE, # indicate whether some of the random intercepts are nested,
    spatial = TRUE, temporal = TRUE, 
    PNG = TRUE,  width_ = 10, height_ = 5,
    n_col = 6, n_row = NULL, # number of columns and rows if automatic calculation not desirable
    wrap_title = FALSE, wrap_width = 100, 
    outdir = 'Output/Model_ass/'){ #output directory

    # example: m_ass(name = "Table S1a - full a", mo = mhs, dat = dh, fixed = c("SD", "FlockSize", "BodyMass", "rad", "rad", "Temp", "Human"), trans = c("log", "log", "log", "sin", "cos", "", ""), outdir = here::here("Outputs/modelAss/")) 
      # fixed = c('bout_m','prop_ip'); trans = 'none'
      
    l=data.frame(summary(mo)$varcor)
    l = l[is.na(l$var2),]
    
    # number of rows in a plot
    base_plots <- 3  # e.g., residuals vs fitted, sqrt residuals, Q-Q
    rand_plots <- nrow(l) # number of random effect Q-Qs
    n <- base_plots + rand_plots + length(cont) + length(categ) +
        (if (temporal) 1 else 0) +
        (if (spatial) 3 else 0) #- 1 # sometimes helpful
   
    if (is.null(n_row)) n_row <- ceiling(n / n_col)

    # plotting device
    if (PNG) {
     png(paste0(outdir,file_name, ".png"), width = width_, height = height_,units="in", res=300) #res = 150 ok for html
     par(mfrow = c(n_row, n_col), tcl = -0.08, cex = 0.5, cex.main = 0.95, mar = c(2, 2, 2, 1), mgp=c(1,0,0),
     oma = c(1,1,4,1))
    } else {
     dev.new(width=width_,height=height_)
     par(mfrow = c(n_row, n_col), tcl = -0.08, cex = 0.5, cex.main = 0.95, mar = c(2, 2, 2, 1), mgp=c(1,0,0), 
     oma = c(1,1,2,1))
    }

    # base plots
    scatter.smooth(fitted(mo),resid(mo),col='grey');abline(h=0, lty=2, col ='red')
    scatter.smooth(fitted(mo),sqrt(abs(resid(mo))), col='grey') #test = data.table(fitted = fitted(mo), sqrt_abs_res =sqrt(abs(resid(mo))))  %>% test[fitted<2]; scatter.smooth(test$fitted,test$sqrt_abs_res, col='grey')
    qqnorm(resid(mo), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='grey');qqline(resid(mo), col = 'red')
    
    # random plots 
      #unique(l$grp[l$grp!="Residual"])
    for(i in unique(l$grp[l$grp!="Residual"])){
      #i = "lat_pop"
      ll <- ranef(mo)[[i]]
      for (colname in names(ll)) {
        qqnorm(ll[[colname]], main = paste(i, colname), col = 'grey')
        qqline(ll[[colname]], col = 'red')
      }
    }
    
    # fixed effects
    ## continuous
    if (!is.null(cont)) {
      for (i in seq_along(cont)) {
        # i = 1
        var <- dat[[cont[i]]] # var = dat[["bout_m_z"]]
        trans_type <- trans[i]
        if (trans_type == 'none'){ xlab_ = cont[i] } else {xlab_ = paste0(trans_type, "(", cont[i], ")")}
        if (trans_type == 'log') var <- log10(var)
        if (trans_type == 'ln') var <- log(var)
        if (trans_type == 'abs') var <- abs(var)
        if (trans_type == 'sin') var <- sin(var)
        if (trans_type == 'cos') var <- cos(var)
        scatter.smooth(var, resid(mo), xlab = xlab_, ylab = "residuals", col = 'grey'); abline(h = 0, lty = 2,lwd=1, col = 'red')
      }
    }
    ## categorical
    if (!is.null(categ)) {
      for (cat_var in categ) {
        boxplot(resid(mo) ~ dat[[cat_var]], col = 'grey', ylab = "residuals"); abline(h = 0, lty = 2, lwd=1, col = 'red')
      }
    }

    # autocorrelations       
    if(temporal){
      acf(resid(mo), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
    }
    
    if(spatial){    
      spdata <- data.table(resid = resid(mo), x = dat$lat, y = dat$lon)
      spdata = spdata[x>-40] # to enhance visualisation, removes one far off study site
      spdata[ , col := ifelse(resid < 0, col_neg,
                          col_pos)
            ]
      cex_vals <- c(1, 1.5, 2, 2.5, 3)
      spdata[, cex := as.numeric(cut(abs(resid), 5, labels = cex_vals))]
      
      plot(spdata$x, spdata$y, col = spdata$col, cex = spdata$cex, pch = 16, main = "Spatial distribution of residuals", xlab = "longitude", ylab = "latitude")
      legend("topleft", pch=16, cex=0.8, legend=c('<0','>=0'), col=c(col_neg,col_pos))

      spdata_neg = spdata[resid<0]
      spdata_pos = spdata[resid>=0]
      plot(spdata_neg$x, spdata_neg$y,col=spdata_neg$col, cex=spdata_neg$cex, pch= 16, main=list('Spatial distribution of residuals (<0)', cex=0.8), xlab = "longitude", ylab = "latitude")
      
      plot(spdata_pos$x, spdata_pos$y,col=spdata_pos$col, cex=spdata_pos$cex, pch= 16, main=list('Spatial distribution of residuals (>=0)', cex=0.8), xlab = "longitude", ylab = "latitude")
    }

    # title 
    if (wrap_title) {
      title_text <- strwrap(
        paste0("Model check: ", slot(mo, "call")[1], "(", slot(mo, "call")[2], ")"),
        width = wrap_width
      ) 
      mtext(paste(title_text, collapse = "\n"), side = 3, line = 1, cex = 0.5, outer = TRUE)
      } else {
        mtext(paste0("Model check: ", slot(mo, "call")[1], "(", slot(mo, "call")[2], ")"), side = 3, line = 1, cex = 0.5, outer = TRUE)
      }

    if (PNG) dev.off()
}
  
# generate for the complex models
m_ass('completeness',mo = mC1p, dat = dC, 
    cont = c('ndvi','pct_pa','pop_per_km','mean_temp_c','mean_precip_mm'),
    categ = c('holc_grade'),
    trans = c('none','none','none','none','none','none'),
    temporal = FALSE
    )

m_ass('completeness_mC2p',mo = mC2p, dat = dC, 
    cont = c('ndvi','pct_pa','pop_per_km','mean_temp_c','mean_precip_mm'),
    categ = c('holc_grade'),
    trans = c('none','none','none','none','none','none'),
    temporal = FALSE
    )    

m_ass('sampling_density',mo = mB1p, dat = dB, 
    cont = c('ndvi','pct_pa','pop_per_km','mean_temp_c','mean_precip_mm'),
    categ = c('holc_grade'),
    trans = c('none','none','none','none','none','none'),
    temporal = FALSE
    )

m_ass('sampling_density_mB2p',mo = mB2p, dat = dB, 
    cont = c('ndvi','pct_pa','pop_per_km','mean_temp_c','mean_precip_mm'),
    categ = c('holc_grade'),
    trans = c('none','none','none','none','none','none'),
    temporal = FALSE
    )      

m_ass('sampling01_m1p_g',mo = m1p_g, dat = dA, 
    cont = c('ndvi','pct_pa','pop_per_km','mean_temp_c','mean_precip_mm'),
    categ = c('holc_grade'),
    trans = c('none','none','none','none','none','none'),
    temporal = FALSE
    )  

m_ass('sampling01_m2p_g',mo = m2p_g, dat = dA, 
    cont = c('ndvi','pct_pa','pop_per_km','mean_temp_c','mean_precip_mm'),
    categ = c('holc_grade'),
    trans = c('none','none','none','none','none','none'),
    temporal = FALSE
    )  

# generate for trend models 2000-2020
m_ass('trend00-20_r-int',mo = ma, dat = dd, 
    cont = c('year'),
    categ = c('holc_grade'),
    trans = c('none')
    ) 

m_ass('trend00-20_r-int-nested',mo = mb, dat = dd, 
    cont = c('year'),
    categ = c('holc_grade'),
    trans = c('none')
    )  

m_ass('trend00-20_r-int_r-sl-year-city-state',mo = mas1, dat = dd, 
    cont = c('year'),
    categ = c('holc_grade'),
    trans = c('none')
    )

m_ass('trend00-20_r-int-nested_r-sl-year',mo = mbs1, dat = dd, 
    cont = c('year'),
    categ = c('holc_grade'),
    trans = c('none')
    )  

m_ass('trend00-20_r-int-nested_r-sl-year_2',mo = msab1, dat = dd, 
    cont = c('year'),
    categ = c('holc_grade'),
    trans = c('none')
    )  

# generate for trend models 2010-2020    
m_ass('trend10-20_r-int',mo = ma_, dat = dd10, 
    cont = c('year'),
    categ = c('holc_grade'),
    trans = c('none')
    )  
m_ass('trend10-20_r-int-nested',mo = mb_, dat = dd10, 
    cont = c('year'),
    categ = c('holc_grade'),
    trans = c('none')
    )  

m_ass('trend10-20_r-int_r-sl-year-city-state',mo = mas1_, dat = dd10, 
    cont = c('year'),
    categ = c('holc_grade'),
    trans = c('none')
    )

m_ass('trend10-20_r-int-nested_r-sl-year',mo = mbs1_, dat = dd10, 
    cont = c('year'),
    categ = c('holc_grade'),
    trans = c('none')
    )  

m_ass('trend10-20_r-int-nested_r-sl-year_2',mo = msab1_, dat = dd10, 
    cont = c('year'),
    categ = c('holc_grade'),
    trans = c('none')
    )  

# negative binomial
m_ass('Fig_Z2_nb_glmmTMB_5',mo = msab1_, dat = dd10, 
    cont = c('year'),
    categ = c('holc_grade'),
    trans = c('none')
    ) 

# generate for trend model on all data 



# further checks
simulateResiduals(ma) |> testDispersion()
simulateResiduals(mbs1) |> testDispersion(); testSpatial();      