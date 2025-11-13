m_ass = function(
    name = 'diagnostics', # define model name (used as file name and in a title)
    mo = your_model, # mo: model
    dat = your_data,  # dat: data used in the model
    accumulate = FALSE,   # collect panels (TRUE) or draw immediately (FALSE)

    # lists of variables to visualize against residuals
    cont = NULL, # vector of variable names used as continues fixed effects
    categ = NULL, # vector of variable names used as categorical fixed effects
    trans = "none", # vector of transformations used for each fixed effect
    #nested = FALSE, # indicate whether some of the random intercepts are nested,
    # plots to show
    offset = FALSE, # TRUE if offset was used
    show_base     = TRUE,   # fitted-vs-resid, sqrt|res|
    show_binned    = FALSE,  # observed vs fitted (binned fitted and means of observed)
    show_reqq     = TRUE,      # random-effects Q–Q
    show_temporal = TRUE, # PACF(residuals)
    show_temporal_grouped = NULL, # PACF on means of temporal variable (e.g. year)
    show_spatial = TRUE, # PACF(residuals)
    lat_var = 'lat',
    lon_var = 'lon',
    max_points = 10000, # max number of points to plot; if n higher, the data will be sampled to get 5000
    PNG = TRUE,  width_ = 10, height_ = 2,
    n_col = 6, n_row = NULL, # number of columns and rows if automatic calculation not desirable
    colour_pt = rgb(0,0,0,0.15),
    wrap_title = FALSE, wrap_width = 100, 
    outdir = 'Output/Model_ass/'
  ){ #output directory

  ## example ##
   #m_ass(name = "Table S1a - full a", mo = mhs, dat = dh, fixed = c("SD", "FlockSize", "BodyMass", "rad", "rad", "Temp", "Human"), trans = c("log", "log", "log", "sin", "cos", "", ""), outdir = here::here("Outputs/modelAss/")) 
  ## testing ##
  # name = "test1"; mo = d_ri; dat = hB_; accumulate = FALSE; cont = NULL; categ = 'holc_grade'; trans = 'none'; show_base = TRUE; show_binned = FALSE; show_reqq = TRUE; show_temporal = TRUE; show_temporal_grouped = NULL; show_spatial = TRUE; lat_var = 'lat'; lon_var = 'lon'; PNG = FALSE;  width_ = 10; height_ = 5; n_col = 6; n_row = NULL; wrap_title = FALSE; wrap_width = 100; outdir = 'Output/Model_ass/'

  ## functions and constants ##
    have     <- function(x) !is.null(x) && length(x) && all(x %in% names(dat))
    safe_num <- function(x) as.numeric(x)
    col_neg <- rgb(83, 95, 124, alpha = 100, maxColorValue = 255)
    col_pos <- rgb(253, 184, 19, alpha = 100, maxColorValue = 255)
    
    ### flatten (grp -> coef -> vector) into (grp:coef -> vector)
    flatten_re <- function(re_list) {
      out <- list()
      for (g in names(re_list)) {
        x <- re_list[[g]]
        if (is.list(x)) for (c in names(x)) out[[paste(g, c, sep=":")]] <- x[[c]] else out[[g]] <- x
      }
      out
    }

    ### extrat random effets (blups), if present; which`'all' get also slopes
    get_re_vals <- function(mo, which = "all") {
      pick_cols <- function(dd) {
        if (which == "all") {
          out <- lapply(names(dd), function(col) setNames(dd[[col]], rownames(dd)))
          names(out) <- names(dd); out
        } else if (which == "first") {
          list(setNames(dd[[1L]], rownames(dd)))
        } else { # "(Intercept)"
          if ("(Intercept)" %in% names(dd)) list(setNames(dd[["(Intercept)"]], rownames(dd)))
          else list(setNames(dd[[1L]], rownames(dd)))
        }
      }


      if (inherits(mo, "lmerMod") || inherits(mo, "merMod")) {
        re <- lme4::ranef(mo)
        out <- lapply(re, pick_cols)
        # flatten one level if 'which' picks single column
        if (which != "all") out <- lapply(out, `[[`, 1L)
        return(out)

      } else if (inherits(mo, "glmmTMB")) {
        re <- glmmTMB::ranef(mo)
        re <- if (!is.null(re$cond)) re$cond else re[[1L]]
        out <- lapply(re, pick_cols)
        if (which != "all") out <- lapply(out, `[[`, 1L)
        return(out)

      } else if (inherits(mo, "gam")) {  # mgcv::bam
        beta <- coef(mo); sm <- mo$smooth; out <- list()
        for (s in sm) if (isTRUE(s$bs %in% "re")) {
          idx <- s$first.para:s$last.para
          lev <- levels(mo$var.summary[[s$term]])
          vals <- beta[idx]; names(vals) <- lev[seq_along(vals)]
          out[[s$term]] <- vals
        }
        return(out)
      
      } else if (inherits(mo, "negbin") || inherits(mo, "glm") || inherits(mo, "lm")) {
      return(list())  
      } else stop("Unsupported model class.")
    }

    ### get RE Names only (uniform)
    get_re_names <- function(mo, which = "all") {
     re <- get_re_vals(mo, which = which)           # your function
     if (which == "all") names(flatten_re(re)) else names(re)
    }

    ### make qq plots for random effects, if present
    plot_re_qq <- function(mo, which="all") {
     re <- get_re_vals(mo, which = which)
     if (!length(re)) return(invisible(NULL))  
     # flatten if which="all": make names like "grp:coef"
     if (which == "all") {
      flat <- list()
      for (grp in names(re)) {
        if (is.list(re[[grp]])) {
          for (coefnm in names(re[[grp]])) {
            flat[[paste(grp, coefnm, sep=":")]] <- re[[grp]][[coefnm]]
          }
        } else flat[[grp]] <- re[[grp]]
      }
      re <- flat
     }

     for (nm in names(re)) {
      panels_add(local({
        x <- re[[nm]]         # capture vector
        ttl <- nm             # capture name
        function() {          # <-- single closure that actually draws
          qqnorm(x, main = ttl, col = colour_pt)
          qqline(x, col = "red")
        }
      }))
     }
    }

   ### panels (collector + immediate draw switch) ##
   panels <- list(); .i <- 0L
   panels_add <- function(fun) {
      if (accumulate) {
        .i <<- .i + 1L
        panels[[.i]] <<- fun
      } else {
        fun()
      }
      invisible(NULL)
   }
  
  ## get fit, res, and family ##
    y_obs <- tryCatch(
      stats::model.response(stats::model.frame(mo)),
      error = function(e) NULL
    )
    if (is.matrix(y_obs) && ncol(y_obs)==2L) y_obs <- y_obs[,1]/rowSums(y_obs) # for binomial response
    fit <- tryCatch(stats::fitted(mo, type="response"), error=function(e) stats::fitted(mo))
    res <- tryCatch(stats::residuals(mo, type="pearson"),  error=function(e) stats::residuals(mo))
    fit <- safe_num(fit); res <- safe_num(res); y_obs <- safe_num(y_obs)

    fam <- tryCatch(tolower(family(mo)$family), error = function(e) NA_character_)
    is_nb <- grepl("negative binomial|nbinom1|nbinom2", fam) |  inherits(mo, "negbin") # MASS::glm.nb etc.
    is_pois <- !is_nb && grepl("poisson", fam) # captures "poisson" and "quasipoisson"
    is_bin  <- !is_nb && !is_pois && grepl("binomial", fam) # captures "binomial", "quasibinomial" but excludes "negative binomial"

    # pretty family label (round NB theta if available)
      fam_txt <- if (!is.na(fam)) {
        if (inherits(mo, "negbin") && !is.null(mo$theta)) {
          paste0("negative binomial(theta: ", round(mo$theta, 2), ")")
        } else {
          fam
        }
      } else "n/a"

  ## stratified subsample for plotting ##
  N <- length(fit)
  idx_plot <- seq_len(N)

  if (!is.null(max_points) && is.finite(max_points) && N > max_points) {
    n_strata <- 50L
    brks <- unique(quantile(fit, probs = seq(0, 1, length.out = n_strata + 1),
                     na.rm = TRUE))
    
    if (length(brks) > 2L) {
      # standard stratified sampling across (length(brks)-1) non-empty bins
      bin <- cut(fit, brks, include.lowest = TRUE, labels = FALSE)

      n_eff_strata <- length(brks) - 1L
      per_bin <- ceiling(max_points / n_eff_strata)

      set.seed(5) 
      idx_plot <- sort(unique(unlist(tapply(idx_plot, bin, function(ii) {
      if (length(ii) <= per_bin) ii else sample(ii, per_bin)}))))

    } else {
      # fallback: too little variation in fitted values to stratify
      # -> simple random sample
      set.seed(5)
      idx_plot <- sort(sample(idx_plot, max_points))
    }
  }

  fit_s = fit[idx_plot]
  res_s = res[idx_plot]
  y_obs_s = y_obs[idx_plot]


  ## MAKE PANELS ##

   ### if drawing immediately, open device now  ###
    if (!accumulate) {
      
      base_plots <-
        (if (show_base) 4 else 0) +                # resid~fit, sqrt|res|, obs~fit, QQ 
        1 +                          # dispersion panel (always when available)
        #(if (show_base && !show_binned &&
        #     !is.null(mo$family) &&
        #     mo$family$family == "gaussian") 1 else 0) +  # normal QQ only for gaussian;  TODO: perhaps add optional/rough for Poisson/NB
        (if (show_binned) 2 else 0) + 
        (if (is_bin) -1 else 0) 

      has_re   <- length(get_re_names(mo)) > 0L
      rand_plots <- if (show_reqq && has_re) length(get_re_names(mo)) else 0L

      n <- base_plots + rand_plots + 
          length(cont) + length(categ) +
          (if (offset) 2 else 0) +
          (if (show_temporal) 1 else 0) +
          (if (!is.null(show_temporal_grouped)) 1 else 0) +
          (if (show_spatial) 5 else 0) # 3 maps + Moran's I panel + semivariogram

      if (n == 0) n <- 1
      if (is.null(n_row)) n_row <- ceiling(n / n_col)

      if (PNG) {
      png(paste0(outdir,name, ".png"), 
        width = width_, height = height_*n_row, 
        units="in", res=300
        ) #res = 150 ok for html
      par(mfrow = c(n_row, n_col), tcl = -0.08, cex = 0.5, 
        cex.main = 0.95, mar = c(2, 2, 2, 1), mgp=c(1,0,0),
        oma = c(1,1,4,1))
      } else {
      dev.new(width=width_,height=height_*n_row)
      par(mfrow = c(n_row, n_col), tcl = -0.08, cex = 0.5, 
      cex.main = 0.95, mar = c(2, 2, 2, 1), mgp=c(1,0,0), 
      oma = c(1,1,2,1))
      }
    }


  ### fitted vs residuals ###
    if (show_base) {
      # patterns/funnel shapes → hints at over/underdispersion or mean-variance misfit
      panels_add(function() {    
        plot(fit_s, res_s,
            xlab = "Fitted (response)",
            ylab = "Pearson residuals",
            main = "Pearson residuals vs fitted",
            pch  = 16, col  = colour_pt
          )#scatter.smooth(fit,res,col=colour_pt, xlab="Fitted (response)", ylab="Pearson residuals", main="Pearson residuals vs fitted")
          ok <- is.finite(fit) & is.finite(res)
          lines(lowess(fit[ok], res[ok]), lwd = 1.2) # smoother on full data
          abline(h=0, lty=2, col ='red')
        })

      # highlights non-constant residual spread with μ
      panels_add(function() {  
        plot(fit_s, sqrt(abs(res_s)),
            xlab = "Fitted (response)",
            ylab = "Sqrt(|Pearson res|)",
            main = "Sqrt(|res|) vs fitted",
            pch  = 16, col  = colour_pt
          ) # scatter.smooth(fit,sqrt(abs(res)), col=colour_pt, xlab="Fitted (response)", ylab="Sqrt(|Pearson res|)", main="Sqrt(|res|) vs fitted")
          ok <- is.finite(fit) & is.finite(res)
          lines(lowess(fit[ok], sqrt(abs(res[ok]))), lwd = 1.2) # smoother on full data
          abline(h=0, lty=2, col ='red')
        })  
    }

    if (show_base && !is_pois && !is_nb && !is_bin) {
      panels_add(function() {
        plot(fit_s, y_obs_s,
            xlab = "Fitted (response)",
            ylab = "Observed",
            main = "Observed vs fitted",
            pch  = 16, col = colour_pt)
        abline(0, 1, lty = 2, col = "red")
        ok <- is.finite(fit) & is.finite(y_obs)
        lines(lowess(fit[ok], y_obs[ok]), lwd = 1.2)
      })
    }  

    if (show_binned) {
      # interpratation: for Poisson/NB counts: systematic deviation from 1:1 or too-wide scatter suggests misspecification or extra structure; for interpretation of effects/predictions, use rates if that’s the scientific quantity

      # panel with equal-count bins for counts
      panels_add(function() {  

       if (is_bin) { # Binomial / quasibinomial

        # bins on fitted probabilities
        seq_ = seq(0.05, 0.95, by = 0.1)
        bins_ = cut(fit, seq(0, 1, by = 0.1), include.lowest = TRUE)

        # bin means & SE 
        means = tapply(y_obs, bins_, mean)
        se = tapply(y_obs, bins_, function(x) sd(x)/sqrt(length(x))) # binomial approx: n <- tapply(y, b, length); se <- sqrt(pmax(means * (1 - means) / n, 0)) #
        
        plot(fit_s, jitter(y_obs_s, amount=0.05), 
          xlab = "Fitted values", ylab= "Observed proportion", 
          col = colour_pt, main="Binned observed vs fitted")
          abline(0, 1, lty = 2, col = "red")
          points(seq_, means, pch = 16, col = "orange")
          segments(seq_, means-2*se, seq_, means+2*se, col = "orange", lwd = 2)
       
       } else if (is_pois || is_nb) { # Poisson / NB (incl. glmmTMB, bam NB, glm.nb)
                                      # For NB, large scatter but no clear bias is a norm; For negbin, large scatter but no clear bias is normal; consistent bias across bins signals poor mean structure.
 
        n_bins <- 10
        brks   <- unique(quantile(fit, probs = seq(0, 1, length.out = n_bins + 1),
                          na.rm = TRUE))

        if (length(brks) > 2L) {
         bins_  <- cut(fit, brks, include.lowest = TRUE)

         obs_mean <- tapply(y_obs, bins_, mean)
         fit_mean <- tapply(fit,   bins_, mean)
         se       <- tapply(y_obs, bins_, function(x)
                            sd(x, na.rm = TRUE) / sqrt(length(x)))

         plot(fit_mean, obs_mean,
            xlab = "Mean fitted count", ylab = "Mean observed count",
            main = "Binned observed vs fitted counts",
            pch = 16, col = "orange")
         abline(0, 1, lty = 2, col = "red")
         segments(fit_mean, obs_mean - 2 * se,
                fit_mean, obs_mean + 2 * se,
                col = "orange", lwd = 2)
        } else {
          ## Fallback: not enough unique fitted values for stable binning ##
          plot(fit_s, res_s,
               xlab = "Fitted", ylab = "Residual",
               main = "Binned plot skipped: low variation in fitted values",
               col  = colour_pt)
          abline(h = 0, lty = 2, col = "red")
        }

       } else {
       ## Fallback for other families ##
       plot(fit_s, res_s,
           xlab = "Fitted", ylab = "Residual",
           main = paste("Binned plot not defined for", fam_txt),
           col = colour_pt)
       abline(h = 0, lty = 2, col = "red")
       }
      })

      # zoomed binned plot 
      panels_add(function() {

        zoom_q <- 0.95  # use central 95% of fitted values

        if (is_bin) {

          q    <- quantile(fit, zoom_q, na.rm = TRUE)
          idx  <- fit <= q
          if (!any(idx, na.rm = TRUE)) { idx <- rep(TRUE, length(fit)) }

          # bins on truncated range
          seq_  <- seq(0.05, 0.95, by = 0.1)
          bins_ <- cut(fit[idx], seq(0, 1, by = 0.1), include.lowest = TRUE)

          means <- tapply(y_obs[idx], bins_, mean)
          se    <- tapply(y_obs[idx],  bins_, function(x) sd(x)/sqrt(length(x)))

          idx_s <- fit_s <= q

          plot(fit_s[idx_s], jitter(y_obs_s[idx_s], amount = 0.05),
               xlab = "Fitted values (zoomed)", ylab = "Observed proportion",
               col  = colour_pt,
               main = paste0("Zoomed binned (≤ ", zoom_q * 100, "% fitted)"))
          abline(0, 1, lty = 2, col = "red")
          points(seq_, means, pch = 16, col = "orange")
          segments(seq_,
                   means - 2 * se,
                   seq_,
                   means + 2 * se,
                   col = "orange", lwd = 2)

        } else if (is_pois || is_nb) {

          q    <- quantile(fit, zoom_q, na.rm = TRUE)
          idx  <- fit <= q
          if (!any(idx, na.rm = TRUE)) { idx <- rep(TRUE, length(fit)) }

          n_bins <- 10
          brks_z <- unique(quantile(fit[idx],
                             probs = seq(0, 1, length.out = n_bins + 1),
                             na.rm = TRUE))
          
          if (length(brks_z) > 2L) {
           bins_z <- cut(fit[idx], brks_z, include.lowest = TRUE)

           obs_mean_z <- tapply(y_obs[idx], bins_z, mean)
           fit_mean_z <- tapply(fit[idx],   bins_z, mean)
           se_z       <- tapply(y_obs[idx], bins_z, function(x)
                                 sd(x, na.rm = TRUE) / sqrt(length(x)))

           idx_s <- fit_s <= q

           plot(fit_s[idx_s], y_obs_s[idx_s],
               xlab = "Fitted count (zoomed)",
               ylab = "Observed count",
               main = paste0("Binned observed vs fitted (≤ ", zoom_q * 100, "% fitted)"),
               pch  = 16, col = colour_pt)
           abline(0, 1, lty = 2, col = "red")
           segments(fit_mean_z,
                   obs_mean_z - 2 * se_z,
                   fit_mean_z,
                   obs_mean_z + 2 * se_z,
                   col = "orange", lwd = 2)
           points(fit_mean_z, obs_mean_z,
                 pch = 16, col = "orange")
          } else {
            ## Fallback zoom
            idx_s <- fit_s <= q
            if (!any(idx_s, na.rm = TRUE)) idx_s <- rep(TRUE, length(fit_s))
            plot(fit_s[idx_s], res_s[idx_s],
                 xlab = "Fitted (zoomed)", ylab = "Residual",
                 main = paste0("Zoomed residuals vs fitted (", zoom_q * 100, "%)"),
                 col  = colour_pt)
            abline(h = 0, lty = 2, col = "red")
          }

        } else {

          ## Fallback zoom: residuals vs fitted in dense region
          q    <- quantile(fit, zoom_q, na.rm = TRUE)
          idx  <- fit_s <= q
          if (!any(idx, na.rm = TRUE)) { idx <- rep(TRUE, length(fit_s)) }

          plot(fit_s[idx], res_s[idx],
               xlab = "Fitted (zoomed)", ylab = "Residual",
               main = paste("Zoomed residuals vs fitted (", zoom_q * 100, "%)", sep = ""),
               col  = colour_pt)
          abline(h = 0, lty = 2, col = "red")
        }
      })

    }

     # TODO: observe the behaviour of the chunk below and perhaps remove QQ if bin model present (and do so likely using family info from mo)
     #if (show_base & !show_binned) { # noraml QQ use only for Gaussian
     if (show_base & !is_bin) { 
      panels_add(function() {  
         qqnorm(res, main=list("Normal Q-Q Plot: residuals", cex=0.8),col=colour_pt);qqline(res, col = 'red')
        })
    }

  ### offset ###
    if (offset) {
      off <- tryCatch(
        stats::model.offset(stats::model.frame(mo)),
        error = function(e) NULL
      )

      if (!is.null(off)) {

        # (1) Residuals vs offset (scale used in model)
        panels_add(local({
          v  <- as.numeric(off)
          r0 <- res
          function() {
            scatter.smooth(v, r0,
              xlab = "Offset (linear predictor scale)",
              ylab = "Pearson residuals",
              col  = colour_pt,
              main = "Residuals vs offset"
            )
            abline(h = 0, lty = 2, col = "red")
          }
        }))

        # (2) Leverage vs offset (if hatvalues exist); interpretation: points with high hatvalues (>2*#_of_parameters/n or >3*#_of_parameters/n) and extreme offset → those combinations can dominate estimation; if plot shows hatvalues (e.g. below 1) in a modest range and no crazy outliers, you’re fine. If most points are clustered and only a few are clearly higher, those few are the ones to inspect (especially if they also have extreme residuals or offset).

        hv <- tryCatch(stats::hatvalues(mo), error = function(e) NULL)
        if (!is.null(hv)) {
          panels_add(local({
            h <- hv
            v <- as.numeric(off)
            function() {
              plot(h, v,
                xlab = "Leverage (hatvalues)",
                ylab = "Offset (linear predictor scale)",
                main = "Leverage vs offset", col = colour_pt
              )
            }
          }))
        }
      }
    }
  
  ### random effects ###
    if (show_reqq) plot_re_qq(mo, which="all")
   
  ### fixed effects ###
   #### continuous
    if (any(!is.na(cont) & nzchar(trimws(cont)))) {
      cont  <- trimws(cont)
      trans <- tolower(if (length(trans)) rep_len(trans, length(cont)) else rep("none", length(cont)))

      for (i in seq_along(cont)) {
        vname <- cont[i]; if (!vname %in% names(dat)) next
        if (!is.numeric(dat[[vname]])) next

        v  <- dat[[vname]]
        tr <- trans[i]; lab <- vname
        
        if (tr %in% c("log","ln")) { v[v <= 0] <- NA; lab <- if (tr=="log") paste0("log10(",vname,")") else paste0("ln(",vname,")"); v <- if (tr=="log") log10(v) else log(v) }
        if (tr == "abs") { v <- abs(v); lab <- paste0("abs(",vname,")") }
        if (tr == "sin") { v <- sin(v); lab <- paste0("sin(",vname,")") }
        if (tr == "cos") { v <- cos(v); lab <- paste0("cos(",vname,")") }
        if (all(is.na(v))) next  

        panels_add(local({
          vx <- v[idx_plot]; xl <- lab; vn <- vname; r0 <- res_s; 
          function() {
            plot(vx, r0,
                xlab = xl, ylab = "Pearson residuals",
                main = paste("Residuals vs", vn),
                pch  = 16, col  = colour_pt
              )#scatter.smooth(vx, r0, xlab=xl, ylab="Pearson residuals", col=colour_pt, main=paste("Residuals vs", vn))
            ok <- is.finite(v) & is.finite(res)
          if (sum(ok) > 1L) lines(lowess(v[ok], res[ok]), lwd = 1.2) # smoother on full data
            abline(h=0, lty=2, col ='red')
          }
        }))
      }
    }

   ### categorical ###
   if (any(!is.na(categ) & nzchar(trimws(categ)))) {
    for (cat_var in categ) {
      if (!cat_var %in% names(dat)) next
      panels_add(local({  
        cv <- cat_var
        function() { boxplot(res ~ dat[[cv]], border="grey40", ylab="Pearson residuals", xlab = cv, main=paste("Residuals by", cv)); abline(h=0, lty=2, col="red")}
      }))
    }
   }

  ### autocorrelations ###
   if(show_temporal){
     panels_add(function() {  
      acf(res, type="p", ylab = "Partial series residual (PACF)", main="Temporal autocorrelation")
     })
   }

   if (!is.null(show_temporal_grouped)) { # emulates grouping lightly (can reveal group-level leftover structure); if PACF shows decay, but the group-level PACF is flat → the apparent decay comes from pooled residuals across groups, not true within-group autocorrelation
    g  <- dat[[show_temporal_grouped]]
    ok <- !is.na(g) & !is.na(res)         
    by <- aggregate(res[ok], list(grouping = g[ok]), mean) #  mean residual per grouping variable
    nn <- aggregate(res[ok], list(grouping = g[ok]), length)  # sizes

    o  <- order(by$grouping)
    m  <- by$x[o]
    n  <- nn$x[o]
    z  <- m * sqrt(n / max(n)) # variance-stabilized means

    panels_add(local({
      zz <- z; yl <- paste("PACF of", show_temporal_grouped, "mean residuals (weighted)")
      function() { pacf(zz, main="Temporal autocorrelation across groups", ylab = yl)}
    }))
   }

   if(show_spatial && have(c(lon_var, lat_var))) {  

      spdata <- data.table(resid = res_s, x = dat[[lon_var]][idx_plot], y = dat[[lat_var]][idx_plot])
      spdata[ , col := ifelse(resid < 0, col_neg, col_pos)]
      cex_vals <- c(1, 1.5, 2, 2.5, 3)
      spdata[, cex := as.numeric(cut(abs(resid), 5, labels = cex_vals))]
      
      x  <- dat[[lon_var]]
      y  <- dat[[lat_var]]
      ok <- is.finite(x) & is.finite(y) & is.finite(res)
      coords_ok <- cbind(x[ok], y[ok])

      # tiny jitter for exact duplicates to satisfy knearneigh / variogram
      dup <- duplicated(coords_ok)
      if (any(dup)) {
        eps <- sqrt(.Machine$double.eps)
        n_dup <- sum(dup)
        coords_ok[dup, ] <- coords_ok[dup, ] +
          matrix(runif(2L * n_dup, -eps, eps), ncol = 2L)
      }

      panels_add(function() { 
       plot(spdata$x, spdata$y, col = spdata$col, cex = spdata$cex, pch = 16, main = "Spatial distribution of residuals", xlab = "longitude", ylab = "latitude")
       legend("topleft", pch=16, cex=0.8, legend=c('<0','>=0'), col=c(col_neg,col_pos))
      })

      panels_add(function() { 
       spdata_neg = spdata[resid<0]  
       plot(spdata_neg$x, spdata_neg$y, col = spdata_neg$col, cex = spdata_neg$cex, pch = 16, main = "Spatial distribution of residuals (<0)", xlab = "longitude", ylab = "latitude")
      })
      
      panels_add(function() {    
       spdata_pos = spdata[resid>=0]  
       plot(spdata_pos$x, spdata_pos$y,col=spdata_pos$col, cex=spdata_pos$cex, pch= 16, main=list('Spatial distribution of residuals (>=0)', cex=0.8), xlab = "longitude", ylab = "latitude")
      })

      panels_add(function() {# Moran's I summary panel
          plot.new()
          box()
          title("Spatial autocorrelation (Moran's I)")
          
          if (!requireNamespace("spdep", quietly = TRUE)) {
            text(0.05, 0.8, adj = 0,
                 "Package 'spdep' not available.\nSkipping Moran's I.")
            return(invisible())
          }

          if (sum(ok) < 10L) {
            text(0.05, 0.8, adj = 0, 
              "Too few valid points for Moran's I.") 
            return(invisible())
          }
          
          # k-NN weights; robust to irregular sampling
          mm <- tryCatch({
            nb <- spdep::knearneigh(coords_ok, k = 4) |>
                  spdep::knn2nb()
            lw <- spdep::nb2listw(nb, style = "W")
            spdep::moran.test(res[ok], lw, na.action = na.exclude)
          }, error = function(e) NULL)
          
          if (is.null(mm)) {
            text(0.05, 0.8, adj = 0,
                "Moran's I failed (neighbors).\nCheck coordinates.")
            return(invisible())
          }
          
          Ival <- unname(mm$estimate[["Moran I statistic"]])
          pval <- mm$p.value
          
          text(0.05, 0.8, adj = 0, labels = sprintf("Moran's I: %.3f", Ival))
          text(0.05, 0.7, adj = 0, labels = sprintf("p-value:   %.3g", pval))
          text(0.05, 0.5, adj = 0, labels = "I ≈ 0 & ns:\nno strong global spatial autocorrelation")
          text(0.05, 0.3, adj = 0, labels = "|I| large & sig.:\n consider spatial structure")
      })
         
      panels_add(function() { ## Residual semivariogram
          if (!requireNamespace("gstat", quietly = TRUE) ||
              !requireNamespace("sp", quietly = TRUE)) {
            plot.new()
            box()
            title("Residual semivariogram")
            text(0.05, 0.8, adj = 0,
                 "Need 'gstat' + 'sp'.\nSkipping semivariogram.")
            return(invisible())
          }
          
          if (sum(ok) < 20L) {
            plot.new()
            box()
            title("Residual semivariogram")
            text(0.05, 0.8, adj = 0,
                 "Too few valid points\nfor stable semivariogram.")
            return(invisible())
          }
          
          sdat <- data.frame(lon = coords_ok[,1], lat = coords_ok[,2], res = res[ok])
          sp::coordinates(sdat) <- ~ lon + lat
          
          plot(gstat::variogram(res ~ 1, sdat), main = "Residual semivariogram", col = colour_pt, cex = 16)
      })
  
    }


  ### Pearson dispersion text panel ###
  # note: for standard lm/Gaussian GLM with estimated σ², the Pearson dispersion is essentially the same as the residual variance estimate, so ≈1 by construction
  disp <- NA_real_

   df_resid <- tryCatch(stats::df.residual(mo), error = function(e) NA_real_) 
   if (!is.null(res) &&
      is.finite(df_resid) &&
      df_resid > 0) {
    disp <- sum(res^2, na.rm = TRUE) / df_resid
   }

   if (!is.na(disp)) {
    panels_add(local({
      d   <- disp
      function() {
        plot.new()
        box()
        title("Dispersion summary")
        usr <- par("usr")  # x1 x2 y1 y2
        x0  <- usr[1] + 0.06 * (usr[2] - usr[1])
        y0  <- 0.80
        text(x0, y0, adj = 0, labels = paste0("Family: ", fam_txt))
        text(x0, y0 - 0.20, adj = 0,
             labels = paste0("Pearson dispersion: ", signif(d, 3)))
        text(x0, y0 - 0.30, adj = 0, labels = "\u22481: ok")
        text(x0, y0 - 0.35, adj = 0, labels = ">>1 (e.g. > 5–10): overdispersion") # means remaining overdispersion, suggesting missing structure or misfit even after modeling NB
        text(x0, y0 - 0.40, adj = 0, labels = "<<1: underdispersion")         
      }
    }))
  }
  
  ### title ###
     if (!accumulate) {
      mc <- tryCatch({if (isS4(mo)) slot(mo, "call") else mo$call},
        error = function(e) mo$call
      )
      
      link_txt <- tryCatch(family(mo)$link,    error = \(e) NA_character_)
      disp_txt <- tryCatch({cl <- getCall(mo)
        if (!is.null(cl$dispformula)) {
          paste("dispersion:", paste(deparse(cl$dispformula), collapse = " "))
        } else { NULL}
      }, error = function(e) NULL)
      
      off_txt <- if (offset) {
        tryCatch({
          mf  <- stats::model.frame(mo)
          off <- stats::model.offset(mf)
          if (!is.null(off)) {
            cl <- getCall(mo)
            off_expr <- NULL
            if (!is.null(cl$formula)) {
              f_chr <- paste(deparse(cl$formula), collapse = " ")
              if (grepl("offset\\(", f_chr)) {
                off_expr <- sub(".*offset\\(([^)]*)\\).*", "\\1", f_chr)
              }
            }
            paste0("offset: ", if (!is.null(off_expr)) paste0(off_expr,')') else "present")
          } else { NULL}
        }, error = function(e) NULL)
      } else {NULL}

      main_txt <- tryCatch(
       paste0(
        name, " model check:\n",
        deparse(mc[[1L]]),
        "(", paste(deparse(mc[[2L]]), collapse = " "), ")\n", 
        if (!is.na(fam_txt)) paste0('family: ', fam_txt) else "", 
        if (!is.na(link_txt)) sprintf(" (%s link)", link_txt) else "", 
        if (!is.null(off_txt)) paste0("; ", off_txt) else "", 
        if (!is.null(disp_txt)) paste0("; ", disp_txt) else ""
       ),
       error = function(e)  paste0(name, " model check")
      )

      if (wrap_title) {
        title_text <- strwrap(main_txt, width = wrap_width) 
        mtext(paste(title_text, collapse = "\n"), side = 3, line = 1, cex = 0.5, outer = TRUE, col = 'red2')
        } else {
          mtext(main_txt, side = 3, line = 1, cex = 0.5, outer = TRUE, col = 'red2')
        }
      if (PNG) dev.off()
     } # option for accumulate not prepared
}
# END