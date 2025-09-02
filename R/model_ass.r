# packages
require('DHARMa') 
# function
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
      spdata[ , col := ifelse(resid < 0, rgb(83, 95, 124, 100, maxColorValue = 255),
                          rgb(253, 184, 19, 100, maxColorValue = 255))
            ]
      cex_vals <- c(1, 1.5, 2, 2.5, 3)
      spdata[, cex := as.numeric(cut(abs(resid), 5, labels = cex_vals))]
      
      plot(spdata$x, spdata$y, col = spdata$col, cex = spdata$cex, pch = 16, main = "Spatial distribution of residuals", xlab = "longitude", ylab = "latitude")
      legend("topleft", pch=16, cex=0.8, legend=c('<0','>=0'), col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)))

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

# further checks
simulateResiduals(ma) |> testDispersion()
simulateResiduals(mbs1) |> testDispersion(); testSpatial();      