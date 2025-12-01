require(rmarkdown)

rmarkdown::render('R/rep_SI_v2.R', output_dir = 'Output', output_file = 'SI_v3.html') #, quiet = TRUE)

rmarkdown::render('R/test.R', output_dir = 'Output',  output_file = 'test.html')#, quiet = TRUE)

