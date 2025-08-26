require(rmarkdown)

rmarkdown::render('R/rep_SI.R', output_dir = 'Output',  output_file = 'MS_test_2.html') #, quiet = TRUE)

rmarkdown::render('R/test.R', output_dir = 'Output',  output_file = 'test.html')#, quiet = TRUE)

