# test figure on  sampling density distribution

# A. Facet by HOLC grade (or city) → Quickly shows that a few polygons have huge densities, mostly in A.

ggplot(d, aes(x = sampling_density)) + geom_density() + facet_wrap(~holc_grade, ncol = 1) + 
  scale_x_continuous(trans = 'log')


B. Show disparity curves with and without the extreme tail

For example remove the top 0.5% or 1% of polygon-level sampling densities.

B. Show disparity curves with and without the extreme tail

For example remove the top 0.5% or 1% of polygon-level sampling densities.

Your figure will show:

The coverage disparity barely changes

The intensity disparity collapses significantly

The total disparity line becomes much flatter

The authors’ “35.6% increase” disappears

This is very strong evidence that their claim is driven by a statistical artefact — a small number of hotspots.