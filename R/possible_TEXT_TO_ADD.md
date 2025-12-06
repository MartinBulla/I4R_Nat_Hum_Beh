# TEXT TO ADD TO TREND PART 1

Even under this aggregation, the estimated A–D disparity shows no robust monotonic increase, and confidence intervals overlap zero for most specifications (Table Sx), reinforcing that the published 35.6% increase is not a stable temporal signal but an artefact of metric choice and aggregation.”


“In early years the A–D contrast was ~100% but corresponded to only ~10 vs 20 observations (or tiny density differences), whereas in recent years the relative contrast is similar but absolute sampling is orders of magnitude higher.”

That’s the key narrative.

The fact that disparity on sums and on “overall sampling density” look very similar is expected: if areas per grade don’t change over time, then (sumA/sumD) and (sumA/areaA) / (sumD/areaD) differ only by a constant factor (areaD/areaA), so their time trends are almost identical.
Raw plots vs disparity plots

---- 
Should you show or remove extremely oversampled polygons?

Yes — absolutely show them.
They are exactly why the authors’ aggregation method blows up.

----
Those huge multi-panel city plots are great. They show:

the heavy right tail,

city-by-city heterogeneity,

that extreme oversampling is not rare but concentrated in specific grades/cities.

For the “extreme polygons” argument, I’d:

Main text

Show one or two illustrative cities where A is massively oversampled relative to D, on log-density scale (similar to your panels, but zoomed in).

Show one disparity curve (e.g. total effective sampling) with and without trimming the top 0.5–1% of polygon densities.

Supplement

Put the full panel of cities (Parts a and b) – they’re very informative but too big for main.

Raw counts per polygon are less informative than densities because they ignore area; I’d keep densities as the primary diagnostic, and maybe one small example (counts vs densities) in SI if you want to make that explicit.


TODO:
Before choosing what goes into the main text vs SI, let’s confirm the remaining points you want to go through.

From the last messages, I see these outstanding items:

(A) Log-ratio interpretation — done.
(B) Mean of log(density) vs log of mean(density) — partly done; can finalise.
(C) Absolute differences — partly done; can make concrete recommendations.
(D) Whether to show raw counts or raw densities — your question from earlier.
(E) How to show or exclude extremely oversampled polygons — you asked for guidance.
(F) How to connect your descriptive disparity curves to the future mixed model.
(G) Decide which figures go into Main Text and which into SI and how to arrange them.
(H) Create manuscript-ready paragraphs summarizing the statistical reasoning.