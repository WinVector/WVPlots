

# WVPlots 1.3.5 2023-08-09

 * Fix false-positive CRAN checks.
 * Set data.table threads to 1 in all examples and tests.
 * Cut down number of examples active.

# WVPlots 1.3.4 2023-08-08

 * Fix docs.
 * Remove smoother from perf graphs.
 * ggplot2 API changes.

# WVPlots 1.3.3 2023-04-14

 * Documentation fixes.
 * Work away from aes_string().

# WVPlots 1.3.2 2021-01-04

 * Fix error msg on ThresholdPlot
 * Add accuracy as a ThresholdPlot metric.
 * Point controls in ConditionalSmoothedScatterPlot.

# WVPlots 1.3.1 2020-10-17

 * More control on ideal ROC curve plots.
 * Convex hull on ROC plot.
 * Move to tinytest.

# WVPlots 1.3.0 2020-10-03

 * Fix ROC smoothing doc.
 * Add more paramteric ROC fitters.
 * Move back to sigr for ROC fitting.
 * More curves on lift plot.
 * Add some aliases.

# WVPlots 1.2.9 2020-09-19

 * Add ideal curve to ROC plot, see: https://win-vector.com/2020/09/13/why-working-with-auc-is-more-powerful-than-one-might-think/
 * Add ROCPairList() as prefered name for  ROCPlotPairList().
 * More gain curve annotation controls.

# WVPlots 1.2.8 2020-09-03

 * Add ROCPlotPairList().

# WVPlots 1.2.7 2020-08-24

 * Fix ThresholdPlot boundary conditions and remove non-zero/one metrics.

# WVPlots 1.2.6 2020-08-11

 * Add performance plots from wvpy https://github.com/WinVector/wvpy 
 * Improve documentation.
 * Change to https where possible.
 * Deal with aliased documentation links.

# WVPlots 1.2.5 2020-05-01

 * Work around ggplot2 to deprecating fun.y, fun.ymin, fun.ymax.
 * Work around ggplot2 variations in label data structure and NA labels.
 * Fix GainCurvePlotC color mapping issue.
 * Make more arguments to geom_smooth explicit to avoid default messages.

# WVPlots 1.2.4 2020-02-19

 * Fix character/factor issue (upcoming in R4.0.0).
 * Add rmarkdown suggest.
 * Badges.
 
# WVPlots 1.2.3 2020-01-18

 * Fix gini report.

# WVPlots 1.2.2 2019-11-03

 * Fix typo in loglog plot (and remove sig presentation).

# WVPlots 1.2.1 2019-10-12

 * Fix PlotDistCountBinomial.
 * More plots, adjust plots.
 
# WVPlots 1.2.0 2019-10-03

 * Bring in rqdatatable for data aggregation tasks.
 
# WVPlots 1.1.2 2019-09-10

 * Allowed palette controls on more plots.

# WVPlots 1.1.1 2019-07-24

 * Adjust license.

# WVPlots 1.1.0 2019-04-07

 * Fix no-wizard option in lift plot.

# WVPlots 1.0.9 2019-02-20

 * Switch to RUnit.

# WVPlots 1.0.8 2019-01-26

 * Add baseline in lift curve plot.
 * Truth targets.

# WVPlots 1.0.7 2018-12-17

 * Add PRTPlot.
 
# WVPlots 1.0.6 2018-11-06

 * Add LiftCurvePlot
 
# WVPlots 1.0.5 2018-10-22

 * Add PairPlot

# WVPlots 1.0.4 2018-09-17

 * Documentation improvements.

# WVPlots 1.0.3 2018-09-17

 * Start removing frame narrowing effect where not needed.
 * ShadedDensity more controls on annotation and text.
 * Documentation improvements.
 
# WVPlots 1.0.2 2018-07-20

 * Documentation fixes.
 
# WVPlots 1.0.1 2018-07-11

 * narrow interface to sigr.
 * Documentation fixes.

# WVPlots 1.0.0 2018-05-24

 * Added hexbin plot.
 * Prepare for next ggplot2 version.
 * Added shadow plot.
 * Improve argument checking.
 * Add frame narrowing.
 * Make significance tests user selectable.
 * Clean up F-test in ScatterHist.
 
# WVPlots 0.3.2 2018-05-07

 * Remove RSQLite dependence.
 * Report F-test on linear fit in more cases in ScatterHist.
 
# WVPlots 0.3.1 2018-04-03

 * Remove R version dependence.
 * Speed up gain curve.
 * Make heavy weight packages suggested.

# WVPlots 0.3.0 2018-03-20

 * Make dependencies more flexible.
 * Remove replyr dependency.
 * Stop re-exporting wrapr functions.

# WVPlots 0.2.9 2018-03-13

 * Add LogLogPlot.
 * Lower dependencies.

# WVPlots 0.2.8 2018-01-20

 * Add new performance plot.
 * Switch to new cdata signatures.

# WVPlots 0.2.7 2017-11-28

 * Fix documentation fixes.
 * Switch from reshape2 to cdata.
 * Move to wrapr 1.0.2.
 
# WVPlots 0.2.6 2017-08-28

 * Fix plotly labels.

# WVPlots 0.2.5 2017-07-04

 * Minor documentation fixes.

# WVPlots 0.2.4 2017-05-05

 * More argument checking.
 * NA guard on ScatterHist.
 * plotly ROC.
 
# WVPlots 0.2.3 2017-03-24

 * Fix right-plot alignment in ScatterHist
 * Expose ROC data structures

# WVPlots 0.2.2 2017-02-17

 * Move debug to wrapr https://github.com/WinVector/wrapr

# WVPlots 0.2.1 2017-01-17

 * Fix warnings.
 * Improve Imports/Depends/Suggests.
 * Fix smoothing bug in BinaryYScatterPlot.
 
# WVPlots 0.2.0 2017-01-15

 * First CRAN submission.

