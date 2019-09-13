#
# Examples
#
set.seed(23590)
class_size = 35
nclasses = 100
true_frate = 0.4

fdata = data.frame(n_female = rbinom(nclasses, class_size, true_frate))

# compare to empirical p
PlotDistCountBinomial(fdata, "n_female", class_size, "Distribution of count of female students, class size = 50")

# compare to theoretical p of 0.5
PlotDistCountBinomial(fdata, "n_female", class_size, "Distribution of count of female students, class size = 50",
                      p = 0.5)

fdata2 = rbind(data.frame(n_female = rbinom(50, class_size, 0.25)),
               data.frame(n_female = rbinom(10, class_size, 0.60)) )
PlotDistCountBinomial(fdata2, "n_female", class_size, "Two populations, class size = 50")


# differing trial sizes
set.seed(52523)
N = 100
pgray = 0.1  # rate of gray horses in the population
herd_size = round(runif(N, min=25, 50))
ngray = rbinom(N, herd_size, pgray)
hdata = data.frame(n_gray=ngray, herd_size=herd_size)

# observed rate of gray horses in each herd
hdata$rate_gray = with(hdata, ngray/herd_size)

PlotDistDensityBeta(hdata, "rate_gray",
                    "Estimated prevalence of gray horses in population") +
  ggplot2::geom_vline(xintercept = pgray, linetype=4, color="maroon") +
  ggplot2::annotate("text", x=pgray+0.01, y=0.01, hjust="left",
           label = paste("True prevalence =", pgray))

PlotDistHistBeta(hdata, "rate_gray",
                    "Estimated prevalence of gray horses in population") +
  ggplot2::geom_vline(xintercept = pgray, linetype=4, color="maroon") +
  ggplot2::annotate("text", x=pgray+0.01, y=0.01, hjust="left",
                    label = paste("True prevalence =", pgray))
