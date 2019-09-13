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
