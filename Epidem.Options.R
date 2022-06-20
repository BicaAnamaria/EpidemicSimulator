getOptions = function() {
  opt = list(
    ### Global Options - Models
    delay.vacc = 60,
    delay.2V = 160,
    p.old = 0.2, # 0.2 = 20% of the population
    p.children = 0.2,
    death.rate.scale = 24,
    hosp.rate.scale = 12,
    sensitivity.lty = 4,
    opt.2V.permutation = 0.1, # 10% of V1 will mutate to V2;

    ### Global options - Analysis
    stat.max.cutoff = 0.8, # 80% of maximum value 
    population.size = 1E+6,

    # Sensitivity Analysis
    sensitivity.infect.min = 0.75,
    sensitivity.infect.max = 1.25)
}

# Temporary:
opt0 = getOptions()
