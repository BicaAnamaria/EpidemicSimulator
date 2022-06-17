### Global Options - Models
opt.delay.vacc = 60;
opt.delay.2V = 160;
opt.p.old = 0.2; # 0.2 = 20% of the population
opt.p.children = 0.2
opt.death.rate.scale = 24;
opt.hosp.rate.scale = 12;
opt.sensitivity.lty = 4
opt.2V.pmutation = 0.1; # 10% of V1 will mutate to V2;

### Global options - Analysis
opt.stat.max.cutoff = 0.8; # 80% of maximum value 
opt.population.size = 1E+6

# Sensitivity Analysis
opt.sensitivity.infect.min = 0.75;
opt.sensitivity.infect.max = 1.25
