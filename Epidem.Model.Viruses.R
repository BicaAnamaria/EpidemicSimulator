######################
###
### Epidemic Simulator - Bachelor Thesis
### Student: Anamaria Bica
### West University, Timisoara
### Year 3, Faculty of Computer Science
###
### Coordinator: Daniela Zaharie
### Supervisor: Leonard Mada
### Syonic SRL

### parts of the project are based
### on a team project 2021
### (see comments in Epidem.app.R)

#######################
### 2 Viruses Model ###
#######################

### Options for filter controller
getDisplayTypes2Viruses = function(){
  c("All", "Compact", "V1", "V2")
}

### Options for sensitivity analysis controller
getSensitivity2Viruses = function() {
  c("2 Viruses Model" = "2V", 
    "Infection rate (V1)" = "infectV1",
    "Infection rate (V2)" = "infectV2",
    "Hosp rate (V1)" = "hospV1", "Hosp rate (V2)" = "hospV2",
    "Death rate (V1)" = "deathV1", "Death rate (V2)" = "deathV2",
    "Death rate hosp(V1)" = "deathV1.h", "Death rate hosp(V2)" = "deathV2.h"
  );
}

### Differential ecuations for 2V model
sir2Viruses <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {

    dI1 = S * infect.v1 * (IV1 + HV1);
    dI2 = S * infect.v2 * (IV2 + HV2);
    
    # Susceptible
    dS = - dI1 - dI2;
    # Infected virus 1/virus 2
    dIV1 = dI1 - (death.v1 + hosp.v1 + recov.v1) * IV1;
    dIV2 = dI2 - (death.v2 + hosp.v2 + recov.v2) * IV2;
    # Hospitalised virus 1/virus 2
    dHV1 = hosp.v1 * IV1 - recov.hv1 * HV1 - death.hv1 * HV1;
    dHV2 = hosp.v2 * IV2 - recov.hv2 * HV2 - death.hv2 * HV2;
    # Hospitalised cumulated
    dHcum = hosp.v1 * IV1 + hosp.v2 * IV2;
    # Death virus 1/virus 2
    dDV1 = death.v1 * IV1 + death.hv1 * HV1;
    dDV2 = death.v2 * IV2 + death.hv2 * HV2;
    # Recover virus 1/virus 2
    dRV1 = recov.v1 * IV1 + recov.hv1 * HV1;
    dRV2 = recov.v2 * IV2 + recov.hv2 * HV2;
    
    # list with all ecuations for every compartment
    return(list(c(dS, dIV1, dIV2, dHcum, dHV1, dHV2, dDV1, dDV2, dRV1, dRV2)));
  })
}

### Function for initializing:
# - parameters (got from the user/test parameters already declared in the app)
# - compartments
initSIR_2Viruses = function(param, end.time, options)
{
  parameters = list(infect.v1 = param$infectV1,
                    infect.v2 = param$infectV2,
                    hosp.v1 = param$hospV1,
                    hosp.v2 = param$hospV2,
                    recov.v1 = param$recovV1 * (1 - param$deathV1),
                    recov.v2 = param$recovV2 * (1 - param$deathV2),
                    death.v1 = param$recovV1 * param$deathV1,
                    death.v2 = param$recovV2 * param$deathV2,
                    recov.hv1 = param$recovV1.h * (1 - param$deathV1.h),
                    recov.hv2 = param$recovV2.h * (1 - param$deathV2.h),
                    death.hv1 = param$recovV1.h * param$deathV1.h,
                    death.hv2 = param$recovV2.h * param$deathV2.h)
  # initialise the compartments
  init = c(S = (1 - 1e-6), IV1 = 1e-6 , IV2 = 0.0, 
           Hcum = 0.0, HV1 = 0.0, HV2 = 0.0, 
           DV1 = 0.0, DV2 = 0.0, RV1 = 0.0, RV2 = 0.0)
  
  # Period 1:
  # set also: initi$IV2 = 0;
  delayV2 = param$delayV2
  times1 = seq(0, delayV2);
  out1 = solve.sir(sir2Viruses, init, parameters, times1);
  # Period 2:
  times2 = seq(delayV2, end.time);
  idIV1 = which(names(init) == 'IV1')
  idIV2 = which(names(init) == 'IV2')
  len = nrow(out1);
  init = unlist(out1[len, -1]);
  init[idIV2] = init[idIV1] * options$opt.2V.permutation;
  init[idIV1] = init[idIV1] - init[idIV2];
  out2 = solve.sir(sir2Viruses, init, parameters, times2);
  # Merge results
  out1 = out1[ - len, ];
  out = rbind(out1, out2);
  # out = solve.sir(sir2Viruses, init, parameters, times)
  attr(out, "Model") = "2 Viruses";
  return(out);
}

### Function for plotting 
plotSIR_2Viruses = function(out, flt="V1", add = FALSE, plot.legend = TRUE, ...) {
  
  # legend labels
  lbl = c( "Susceptible", "Infected (Virus1)", "Infected (Virus2)", "Hosp (cumulative)", 
           "Hosp (Virus1)", "Hosp (Virus2)", "Death (Virus1)", "Death (Virus2)",
           "Recovered (Virus1)", "Recovered (Virus2)") ;
  # legend position
  leg.xy = c(0.0, 0.9)
  
  # controller for filtering
  type = match(flt, getDisplayTypes2Viruses());
  
  # type1 = All; type2 = Compact
  # type3 = virus1; type4 = virus2
  if(type > 1) {
    
    if(type == 2) {
      r = filter.out(out, c("S"), lbl);
      leg.xy = c(0.0, max(out$Hcum, out$RV1, out$RV2, na.rm=TRUE) * 0.8)
    } else if(type == 3) {
      r = filter.out(out, c("S", "IV2", "HV2", "DV2", "RV2"), lbl);
      leg.xy = c(0.0, max(out$Hcum, out$RV1, out$RV2, na.rm=TRUE) * 0.8)
    } 
    else if(type == 4){
      r = filter.out(out, c("S", "IV1", "HV1", "DV1", "RV1"), lbl);
      leg.xy = c(0.0, max(out$Hcum, out$RV1, out$RV2, na.rm=TRUE) * 0.8)
    }
    
    out = r$out; lbl = r$lbl;
  }
  
  # plotting the grafic
  plot.sir(out, legend.lbl = lbl, legend.xy = leg.xy, title = "SIR 2 Virus Model", 
           add = add, plot.legend = plot.legend, ...)
}

### Sensivity Analysis
Sensitivity_2Viruses = function(paramName, parameters, end.time, min=0, max=1, flt = "V1", options) {
  by = (max - min)/20;
  # for this interval, run the simulation and plot the result
  for(p in seq(min, max, by = by)) {
    parameters[[paramName]] = p;
    
    out = initSIR_2Viruses(parameters, end.time, options);
    
    plotSIR_2Viruses(out, flt = flt, add = if(p == min) FALSE else TRUE,
                    plot.legend = FALSE, lty = options$sensitivity.lty);
  }
  
  parameters[[paramName]] = min;
  
  out = initSIR_2Viruses(parameters, end.time, options);
  
  plotSIR_2Viruses(out, flt = flt,
                  add = TRUE, plot.legend = TRUE,
                  lty = 1);
}