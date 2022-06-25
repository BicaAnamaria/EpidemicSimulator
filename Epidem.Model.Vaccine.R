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


###################
### Vaccination ###
###################

# this whole section is from SIR_Vacc_OlderGroup from TEAM
# - preferential vaccination in older individuals:
#   model with old age group;

### Sensitivity Analysis
getSensitivityVacc = function() {
  c("Vaccination Model" = "Vacc", 
    "Infection rate " = "infect",
    "Hosp rate (Young)" = "hosp.y", "Hosp rate (Old)" = "hosp.o",
    "Death rate (Young)" = "death.y", "Death rate (Old)" = "death.o",
    "Death rate (Hosp)" = "death.h",
    "Vaccination rate (Young)" = "vacc.y", "Vaccination rate (Old)" = "vacc.o"
  );
}

getDisplayTypesVacc = function(){
  c("All", "Young", "Old", "Old + Iy", "Totals")
}

sirVacc <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    
    if(time < delay.vacc){
      dVy = 0; 
      dVo = 0;
    } 
    else 
    {
      dVy = min(Sy, vacc.y);
      dVo = min(So, vacc.o);
    }
    dSy = -infect * Sy * Iy - infect * Sy * Io - infect * Sy * H - dVy; # both I & H infect! S = young
    dSo = -infect * So * Iy - infect * So * Io - infect * So * H - dVo;
    dT = (-infect * Sy * Iy - infect * Sy * Io - infect * Sy * H) + (-infect * So * Iy - infect * So * Io - infect * So * H) - dVy -dVo;
    dIy = infect * Sy * (Iy + Io + H) - (death.y + hosp.y + recov) * Iy;
    dIo = infect * So * (Iy + Io + H) - (death.o + hosp.o + recov) * Io;
    #dI =  infect * S * I + infect * S * H + infect * O * I + infect * O * H - recov * I - death.y * I - hosp * I;
    dD =  death.h * H + death.y * Iy + death.o * Io;
    dH =  hosp.y * Iy + hosp.o * Io - recov.h * H - death.h * H;
    dR =  recov * Iy + recov * Io + recov.h * H;
    dHcum = hosp.y * Iy + hosp.o * Io;

    return(list(c(dT, dSy, dSo, dIy, dIo, dHcum, dH, dD, dR, dVy, dVo)));
  })
}


initSIR_Vaccine = function(param, end.time, options) 
{
  
  times = seq(0, end.time, by = 1)
  # (!) S represents the nr of susceptible young people
  parameters = list(infect = param$infect, 
                    recov = param$recov.c, 
                    recov.h = (1 -param$death.h) * param$recov.h, 
                    death.y = param$recov.c * param$death.y,
                    death.o = param$recov.c * param$death.o,
                    death.h = param$death.h * param$recov.h, 
                    hosp.y = param$hosp.y,             
                    hosp.o = param$hosp.o, 
                    vacc.y = param$vacc.y,     
                    vacc.o = param$vacc.o,
                    delay.vacc = options$delay.vacc)
  
  init = c(T = 1 - 1e-6, Sy = (1 - 1e-6) * (1 - options$p.old), So = (1 - 1e-6) * options$p.old, 
           Iy = 1e-6 * (1 - options$p.old), Io = 1e-6 * options$p.old, 
           Hcum = 0.0, H = 0.0, D = 0.0, R = 0.0, Vy =0.0, Vo = 0.0)
  
  ### Solve using ode
  out = solve.sir(sirVacc, init, parameters, times)
  attr(out, "Model") = "Vaccination";
  return(out);
}


plotSIR_Vaccine = function(out, flt = "Old", options, add = FALSE, plot.legend = TRUE, ...) {
  
  lbl = c("Total", "Young", "Old", "Infected (Young)", "Infected (Old)", "Hosp (cumulative)", "Hosp", 
          "Death", "Recovered", "Vaccinated (Young)", "Vaccinated (Old)");
  #leg.off=c(-0.0, 0.3);
  leg.xy = c(0.7 * max(out$time), 0.75)
  ncol = 2
  
  type = match(flt, getDisplayTypesVacc());
  if(type > 1) {
    out$DeathRate = c(out$D[1], diff(out$D, lag = 1)) * options$death.rate.scale; 
    out$HospRate = c(0, diff(out$Hcum)) * options$hosp.rate.scale;
    lbl = c(lbl, paste0("Death Rate [scale = x", options$death.rate.scale, "]"),
            paste0("Hosp Rate  [scale = x", options$hosp.rate.scale, "]") );
    ncol = 1
    if(type == 1) {
      r = filter.out(out, c("T", "Hcum"), lbl);
      leg.xy = c(0, out$Sy[1])
    } else if(type == 2) {
      r = filter.out(out, c("T", "Hcum", "So", "Io", "Vo"), lbl);
      leg.xy = c(0, (1 - options$p.old) * 0.8)
    } else if(type == 3) {
      r = filter.out(out, c("T", "Hcum", "Sy", "R", "Vy", "Iy"), lbl);
      leg.xy = c(0.7 * max(out$time), options$p.old * 0.8)
    } else if(type == 4) {
      # R: can filter, as it does NOT convey any additional information;
      # Iy: keep as a reference;
      r = filter.out(out, c("T", "Hcum", "Sy", "R", "Vy"), lbl);
      leg.xy = c(0.7 * max(out$time), max(options$p.old, out$Iy) * 0.8)
    } 
    else if(type == 5){
      out$T = out$Sy + out$So;
      out$I = out$Iy + out$Io;
      out$V = out$Vy + out$Vo;
      lbl = c(lbl, "Infected (Total)", "Vaccinated (Total)")
      r = filter.out(out, c("Iy", "Io", "Vo", "Vy"), lbl);
      leg.xy = c(0.7 * max(out$time), 0.75)
      ncol = 2
    }
    
    out = r$out; lbl = r$lbl;
  }
  plot.sir(out, legend.lbl = lbl, legend.xy=leg.xy, ncol = ncol,
           title = "SIR Vaccination Model", add = add, plot.legend = plot.legend, ...)
}

### Sensitivity Analysis
Sensitivity_Vaccine = function(param, opt, end.time, min=0, max=1, options, flt = "Old") {
  by = (max - min)/20;
  for(p in seq(min, max, by = by)) {
    opt[[param]] = p;
    
    out = initSIR_Vaccine(opt, end.time);
    
    plotSIR_Vaccine(out, flt = flt, add = if(p == min) FALSE else TRUE,
                    plot.legend = FALSE, lty = options$sensitivity.lty);
  }
  
  opt[[param]] = min;
  
  out = initSIR_Vaccine(opt, end.time);
  
  plotSIR_Vaccine(out, flt = flt,
                  add = TRUE, plot.legend = TRUE,
                  lty = 1);
}

##############################
### Vaccination Stratified ###
##############################

getSensitivityVaccStrat = function() {
  c("Vaccination Stratified Model" = "VaccStrat", 
    "Infection rate " = "infect",
    "Hospitalization rate (Young)" = "hosp.y", "Hospitalization rate (Old)" = "hosp.o", 
    "Death rate (Young)" = "death.y", "Death rate (Old)" = "death.o", 
    "Recovery rate (Young)" = "recov.y", "Recovery rate (Old)" = "recov.o",
    "Vaccination rate (Young)" = "vacc.y", "Vaccination rate (Old)" = "vacc.o"
  );
}

getDisplayTypesVaccStrat = function(){
  c("All", "Young", "Old", "Totals")
}

sirVaccStrat <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    
    if(time < delay.vacc){
      dVy = 0;
    } else  dVy = min(Sy, vacc.y); # variable origin: state
    
    if(time < delay.vacc){
      dVo = 0;
    } else  dVo = min(So, vacc.o);
    
    dSy = -infect * Sy * ( Iy + Io + Hy + Ho) - dVy;
    dSo = -infect * So * ( Iy + Io + Hy + Ho) - dVo;
    dIy = infect * Sy * (Iy + Io + Hy + Ho) - (death.y + hosp.y + recov.y) * Iy;
    dIo = infect * So * (Iy + Io + Hy + Ho) - (death.o + hosp.o + recov.o) * Io;
    dHy = hosp.y * Iy - recov.y * Hy - death.hy * Hy;
    dHo = hosp.o * Io - recov.o * Ho - death.ho * Ho;
    dDy = death.hy * Hy + death.y * Iy;
    dDo = death.ho * Ho + death.o * Io;
    dR = recov.y * Iy + recov.o * Io + recov.y * Hy + recov.o * Ho;
    dHcum = hosp.y * Iy + hosp.o * Io;
    dT = dSy + dSo;
    
    return(list(c(dT, dSy, dSo, dIy, dIo, dHcum, dHy, dHo, dDy, dDo, dR, dVy, dVo)))
    
  }
  )}

initSIR_VaccineStrat = function(param, end.time, options)
{
  
  times = seq(0, end.time, by = 1)
  
  parameters = list(infect = param$infect,
                    recov.hy = param$recov.h * (1 - param$death.hy),
                    recov.ho = param$recov.h * (1 - param$death.ho),
                    recov.y = param$recov.y,
                    recov.o = param$recov.o,
                    death.y = param$death.y, 
                    death.o = param$death.o,
                    death.hy = param$recov.h * param$death.hy,
                    death.ho = param$recov.h * param$death.ho,
                    hosp.y   = param$hosp.y,
                    hosp.o = param$hosp.o,
                    vacc.y = param$vacc.y,     
                    vacc.o = param$vacc.o,
                    delay.vacc = options$delay.vacc)
  
  init = c(T = 1, Sy = (1 - 1e-6) * (1 - options$p.old), So = (1 - 1e-6) * options$p.old,
           Iy = 1e-6 * (1 - options$p.old), Io = 1e-6 * options$p.old, Hcum = 0.0, Hy = 0.0, 
           Ho = 0.0, Dy = 0.0, Do = 0.0, R = 0.0, Vy =0.0, Vo = 0.0)
  
  
  ### Solve using ode
  out = solve.sir(sirVaccStrat, init, parameters, times)
  attr(out, "Model") = "Vaccination Stratified";
  return(out);
}
# Function for display options
plotSIR_VaccineStrat = function(out, options,  flt = "Old", add = FALSE, plot.legend = TRUE, ...) {
  head(out, 10)
  # legend labels
  lbl = c("Total", "Susceptible (Young)", "Susceptible (Old)", "Infected (Young)", 
          "Infected (Old)", "Hosp (cumulated)", "Hosp (Young)", "Hosp (Old)", 
          "Death (Young)", "Death (Old)", "Recovered", "Vaccinated (Young)", "Vaccinated (Old)");
  # controller for filtering
  type = match(flt, getDisplayTypesVaccStrat());
  # legend position
  leg.xy = c(0.7 * max(out$time), 1)
  # number of colums for legend
  ncol = 2
 
  if(type > 1) {
    out$HospRate = c(0, diff(out$Hcum)) * options$hosp.rate.scale;
    lbl = c(lbl, paste0("Hosp Rate  [scale = x", options$hosp.rate.scale, "]") );
    ncol = 1
    if(type == 2) {
      r = filter.out(out, c("T", "Ho", "Io", "So", "Vo", "Do"), lbl);
      leg.xy = c(0.9, max(1-options$p.old, r$out$So[1], r$out$Hcum) * 0.9);
    } else if(type == 3) {
      r = filter.out(out, c("T", "Hy", "Sy", "Iy", "Vy", "R", "Dy"), lbl);
      leg.xy = c(0.0, max(options$p.old, r$out$So) * 0.9);
    } else if(type == 4){
      out$I = out$Iy + out$Io;
      out$H = out$Hy + out$Ho;
      out$V = out$Vy + out$Vo;
      lbl = c(lbl, "Infected (Total)", "Hospitalised (Total)", "Vaccinated (Total)")
      r = filter.out(out, c("Iy", "Io", "Vy", "Vo", "Hy", "Ho"), lbl);
      leg.xy = c(0.0, 0.74);
      ncol = 2
    }
    out = r$out; lbl = r$lbl;
  }
  plot.sir(out, legend.lbl = lbl, legend.xy = leg.xy, ncol = ncol,
           title = "SIR Vaccination Stratified Model", add = add, plot.legend = plot.legend, ...)
}

### Sensitivity Analysis
Sensitivity_VaccineStrat = function(param, opt, end.time, min=0, max=1, options, flt = "Old") {
  by = (max - min)/20;
  for(p in seq(min, max, by = by)) {
    opt[[param]] = p;
    
    out = initSIR_VaccineStrat(opt, end.time);
    
    plotSIR_VaccineStrat(out, flt = flt, add = if(p == min) FALSE else TRUE,
                         plot.legend = FALSE, lty = options$sensitivity.lty);
  }
  
  opt[[param]] = min;
  
  out = initSIR_VaccineStrat(opt, end.time);
  
  plotSIR_VaccineStrat(out, flt = flt,
                       add = TRUE, plot.legend = TRUE,
                       lty = 1);
}