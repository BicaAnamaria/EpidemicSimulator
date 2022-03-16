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

### based on:
### Team Project 2021
### Students:
###   Dora Calea, Ioana Obreja,
###   Liviu Sopon and Dragos Ursan
###   West University, Timisoara
###
### Supervisor: Leonard Mada
### Syonic SRL

### Load Libraries
library(ggplot2)
library(deSolve)

### Global Options
opt.delay.vacc = 60;
opt.delay.2V = 60;
opt.p.old = 0.2;
opt.death.rate.scale = 24;
opt.hosp.rate.scale = 12;
opt.sensitivity.lty = 4;

#####################
#####################

### Basic Functions

### Solve SIR
solve.sir = function(sir.f, init, parameters, times) {
  ## Solve using ode (General Solver for Ordinary Differential Equations)
  out = ode(y = init, times = times, func = sir.f, parms = parameters)
  ## change to data frame
  out = as.data.frame(out)
  return(out)
}


### Plot SIR
basic.lbl = c("Susceptible", "Infected", "Recovered");

legend.xyf = function(times, x=c(0,0)) {
  c(max(times)*2/3, 0.7) + x;
}

plot.sir = function(y, times = NULL, legend.lbl = basic.lbl, legend.xy, leg.off = c(0,0),
                    ylab = "Susceptible and Recovered", title = "SIR Model",
                    lty = 1, lwd = 2, col = 2:10,
                    add = FALSE, plot.legend=TRUE, ...) {
  if(is.null(times)) {
    times = y$time;
    if(is.null(times)) stop("The times argument is missing!");
  }
  y$time = NULL;
  
  if(missing(legend.xy)) legend.xy = legend.xyf(times, leg.off)
  if(add) {
    matplot(x = times, y = y, type = "l", lwd = lwd, lty = lty, bty = "l", col = col, add=TRUE);
  } else {
    matplot(x = times, y = y, type = "l",
            xlab = "Time", ylab = ylab, main = title,
            lwd = lwd, lty = lty, bty = "l", col = col, ...)
  }
  
  ### Add legend
  if(plot.legend) {
    legend(legend.xy[1], legend.xy[2], legend.lbl,
           pch = 1, col = col, bty = "n")
  }
}

filter.out = function(x, flt, lbl) {
  id = match(flt, names(x)); # look for filter name, witch are excluded from the legend 
  id = id[ ! is.na(id)];
  if(length(id) > 0) {
    x = x[, - id];
    idTime = match("time", names(x)); # verify if x is in the time parameters
    if(! is.na(idTime)) id = id - 1; # correct for x$time
    lbl = lbl[-id];
  }
  return(list(out = x, lbl = lbl));
}

#####################
#####################

### Basic SIR
basic_sir <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS = - infect * S * I;
    dI = - dS - recov * I;
    dR =  recov * I;
    
    return(list(c(dS, dI, dR)))
  })
}
initSIR_Basic = function(list, end.time){
  times = seq(0, end.time, by=1)
  parameters = c(infect = list[1], recov = list[2])
  print(parameters)
  
  init = c(S = 1 - 1e-6, I = 1e-6, R = 0.0)  ### Solve using ode
  ### Solve using ode
  out = solve.sir(basic_sir, init, parameters, times)
  attr(out, "Model") = "Basic";
  head(out, 10)
  
  ### Plot
  plot.sir(out, legend.lbl = c("Susceptible", "Infected", "Recovered"), leg.off=c(-0.1, 0.3))

}


#######################
#######################

#######################
### Hospitalization ###
#######################

### SIR + Old Age + Hospital
# - different mortalities for Hospital vs Community;

### Sensitivity Analysis
getSensitivity_Hosp = function() {
  c("Basic Model" = "SIR", "Infection rate" = "infect",
    "Hospitalization rate (Old)" = "hosp.o", "Hospitalization rate (Young)" = "hosp.y",
    "Death rate (Hospital)" = "death.h", 
    "Death rate (Community Old)" = "death.o", "Death rate (Community Young)" = "death.y"
  );
}

### Hospitalization
getDisplayTypes = function() {
  c("All", "Compact", "Young", "Old");
}

sirHosp <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    ITot = Iy + Io; # all infected (young & old) can infect!
    dSy = -infect * Sy * ITot - infect * Sy * H; # both I & H infect!
    dSo = -infect * So * ITot - infect * So * H;
    dT  =  dSy + dSo; # not really needed;
    # Infected
    dIy = -dSy - recov.c * Iy - death.y * Iy - hosp.y * Iy;
    dIo = -dSo - recov.c * Io - death.o * Io - hosp.o * Io;
    # Hospitalized
    dHcum = hosp.y * Iy + hosp.o * Io; # used to extract daily rates;
    dHy =  hosp.y * Iy - recov.h * Hy - death.h * Hy;
    dHo =  hosp.o * Io - recov.h * Ho - death.h * Ho;
    dH  =  dHcum - recov.h * H - death.h * H;
    dR  =  recov.c * Iy + recov.c * Io + recov.h * H;
    dDc =  death.y * Iy + death.o * Io; # check if death.old useful?
    dDh =  death.h * H;
    return(list(c(dT, dSy, dSo, dIy, dIo, dR, dHcum, dH, dHy, dHo, dDc, dDh)));
  })
}

initSIR_Hosp = function(opt, end.time, p.old = opt.p.old) {
  times = seq(0, end.time, by = 1) # flt = filter
  # - S = susceptible young people;
  # - O = susceptible old people;
  # - Hcum = cumulative hospitalization;
  # - Dc = Deceased (community); Dh = Deceased hospital;
  parameters = c(infect = opt$infect,
                 recov.c = opt$recov.c, 
                 recov.h = opt$recov.h,
                 # Death rate: scaled by opt$recov: remove scaling?
                 death.y = opt$recov.c * opt$death.y, 
                 death.o = opt$recov.c * opt$death.o,
                 death.h = opt$recov.h * opt$death.h,
                 hosp.y = opt$hosp.y, 
                 hosp.o = opt$hosp.o)
  I0 = 1E-6; 
  init = c(T = 1 - I0, Sy = (1 - I0) * (1 - p.old), So = (1 - I0) * p.old,
           Iy = I0, Io = 0.0, R = 0.0,
           Hcum = 0.0, H = 0.0, Hy = 0.0, Ho = 0.0, Dc = 0.0, Dh = 0.0); # init = state
  
  ### Solve using ode
  out = solve.sir(sirHosp, init, parameters, times)
  attr(out, "Model") = "Hospitalization";
  return(out);
}

plotSIR_Hosp = function (out, p.old = opt.p.old, flt="Old", add = FALSE, plot.legend = TRUE, ...)
{ 
  ### Plot
  lbl = c("Total", "Young", "Old", "Infected: Young (in community)", "Infected: Old (in community)",
          "Recovered", "Hosp (cumulative)", "Hosp: All", "Hosp: Young", "Hosp: Old",
          "Death: Community", "Death: Hospital");
  leg.off=c(-0.1, 0.3);
  
  ### Display Types
  type = match(flt, getDisplayTypes()); # verify if the first parameter is in the list generated by getDisplay
  
  # filter results
  if(type > 1) {
    out$DeathAll = out$Dc + out$Dh;
    out$HospRate = c(out$Hcum[1], diff(out$Hcum)) * opt.hosp.rate.scale; # modify hospitalisation rate 

    lbl = c(lbl, "Death", paste0("Hosp (rate)[scale = x", opt.hosp.rate.scale, "]"));
    if(type == 2) {
      r = filter.out(out, c("T", "Hy", "Ho", "Dc", "Dh"), lbl);
    } else if(type == 3) {
      r = filter.out(out, c("T", "So", "Ho", "Dc", "Dh", "IOld"), lbl); 
    } else if(type == 4) {
      r = filter.out(out, c("T", "Sy", "Hy", "Dc", "Dh", "R"), lbl);
      leg.off[2] = max(p.old, out$I, max(out$Hcum) - 0.1) - 0.7;
    } else r = filter.out(out, c("Hy", "Ho"), lbl=lbl);
    out = r$out; lbl = r$lbl;
  }
  plot.sir(out, legend.lbl = lbl, leg.off = leg.off, add = add, plot.legend = plot.legend, title = "SIR Hospitalisation Model", ...);
}

### Sensitivity

Sensitivity_Hosp = function(param, opt, end.time, min=0, max=1, p.old = opt.p.old, flt = "Old") {
  by = (max - min)/20;
  for(p in seq(min, max, by = by)) {
    opt[[param]] = p;
    print(opt)
    
    out = initSIR_Hosp(opt, end.time); 
    
    plotSIR_Hosp(out, flt = flt, add = if(p == min) FALSE else TRUE,
                 plot.legend = FALSE, lty = opt.sensitivity.lty);
  }
  
  opt[[param]] = min;
  
  out = initSIR_Hosp(opt, end.time);
  
  plotSIR_Hosp(out, flt = flt,
                   add = TRUE, plot.legend = TRUE,
                   lty = 1);
}


###################

###################
### Vaccination ###
###################

# this whole section is from SIR_Vacc_OlderGroup from TEAM
# - preferential vaccination in older individuals:
#   model with old age group;

### Sensitivity Analysis
getSensitivityVacc = function() {
  c("Vaccination Model" = "Vacc", 
    "Infection rate (Young)" = "infect",
    "Hosp rate (Young)" = "hosp.y", "Hosp rate (Old)" = "hosp.o",
    "Death rate (Young)" = "death.y", "Death rate (Old)" = "death.o",
    "Death rate (Hosp)" = "death.h",
    "Vaccination rate (Young)" = "vacc.y", "Vaccination rate (Old)" = "vacc.o"
  );
}

getDisplayTypesVacc = function(){
  c("All", "Young", "Old", "Totals")
}

sirVacc <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
   
    if(time < opt.delay.vacc){
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
    #return(list(c(dT, dS, dI, dR, dD, dH, dV, dRo, dHo, dDo)));
    return(list(c(dT, dSy, dSo, dIy, dIo, dHcum, dH, dD, dR, dVy, dVo)));
  })
}


initSIR_Vaccine = function(param, end.time, p.old = opt.p.old) 
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
                    vacc.o = param$vacc.o     
  )
  
  init = c(T = 1 - 1e-6, Sy = (1 - 1e-6) * (1 - p.old), So = (1 - 1e-6) * p.old, 
           Iy = 1e-6 * (1 - p.old), Io = 1e-6 * p.old, 
           Hcum = 0.0, H = 0.0, D = 0.0, R = 0.0, Vy =0.0, Vo = 0.0)
  
  ### Solve using ode
  out = solve.sir(sirVacc, init, parameters, times)
  attr(out, "Model") = "Vaccination";
  return(out);
}
  

plotSIR_Vaccine = function(out, flt = "Old", p.old = opt.p.old, add = FALSE, plot.legend = TRUE, ...) {
  
  lbl = c("Total", "Young", "Old", "Infected (Young)", "Infected (Old)", "Hosp (cumulative)", "Hosp", 
          "Death", "Recovered", "Vaccinated (Young)", "Vaccinated (Old)");
  leg.off=c(-0.0, 0.3);
  
  type = match(flt, getDisplayTypesVacc());
  if(type > 1) {
    out$DeathAll = c(out$D[1], diff(out$D, lag = 1)) * opt.death.rate.scale; 
    out$HospRate = c(0, diff(out$Hcum)) * opt.hosp.rate.scale;
    lbl = c(lbl, paste0("Death Rate [scale = x", opt.death.rate.scale, "]"),
                 paste0("Hosp Rate  [scale = x", opt.hosp.rate.scale, "]") );
    if(type == 1) {
      r = filter.out(out, c("T", "Hcum"), lbl); }
    else if(type == 2) {
      r = filter.out(out, c("T", "Hcum", "Io", "So", "Vo"), lbl);
      leg.off[2] = max(1 - p.old, out$Iy, out$R) - 0.7;
    } else if(type == 3) {
      r = filter.out(out, c("T", "Hcum", "Sy", "Vy", "R"), lbl);
      leg.off[2] = max(p.old, out$Iy) - 0.7;
    } 
    else if(type == 4){
      out$T = out$Sy + out$So;
      out$I = out$Iy + out$Io;
      out$V = out$Vy + out$Vo;
      lbl = c(lbl, "Infected (Total)", "Vaccinated (Total)")
      r = filter.out(out, c("Iy", "Io", "Vo", "Vy"), lbl);
    }
    
    out = r$out; lbl = r$lbl;
  }
  plot.sir(out, legend.lbl = lbl, leg.off=leg.off, title = "SIR Vaccination Model", 
           add = add, plot.legend = plot.legend, ...)
}
# elimin com de la sensitivity hosp

Sensitivity_Vaccine = function(param, opt, end.time, min=0, max=1, p.old = opt.p.old, flt = "Old") {
  by = (max - min)/20;
  for(p in seq(min, max, by = by)) {
    opt[[param]] = p;
    
    out = initSIR_Vaccine(opt, end.time);
    
    plotSIR_Vaccine(out, flt = flt, add = if(p == min) FALSE else TRUE,
                 plot.legend = FALSE, lty = opt.sensitivity.lty);
  }
  
  opt[[param]] = min;
  
  out = initSIR_Vaccine(opt, end.time);
  
  plotSIR_Vaccine(out, flt = flt,
               add = TRUE, plot.legend = TRUE,
               lty = 1);
}


### Daily Mortality
#lines(c(0, times), (c(out$D, 0) - c(0, out$D)) * 30, col="red")
day_mort=function()
{
  dD = diff(out$D, lag=1)
  dD = c(0, dD)
  lines(times, dD * 30, col="red") # * 30 = uses a different scaling!
  max(dD*100)
  plot(cut(dD*100, breaks = 100))
}



###################

###############################
### Vaccination Stratified ###
###############################

getSensitivityVaccStrat = function() {
  c("Vaccination Startified Model" = "VaccStart", 
    "Infection rate (Young)" = "infect",
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
    
    if(time < opt.delay.vacc){
      dVy = 0;
    } else  dVy = min(Sy, vacc.y); # variable origin: state
    
    if(time < opt.delay.vacc){
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

# separat models_hosp, models_vaccine -> 3 fisiere

initSIR_VaccineStrat = function(param, end.time, p.old = opt.p.old)
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
                    vacc.o = param$vacc.o)
  
  init = c(T = 1, Sy = (1 - 1e-6) * (1 - p.old), So = (1 - 1e-6) * p.old,
  Iy = 1e-6 * (1 - p.old), Io = 1e-6 * p.old, Hcum = 0.0, Hy = 0.0, 
  Ho = 0.0, Dy = 0.0, Do = 0.0, R = 0.0, Vy =0.0, Vo = 0.0)
  
  
  ### Solve using ode
  out = solve.sir(sirVaccStrat, init, parameters, times)
  attr(out, "Model") = "Vaccination Stratified";
  return(out);
}
# Function for display options
plotSIR_VaccineStrat = function(out, p.old = opt.p.old,  flt = "Old", add = FALSE, plot.legend = TRUE, ...) {
  head(out, 10)
  lbl = c("Total", "Susceptible (Young)", "Susceptible (Old)", "Infected (Young)", 
          "Infected (Old)", "Hosp (cumulated)", "Hosp (Young)", "Hosp (Old)", 
          "Death (Young)", "Death (Old)", "Recovered", "Vaccinated (Young)", "Vaccinated (Old)");
  leg.off=c(-0.1, 0.3);
  
  
  type = match(flt, getDisplayTypesVaccStrat());
  if(type > 1) {
    out$HospRate = c(0, diff(out$Hcum)) * opt.hosp.rate.scale;
    
    lbl = c(lbl, paste0("Hosp Rate  [scale = x", opt.hosp.rate.scale, "]") );
   
    if(type == 2) {
      r = filter.out(out, c("T", "Ho", "Io", "So", "Vo", "Do"), lbl);
    } else if(type == 3) {
      r = filter.out(out, c("T", "Hy", "Sy", "Vy", "R", "Dy"), lbl);
      leg.off[2] = max(r$out$So[1], r$out$Hcum) - 0.7;
    } 
    else if(type == 4){
      out$I = out$Iy + out$Io;
      out$H = out$Hy + out$Ho;
      out$V = out$Vy + out$Vo;
      lbl = c(lbl, "Infected (Total)", "Hospitalised (Total)", "Vaccinated (Total)")
      r = filter.out(out, c("Iy", "Io", "Vy", "Vo", "Hy", "Ho"), lbl);
      # r = filter.out(out, c("Vo", "Vy", "Dy", "Do", "Ho", "Hy"), lbl);
    }
    
    out = r$out; lbl = r$lbl;
  }
  plot.sir(out, legend.lbl = lbl, leg.off=leg.off, title = "SIR Vaccination Stratified Model", 
           add = add, plot.legend = plot.legend, ...)
}
# Sensitivity Analysis
Sensitivity_VaccineStrat = function(param, opt, end.time, min=0, max=1, p.old = opt.p.old, flt = "Old") {
  by = (max - min)/20;
  for(p in seq(min, max, by = by)) {
    opt[[param]] = p;
    
    out = initSIR_VaccineStrat(opt, end.time);
    
    plotSIR_VaccineStrat(out, flt = flt, add = if(p == min) FALSE else TRUE,
                    plot.legend = FALSE, lty = opt.sensitivity.lty);
  }
  
  opt[[param]] = min;
  
  out = initSIR_VaccineStrat(opt, end.time);
  
  plotSIR_VaccineStrat(out, flt = flt,
                  add = TRUE, plot.legend = TRUE,
                  lty = 1);
}

######################
######2 Viruses#######
######################
IV2 = 0;

sir2Viruses <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    
    if(time < opt.delay.2V){
      #IV2 = 0;
      infect.v2 = 0;
    } 
    dI1 = S * infect.v1 * (IV1 + HV1);
    dI2 = S * infect.v2 * (IV2 + HV2);
    
    dS = - dI1 - dI2;
    dIV1 = dI1 - (death.v1 + hosp.v1 + recov.v1) * IV1;
    dIV2 = dI2 - (death.v2 + hosp.v2 + recov.v2) * IV2;
    dHV1 = hosp.v1 * IV1 - recov.hv1 * HV1 - death.hv1 * HV1;
    dHV2 = hosp.v2 * IV2 - recov.hv2 * HV2 - death.hv2 * HV2;
    dHcum = hosp.v1 * IV1 + hosp.v2 * IV2;
    dDV1 = death.v1 * IV1 + death.hv1 * HV1;
    dDV2 = death.v2 * IV2 + death.hv2 * HV2;
    dRV1 = recov.v1 * IV1 + recov.hv1 * HV1;
    dRV2 = recov.v2 * IV2 + recov.hv2 * HV2;
    
    #dIV12 = 0;
    #dIV21 = 0;
    #dSV12 = 0;
    #dSV21 = 0;
    #dHV12 = 0;
    #dHV21 = 0;
    
    #return(list(c(dT, dS, dI, dR, dD, dH, dV, dRo, dHo, dDo)));
    return(list(c(dS, dIV1, dIV2, dHcum, dHV1, dHV2, dDV1, dDV2, dRV1, dRV2)));
  })
}

initSIR_2Viruses = function(param, end.time)
{
  
  times = seq(0, end.time, by = 1)
  print(param)
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
                    death.hv2 = param$recovV2.h * param$deathV2.h
                    #infect.v1_v2 = param$infectV1V2,
                    #infect.v2_v1 = param$infectV2V1,
                    )
  print(parameters)
  init = c(S = (1 - 1e-6), IV1 = 1e-6 , IV2 = 1e-3, 
           Hcum = 0.0, HV1 = 0.0, HV2 = 0.0, 
           DV1 = 0.0, DV2 = 0.0, RV1 = 0.0, RV2 = 0.0)
  
  
  ### Solve using ode
  out = solve.sir(sir2Viruses, init, parameters, times)
  attr(out, "Model") = "2 Viruses";
  return(out);
}

plotSIR_2Viruses = function(out, add = FALSE, plot.legend = TRUE, ...) {

  lbl = c( "Susceptible", "Infected (Virus1)", "Infected (Virus2)", "Hosp (cumulative)", 
           "Hosp (Virus1)", "Hosp (Virus2)", "Death (Virus1)", "Death (Virus2)",
           "Recovered (Virus1)", "Recovered (Virus2)") ;
  leg.off=c(-0.0, 0.3);
  plot.sir(out, legend.lbl = lbl, leg.off=leg.off, title = "SIR 2 Virus Model", 
           add = add, plot.legend = plot.legend, ...)
}

########################
####Age Groups Model####
########################
sir2AG3 <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    
  
    dSc = -infect * Sc * ( Ic + Ia + Io + Hc + Ha + Ho) 
    dSa = -infect * Sa * ( Ic + Ia + Io + Hc + Ha + Ho) 
    dSo = -infect * So * ( Ic + Ia + Io + Hc + Ha + Ho) 
    dIc = infect * Sc * ( Ic + Ia + Io + Hc + Ha + Ho)  - (death.c + hosp.c + recov.c) * Ic;
    dIa = infect * Sa * ( Ic + Ia + Io + Hc + Ha + Ho)  - (death.a + hosp.a + recov.a) * Ia;
    dIo = infect * So * ( Ic + Ia + Io + Hc + Ha + Ho)  - (death.o + hosp.o + recov.o) * Io;
    dHc = hosp.c * Ic - recov.c * Hc - death.hc * Hc;
    dHa = hosp.a * Ia - recov.a * Ha - death.ha * Ha;
    dHo = hosp.o * Io - recov.o * Ho - death.ho * Ho;
    dDc = death.hc * Hc + death.c * Ic;
    dDa = death.ha * Ha + death.a * Ia;
    dDo = death.ho * Ho + death.o * Io;
    dR = recov.c * Ic + recov.a * Ia + recov.o * Io + recov.c * Hc + recov.a * Ha + recov.o * Ho;
    dHcum = hosp.c * Io + hosp.a * Ia + hosp.o * Io;
    dT = dSc + dSa + dSo;
    
  
    return(list(c(dT, dSc, dSa, dSo, dIc, dIa, dIo, dHcum, dHc, dHa, dHo, dDc, dDa, dDo, dR)));
  })
}

initSIR_AG3 = function(param, end.time)
{
  
  times = seq(0, end.time, by = 1)
  print(param)
  parameters = list(infect.c = param$infectAG3.c,
                    infect.a = param$infectAG3.a,
                    infect.o = param$infectAG3.o,
                    hosp.c = param$hospAG3.c,
                    hosp.a = param$hospAG3.a,
                    hosp.o = param$hospAG3.o,
                    death.c = param$recovAG3.c * param$deathAG3.c,
                    death.a = param$recovAG3.a * param$deathAG3.a,
                    death.o = param$recovAG3.o * param$deathAG3.o,
                    death.hc = param$recovAG3.hc * param$deathAG3.hc,
                    death.ha = param$recovAG3.ha * param$deathAG3.ha,
                    death.ho = param$recovAG3.ho * param$deathAG3.ho,
                    recov.c = param$recovAG3.c * (1 - param$deathAG3.c),
                    recov.a = param$recovAG3.a * (1 - param$deathAG3.a),
                    recov.o = param$recovAG3.o * (1 - param$deathAG3.o),
                    recov.hc = param$recovAG3.hc * (1 - param$deathAG3.hc),
                    recov.ha = param$recovAG3.ha * (1 - param$deathAG3.ha),
                    recov.ho = param$recovAG3.ho * (1 - param$deathAG3.ho)
  )
  print(parameters)
  init = c(T = 1, Sc = (1 - 1e-6), Sa = (1 - 1e-6), So = (1 - 1e-6),
           Ic = 1e-6 , Ia = 1e-3, Io = 1e-3, 
           Hcum = 0.0, Hc = 0.0, Ha = 0.0, Ho = 0.0, 
           Dc = 0.0, Da = 0.0, Do = 0.0,
           Rc = 0.0, Ra = 0.0, Ro = 0.0)
  
  
  ### Solve using ode
  out = solve.sir(sirAG3, init, parameters, times)
  attr(out, "Model") = "AG3";
  return(out);
}
plotSIR_AG3 = function(out, add = FALSE, plot.legend = TRUE, ...) {
  
  lbl = c( "Total", "Susceptible (Children)", "Susceptible (adults)", "Susceptible (Elders)",
           "Infected (Children)", "Infected (Adults)", "Infected (Elders)", 
           "Hosp (Cumulative)", "Hosp (Children)", "Hosp (Adults)", "Hosp (Elders)", 
           "Death (Children)", "Death (Adults)", "Death (Elders)",
           "Recovered (Children)", "Recovered (Adults)", "Recovered (Elders)") ;
  leg.off=c(-0.0, 0.3);
  plot.sir(out, legend.lbl = lbl, leg.off=leg.off, title = "SIR Age Groups Model", 
           add = add, plot.legend = plot.legend, ...)
}