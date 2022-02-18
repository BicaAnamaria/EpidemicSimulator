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
getSensitivity = function() {
  c("Basic Model" = "SIR", "Infection rate" = "infect",
    "Hospitalization rate (Old)" = "hosp.old", "Hospitalization rate (Young)" = "hosp",
    "Death rate (Hospital)" = "death.h", "Death rate (Community)" = "death"
  );
}

### Hospitalization
getDisplayTypes = function() {
  c("All", "Compact", "Young", "Old");
}

sirHosp <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    IYng = I;
    ITot = IYng + IOld; # all infected (young & old) can infect!
    dSy = -infect * Sy * ITot - infect * Sy * H; # both I & H infect!
    dSo = -infect * So * ITot - infect * So * H;
    dT  =  dSy + dSo; # not really needed;
    # Infected
    dIy = -dSy - recov * IYng - death * IYng - hosp * IYng;
    dIo = -dSo - recov * IOld - death.old * IOld - hosp.old * IOld;
    # Hospitalized
    dHcum = hosp * IYng + hosp.old * IOld; # used to extract daily rates;
    dHy =  hosp * IYng - recov.h * Hy - death.h * Hy;
    dHo =  hosp.old * IOld - recov.h * Ho - death.h * Ho;
    dH  =  dHcum - recov.h * H - death.h * H;
    dR  =  recov * IYng + recov * IOld + recov.h * H;
    dDc =  death * IYng + death.old * IOld; # check if death.old useful?
    dDh =  death.h * H;
    return(list(c(dT, dSy, dSo, dIy, dIo, dR, dHcum, dH, dHy, dHo, dDc, dDh)));
  })
}

initSIR_Hosp_Com = function(opt, end.time, p.old = opt.p.old) {
  times = seq(0, end.time, by = 1) # flt = filter
  # - S = susceptible young people;
  # - O = susceptible old people;
  # - Hcum = cumulative hospitalization;
  # - Dc = Deceased (community); Dh = Deceased hospital;
  parameters = c(infect = opt$infect,
                 recov = opt$recov, recov.h = opt$recov.h,
                 # Death rate: scaled by opt$recov: remove scaling?
                 death = opt$recov*opt$death, death.old = opt$recov*opt$death.old,
                 death.h = opt$recov.h*opt$death.h,
                 hosp = opt$hosp, hosp.old = opt$hosp.old)
  I0 = 1E-6;
  init = c(T = 1 - I0, Sy = (1 - I0) * (1 - p.old), So = (1 - I0) * p.old,
           I = I0, IOld = 0.0, R = 0.0,
           Hcum = 0.0, H = 0.0, Hy = 0.0, Ho = 0.0, Dc = 0.0, Dh = 0.0); # init = state
  
  ### Solve using ode
  out = solve.sir(sirHosp, init, parameters, times)
  
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

Sensitivity_Hosp_Com = function(param, opt, end.time, min=0, max=1, p.old = opt.p.old, flt = "Old") {
  by = (max - min)/20;
  for(p in seq(min, max, by = by)) {
    opt[[param]] = p;
    
    out = initSIR_Hosp_Com(opt, end.time);
    
    plotSIR_Hosp(out, flt = flt, add = if(p == min) FALSE else TRUE,
                 plot.legend = FALSE, lty = opt.sensitivity.lty);
  }
  
  opt[[param]] = min;
  
  out = initSIR_Hosp_Com(opt, end.time);
  
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


initSIR_Vaccine = function(list1, end.time, p.old = opt.p.old) # list1 = param modificare
{
  
  times = seq(0, end.time, by = 1)
  # (!) S represents the nr of susceptible young people
  parameters = list(infect = list1$infect, 
                    recov = list1$recov.c, 
                    recov.h = (1 -list1$death.h) * list1$recov.h, 
                    death.y = list1$recov.c * list1$death.y,
                    death.o = list1$recov.c * list1$death.o,
                    death.h = list1$death.h * list1$recov.h, 
                    hosp.y = list1$hosp.y,             
                    hosp.o = list1$hosp.o, 
                    vacc.y = list1$vacc.y,     
                    vacc.o = list1$vacc.o     
  )
  
  init = c(T = 1 - 1e-6, Sy = (1 - 1e-6) * (1 - p.old), So = (1 - 1e-6) * p.old, Iy = 1e-6 * (1 - p.old), Io = 1e-6 * p.old, 
           Hcum = 0.0, H = 0.0, D = 0.0, R = 0.0, Vaccy =0.0, Vacco = 0.0)
  
  ### Solve using ode
  out = solve.sir(sirVacc, init, parameters, times)
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
      r = filter.out(out, c("T", "Hcum", "Io", "So", "Vacco"), lbl);
      leg.off[2] = max(1 - p.old, out$Iy, out$R) - 0.7;
    } else if(type == 3) {
      r = filter.out(out, c("T", "Hcum", "Sy", "Vaccy", "R"), lbl);
      leg.off[2] = max(p.old, out$Iy) - 0.7;
    } 
    else if(type == 4){
      out$T = out$Sy + out$So;
      out$I = out$Iy + out$Io;
      out$V = out$Vaccy + out$Vacco;
      lbl = c(lbl, "Infected (Total)", "Vaccinated (Total)")
      r = filter.out(out, c("Iy", "Io", "Vacco", "Vaccy"), lbl);
    }
    
    out = r$out; lbl = r$lbl;
  }
  plot.sir(out, legend.lbl = lbl, leg.off=leg.off, title = "SIR Vaccination Model", 
           add = add, plot.legend = plot.legend)
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
    "Infection rate (Young)" = "infect.y", "Infection rate (Old)" = "infect.o",
    "Hospitalization rate (Old)" = "hosp.o", "Hospitalization rate (Young)" = "hosp.y",
    "Death rate (Young)" = "death.y", "Death rate (Old)" = "death.o", 
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
    dHy = hosp.y * Iy - recov.y * Hy - death.hosp.y * Hy;
    dHo = hosp.o * Io - recov.o * Ho - death.hosp.o * Ho;
    dDy = death.hosp.y * Hy + death.y * Iy;
    dDo = death.hosp.o * Ho + death.o * Io;
    dR = recov.y * Iy + recov.o * Io + recov.y * Hy + recov.o * Ho;
    dHcum = hosp.y * Iy + hosp.o * Io;
    dT = dSy + dSo;
    
    return(list(c(dT, dSy, dSo, dIy, dIo, dHcum, dHy, dHo, dDy, dDo, dR, dVy, dVo)))
    
  }
  )}


initSIR_VaccineStrat = function(list ,end.time, p.old = opt.p.old, add = FALSE, plot.legend = TRUE, ...)
{
  
  times = seq(0, end.time, by = 1)
  
  parameters = list(infect = list$infect,
                    recov.h.y = list$recov.h * (1 - list$death.hosp.y),
                    recov.h.o = list$recov.h * (1 - list$death.hosp.o),
                    recov.y = list$recov.y,
                    recov.o = list$recov.old,
                    death.y = list$death.y, 
                    death.o = list$death.old,
                    death.hosp.y = list$recov.h * list$death.hosp.y,
                    death.hosp.o = list$recov.h * list$death.hosp.o,
                    hosp.y   = list$hosp.y,
                    hosp.o = list$hosp.old,
                    vacc.y = list$vacc.y,     
                    vacc.o = list$vacc.old)
  
  init = c(T = 1, Sy = (1 - 1e-6) * (1 - p.old), So = (1 - 1e-6) * p.old,
  Iy = 1e-6 * (1 - p.old), Io = 1e-6 * p.old, Hcum = 0.0, Hy = 0.0, 
  Ho = 0.0, Dy = 0.0, Do = 0.0, R = 0.0, Vy =0.0, Vo = 0.0)
  
  
  ### Solve using ode
  out = solve.sir(sirVaccStrat, init, parameters, times, add = add, plot.legend = plot.legend)
  return(out);
}
plotSIR_VaccineStrat = function(out, p.old = opt.p.old,  flt = "Old") {
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
  plot.sir(out, legend.lbl = lbl, leg.off=leg.off, title = "SIR Vaccination Startified Model")
}

