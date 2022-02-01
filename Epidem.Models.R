######################
###
### Epidemic Simulator
###
### part of:
### Team Project 2021
### Students:
###   Dora Calea, Ioana Obreja,
###   Liviu Sopon and Dragos Ursan
###   West University, Timisoara
###
### Supervisor: Leonard Mada
### Syonic SRL

### incarcare librarii
library(ggplot2)
library(deSolve)


#####################
#####################

### Basic Functions

### Solve SIR
solve.sir = function(sir.f, init, parameters, times) {
  ## Solve using ode (General Solver for Ordinary Differential Equations)
  out = ode(y = init, times = times, func = sir.f, parms = parameters)
  ## change to data frame
  out = as.data.frame(out)
  ## Delete time variable
  out$time <- NULL
  return(out)
}


### Plot SIR
basic.lbl = c("Susceptible", "Infected", "Recovered");

legend.xyf = function(times, x=c(0,0)) {
  c(max(times)*2/3, 0.7) + x;
}

plot.sir = function(y, times, legend.lbl = basic.lbl, legend.xy, leg.off = c(0,0),
                    ylab = "Susceptible and Recovered", lty = 1, lwd = 2, col = 2:10,
                    add = FALSE, plot.legend=TRUE, ...) {
  if(missing(legend.xy)) legend.xy = legend.xyf(times, leg.off)
  if(add) {
    matplot(x = times, y = y, type = "l", lwd = lwd, lty = lty, bty = "l", col = col, add=TRUE);
  } else {
    matplot(x = times, y = y, type = "l",
            xlab = "Time", ylab = ylab, main = "SIR Model",
            lwd = lwd, lty = lty, bty = "l", col = col, ...)
  }
  
  ### Add legend
  if(plot.legend) {
    legend(legend.xy[1], legend.xy[2], legend.lbl,
           pch = 1, col = col, bty = "n")
  }
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
  
  init = c(S = 1-1e-6, I = 1e-6, R = 0.0)  ### Solve using ode
  ### Solve using ode
  out = solve.sir(basic_sir, init, parameters, times)
  head(out, 10)
  
  ### Plot
  plot.sir(out, times, legend.lbl = c("Susceptible", "Infected", "Recovered"), leg.off=c(-0.1, 0.3))
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
initSIR_Hosp_Com = function(opt, end.time, p.old = 0.2, flt="Old", add = FALSE, plot.legend = TRUE, ...) {
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
  init = c(T = 1 - I0, S = (1 - I0) * (1 - p.old), O = (1 - I0) * p.old,
           I = I0, IOld = 0.0, R = 0.0,
           Hcum = 0.0, H = 0.0, Hy = 0.0, Ho = 0.0, Dc = 0.0, Dh = 0.0);
  
  ### Solve using ode
  out = solve.sir(sirHosp, init, parameters, times)
  # head(out, 10)
  
  ### Plot
  lbl = c("Total", "Young", "Old", "Infected: Young (in community)", "Infected: Old (in community)",
          "Recovered", "Hosp (cumulative)", "Hosp", "Hosp: Young", "Hosp: Old",
          "Death: Community", "Death: Hospital");
  leg.off=c(-0.1, 0.3);
  
  ### Display Types
  type = match(flt, getDisplayTypes()); # verifica primul parametru cu lista2 generatat de getDisplay
  
  # filter results
  if(type > 1) {
    out$DeathAll = out$Dc + out$Dh;
    hosp.rate.scale = 20;
    out$HospRate = c(0, diff(out$Hcum)) * hosp.rate.scale;
    lbl = c(lbl, "Death", paste0("Hosp (rate)[scale = x", hosp.rate.scale, "]"));
    if(type == 2) {
      r = filter.out(out, c("T", "Hy", "Ho", "Dc", "Dh"), lbl);
    } else if(type == 3) {
      r = filter.out(out, c("T", "Dc", "Dh", "O", "IOld", "Ho"), lbl);
    } else if(type == 4) {
      r = filter.out(out, c("T", "Dc", "Dh", "S", "Hy", "R"), lbl);
      leg.off[2] = max(p.old, out$I, max(out$Hcum) - 0.1) - 0.7;
    } else r = filter.out(out, c("Hy", "Ho"), lbl=lbl);
    out = r$out; lbl = r$lbl;
  }
  plot.sir(out, times, legend.lbl = lbl, leg.off = leg.off, add = add, plot.legend = plot.legend, ...);
}
filter.out = function(x, flt, lbl) {
  id = match(flt, names(x));
  id = id[ ! is.na(id)];
  if(length(id) > 0) {
    x = x[, - id];
    lbl = lbl[-id];
  }
  return(list(out = x, lbl = lbl));
}
sirHosp <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    IYng = I;
    ITot = IYng + IOld; # all infected (young & old) can infect!
    dSy = -infect * S * ITot - infect * S * H; # both I & H infect!
    dSo = -infect * O * ITot - infect * O * H;
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


### Sensitivity

Sensitivity_Hosp_Com = function(param, opt, end.time, p.old = 0.2, flt = "Old") {
  for(p in seq(0, 1, by=0.05)) {
    opt[[param]] = p;
    initSIR_Hosp_Com(opt, end.time, flt = flt,
                     add = if(p == 0) FALSE else TRUE,
                     plot.legend = FALSE,
                     lty = opt.sensitivity.lty);
  }
  
  opt[[param]] = 0;
  initSIR_Hosp_Com(opt, end.time, flt = flt,
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

getDisplayTypesVacc = function(){
  c("All", "Young", "Old")
}

sirVacc <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    # TODO: debug;
    dVy = min(S, vacc);
    dVo = min(O, vacc.o);
    dS = -infect * S * Iy - infect * S * Io - infect * S * H - dVy; # both I & H infect! S = young
    dO = -infect * O * Iy - infect * O * Io - infect * O * H - dVo;
    dT = (-infect * S * Iy - infect * S * Io - infect * S * H) + (-infect * O * Iy - infect * O * Io - infect * O * H) - dVy -dVo;
    dIy = infect * S * (Iy + Io + H) - (death.y + hosp + recov) * Iy;
    dIo = infect * O * (Iy + Io + H) - (death.o + hosp + recov) * Io;
    #dI =  infect * S * I + infect * S * H + infect * O * I + infect * O * H - recov * I - death.y * I - hosp * I;
    dD =  death.h * H + death.y * Iy + death.o * Io;
    dH =  hosp * Iy + hosp * Io - recov.h * H - death.h * H;
    dR =  recov * Iy + recov * Io + recov.h * H;
    dHcum = hosp * Iy + hosp * Io;
    #return(list(c(dT, dS, dI, dR, dD, dH, dV, dRo, dHo, dDo)));
    return(list(c(dT, dS, dO, dIy, dIo, dHcum, dH, dD, dR, dVy, dVo)));
  })
}

# probability of infection;
# recovery rate;
# death rate;
# hospitalization
# vaccination rate
initSIR_Vaccine = function(list1, end.time, p.old = 0.2,  flt = "Old")
{
  
  times = seq(0, end.time, by = 1)
  # (!) S represents the nr of susceptible young people
  parameters = list(infect = list1$infect, #list1$param
                    recov = list1$recov, # => 2 categorii death.h
                    recov.h = (1 -list1$death.hosp) * list1$recov.hosp, #list1$recov.hosp,
                    death.y = list1$recov * list1$death,
                    death.o = list1$recov * list1$death.old,
                    death.h = list1$death.hosp * list1$recov.hosp, #recov.h - nu il recunoaste #list1$recov.hosp * list1$death.hosp, 
                    hosp = list1$hosp,             
                    hosp.v = list1$hosp.vacc,           # recov.h = (1 - death.h.base) * recov.h 
                    vacc = list1$vacc.young,     #list1$vacc.old,
                    vacc.o = list1$vacc.old     #list1$vacc.young,
  )
  init = c(T = 1 - 1e-6, S = (1 - 1e-6) * (1 - p.old), O = (1 - 1e-6) * p.old, Iy = 1e-6, Io = 1e-6, Hcum = 0.0, H = 0.0, D = 0.0, R = 0.0, Vy =0.0, Vo = 0.0)
  # de mutat o inainte de I
  
  ### Solve using ode
  out = solve.sir(sirVacc, init, parameters, times)
  head(out, 10)
  lbl = c("Total", "Young", "Old", "Infected(Young)", "Infected(Old)", "Hosp (cumulative)", "Hosp", "Death", "Recovered", "Vaccinated (Young)", "Vaccinated (Old)");
  leg.off=c(-0.0, 0.3);
  
  
  type = match(flt, getDisplayTypesVacc());
  if(type > 1) {
    out$DeathAll = out$O; #out$Dc + out$Dh;
    hosp.rate.scale = 20;
    out$HospRate = c(0, diff(out$Hcum)) * hosp.rate.scale;
    lbl = c(lbl, "Death", paste0("Hosp (rate)[scale = x", hosp.rate.scale, "]"));
    if(type == 2) {
      r = filter.out(out, c("T", "Io", "O", "Vo"), lbl);
    } else if(type == 3) {
      r = filter.out(out, c("T", "Iy", "Vy", "R"), lbl);
      # leg.off[2] = max(p.old, out$I, max(out$Hcum) - 0.1) - 0.7;
    } else r = filter.out(out, c("T"), lbl=lbl);
    
    
    out = r$out; lbl = r$lbl;
    
  }
  
  ### Plot
  plot.sir(out, times, legend.lbl =  lbl, leg.off=leg.off)
  
  
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

sirVaccStrat <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    
    
  }
  )}


initSIR_VaccineStrat = function(list ,end.time, p.old = 0.2,  flt = "All")
{
  
  times = seq(0, end.time, by = 1)
  
  parameters = list(infect = list$infect, 
                    recov = list$recov, 
                    recov.h = (1 -list$death.hosp) * list$recov.hosp, 
                    recov.y = list$recov.y,
                    recov.old = list$recov.old,
                    death.y = list$recov * list$death, 
                    death.h = list$death.hosp * list$recov.hosp, 
                    hosp = list$hosp,
                    hosp.y = list$hosp.y,
                    hosp.old = list$hosp.old,
                    hosp.vaccY = list$hosp.vaccY,
                    hosp.vaccOld = list$hosp.vaccOld,     
                    vacc.y = list$vacc.young,     
                    vacc.old = list$vacc.old,    
                    death.o = list$recov * list$death.old,
                    death.oh = list$recov.hosp * list$death.oldhosp)
  init = c(T = 1 - 1e-6, S = (1 - 1e-6) * (1 - p.old), I = 1e-6,  O = (1 - 1e-6) * p.old, Hcum = 0.0, H = 0.0, Dy = 0.0, Do = 0.0, R = 0.0, Vy =0.0, Vo = 0.0)
  
  
  ### Solve using ode
  out = solve.sir(sirVaccStrat, init, parameters, times)
  head(out, 10)
  lbl = c("Total", "Young", "Infected", "Recovered", "Death", "Hosp", "Old", "OldDeath", "VaccinatedYoung", "VaccinatedOld");
  leg.off=c(-0.1, 0.3);
  
  
  
}