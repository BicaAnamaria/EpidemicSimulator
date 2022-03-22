########################
####Age Groups Model####
########################

getDisplayTypesAG3 = function(){
  c("All", "Compact", "Children", "Adults", "Old")
}

sirAG3 <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    
    # infect.xy = Ix infects Sy;
    # in hospital: children get infected as non-children;
    # dSc = -infect * Sc * ( Ic + Ia + Io + Hc + Ha + Ho) 
    dSc = - Sc * (infect.cc * Ic + infect.nc * (Ia + Io + Hc + Ha + Ho))
    dSa = - Sa * (infect.cn * Ic + infect.nn * (Ia + Io + Hc + Ha + Ho))
    dSo = - So * (infect.cn * Ic + infect.nn * (Ia + Io + Hc + Ha + Ho))
    dIc = - dSc - (death.c + hosp.c + recov.c) * Ic;
    dIa = - dSa - (death.a + hosp.a + recov.a) * Ia;
    dIo = - dSo - (death.o + hosp.o + recov.o) * Io;
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
  parameters = list(infect.cc = param$infectAG3.cc,
                    infect.cn = param$infectAG3.cn,
                    infect.nn = param$infectAG3.nn,
                    infect.nc = param$infectAG3.nc,
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
  init = c(T = 1, Sc = (1 - 1e-6) * opt.p.children, 
           Sa = (1 - 1e-6) * (1 - opt.p.children - opt.p.old), 
           So = (1 - 1e-6) * opt.p.old,
           Ic = 0.0, Ia = 1e-6, Io = 0.0, 
           Hcum = 0.0, Hc = 0.0, Ha = 0.0, Ho = 0.0, 
           Dc = 0.0, Da = 0.0, Do = 0.0,
           R = 0.0)
  print(init)
  
  ### Solve using ode
  out = solve.sir(sirAG3, init, parameters, times)
  attr(out, "Model") = "AG3";
  return(out);
}
plotSIR_AG3 = function(out, flt = "Adults", add = FALSE, plot.legend = TRUE, ...) {
  
  lbl = c( "Total", "Susceptible (Children)", "Susceptible (Adults)", "Susceptible (Elders)",
           "Infected (Children)", "Infected (Adults)", "Infected (Elders)", 
           "Hosp (Cumulative)", "Hosp (Children)", "Hosp (Adults)", "Hosp (Elders)", 
           "Death (Children)", "Death (Adults)", "Death (Elders)",
           "Recovered (Children)", "Recovered (Adults)", "Recovered (Elders)") ;
  leg.off=c(-0.0, 0.3);
  type = match(flt, getDisplayTypesAG3());
  if(type > 1) {
    
    if(type == 2) {
      r = filter.out(out, c("T"), lbl);
    } else if(type == 3) {
      r = filter.out(out, c("T", "Sa", "So","Ia", "Io", "Ha", "Ho", "Da", "Do"), lbl);
    } 
    else if(type == 4){
      r = filter.out(out, c("T", "Ic", "Io", "Hc", "Ho", "Dc", "Do"), lbl);
      # r = filter.out(out, c("Vo", "Vy", "Dy", "Do", "Ho", "Hy"), lbl);
    }
    else if(type == 5){
      r = filter.out(out, c("T", "Ic", "Ia", "Hc", "Ha", "Dc", "Da"), lbl);
    }
    
    out = r$out; lbl = r$lbl;
  }
  
  plot.sir(out, legend.lbl = lbl, leg.off=leg.off, title = "SIR Age Groups Model", 
           add = add, plot.legend = plot.legend, ...)
}