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