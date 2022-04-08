helpEpidem = function(){
  txtSIR = c("S = Susceptible", "I = Infected", "R = Recovered")
  txtHosp = c("Sy = Susceptible young", "So = Susceptible old", "Iy = Infected young", "Io = Infected old", 
              "H = Hospitalised", "D = Death", "R = Recovered")
  txtV = c("Sy = Susceptible young", "So = Susceptible old", "Iy = Infected young", "Io = Infected old", 
           "H = Hospitalised", "D = Death", "R = Recovered", "Vy = Vaccinated young", "Vo = Vaccinated old")
  txtVS = c("Sy = Susceptible young", "So = Susceptible old", "Iy = Infected young", "Io = Infected old", 
            "Hy = Hospitalised young", "Ho = Hospitalised old", "Dy = Death young", "Do = Death old",
            "R = Recovered", "Vy = Vaccinated young", "Vo = Vaccinated old")
  txt2V = c("S = Susceptible", "IV1 = Infected with virus 1", "IV2 = Infected with virus 2",
            "HV1 = Hospitalised infected with virus 1", "HV2 = Hospitalised infected with virus 2",
            "DV1 = Death infected with virus 1", "DV2 = Death infected with virus 2", 
            "RV1 = Recovered infected with virus 1", "RV2 = Recovered infected with virus 2")
  txtAG3 = c("Sc = Susceptible children", "Sa = Susceptible adults", "So = Susceptible old",
             "Ic = Infected children", "Ia = Infected adults", "Io = Infected old",
             "Hc = Hospitalized child", "Ha = Hospitalized adults", "Ho = Hospitalized old",
             "Dc = Death children", "Da = Death adults", "Do = Death old",
             "R = Recovered (R = Rc + Ra + Ro)")
  # title = list(description, abrevetions/compartments)
  txtHelp = list("SIR" = list(txt = "Simple SIR Model", C = txtSIR), 
              "Hospitalization Model" =  
                list(txt = "The SIR Model was extended with a 
                            Hospitalization compartment.", C = txtHosp),
              "Vaccination Model" = 
                list(txt = "Vaccination Model", C = txtV), 
              "Vaccine Stratified Model" = 
                list(txt = "Vaccine Stratified Model", C = txtVS), 
              "2 Viruses Model" = 
                list(txt = "2 Viruses Model", C = txt2V),
              "Age Groups Model" = 
                list(txt = "Age Groups Model", C = txtAG3),
              "Analysis" = list(txt = "Analysis", C = NULL))
  txtTitles = names(txtHelp)
  txt = lapply(seq(length(txtTitles)), function(id){
    fluidRow(HTML("<p> <b>", txtTitles[id], "</b> <br>", 
                  txtHelp[[id]]$txt, "<br>",
                  paste("&nbsp;", txtHelp[[id]]$C, collapse = "<br>"), "</p>") )
  })
  return(txt)
}