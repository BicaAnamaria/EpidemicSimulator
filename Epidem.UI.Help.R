helpEpidem = function(){
  txtTitles = c("SIR", "Hospitalization Model", "Vaccine Model", 
              "Vaccine Stratified Model", "2 Viruses Model",
              "Age Groups Model", "Analysis")
  txtSIR = c("S = Susceptible", "I = Infected", "R = Recovered")
  txtHosp = c("Sy = Susceptible young", "So = Susceptible old", "Iy = Infected young", "Infected old", 
              "H = Hospitalised", "D = Death", "R = Recovered")
  txtV = c("Sy = Susceptible young", "So = Susceptible old", "Iy = Infected young", "Infected old", 
           "H = Hospitalised", "D = Death", "R = Recovered", "Vy = Vaccinated young", "Vo = Vaccinated old")
  txtVS = c("Sy = Susceptible young", "So = Susceptible old", "Iy = Infected young", "Infected old", 
            "Hy = Hospitalised young", "Ho = Hospitalised old", "Dy = Death young", "Do = Death old",
            "R = Recovered", "Vy = Vaccinated young", "Vo = Vaccinated old")
  txt2V = c("S = Susceptible", "IV1 = Infected with virus 1", "IV2 = Infected with virus 2",
            "HV1 = Hospitalised infected with virus 1", "HV2 = Hospitalised infected with virus 2",
            "DV1 = Death infected with virus 1", "DV2 = Death infected with virus 2", 
            "RV1 = Recovered infected with virus 1", "RV2 = Recovered infected with virus 2")
  txtAG3 = c("Sc = Susceptible children", "Sa = Susceptible adults", "So = Susceptible old",
             "Ic = Infected children", "Ia = Infected adults", "Io = Infected old",
             "Hc = Hospitalized child", "Ha = Hospitalized adults", "Ho = Hospitalized old",
             "Dc = Death children", "Da = Death adults", "Death old",
             "R = Recovered")
  txtHelp = c("Test1", "Test2", "", "", "", "", "")
  txt = lapply(seq(length(txtTitles)), function(id){
    fluidRow(HTML("<b>", txtTitles[id], "</b> <br>", txtHelp[id]) )
  })
  return(txt)
}