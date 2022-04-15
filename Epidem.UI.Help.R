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
                list(txt = "Simple model which includes the emergence of a 
                     virus mutant: The initial virus (Virus 1) can develop 
                     a mutation during the course of the epidemic. A proportion 
                     of infected individuals with Virus 1 will be converted to 
                     infected individuals with Virus 2 on a specified day of 
                     the infection. This delay can be set explicitly using 
                     the Delay slider. The proportion is set using the option 
                     opt..delay.2V (see file Epidem.Options.R). Recovered 
                     patients become immune to both variants of the virus, 
                     independent of the infecting virus strain. Currently, 
                     only infected individuals in the community develop 
                     the initial mutation. The model is implemented using 
                     a 'Stop and Go' approach.", C = txt2V),
              "Age Groups Model" = 
                list(txt = "Age Groups Model", C = txtAG3),
              "Analysis" = list(txt = "Analysis", C = NULL),
              "Dispay" = 
                list(txt = "Function for filtering results and desplaying them", C = NULL),
              "Sensitivity Analysis" = 
              list(txt = "A sequence of values between 0 and 1 is generated 
                   for the selected parameter. The Sensitivity analysis runs 
                   the simulation for each of these values. In the case of 
                   the infection rate: the values vary between 0.75x and 1.75x 
                   the current value of the infection rate. These limits are set 
                   as global options: opt.sensitivity.infect.min and 
                   opt.sensitivity.infect.max (see file Epidem.Options.R).", C = NULL)
              )
  txtTitles = names(txtHelp)
  txt = lapply(seq(length(txtTitles)), function(id){
    fluidRow(HTML("<p> <b>", txtTitles[id], "</b> <br>", 
                  txtHelp[[id]]$txt, "<br>",
                  paste("&nbsp;", txtHelp[[id]]$C, collapse = "<br>"), "</p>") )
  })
  return(txt)
}
