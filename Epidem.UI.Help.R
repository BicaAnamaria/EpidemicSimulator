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

helpEpidem = function(id = "Help1"){
  txtSIR = c("S = Susceptible", "I = Infected", "R = Recovered")
  txtHosp = c("Sy = Susceptible young", "So = Susceptible old", 
              "Iy = Infected young (staying in the community)", "Io = Infected old (staying in the community)", 
              "H = Hospitalised", "Hcum = Hospitalisation Cumulated",
              "Hy = Hospitalised young (infected and hospitalised)", "Ho = Hospitalised old (infected and hospitalised)",
              "Dc = Death community", "Dh = Death Hospitalised",
              "R = Recovered")
  txtHospEH = c("Sy = Susceptible young", "So = Susceptible old", 
                "Ey = Exposed young", "Eo = Exposed old",
                "Iy = Infected young (staying in the community)", "Io = Infected old (staying in the community)", 
                "H = Hospitalised", "Hcum = Hospitalisation Cumulated",
                "Hy = Hospitalised young (infected and hospitalised)", "Ho = Hospitalised old (infected and hospitalised)",
                "Dc = Death community", "Dh = Death Hospitalised",
                "R = Recovered")
  txtV = c("Sy = Susceptible young", "So = Susceptible old", 
           "Iy = Infected young (staying in the community)", " Io = Infected old (staying in the community)", 
           "H = Hospitalised", "Hcum = Hospitalisation Cumulated",
           "D = Death", 
           "R = Recovered", 
           "Vy = Vaccinated young", "Vo = Vaccinated old")
  txtVS = c("Sy = Susceptible young", "So = Susceptible old", 
            "Iy = Infected young (staying in the community)", "Io = Infected old (staying in the community)", 
            "Hcum = Hospitalisation Cumulated",
            "Hy = Hospitalised young (infected and hospitalised)", "Ho = Hospitalised old (infected and hospitalised)", 
            "Dy = Death young", "Do = Death old",
            "R = Recovered", 
            "Vy = Vaccinated young", "Vo = Vaccinated old")
  txt2V = c("S = Susceptible", 
            "IV1 = Infected with virus 1", "IV2 = Infected with virus 2",
            "Hcum = Hospitalisation Cumulated",
            "HV1 = Hospitalised infected with virus 1", "HV2 = Hospitalised infected with virus 2",
            "DV1 = Death infected with virus 1", "DV2 = Death infected with virus 2", 
            "RV1 = Recovered infected with virus 1", "RV2 = Recovered infected with virus 2")
  txtAG3 = c("Sc = Susceptible children", "Sa = Susceptible adults", "So = Susceptible old",
             "Ic = Infected children", "Ia = Infected adults", "Io = Infected old",
             "Hcum = Hospitalisation Cumulated",
             "Hc = Hospitalised children", "Ha = Hospitalised adults", "Ho = Hospitalised old (infected and hospitalised)",
             "Dc = Death children", "Da = Death adults", "Do = Death old",
             "R = Recovered (R = Rc + Ra + Ro)")
  txtDisplay = c("All = All compartments;",
                 "Compact = Hide S Total, and plot H (All) and D (All), where All = joined strata;",
                 "Young = Plot results only for young adults;",
                 "Old = Plot results only for old age;")
  # title = list(description, abrevetions/compartments)
  txtHelp = list(
              "SIR Model" = 
                list(txt = "Simple SIR Model", C = txtSIR), 
              "Hospitalisation Model" =  
                list(txt = "The SIR Model was extended with a Hospitalisation 
                     compartment. Infected patients can either recover 
                     (becoming immune, R) or they can die (D). The population 
                     is stratified into young and old individuals 
                     The H compartment and the final states R and D are 
                     not stratified in order to reduce visual clutter.", C = txtHosp),
              "Extended Hospitalisation Model" =  
                list(txt = "The Hospitalisation Model was extended with 
                     an Exposed compartment which includes individuals 
                     who are infected, but not yet infectious 
                     (i.e. are in the incubation period and cannot infect 
                     other individuals).", C = txtHospEH),
              "Vaccination Model" = 
                list(txt = "The Hospitalisation model was extended with a 
                     Vaccination compartment, The population is stratified 
                     into young and old individuals (S, I, and V compartments). 
                     The final states H, R, and D are not stratified in order 
                     to reduce visual clutter. For a fully stratified population, 
                     see the next model.", C = txtV), 
              "Vaccine Stratified Model" = 
                list(txt = "This model is developed on the basis of the 
                     simple vaccination model, including an age stratification 
                     of most compartments with the exception of
                     the recovery compartment. ", C = txtVS), 
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
                list(txt = "The SIHRD compartments are stratified into 3 strata: 
                     children, young adults and old. The transmission rate is much 
                     higher between children (schools, kindergartens), then between 
                     adults. The transmission rates can be set using the sliders: 
                     between children-children, children-adults and adults-adults. 
                     The mortality and the hospitalization rate can be much higher 
                     in older persons than in young adults and can be set using the 
                     specific sliders.", C = txtAG3),
              "Analysis" = list(txt = "Analysis", C = NULL),
              "Display" = 
                list(txt = "Various filters are available in the Display drop-down combo-box. 
                     It is possible to display only the compartments for one of the population 
                     strata (either old age or young adults, for models stratified using these strata). 
                     The plots are less cluttered and finer details become visible. The Compact
                     filter removes the Total compartment and joins the Hospitalized strata and 
                     the Death strata into one H (All) and one D (All) compartments.", C = txtDisplay),
              "Sensitivity Analysis" = 
              list(txt = helpTipSensitivity(), C = NULL)
              )
  txtTitles = names(txtHelp)
  idActive = as.numeric(substr(id, 5, 6))
  txt = lapply(seq(length(txtTitles)), function(id){
    if(id == idActive){
      txtActive =  #paste(txtHelp[[id]]$txt, "<br>", 
                   paste("&nbsp;", txtHelp[[id]]$C, collapse = "<br>")
    } else {
      txtActive = ""
    }
    fluidRow(
      column(1, actionButton(paste0("Help", id), label = "", icon = icon("expand"))),
      column(10, HTML("<p> <b>", txtTitles[id], "</b> <br>",
                txtHelp[[id]]$txt, "<br>", txtActive ) ) )
                 # paste("&nbsp;", txtHelp[[id]]$C, collapse = "<br>"), "</p>") )
  })
  return(txt)
}

helpTipSensitivity = function(opt.brief = FALSE)
{
  if(opt.brief){
    txt = "A sequence of values between 0 and 1 is generated for the selected parameter. The Sensitivity analysis runs the simulation for each of these values. In the case of the infection rate: the values vary between 0.75x and 1.75x the current value of the infection rate. These limits are set as global options: opt.sensitivity.infect.min and opt.sensitivity.infect.max (see file Epidem.Options.R)."
    return(txt)
  }
  txt = "A sequence of values between 0 and 1 is generated 
      for the selected parameter. The Sensitivity analysis runs 
      the simulation for each of these values. In the case of 
      the infection rate: the values vary between 0.75x and 1.75x 
      the current value of the infection rate. These limits are set 
      as global options: opt.sensitivity.infect.min and 
      opt.sensitivity.infect.max (see file Epidem.Options.R)."
  return(txt)
}
