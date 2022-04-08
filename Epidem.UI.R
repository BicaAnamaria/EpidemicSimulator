
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
###
### based of:
### Team Project 2021
### Students:
###   Dora Calea, Ioana Obreja,
###   Liviu Sopon and Dragos Ursan
###   West University, Timisoara
###
### Supervisor: Leonard Mada
### Syonic SRL
###
### L. Mada: modified/improved;


### draft v.0.2a
# - split into modules;


#######################

# library(shiny)
# library(shinyjs)

# library(rsconnect)


### Options
opt = list(
  H = list(hosp.young = 0.01, hosp.old = 0.1, death.young = 0.001, death.old = 0.05, death.h = 0.07),
  V = list(hosp.young = 0.01, hosp.old = 0.1, death.young = 0.001, death.old = 0.05, death.h = 0.07)
);



### Helper Functions
sliderTime = function(id, label = "Number of days", val = 200, max = 720, step = 1) {
  sliderInput(inputId = id, label = label,
              value = val, min = 1, max = max, step = step)
}
sliderBase = function(id, label, val = 0.05, min = 0, max = 0.8, step = 0.001) {
  sliderInput(inputId = id, label=label,
              value = val, min = min, max = max, step = step)
}

vaccine.rate.scale = 100;
sliderVaccine = function(id, label, val = 0.001, min = 0, max = 0.01, step = 0.0002, scale=vaccine.rate.scale) {
  sliderInput(inputId = id, label=label,
              value = val * scale, min = min * scale, max = max * scale, step = step * scale);
}


### UI
### H = Hospital
ui <- fluidPage("Epidemic Simulation", useShinyjs(),
                tabsetPanel(
                  tabPanel("Hospitalization SIR model",
                           fluidRow(
                             column(4, textOutput("txtHosp"),
                                    checkboxInput("toggleH", "Toggle between plot and diagram")), 
                             column(4, selectInput("optType", "Display", getDisplayTypes(), selected="Old")),
                             column(4, selectInput("optSensitivityH", "Sensitivity Analysis", getSensitivity_Hosp(), selected="SIR"))
                           ),
                           plotOutput("Hosp"),
                           # hr(),
                           
                           fluidRow(
                             column(4, sliderTime("timeH"),
                                    sliderBase("recov.cH", label="Recovery rate"),
                                    sliderBase("recov.hH", label="Recovery rate (Hosp)")
                             ),
                             column(4,
                                    sliderBase("infectH", label="Infection rate", 0.25),
                                    sliderBase("hosp.oH", label="Hospitalization rate (Old)", opt$H$hosp.old),
                                    sliderBase("hosp.yH", label="Hospitalization rate (Young)", opt$H$hosp.young)
                             ),
                             column(4,
                                    sliderBase("death.oH", label="Death rate (Old)", opt$H$death.old),
                                    sliderBase("death.yH", label="Death rate (Young)", opt$H$death.young),
                                    sliderBase("death.hH", label="Death rate (Hosp)", opt$H$death.h)
                             )
                           )),
                  
                  tabPanel("Basic SIR model",
                           textOutput("BasicT"),
                           checkboxInput("toggleB", "Toggle between diagram and plot", value=FALSE), 
                           plotOutput("BasicPl"),
                           hr(),
                           
                           fluidRow(
                             column(4, sliderTime("timeB")),
                             column(4, sliderBase("infectB", label="Infection rate", 0.25)),
                             column(4, sliderBase("recovB", label="Recovery rate"))
                           )),
                  
                  tabPanel("Vaccination SIR model",
                           fluidRow(
                             column(4, textOutput("txtVacc"),
                                    checkboxInput("toggleV","Toggle between plot and diagram")), 
                             column(4, selectInput("optTypeV", "Display", getDisplayTypesVacc(), selected="Old")),
                             column(4, selectInput("optSensitivityVacc", "Sensitivity Analysis", getSensitivityVacc(), selected="Vacc"))),
                           plotOutput("Vacc"),
                           #hr() 
                           
                           
                           fluidRow(
                             column(3, sliderTime("timeV"),
                                    sliderBase("recovV", label="Recovery rate"),
                                    sliderBase("recov.hV", label="Recovery rate (Hosp)")
                             ),
                             column(3,
                                    sliderBase("infectV", label="Infection rate", 0.25),
                                    sliderBase("hosp.vV", label="Hospitalization rate (Old)", opt$V$hosp.old),
                                    sliderBase("hosp.yV", label="Hospitalization rate (Young)", opt$V$hosp.young)
                             ),
                             column(3,
                                    sliderBase("death.oV", label="Death rate (Old)", opt$V$death.old),
                                    sliderBase("deathV", label="Death rate (Young)", opt$V$death.young),
                                    sliderBase("death.hV", label="Death rate (Hosp)", opt$V$death.h)),
                             column(3,
                                    sliderVaccine("vacc.oV", label="Vaccination rate (Old%) "),
                                    sliderVaccine("vacc.yV", label="Vaccination rate (Young%) ")
                             )
                           )),
                  
                  tabPanel("Vaccination Stratified Model",
                           fluidRow(
                             column(3, textOutput("txtVaccStratified"),
                                    checkboxInput("toggleVS","Toggle between plot and diagram")),
                             column(3, selectInput("optTypeVS", "Display", getDisplayTypesVaccStrat(), selected="Old")), 
                             column(3, selectInput("optSensitivityVaccStrat", "Sensitivity Analysis", getSensitivityVaccStrat(), selected="VaccStrat")) ),
                           plotOutput("VaccVS"),
                           # hr() 
                           
                           
                           fluidRow(
                             column(3, sliderTime("timeVS"),
                                    sliderBase("infectVS", label="Infection rate", 0.25),
                                    sliderBase("hosp.oVS", label="Hospitalization rate (Old)"),
                                    sliderBase("hosp.yVS", label="Hospitalization rate (Young)")
                             ),
                             # sliderBase("recov.hosp.yVS", label="Rate of recovery in hospitals (young)")),
                             column(3,
                                    sliderBase("recov.oVS", label="Recovery rate (Old)"),
                                    sliderBase("recov.yVS", label="Recovery rate (Young)"),
                                    sliderBase("recov.hVS", label="Recovery rate (Hosp)")
                             ),
                             column(3,
                                    sliderBase("death.oVS", label="Death rate (Old)"),
                                    sliderBase("death.yVS", label="Death rate (Young)"),
                                    sliderBase("death.hyVS", label="Death rate in hospital (Young)")
                                    #sliderBase("recov.hosp.oVS", label="Rate of recovery in hospitals (old)")
                             ),
                             
                             column(3,
                                    sliderBase("death.hoVS", label="Death rate in hospital (Old)"),
                                    sliderVaccine("vacc.oVS", label="Vaccination rate (Old%)"), #, max = 0.01, val = 0.001,  step = 2E-4)
                                    sliderVaccine("vacc.yVS", label="Vaccination rate (Young%)" ) #, max = 0.01, val = 0.001,  step = 2E-4),
                             )
                           )),
                  
                  
                  
                  
                  tabPanel("Two Viruses",
                           fluidRow(
                             column(4, textOutput("VirusT"),
                                    checkboxInput("toggle2V","Toggle between plot and diagram")),
                             column(4, selectInput("optType2V", "Display", getDisplayTypes2V(), selected="Compact")),
                             column(4, selectInput("optSensitivity2V", "Sensitivity Analysis", getSensitivity2V(), selected="2V")) ,
                             #column(4, selectInput("optSensitivityH", "Sensitivity Analysis", getSensitivity_Hosp(), selected="SIR"))
                           ),
                           plotOutput("Virus"),
                           
                           
                           column(3, sliderTime("time2V", label = "Time", 400),
                                  sliderTime("delayV2", label = "Time delay", 110, max = 200),
                                  sliderBase("recovV1", label="Recovery rate (Virus 1)"),
                                  sliderBase("recovV2", label="Recovery rate (Virus 2)")
                           ),
                           
                           column(3,
                                  sliderBase("infectV1", label="Infection rate (Virus 1)", 0.15),
                                  sliderBase("infectV2", label="Infection rate (Virus 2)", 0.25),
                                  sliderBase("recovV1.h", label="Recovery rate (Hosp, Virus 1)", 0.1),
                                  sliderBase("recovV2.h", label="Recovery rate (Hosp, Virus 2)", 0.1),
                           ),
                           column(3,
                                  sliderBase("hospV1", label="Hospitalization rate (Virus 1)"),
                                  sliderBase("hospV2", label="Hospitalization rate (Virus 2)")
                           ),
                           column(3,
                                  #sliderBase("infectV1V2", label="Infection rate V1V2"),
                                  #sliderBase("infectV2V1", label="Infection rate V1V2"),
                                  sliderBase("deathV1", label="Death rate (Virus 1)", 0.1),
                                  sliderBase("deathV2", label="Death rate (Virus 2)", 0.15),
                                  sliderBase("deathV1.h", label="Death rate (Hosp, Virus 1)"),
                                  sliderBase("deathV2.h", label="Death rate (Hosp, Virus 2)") 
                           )
                           
                           
                  ),
                  
                  tabPanel("Age Groups Stratified Model",
                           column(4, textOutput("txtAgeGroups3"),
                                  checkboxInput("toggleAG3","Toggle between plot and diagram"),),
                           column(4, selectInput("optTypeAG3", "Display", getDisplayTypesAG3(), selected="Adults")),
                           column(4, selectInput("optSensitivityAG3", "Sensitivity Analysis", getSensitivityAG3(), selected="AG3")) ,
                           plotOutput("AgeGroupsModel"),
                           
                           fluidRow(
                             column(3, 
                                    sliderTime("timeAG3"),
                                    sliderBase("infectAG3.cc", label="Infection rate btw children", 0.35),
                                    sliderBase("infectAG3.cn", label="Infection rate btw children and others", 0.15),
                                    sliderBase("infectAG3.nn", label="Infection rate btw others and others", 0.15),
                                    sliderBase("infectAG3.nc", label="Infection rate btw others and children", 0.15),
                                    
                             ),
                             column(3,
                                    sliderBase("recovAG3.c", label="Recovery rate children"),
                                    sliderBase("recovAG3.a", label="Recovery rate adults"),
                                    sliderBase("recovAG3.o", label="Recovery rate elders"),
                                    sliderBase("recovAG3.hc", label="Recovery rate children (Hosp)"),
                                    sliderBase("deathAG3.hc", label="Death rate children(Hosp)")
                                    
                             ),
                             column(3,
                                    sliderBase("deathAG3.c", label="Death rate children"),
                                    sliderBase("deathAG3.a", label="Death rate adults"),
                                    sliderBase("deathAG3.o", label="Death rate elders"),
                                    sliderBase("recovAG3.ha", label="Recovery rate adults (Hosp)"),
                                    sliderBase("deathAG3.ha", label="Death rate adults(Hosp)")
                                    
                             ),
                             
                             column(3,
                                    sliderBase("hospAG3.c", label="Hospitalization rate children"),
                                    sliderBase("hospAG3.a", label="Hospitalization rate adults"),
                                    sliderBase("hospAG3.o", label="Hospitalization rate elders"),
                                    sliderBase("recovAG3.ho", label="Recovery rate elders (Hosp)"),
                                    sliderBase("deathAG3.ho", label="Death rate elders(Hosp)")
                                    
                                    
                             )
                             
                           )
                  ), 
                  
                  
                  tabPanel("Analysis",
                           tableOutput("doStatistics"),
                           downloadButton("downloadData", "Download")
                  ),
                  
                  tabPanel("Help",
                         fluidRow( 
                           column(1, div(HTML("&nbsp;"))),
                           
                           column(10, helpEpidem()) )   
                  )
                ))

##############

# to implement
# tabsets(with their own models and parameters)
# fancy(-er) display(plot up, params down)