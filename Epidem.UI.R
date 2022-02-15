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
opt = list(hosp.young = 0.01, hosp.old = 0.1, death.young = 0.001, death.old = 0.05);



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
                             column(4, selectInput("optSensitivity", "Sensitivity Analysis", getSensitivity(), selected="SIR"))
                           ),
                           plotOutput("Hosp"),
                           # hr(),
                           
                           fluidRow(
                             column(4, sliderTime("timeH"),
                                    sliderBase("recovH", label="Recovery rate"),
                                    sliderBase("recov.hH", label="Recovery rate (Hosp)")
                                    ),
                             column(4,
                                    sliderBase("infectH", label="Infection rate", 0.25),
                                    sliderBase("hosp.vH", label="Hospitalization rate (Old)", opt$hosp.old),
                                    sliderBase("hospH", label="Hospitalization rate (Young)", opt$hosp.young)
                                   ),
                             column(4,
                                    sliderBase("death.oldH", label="Death rate (Old)", opt$death.old),
                                    sliderBase("deathH", label="Death rate (Young)", opt$death.young),
                                    sliderBase("death.hH", label="Death rate (Hosp)")
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
                                    sliderBase("hosp.vV", label="Hospitalization rate (Old)"),
                                    sliderBase("hosp.yV", label="Hospitalization rate (Young)")
                             ),
                             column(3,
                                    sliderBase("death.oV", label="Death rate (Old)"),
                                    sliderBase("deathV", label="Death rate (Young)"),
                                    sliderBase("death.hV", label="Death rate (Hosp)")),
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
                            plotOutput("VaccS"),
                              # hr() 
                              
                           
                           fluidRow(
                             column(3, sliderTime("timeVS"),
                                    sliderBase("infectVS", label="Infection rate", 0.25),
                                    sliderBase("hosp.oldVS", label="Hospitalization rate (Old)"),
                                    sliderBase("hosp.yVS", label="Hospitalization rate (Young)")
                                    ),
                                   # sliderBase("recov.hosp.yVS", label="Rate of recovery in hospitals (young)")),
                             column(3,
                                    sliderBase("recov.oldVS", label="Recovery rate (Old)"),
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
                           textOutput("VirusT"),
                           plotOutput("Virus"),
                           hr(),
                           
                           fluidRow(
                             column(12,
                                    sliderInput(inputId = "iters", label="Number of iterations",
                                                value=1, min=1, max=720, step=1,
                                                animate = animationOptions(interval = 250, loop = FALSE))           
                             ))
                ),
                
                tabPanel("Download",
                         downloadButton("downloadData", "Download")
                )
))

##############

# to implement
# tabsets(with their own models and parameters)
# fancy(-er) display(plot up, params down)