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


### UI
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
                           hr(),
                           
                           fluidRow(
                             column(4, sliderTime("timeH"),
                                    sliderBase("recovH", label="Rate of recovery"),
                                    sliderBase("recov.hH", label="Rate of recovery (Hosp)")),
                             column(4,
                                    sliderBase("infectH", label="Rate of infection", 0.25),
                                    sliderBase("death.oldH", label="Rate of death (Old)", opt$death.old),
                                    sliderBase("deathH", label="Rate of death (Young)", opt$death.young)),
                             column(4,
                                    sliderBase("hosp.vH", label="Rate of hospitalisation (Old)", opt$hosp.old),
                                    sliderBase("hospH", label="Rate of hospitalisation (Young)", opt$hosp.young),
                                    sliderBase("death.hH", label="Rate of death (Hosp)"))
                           )),
                  
                  tabPanel("Basic SIR model",
                           textOutput("BasicT"),
                           checkboxInput("toggle", "Toggle between diagram and plot", value=FALSE),
                           plotOutput("BasicPl"),
                           hr(),
                           
                           fluidRow(
                             column(4, sliderTime("timeB")),
                             column(4, sliderBase("infectB", label="Rate of infection", 0.25)),
                             column(4, sliderBase("recovB", label="Rate of recovery"))
                           )),
                  
                  tabPanel("Vaccination SIR model",
                           tabPanel("Vaccination SIR model",
                                    column(4, textOutput("txtVacc"),
                                           checkboxInput("toggleC","Toggle between plot and diagram")),
                                    column(4, selectInput("optTypeV", "Display", getDisplayTypesVacc(), selected="Old")),
                                    plotOutput("Vacc"),
                                    hr(), 
                           
                           fluidRow(
                             column(3, sliderTime("timeV"),
                                    sliderBase("recovV", label="Rate of recovery"),
                                    sliderBase("infectV", label="Rate of infection", 0.25)
                                    ),
                             column(3,
                                    sliderBase("deathV", label="Rate of death (Young)"),
                                    sliderBase("death.oV", label="Rate of death (Old)"),
                                    sliderBase("death.hV", label="Rate of death (Hosp)")),
                             column(3,
                                    sliderBase("hosp.yV", label="Rate of hospitalisation (Young)"),
                                    sliderBase("hosp.vV", label="Rate of hospitalisation (Old)"),
                                    sliderBase("recov.hV", label="Rate of recovery (Hosp)")
                                    ),
                             column(2,
                                    sliderBase("vacc.oV", label="Rate of vaccination (Young) ", max = 0.01, val = 0.001),
                                    sliderBase("vacc.yV", label="Rate of vaccination (Old) ", max = 0.01, val = 0.001))
                           ))),
                  
                  tabPanel("Vaccination Stratified Model",
                           tabPanel(
                              textOutput("txtVaccStratified"),
                              checkboxInput("toggleVS","Toggle between plot and diagram"),
                              plotOutput("VaccS"),
                              hr(),
                           
                           fluidRow(
                             column(4, sliderTime("timeVS"),
                                    sliderBase("infectVS", label="Rate of infection", 0.25),
                                    sliderBase("hosp.yVS", label="Rate of hospitalization (Young)"),
                                    sliderBase("hosp.oldVS", label="Rate of hospitalisation (Old)")
                                    ),
                                   # sliderBase("recov.hosp.yVS", label="Rate of recovery in hospitals (young)")),
                             column(3,
                                    sliderBase("recov.yVS", label="Rate of recovery (Young)"),
                                    sliderBase("recov.oldVS", label="Rate of recovery (Old)"),
                                    sliderBase("recov.hVS", label="Rate of recovery (Hosp)")
                                    ),
                             column(3,
                                    sliderBase("death.yVS", label="Rate of death (Young)"),
                                    sliderBase("death.oVS", label="Rate of death (Old)"),
                                    sliderBase("death.hyVS", label="Rate of death in hospital (Young)")
                                    #sliderBase("recov.hosp.oVS", label="Rate of recovery in hospitals (old)")
                                    ),
                             
                             column(3,
                                    sliderBase("death.hoVS", label="Rate of death in hospital (Old)"),
                                    sliderBase("vacc.yVS", label="Rate of vaccination (Young)", max = 0.01, val = 0.001,  step = 2E-4),
                                    sliderBase("vacc.oVS", label="Rate of vaccination (Old)", max = 0.01, val = 0.001, step = 2E-4)
                                    )
                             ))),
                
                  
                  
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
                )
))

##############

# to implement
# tabsets(with their own models and parameters)
# fancy(-er) display(plot up, params down)