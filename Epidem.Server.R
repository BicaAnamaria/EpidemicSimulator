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

################

### Server
server <- function(input, output){
  output$txtVirus = renderText("A model showcasing the spread of two viruses over time")
  output$txtBasic = renderText("Basic Model: (S)usceptible (I)nfectious and (R)emoved model")
  output$txtHosp  = renderText("Complex model: includes a hospitalization compartment")
  output$txtVacc  = renderText("Complex model: includes a vaccination compartment")
  output$txtVaccStratified = renderText("Complex model: inlude age-stratified vaccination model")
  output$txtTwoVirus = renderText("Complex model: includes two viruses")
  
  values = reactiveValues();
  # active Tab
  values$Active = "H";
  
  # synchronise times across pages
  GetTime = function(type, idInput) {
    if(values$Active == type) {
      valTime = input[[idInput]];
      values$time = valTime;
    } else {
      values$Active = type;
      valTime = values$time;
      updateNumericInput(inputId = idInput, value = valTime);
    }
    return(valTime);
  }
  
  ####################
  
  ### SIR: Basic Model
  output$BasicPl = renderPlot({
    valTime = GetTime("B", "timeB");
    custom = c(input$infectB, input$recovB)
    if(input$toggle == FALSE)
      initSIR_Basic(custom, valTime)
    else
      diagram1();
  })
  
  ### Extended Model: Old Age + Hospital
  output$Hosp <- renderPlot({
    valTime = GetTime("H", "timeH");
    custom = list(infect = input$infectH,
                  recov = input$recovH, recov.h = input$recov.hH,
                  death = input$deathH, death.old = input$death.oldH,
                  death.h = input$death.hH,
                  hosp = input$hospH, hosp.old = input$hosp.vH);
    # Page:
    if(input$toggleH == FALSE) {
      if(input$optSensitivity == "SIR") {
        initSIR_Hosp_Com(custom, valTime, flt=input$optType);
      } else {
        Sensitivity_Hosp_Com(input$optSensitivity, custom, valTime, flt=input$optType);
      }
    } else
      diagram.H();
  })
  
  ### Vaccination
  # TODO
  output$Vacc = renderPlot({
    ##custom = c(input$infectV,
    #input$recovV,
    #input$recov.hV,
    #input$deathV,
    #input$death.hV,
    #input$hospV,
    #input$hosp.vV,
    #input$vacc.oV,
    #input$vacc.yV,
    #input$death.oV,
    #input$death.ohV)
    valTime = GetTime("V", "timeV");
    custom = list(infect = input$infectV,
                  recov = input$recovV,
                  recov.hosp = input$recov.hV,
                  death = input$deathV,
                  death.hosp = input$death.hV,
                  hosp.y = input$hosp.yV,
                  hosp.vacc = input$hosp.vV,
                  vacc.young = input$vacc.yV / vaccine.rate.scale,
                  vacc.old = input$vacc.oV / vaccine.rate.scale,
                  death.old = input$death.oV,
                  death.oldhosp = input$death.ohV)
    if(input$toggleC == FALSE)
      initSIR_Vaccine(custom, input$timeV, flt = input$optTypeV)
    else
      diagram3(scaleX=0.9, scaleY=0.9)
  })
  
  
  output$VaccS = renderPlot({
    valTime = GetTime("VS", "timeVS");
    custom = list(
      infect = input$infectVS,
      recov.h = input$recov.hVS,
      recov.y = input$recov.yVS,
      recov.old = input$recov.oldVS, # aprox 0.14
      # recov.hosp.y = input$recov.hosp.yVS, # aprox 0.07
      # recov.hosp.o = input$recov.hosp.oVS,
      hosp.y = input$hosp.yVS,
      hosp.old = input$hosp.oldVS,
      # hosp.vaccY = input$hosp.vaccYVS, nu e in model
      # hosp.vaccOld = input$hosp.vaccOldVS,
      vacc.old = input$vacc.oVS / vaccine.rate.scale,
      vacc.y = input$vacc.yVS / vaccine.rate.scale,
      death.y = input$death.yVS,
      death.old = input$death.oVS,
      death.hosp.y = input$death.hyVS, # in ui death.hosp doar in slidere(UI)
      death.hosp.o = input$death.hoVS # la ahitectura -> server/ui; model -> ec dif(tot ce e cu d)/ init
      )
    
    if(input$toggleVS == FALSE)
      initSIR_VaccineStrat(custom, input$timeVS, flt = input$optTypeVS)
    else diagramVS(scaleX=0.4, scaleY=0.4)
    
    
  })
  
  ### 2 Viruses
  output$Virus=renderPlot({
    custom = list(infect = input$infect2V,
                   recov = input$recov2V,
                   death=input$death2V,
                   death.hosp=input$death.h2V,
                   hosp=input$hosp2V,
                   death.old=input$death.o2V,
                   death.oldhosp=input$death.oh2V
    )
    TV3D(input$iters, 2)
  })
}

