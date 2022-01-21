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
  output$VirusT = renderText("A model showcasing the spread of two viruses over time")
  output$BasicT = renderText("Basic Model: (S)usceptible (I)nfectious and (R)emoved model")
  output$HospT  = renderText("Complex model: includes a hospitalization compartment")
  output$VaccT  = renderText("Complex model: includes a vaccination compartment")
  output$TwoVirus = renderText("Complex model: includes two viruses")
  
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
      updateNumericInput(inputId=idInput, value=valTime);
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
    custom = list(infect=input$infectH,
                  recov=input$recovH, recov.h=input$recov.hH,
                  death=input$deathH, death.old=input$death.oldH,
                  death.h=input$death.hH,
                  hosp=input$hospH, hosp.old=input$hosp.vH);
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
    
    custom = list(infect=input$infectV,
                  recov=input$recovV,
                  recov.hosp=input$recov.hV,
                  death=input$deathV,
                  death.hosp=input$death.hV,
                  hosp=input$hospV,
                  hosp.vacc=input$hosp.vV,
                  vacc.old=input$vacc.oV,
                  vacc.young=input$vacc.yV,
                  death.old=input$death.oV,
                  death.oldhosp=input$death.ohV)
    if(input$toggleC==FALSE)
      initSIR_Vaccine(custom, input$timeV, flt=input$optTypeV)
    else
      diagram3(scaleX=0.9, scaleY=0.9)
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

