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
  values$outData = NULL;
  
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
    if(input$toggleB == FALSE)
      initSIR_Basic(custom, valTime)
    else
      diagram1();
  })
  
  ### Extended Model: Old Age + Hospital
  output$Hosp <- renderPlot({
    valTime = GetTime("H", "timeH");
    custom = list(infect = input$infectH,
                  recov = input$recovH, recov.h = input$recov.hH,
                  death = input$deathH, death.old = input$death.oH,
                  death.h = input$death.hH,
                  hosp = input$hospH, hosp.old = input$hosp.vH);
    # Page:
    if(input$toggleH == FALSE) {
      if(input$optSensitivity == "SIR") {
        outData = initSIR_Hosp(custom, valTime);
        values$outData = outData;
        plotSIR_Hosp(outData, flt=input$optType)
      } else {
        Sensitivity_Hosp(input$optSensitivity, custom, valTime, flt=input$optType);
      }
    } else
      diagram.H();
  })
  
  ### Vaccination
  output$Vacc = renderPlot({
    valTime = GetTime("V", "timeV");
    custom = list(infect = input$infectV,
                  recov.c = input$recovV, # c = community, h = hospital
                  recov.h = input$recov.hV,
                  hosp.y = input$hosp.yV,
                  hosp.o = input$hosp.vV,
                  vacc.y = input$vacc.yV / vaccine.rate.scale,
                  vacc.o = input$vacc.oV / vaccine.rate.scale,
                  death.y = input$deathV,
                  death.o = input$death.oV,
                  death.h = input$death.hV
                  )
    if(input$toggleV == FALSE){
      if(input$optSensitivityVacc == "Vacc") {
        outData = initSIR_Vaccine(custom, valTime)
        values$outData = outData;
        plotSIR_Vaccine(outData, flt = input$optTypeV)
      }else {
        idParam = match(input$optSensitivityVacc, c("vacc.y", "vacc.o"));
        max   = if(is.na(idParam)) 1 else 0.005;
        Sensitivity_Vaccine(input$optSensitivityVacc, custom, valTime, max=max, flt=input$optTypeV);
      } 
    }
    else
      diagram3(scaleX=0.9, scaleY=0.9)
    
  })
  
  
  output$VaccS = renderPlot({
    valTime = GetTime("VS", "timeVS");
    custom = list(
      infect = input$infectVS,
      recov.h = input$recov.hVS,
      recov.y = input$recov.yVS,
      recov.o = input$recov.oldVS, # aprox 0.14
      hosp.y = input$hosp.yVS,
      hosp.o = input$hosp.oldVS,
      vacc.o = input$vacc.oVS / vaccine.rate.scale,
      vacc.y = input$vacc.yVS / vaccine.rate.scale,
      death.y = input$death.yVS,
      death.o = input$death.oVS,
      death.hy = input$death.hyVS, 
      death.ho = input$death.hoVS 
      )
    
    if(input$toggleVS == FALSE) {
      if(input$optSensitivityVacc == "VaccStrat") {
        outData = initSIR_VaccineStrat(custom, valTime) 
        values$outData = outData;
        plotSIR_VaccineStrat(outData, flt = input$optTypeVS)
      } else {
        idParam = match(input$optSensitivityVaccStrat, c("vacc.y", "vacc.o"));
        max   = if(is.na(idParam)) 1 else 0.005;
        Sensitivity_VaccineStrat(input$optSensitivityVaccStrat, custom, valTime, max=max, flt=input$optTypeVS);
      }
    } else diagramVS(scaleX=0.4, scaleY=0.4)
    
    
  })
  
  ### Save Data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Results", ".csv", sep = "");
    },
    content = function(file) {
      # TODO: check first if NULL;
      write.csv(values$outData, file, row.names = FALSE)
    }
  )
  
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

