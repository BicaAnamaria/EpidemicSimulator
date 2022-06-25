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
  output$txtHosp  = renderText("Complex model: includes a hospitalisation compartment")
  output$txtEH = renderText("Exposed Hospitalisation model: includes exposed compartment")
  output$txtVacc  = renderText("Complex model: includes a vaccination compartment")
  output$txtVaccStratified = renderText("Complex model: inlude age-stratified vaccination model")
  output$txtTwoVirus = renderText("Complex model: includes two viruses")
  output$txtAgeGroups3 = renderText("Complex model: includes age groups stratified model")
  
  values = reactiveValues();
  # active Tab
  values$Active = "H";
  values$outData = NULL;
  values$options = getOptions();
  
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
                  recov.c = input$recov.cH, recov.h = input$recov.hH,
                  death.y = input$death.yH, death.o = input$death.oH,
                  death.h = input$death.hH,
                  hosp.y = input$hosp.yH, hosp.o = input$hosp.oH);
    # Page:
    if(input$toggleH == FALSE) {
      if(input$optSensitivityH == "SIR") {
        outData = initSIR_Hosp(custom, valTime, options = values$options);
        values$outData = outData;
        plotSIR_Hosp(outData, flt=input$optType, options = values$options)
      } else {
        idParam = match(input$optSensitivityH, c("infect"));
        max = if(is.na(idParam)) 1 else opt0$sensitivity.infect.max * custom$infect;
        min = if(is.na(idParam)) 0 else opt0$sensitivity.infect.min * custom$infect;
        Sensitivity_Hosp(input$optSensitivityH, custom, valTime, min=min, max=max, 
                         flt=input$optType, options = values$options);
      }
    } else
      diagram.H();
  })
  
  
  ### Extended Hospital Model (+ Exposed Compartment)
  output$EH <- renderPlot({
    valTime = GetTime("EH", "timeEH");
    custom = list(infect = input$infectEH,
                  exposed.y = input$exposed.yEH,
                  exposed.o = input$exposed.oEH,
                  recov.c = input$recov.cEH, 
                  recov.h = input$recov.hEH,
                  death.y = input$death.yEH, 
                  death.o = input$death.oEH,
                  death.h = input$death.hEH,
                  hosp.y = input$hosp.yEH, 
                  hosp.o = input$hosp.oEH);
    # Page:
    if(input$toggleEH == FALSE) {
      if(input$optSensitivityEH == "SEIR") {
        outData = initSIR_EH(custom, valTime, options = values$options);
        values$outData = outData;
        plotSIR_EH(outData, flt=input$optTypeEH, options = values$options)
      } else {
        min = 0; max = 1;
        idParam = match(input$optSensitivityEH, c("infect", "exposed.y", "exposed.o"));
        if(!is.na(idParam)) {
          if(idParam == 1) {
            max = opt0$sensitivity.infect.max * custom$infect;
            min = opt0$sensitivity.infect.min * custom$infect;
          } else if(idParam == 2) { 
            max = opt0$sensitivity.infect.max * custom$exposed.y
            min = opt0$sensitivity.infect.min * custom$exposed.y;
          } else {
            max = opt0$sensitivity.infect.max * custom$exposed.o;
            min = opt0$sensitivity.infect.min * custom$exposed.o;
          }
        }
        
        Sensitivity_EH(input$optSensitivityEH, custom, valTime, min=min, max=max, 
                       flt=input$optTypeEH, options = values$options);
      }
    } else
      diagram.EH();
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
        outData = initSIR_Vaccine(custom, valTime, options = values$options)
        values$outData = outData;
        plotSIR_Vaccine(outData, flt = input$optTypeV, options = values$options)
      }else {
        idParam = match(input$optSensitivityVacc, c("vacc.y", "vacc.o"));
        max   = if(is.na(idParam)) 1 else 0.005;
        idParam = match(input$optSensitivityVacc, c("infect"));
        max   = if(is.na(idParam)) max else opt0$sensitivity.infect.max * custom$infect;
        min = if(is.na(idParam)) 0 else opt0$sensitivity.infect.min * custom$infect;
        Sensitivity_Vaccine(input$optSensitivityVacc, custom, valTime, min=min, max=max,
                            flt=input$optTypeV, options = values$options);
        }
    }
    else
      diagramV(scaleX=0.9, scaleY=0.9)
    
  })
  
  # Vaccination Stratified
  output$VaccVS = renderPlot({
    valTime = GetTime("VS", "timeVS");
    custom = list(
      infect = input$infectVS,
      recov.h = input$recov.hVS,
      recov.y = input$recov.yVS,
      recov.o = input$recov.oVS, # aprox 0.14
      hosp.y = input$hosp.yVS,
      hosp.o = input$hosp.oVS,
      vacc.o = input$vacc.oVS / vaccine.rate.scale,
      vacc.y = input$vacc.yVS / vaccine.rate.scale,
      death.y = input$death.yVS,
      death.o = input$death.oVS,
      death.hy = input$death.hyVS, 
      death.ho = input$death.hoVS 
      )
    
    if(input$toggleVS == FALSE) {
      if(input$optSensitivityVaccStrat == "VaccStrat") {
        outData = initSIR_VaccineStrat(custom, valTime, options = values$options) 
        values$outData = outData;
        plotSIR_VaccineStrat(outData, flt = input$optTypeVS, options = values$options)
      } else {
        idParam = match(input$optSensitivityVaccStrat, c("vacc.y", "vacc.o"));
        max   = if(is.na(idParam)) 1 else 0.005;
        idParam = match(input$optSensitivityVaccStrat, c("infect"));
        max   = if(is.na(idParam)) max else opt0$sensitivity.infect.max * custom$infect;
        min = if(is.na(idParam)) 0 else opt0$sensitivity.infect.min * custom$infect;
        Sensitivity_VaccineStrat(input$optSensitivityVaccStrat, custom, valTime, min=min, max=max,
                                 flt=input$optTypeVS, options = values$options);
      }
    } else diagramVS(scaleX=0.4, scaleY=0.4)
    
    
  })
  
  ### 2 Viruses
  output$Virus=renderPlot({
    valTime = input$time2V; # GetTime("2V", "time2V");
    custom = list(delayV2 = input$delayV2,
                  infectV1 = input$infectV1,
                  infectV2 = input$infectV2,
                  #infectV1V2 = input$infectV1V2,
                  #infectV2V1 = input$infectV2V1,
                  recovV1 = input$recovV1,
                  recovV2 = input$recovV2,
                  recovV1.h = input$recovV1.h,
                  recovV2.h = input$recovV2.h,
                  deathV1 = input$deathV1,
                  deathV2 = input$deathV2,
                  deathV1.h = input$deathV1.h,
                  deathV2.h = input$deathV2.h,
                  hospV1 = input$hospV1,
                  hospV2 = input$hospV2
                  
    )
    if(input$toggle2V == FALSE) {
      if(input$optSensitivity2V == "2V") {
        outData = initSIR_2Viruses(custom, valTime, options = values$options) 
        values$outData = outData;
        plotSIR_2Viruses(outData, flt = input$optType2V)
    }
      else{
        idParam = match(input$optSensitivity2V, c("infectV1", "infectV2"));
        if(is.na(idParam)) {
          max = 1
          min = 0
        } else if(idParam == 1){ 
          max = opt0$sensitivity.infect.max * custom$infectV1
          min = opt0$sensitivity.infect.min * custom$infectV1;
        } else {
          max = opt0$sensitivity.infect.max * custom$infectV2;
          min = opt0$sensitivity.infect.min * custom$infectV2;
        }
        Sensitivity_2Viruses(input$optSensitivity2V, custom, valTime, min=min, max=max, 
                             flt = input$optType2V, options = values$options);
      }
    } else diagram.2V(scaleX=0.3, scaleY=0.3)
  })
  
  ### Age Groups
  output$AgeGroupsModel = renderPlot({
    valTime = GetTime("AG3", "timeAG3");
    custom = list(infectAG3.cc = input$infectAG3.cc,
                  infectAG3.cn = input$infectAG3.cn,
                  infectAG3.nn = input$infectAG3.nn,
                  infectAG3.nc = input$infectAG3.nc,
                  recovAG3.c = input$recovAG3.c,
                  recovAG3.a = input$recovAG3.a,
                  recovAG3.o = input$recovAG3.o,
                  recovAG3.hc = input$recovAG3.hc,
                  recovAG3.ha = input$recovAG3.ha,
                  recovAG3.ho = input$recovAG3.ho,
                  deathAG3.c = input$deathAG3.c,
                  deathAG3.a = input$deathAG3.a,
                  deathAG3.o = input$deathAG3.o,
                  deathAG3.hc = input$deathAG3.hc,
                  deathAG3.ha = input$deathAG3.ha,
                  deathAG3.ho = input$deathAG3.ho,
                  hospAG3.c = input$hospAG3.c,
                  hospAG3.a = input$hospAG3.a,
                  hospAG3.o = input$hospAG3.o
                  
    )
    if(input$toggleAG3 == FALSE) {
      if(input$optSensitivityAG3 == "AG3") {
        outData = initSIR_AG3(custom, valTime, options = values$options) 
        values$outData = outData;
        plotSIR_AG3(outData, flt = input$optTypeAG3, options = values$options)
      } else{
        idParam = match(input$optSensitivityAG3, c("infectAG3.cc", "infectAG3.cn", 
                                                   "infectAG3.nc", "infectAG3.nn"));
        if(is.na(idParam)) {
          max = 1
          min = 0
        } else if(idParam == 1){ 
          max = opt0$sensitivity.infect.max * custom$infectAG3.cc
          min = opt0$sensitivity.infect.min * custom$infectAG3.cc;
        } else if(idParam == 2){
          max = opt0$sensitivity.infect.max * custom$infectAG3.cn;
          min = opt0$sensitivity.infect.min * custom$infectAG3.cn;
        } else if(idParam == 3){
          max = opt0$sensitivity.infect.max * custom$infectAG3.nc;
          min = opt0$sensitivity.infect.min * custom$infectAG3.nc;
        } else {
          max = opt0$sensitivity.infect.max * custom$infectAG3.nn;
          min = opt0$sensitivity.infect.min * custom$infectAG3.nn;
        }
        Sensitivity_AG3(input$optSensitivityAG3, custom, valTime, min=min, max=max, 
                        flt = input$optTypeAG3, options = values$options);
      }
    } else diagram.AG3(scaleX=0.2, scaleY=0.2)
  })
  
  output$doAnalysis = renderTable({
    summarySIR(values$outData);
  }, align = c('c'))
  
  output$doBasicStatistics = renderTable({
    summaryBasicSIR(values$outData);
  }, align = c('c'))
  
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
  
  ### Help
  idHelp = reactiveVal("Help1")
  output$HelpUI <- renderUI({
    helpEpidem(id = idHelp());
  })
  checkEpidem = function(sTag){
    if(idHelp() == sTag){
      idHelp("Help0")
    } else{
      idHelp(sTag);
    }
  }
  observeEvent(input$Help1, {
    checkEpidem("Help1")
  })
  observeEvent(input$Help2, {
    checkEpidem("Help2");
  })
  observeEvent(input$Help3, {
    checkEpidem("Help3");
  })
  observeEvent(input$Help4, {
    checkEpidem("Help4");
  })
  observeEvent(input$Help5, {
    checkEpidem("Help5");
  })
  observeEvent(input$Help6, {
    checkEpidem("Help6");
  })
  observeEvent(input$Help7, {
    checkEpidem("Help7");
  })
  observeEvent(input$Help8, {
    checkEpidem("Help8");
  })
  observeEvent(input$Help9, {
    checkEpidem("Help9");
  })
  observeEvent(input$Help10, {
    checkEpidem("Help10");
  })
}

