# Fire effects on methane and carbon dioxide fluxes in the Yukon-Kuskokwim Delta of Alaska
## Polaris Project, Anneka Williams 2019

  This project is led by Anneka Williams from the 2019 Polaris Project expedition. The Arctic is warming at an accelerated rate and wildfire frequency and severity are increasing.
The Yukon-Kuskokwim Delta (YKD) of Alaska is a subarctic tundra wetland region with discontinuous permafrost. In 2015, more area burned in the YKD than the previous several decades combined. Wildfire can have longterm indirect consequences on carbon biogeochemical cycling through thawing permafrost, changes in coupled nutrient cycling, effects on microbial communities,altering the source of carbon substrates available for decomposition, and changing the vegetation community. During the 2015 wildfire, the abundant fen systems in the YKD were wet enough to often served as fire breaks or remain unburned despite tundra burning on either side. Though fens were rarely burned directly, their watersheds integrate the effects of fire through hydrologic inputs. Plant-mediated methane flux can be a significant source of methane emissions, as arenchyma act as straws venting methane and bypassing oxygenated soil and moss layers where methanotrophy is active. This project asks the questions: How does fire effect carbon dioxide and methane fluxes from sedge (Eriophorum aquatilus) fen ecosystems? What role does plant-mediated fluxes play in total sedge fen fluxes?

  Methane and carbon dioxide fluxes were measured using a Los Gatos Ultraportable Greenhouse Gas Analzyer. Three sites were chosen in an unburned fen and three in a burned fen of similarly sized watersheds. Each siste was selected to have dominant Eriophorum aquatilus with sphganum spcs. as the bed. Within each site, one location was treated as the control (uncut), one location all Eriophorum were clipped to several cm height above the moss layer, and one location was clipped and each stem was sealed with vaseline. Fluxes were measured as the linear increase in gas concentrations over time for a period of 5-10 minutes, using a clear chamber and a permanently installed base to seal to the ground. Fluxes were measured on five dates during the first two weeks of July 2019. Soil smaples were collected from each site and incubated under room temperature conditions with a set serving as a control, and an additional site amended with a labile carbon substrate. Potential methane and carbon dioxide fluxes were measured from these incubations 1 day after treatment.
  
### Data files:
Field fluxes are contained in NEWfieldfluxdata4.csv
Flux units are in umol/m2/s
Incubation fluxes are contained in NEWincubationdata.csv
Flux units are in umol/g-soil/day

### Prerequisites
R version 3.6.1
Packages used include:
tidyverse
lme4
lmerTest
emmeans
dplyr

### Use
This R script is designed to be an educational walk-through to analyze and plot the data from this project. Includes annotations explaining the use of linear mixed-effects models, ANOVAs, and ggplot.
