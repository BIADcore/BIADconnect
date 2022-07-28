#------------------------------------------------------------------
# Example R script for plotting database relationships between tables
#------------------------------------------------------------------
# Created: 28.07.2022
# Last modified: 28.07.2022

library(DiagrammeR)
test<-DiagrammeR::grViz("
digraph {
#data tables
  node [shape = circle
  style = filled,
  color = orange,
  fixedsize = true,
  width = 2.2,
  fontsize = 15]
  Citations; Sites; Phases; PhaseCitation; PhaseType; Graves; GraveIndividuals;
  Strontium; ABotIsotopes; ABotSamples; ABotPhases; Items; Health; C14Samples;
  FaunalBones; FaunalBiometrics; FaunalIsotopes; FaunalIsotopeSequences;
  FaunalSpecies; StrontiumEnvironment; Contexts; HumanIsotopes;

#list tables
  node [shape = box
  style = filled,
  color = lightblue
  fixedsize = true,
  width = 3.0
  fontsize = 15]
  zoptions_ABotAnatomy; zoptions_ABotTaxaList; zoptions_ABot_Preservation;
  zoptions_ABotRecoveryMethods; zoptionsC14laboratories;
  zoptions_FaunalMeasurement; zoptions_SkeletalElements; zoptions_Countries;
  zoptions_Types; zoptions_Periods; zoptions_Cultures; zoptions_Health; 
  zoptions_AgeCategorical; zoptions_GraveConstruction; zoptions_FaunalTaxaList

  edge [color = dimgray]  
Citations -> Sites -> Phases -> Graves
Graves -> GraveConstruction -> GraveIndividuals -> Strontium
Phases -> ABotIsotopes
Phases -> ABotSamples
Phases -> ABotPhases
ABotIsotopes -> Items
Graves -> Items
Citations -> Graves
GraveIndividuals -> Items
Citations -> GraveIndividuals 
GraveIndividuals -> Health
PhaseCitation -> Phases
PhaseType -> Phases
Phases -> C14Samples
C14Samples -> Items
GraveIndividuals -> C14Samples
Citations -> C14Samples
Phases -> FaunalBones
FaunalBones -> FaunalBiometrics
Phases -> FaunalIsotopes
FaunalIsotopes -> Items
Citations -> FaunalIsotopes 
FaunalIsotopes -> FaunalIsotopeSequences
FaunalIsotopes -> Items
Phases -> FaunalSpecies
Strontium -> Items
Sites -> StrontiumEnvironment
zoptions_ABotAnatomy -> ABotIsotopes
zoptions_ABotTaxaList -> ABotIsotopes
zoptions_ABot_Preservation -> ABotIsotopes
zoptions_ABotRecoveryMethods -> ABotPhases
zoptions_ABotAnatomy -> ABotSamples
zoptions_ABotTaxaList -> ABotSamples
zoptions_ABot_Preservation -> ABotSamples
zoptionsC14laboratories -> C14Samples
Phases -> Contexts
zoptions_FaunalMeasurement -> FaunalBiometrics 
zoptions_FaunalTaxaList -> FaunalBones
zoptions_SkeletalElements -> FaunalBones
zoptions_FaunalTaxaList -> FaunalIsotopes
zoptions_SkeletalElements -> FaunalIsotopes
zoption_FaunalTaxaList-> FaunalSpecies
zoptions_GraveConstruction -> GraveConstruction
zoptions_Cultures -> GraveIndividuals
zoptions_AgeCategorical -> GraveIndividuals
Citations -> Health
zoptions_Health -> Health
GraveIndividuals -> HumanIsotopes
HumanIsotopes -> Items
zoptions_SkeletalElements -> HumanIsotopes
Citations -> PhaseCitation
zoptions_Cultures -> Phases
zoptions_Periods -> Phases
zoptions_Types -> PhaseType
zoptions_Countries -> Sites
Citations -> Strontium
zoptions_SkeletalElements -> Strontium
Citations -> StrontiumEnvironment
subgraph cluster {
node [shape = circle
  style = filled,
  color = orange,
  fixedsize = true,
  width = 1,
  fontsize = 10]
  DataTable
  node [shape = box
  style = filled,
  color = lightblue
  fixedsize = true,
  width = 1
  fontsize = 10]
  List
}
}
")
export_svg(test) %>%
  charToRaw %>%
  rsvg_png("../tools/plots/database.foreign.keys.png")
