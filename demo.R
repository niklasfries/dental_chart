source('dental_chart.R')

## List of values per surface, one sublist per tooth with 5 values for molars 
## and 4 values for canines/incisors. 
## Quadrant order, i.e. top right, top left, bottom left, bottom right looking out of the mouth.
## Quadrants are ordered from center line, i.e. incisors to canines to molars.
## Surfaces are lexicographical order, i.e. buccal, distal, lingual, mesial (occlusive).
means = list(C11=c(C11B=0.184, C11D=0.333, C11L=0.199, C11M=0.321),
             C12=c(C12B=0.184, C12D=0.28, C12L=0.269, C12M=0.335),
             C13=c(C13B=0.172, C13D=0.272, C13L=0.173, C13M=0.22),
             C14=c(C14B=0.299, C14D=0.536, C14L=0.262, C14M=0.385, C14O=0.562),
             C15=c(C15B=0.257, C15D=0.571, C15L=0.238, C15M=0.482, C15O=0.603),
             C16=c(C16B=0.313, C16D=0.505, C16L=0.354, C16M=0.598, C16O=0.783),
             C17=c(C17B=0.291, C17D=0.323, C17L=0.237, C17M=0.455, C17O=0.701),
             C21=c(C21B=0.184, C21D=0.331, C21L=0.199, C21M=0.324),
             C22=c(C22B=0.188, C22D=0.284, C22L=0.272, C22M=0.341),
             C23=c(C23B=0.176, C23D=0.28, C23L=0.18, C23M=0.224),
             C24=c(C24B=0.286, C24D=0.539, C24L=0.257, C24M=0.387, C24O=0.565),
             C25=c(C25B=0.262, C25D=0.579, C25L=0.252, C25M=0.487, C25O=0.608),
             C26=c(C26B=0.317, C26D=0.537, C26L=0.375, C26M=0.6, C26O=0.786),
             C27=c(C27B=0.3, C27D=0.339, C27L=0.251, C27M=0.478, C27O=0.705),
             C31=c(C31B=0.0653, C31D=0.124, C31L=0.0659, C31M=0.127),
             C32=c(C32B=0.0665, C32D=0.109, C32L=0.0544, C32M=0.116),
             C33=c(C33B=0.113, C33D=0.112, C33L=0.0611, C33M=0.0957),
             C34=c(C34B=0.232, C34D=0.319, C34L=0.155, C34M=0.21, C34O=0.369),
             C35=c(C35B=0.25, C35D=0.506, C35L=0.207, C35M=0.317, C35O=0.527),
             C36=c(C36B=0.473, C36D=0.622, C36L=0.423, C36M=0.542, C36O=0.775),
             C37=c(C37B=0.355, C37D=0.349, C37L=0.309, C37M=0.538, C37O=0.732),
             C41=c(C41B=0.066, C41D=0.126, C41L=0.0659, C41M=0.126),
             C42=c(C42B=0.0679, C42D=0.111, C42L=0.0552, C42M=0.116),
             C43=c(C43B=0.111, C43D=0.113, C43L=0.0617, C43M=0.0982),
             C44=c(C44B=0.23, C44D=0.325, C44L=0.153, C44M=0.214, C44O=0.37),
             C45=c(C45B=0.248, C45D=0.512, C45L=0.206, C45M=0.328, C45O=0.53),
             C46=c(C46B=0.471, C46D=0.606, C46L=0.398, C46M=0.557, C46O=0.773),
             C47=c(C47B=0.35, C47D=0.339, C47L=0.289, C47M=0.531, C47O=0.727))         

## Convert to colors for visualization
col = lapply(means, function(x) {
  a = rgb(1, 1 - x, 1 - x)
  names(a) = names(x) ## Preserve 
  return(a)
})

## Convert to row-wise order, starting top left looking out of the mouth.
ord = c(14:8, 1:7, 21:15, 22:28)
col = col[ord]

dental_chart(col, main='Ratio of participants with affected surface (white=0, red=1)')
