# Assuming df has columns: pid, picom_index, aff_pol, pc_type

# Create interaction term
df <- df |> 
  mutate(pc_type2 = ifelse(pc_type <2, 0, 1),
         int = picom_z * populism_index,
         int2 = picom_z * pc_type2,
         pc_type2 = factor(pc_type2))

# Define the moderated mediation model
medmod <- '
  # Mediator
  picom_z ~ a1*pid + a2*pc_type2  + country +gender + age + educ
  
  # Moderator effect on the mediator
  int2 ~ a3*pc_type2  + country +gender + age + educ
  
  # Outcome
  affpol_z ~ b1*picom_index + b2*pid + b3*int2  + country +gender + age + educ
  
  # Indirect effect
  ab := a1*b1 + a2*b3
  
  # Total effect
  total := b2 + (a1*b1) + (a2*b3)
'

# Fit the model
m5 <- lavaan::sem(medmod, data = df)

# View the summary of the model
summary(m5, fit.measures = TRUE)

coefs <- lavaan::parameterEstimates(m5, standardized = TRUE)
paths <- coefs[coefs$op %in% c("~", ":="), c("lhs", "op", "rhs", "est", "se","pvalue")]

# Ensure paths has the correct structure and length
if (nrow(paths) < 4) {  # Updated to check for 4 rows (including interaction term)
  stop("Not enough paths extracted. Check model and data.")
}

# Format coefficients to two decimal places with significance markers and standard errors
formatted_paths <- sapply(seq_len(nrow(paths)), function(i) {
  estimate <- format(round(paths$est[i], 2), nsmall = 2)
  stderr <- format(round(paths$se[i], 2), nsmall = 2)
  pvalue <- paths$pvalue[i]
  significance <- ifelse(pvalue < 0.05, "*", "")  # Mark as significant if p < 0.05
  sprintf("%s (%s)%s", estimate, stderr, significance)
})


# Create the Graphviz dot code with interaction term included
graph <- paste(
      'digraph {',
      '  graph [rankdir=TB, splines=polyline, ranksep=1.0, nodesep=0.5, bgcolor=white];',
      '  node [shape=box, style=filled, fillcolor=gray95, fontname="Arial Narrow", fontsize=10];',
      '  edge [color=black, fontname="Arial Narrow", fontsize=10];',
      '',
      '  // Define nodes',
      '  pid [label="PC", pos="0,0!"];',
      '  aff_pol [label="Affective Polarization", pos="1,0!"];',
      '  picom_index [label="Mediation: Conspiracy Mentality", pos="0.5,1!"];',
      '  picom_index_pc_type [label="Moderation: Populist PC", pos="0.5,0.5!"];',
      '',
      '  // Define edges with formatted effect sizes',
      sprintf('  pid -> picom_index [label="%s"];', formatted_paths[1]),
      sprintf('  picom_index -> aff_pol [label="%s"];', formatted_paths[2]),
      sprintf('  pid -> aff_pol [label="%s"];', formatted_paths[3]),
      sprintf('  picom_index_pc_type -> aff_pol [label="%s"];', formatted_paths[4]),
      '',
      '  // Create ranks',
      '  { rank = same; pid; aff_pol; }',
      '  { rank = min; picom_index; picom_index_pc_type; }',
      '}',
      sep = "\n"
)


# Plot the graph
grViz(graph)
