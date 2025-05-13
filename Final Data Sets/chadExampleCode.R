# Load required packages
library(multinet)
library(dplyr)

#0. load data
load("Final Data Sets/nomineeNominatorDetailedData.Rdata")
load("Final Data Sets/prizesAwarded.RData")
data <- detailedData



#0.5 add laureatte data
laureate_names <- prizesWithLaureates$knownName$en
clean <- function(x) gsub("[[:blank:]]", "",trimws(tolower(x)))


data$nomineeLaureatte <- clean(data$Nominee_Name) %in% clean(laureate_names)
data$nominatorLaureatte <- clean(data$Nominator_Name) %in% clean(laureate_names)





# 1. Create empty multilayer network and add layers
net <- ml_empty()
add_layers_ml(net, c("nominator", "nominee"), directed = TRUE)

# 2. Prepare and add vertices
v_nominator <- data.frame(
  actor = data$Nominator_Name,
  layer = "nominator"
)

v_nominee <- data.frame(
  actor = data$Nominee_Name,
  layer = "nominee"
)

vertices <- bind_rows(v_nominator, v_nominee) %>%
  filter(!is.na(actor)) %>%   # Remove NA names if any
  distinct(actor, layer)

add_vertices_ml(net, vertices)

# 3. Prepare and add interlayer edges (from nominator to nominee)
edges <- data.frame(
  actor1 = data$Nominator_Name,
  layer1 = "nominator",
  actor2 = data$Nominee_Name,
  layer2 = "nominee"
) %>%
  filter(!is.na(actor1) & !is.na(actor2))  # Remove NA edges

add_edges_ml(net, edges)

# 4. Add actor-level attributes
# Combine and deduplicate attributes for each actor
combined_attr <- bind_rows(
  data %>%
    select(actor = Nominator_Name,
           gender = Nominator_Gender,
           birth = Nominator_Birth,
           death = Nominator_Death,
           country = Nominator_Country),
  data %>%
    select(actor = Nominee_Name,
           gender = Nominee_Gender,
           birth = Nominee_Birth,
           death = Nominee_Death,
           country = Nominee_Country)
) %>%
  filter(!is.na(actor)) %>%
  group_by(actor) %>%
  summarise(
    gender = first(na.omit(gender)),
    birth = first(na.omit(birth)),
    death = first(na.omit(death)),
    country = first(na.omit(country)),
    .groups = "drop"
  )

# First declare the attributes before setting values
add_attributes_ml(net, attributes = c("gender", "birth", "death", "country", "laureatte"), target = "actor", type = "string")

# Now set the values using named arguments
for (attr in c("gender", "birth", "death", "country")) {
  set_values_ml(
    net,
    attribute = attr,
    actors = data.frame(actor = combined_attr$actor),
    values = combined_attr[[attr]]
  )
}

# List all layers
layers_ml(net)

# Number of layers
num_layers_ml(net)

# List all actors
actors_ml(net)

# Number of unique actors (across all layers)
num_actors_ml(net)

# Number of vertices per layer
num_vertices_ml(net)

# List all vertices (actor + layer)
vertices_ml(net)

# Number of edges
num_edges_ml(net)

# List edges (actor1, layer1, actor2, layer2)
edges_ml(net)

# Get attributes defined in the network
attributes_ml(net)          # actor, vertex, and edge attributes

# Get attribute values for a sample of actors
get_values_ml(net, attribute = "gender", actors = data.frame(actor = head(actors_ml(net)$actor)))



# Step 1: Filter to Chemistry prize in 1953
edges_sub <- data %>%
  filter(Category == "Chemistry", Year == "1953") %>%
  select(from = Nominator_Name, to = Nominee_Name) %>%
  filter(!is.na(from) & !is.na(to))

# Step 2: Get all actors involved
actors_sub <- unique(c(edges_sub$from, edges_sub$to))

# Step 3: Create subnetwork in multinet
subnet <- ml_empty()
add_layers_ml(subnet, c("nominator", "nominee"), directed = TRUE)

# Add vertices
v_sub <- bind_rows(
  data.frame(actor = edges_sub$from, layer = "nominator"),
  data.frame(actor = edges_sub$to, layer = "nominee")
) %>% distinct()

add_vertices_ml(subnet, v_sub)

# Add edges
e_sub <- data.frame(
  actor1 = edges_sub$from,
  layer1 = "nominator",
  actor2 = edges_sub$to,
  layer2 = "nominee"
)

add_edges_ml(subnet, e_sub)

# Step 4: Convert to igraph for visualization
g <- as.igraph(subnet, directed = TRUE)



# Step 5: Plot with basic styling
plot(
  g,
  vertex.label = V(g)$name,
  vertex.color = ifelse(V(g)$name %in% edges_sub$from, "skyblue", "orange"),
  vertex.size = 4,
  vertex.label.cex = 0.8,
  vertex.label.color = "black",
  edge.arrow.size = 0.15,
  layout = layout_with_fr
)