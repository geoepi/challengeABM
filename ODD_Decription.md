challengeABM: Overview, Design concepts, Details
================

- <a href="#comprehensive-odd-description-for-challengeabm"
  id="toc-comprehensive-odd-description-for-challengeabm">Comprehensive
  ODD Description for challengeABM</a>

## Comprehensive ODD Description for challengeABM

The following ODD (Overview, Design concepts, Details) protocol
describes the agent-based model **challengeABM**. This description is a
comprehensive standalone document detailing the model’s purpose,
structure, and functions.

The model description follows the ODD (Overview, Design concepts,
Details) protocol for describing agent-based models [(Visit the CoMSES
Net website)](https://www.comses.net/resources/standards).

### 1. Purpose and Patterns

**Model Purpose**

The primary purpose of the “challengeABM” model is to investigate and
evaluate how preclinical transmission of a pathogen, specifically
Foot-and-Mouth Disease Virus (FMDV), influences outbreak dynamics both
within a single herd and between multiple farms. The model aims to
assess whether pathogen transmission occurring before the onset of
observable clinical signs significantly alters key epidemiological
outcomes such as outbreak size, the speed of its propagation, and its
spatial extent, when compared to scenarios where transmission is limited
to animals with clinical signs individuals only.

The model also aims to: \* Replicate and extend findings from controlled
empirical transmission experiments. \* Support the development of
predictive tools for analyzing potential policy decisions and
intervention strategies for disease control. \* In its between-farm
module, evaluate how livestock movement, particularly of animals in
preclinical stages, contributes to epidemic spread by linking multiple
herd-level simulations via a dynamic contact network.

**Patterns Used for Model Evaluation**

To assess the model’s realism and validity, the following patterns,
derived from empirical studies and expected epidemiological behaviors,
are used as qualitative and quantitative benchmarks:

- **Empirical Transmission Observations:**
  - The occurrence of FMDV transmission at least 24 hours before donor
    animals exhibit clinical signs.
  - Observed variability in the timing of infection among different
    susceptible cohorts exposed sequentially to infected donors.
  - A non-linear (dose-dependent) relationship between the viral load in
    donor animals and the probability of infection in exposed,
    susceptible recipients.
  - Differences in initial viral dynamics in newly infected animals
    based on the exposure dose.
- **Emergent Population-Level Metrics:**
  - **Cumulative infection incidence:** The total proportion of the
    population infected over the course of the simulated outbreak.
  - **Time to first detection:** The simulated time elapsed until the
    first clinical case is detected within the population or farm
    network.
  - **Epidemic peak timing:** The time at which the number of new
    infections or active infectious individuals reaches its maximum.
  - **Number of affected farms:** In the between-farm module, the total
    count of farms experiencing at least one infection.
  - **Agent-based and Farm-based R0 values:** Mean number of secondary
    infections per agent (`R_agent`) and number of downstream farms
    seeded via movement (`R_farm`).
  - **Infection source metrics:** Number of infections originating from
    preclinical versus clinical animals.
  - These population-level metrics are expected to show substantial
    differences when comparing simulation scenarios with preclinical
    infectiousness versus those with clinical-only infectiousness.

### 2. Entities, State Variables, and Scales

**Entities**

The model includes the following types of entities:

- **Agents (Cattle):** These are individual bovine animals that
  represent the fundamental unit of infection and transmission. Each
  agent has individual characteristics and can transition through
  different infection states.
- **Farms:** In the between-farm module, farms are distinct nodes within
  a contact network. Each farm hosts a population of cattle agents and
  can be subject to measures like quarantine.
- **Rooms (in Room-to-Room Module):** Implicitly, rooms act as distinct
  environments where donor agents sequentially expose susceptible
  cohorts.

**State Variables**

**Agent (Cattle) State Variables:**

| Variable Name                | Description                                                                       | Type            | Units/Range               | Dynamic/Static |
|:-----------------------------|:----------------------------------------------------------------------------------|:----------------|:--------------------------|:---------------|
| `id`                         | Unique identifier for the agent.                                                  | Integer         | N/A                       | Static         |
| `infection_status`           | Current infection class: “Noninfectious”, “Preclinical”, “Clinical”, “recovered”. | Categorical     | As listed                 | Dynamic        |
| `infect_agent`               | Logical flag indicating if the agent is currently infected.                       | Boolean         | TRUE/FALSE                | Dynamic        |
| `virus_nasal`                | Viral load in the agent’s nasal passages.                                         | Numeric         | Log10 copies/mL (assumed) | Dynamic        |
| `virus_serum`                | Viral load in the agent’s blood serum.                                            | Numeric         | Log10 copies/mL (assumed) | Dynamic        |
| `score`                      | Clinical sign status.                                                             | Integer/Numeric | e.g., 0-6                 | Dynamic        |
| `score_t`                    | Time of onset of clinical signs.                                                  | Numeric         | Hours                     | Dynamic        |
| `infectious_t`               | Time at which the agent becomes infectious.                                       | Numeric         | Hours                     | Dynamic        |
| `dose`                       | Viral dose received by the agent upon exposure.                                   | Numeric         | Arbitrary units           | Dynamic        |
| `infection_time`             | Time at which the agent was infected.                                             | Numeric         | Hours                     | Dynamic        |
| `growth_rate_nasal`          | Agent-specific rate of viral growth in nasal passages.                            | Numeric         | Per hour                  | Static         |
| `growth_rate_serum`          | Agent-specific rate of viral growth in serum.                                     | Numeric         | Per hour                  | Static         |
| `clearance_rate`             | Rate at which the agent’s immune system clears the virus.                         | Numeric         | Per hour                  | Static         |
| `stochastic_noise`           | Parameter influencing random variation in viral dynamics.                         | Numeric         | Varies                    | Static         |
| `inflection_point_mean`      | Mean time (days) to inflection point in viral growth.                             | Numeric         | Days                      | Static         |
| `growth_cease_mean`          | Mean time (hours) at which viral growth ceases.                                   | Numeric         | Hours                     | Static         |
| `nasal_threshold`            | Viral load threshold in nasal passages for triggering clinical signs.             | Numeric         | Log10 copies/mL (assumed) | Static         |
| `serum_threshold`            | Viral load threshold in serum for triggering clinical signs.                      | Numeric         | Log10 copies/mL (assumed) | Static         |
| `infect_threshold`           | Viral load threshold (typically nasal) for becoming infectious.                   | Numeric         | Log10 copies/mL (assumed) | Static         |
| `nasal_ccap`                 | Carrying capacity (maximum viral load) in nasal passages.                         | Numeric         | Log10 copies/mL (assumed) | Static         |
| `serum_ccap`                 | Carrying capacity (maximum viral load) in serum.                                  | Numeric         | Log10 copies/mL (assumed) | Static         |
| `recovery_time_mean`         | Mean duration until recovery.                                                     | Numeric         | Days                      | Static         |
| `infector_id`                | Unique ID of the agent that was the source of infection.                          | Integer         | N/A                       | Dynamic        |
| `is_donor`                   | Boolean flag indicating if the agent is an initially infected donor.              | Boolean         | TRUE/FALSE                | Static         |
| `has_recovered`              | Boolean flag indicating if the agent has recovered.                               | Boolean         | TRUE/FALSE                | Dynamic        |
| `preclin_onset_median`       | Median time to preclinical infectiousness onset (days).                           | Numeric         | Days                      | Static         |
| `preclin_duration_median`    | Median duration of preclinical phase (days).                                      | Numeric         | Days                      | Static         |
| `clinical_duration_median`   | Median duration of clinical phase (days).                                         | Numeric         | Days                      | Static         |
| `time_to_preclinical`        | Sampled time from infection to start of preclinical state.                        | Numeric         | Hours/Days                | Static         |
| `time_to_clinical`           | Sampled time from infection/preclinical to start of clinical state.               | Numeric         | Hours/Days                | Static         |
| `duration_of_infectiousness` | Sampled total duration the agent is infectious.                                   | Numeric         | Hours/Days                | Static         |

**Farm State Variables (Between-Farm Module):**

| Variable Name                   | Description                                                               | Type    | Units/Range   | Dynamic/Static     |
|:--------------------------------|:--------------------------------------------------------------------------|:--------|:--------------|:-------------------|
| `id`                            | Unique identifier for the farm.                                           | Integer | N/A           | Static             |
| `agent_population`              | Collection of cattle agents residing on the farm.                         | List    | N/A           | Dynamic            |
| `herd_size`                     | Number of animals on the farm.                                            | Integer | e.g., 200-500 | Static (initially) |
| `quarantine_status`             | Whether the farm is currently under quarantine.                           | Boolean | TRUE/FALSE    | Dynamic            |
| `detection_time`                | Time when the first clinical case was detected on the farm.               | Numeric | Hours         | Dynamic            |
| `location`                      | Spatial coordinates or network position of the farm.                      | Varies  | N/A           | Static             |
| `detection_triggers_quarantine` | Whether clinical detection on this farm triggers quarantine.              | Boolean | TRUE/FALSE    | Static             |
| `detection_delay_hours`         | Delay (hours) between detection and quarantine enforcement for this farm. | Numeric | Hours         | Static             |

**Scales**

- **Temporal Scale:**
  - **Resolution:** The model operates in discrete time steps, each
    representing one hour (`delta_t: 1`).
  - **Extent:** Simulations typically run for 720 hours (30 days)
    (`num_hours: 720`).
- **Spatial Scale and Representation:**
  - **Room-to-Room (R2R) Module:** Space is represented by discrete,
    isolated rooms.
  - **Herd Module:** Simulates a single, homogeneously mixed herd.
    Explicit spatial representation within the herd is not a feature.
  - **Between-Farm Module:** Represents a regional network of farms.
    - **Representation:** Farms are nodes in a network. Network types
      include Erdős–Rényi (`erdos`), Watts–Strogatz (`smallworld`), or
      `geometric`. For geometric graphs, edge weights are influenced by
      geographic distances using an exponential decay function:
      $weight = \exp(-\lambda \times distance)$.
    - **Resolution:** The farm is the spatial unit.
    - **Extent:** The network of `n_farms` (e.g., 20).

### 3. Process Overview and Scheduling

The model operates with a hierarchical structure across its modules. The
general sequence within a time step (`delta_t`, 1 hour) is:

**Overall Scheduling Logic:**

1.  **Update Entity States:**
    - **Agent Level:** Update within-host viral dynamics for infected
      agents. Transition agents between infection states (Noninfectious,
      Preclinical, Clinical, Recovered) based on viral loads,
      thresholds, or probabilistic time-to-event mechanisms (especially
      in the between-farm module).
2.  **Evaluate Interactions and Transmission:**
    - Based on the active module, determine potential transmission
      events.
3.  **Implement Farm-Level Processes (Between-Farm Module Only):**
    - Simulate animal movement between farms.
    - Detect clinical cases and manage quarantine status.
4.  **Record Data:** Log state changes, transmission events, movements.
5.  **Advance Time:** Increment simulation clock by `delta_t`.

**Module-Specific Process Overview and Scheduling:**

**1. Room-to-Room (R2R) Module:** At each time step: a. **Donor Viral
Dynamics Update:** Update viral load of donor agent(s). b.
**Transmission Evaluation:** For susceptible agents in the same room as
an infectious donor, calculate exposure dose and determine infection
probability using the dose-response function. If transmission occurs,
initialize the newly infected agent’s viral load and state. c. **Agent
Progression:** All infected agents update viral loads and check for
clinical/infectious status transitions.

**2. Herd Module:** At each time step: a. **Infected Agent Dynamics:**
Update viral dynamics for all infected agents. Evaluate for onset of
clinical signs and infectiousness (conditional on `preclin_infect`). b.
**Contact Simulation:** For each infectious agent, determine the number
of contacts (Poisson draw based on `contact_rate`). For each contact
with a susceptible agent, evaluate transmission using the dose-response
mechanism. c. **Recovery:** Transition agents to “recovered” state based
on `recovery_time_mean` and `recovery_time_sd`.

**3. Between-Farm Module:** This module simulates inter-farm
transmission by linking multiple farm-level simulations. The function
`simulate_between_herds` manages the main simulation loop. At each
global time step (representing, e.g., one hour, though movements may
occur at larger intervals like `movement_interval`): a. **Within-Farm
Dynamics Update:** For each farm: i. The internal herd epidemic process
is simulated using the `simulate_within_herd_probabilistic_b` function.
This function updates agent states (Noninfectious, Preclinical,
Clinical) based on probabilistic transitions. Each agent’s onset and
duration for these states are sampled from Weibull distributions
(calibrated from `preclin_onset_median`, `preclin_duration_median`,
`clinical_duration_median`, and their CIs). This approach uses
agent-level state probabilities rather than direct modeling of viral
titers for computational efficiency. ii. Intra-farm transmission occurs
based on contact rates and dose-response parameters within this
probabilistic framework. b. **Detection and Quarantine:** i. If new
clinical signs are detected on a farm: 1. If
`detection_triggers_quarantine` is TRUE, a quarantine process is
initiated. 2. Quarantine enforcement (blocking outgoing and incoming
animal movements) begins after `detection_delay_hours`. c. **Inter-Farm
Movement (`simulate_farm_movements`):** i. Movement events are triggered
stochastically based on `movement_prob` and occur at fixed
`movement_interval` (e.g., every 24 hours). ii. For farms NOT under
quarantine, not donor-only, and not empty: 1. A fraction of their herd
(`animals_moved_frac`) is selected for movement. 2. Animals are moved to
a connected farm. The destination is chosen based on network topology
and distance-weighted probabilities (for geometric graphs, using
$weight = \exp(-\lambda \times distance)$). d. **Inter-Farm Seeding:**
i. If any animals moved to a recipient farm are in an infectious state
(preclinical or clinical): 1. They may seed a new outbreak on the
recipient farm. 2. The recipient farm’s simulation is then updated
(re-simulated by `simulate_within_herd_probabilistic_b`) with these new
infection seeds (susceptible agents are effectively replaced by the
incoming infected ones). The `transmission_prob` parameter governs the
likelihood of successful seeding per infectious animal moved or per
contact event resulting from movement.

### 4. Design Concepts

- **Basic Principles:** The model uses SIR-type dynamics, within-host
  viral kinetics, dose-response infection, and network theory for farm
  contacts. Preclinical transmission is a central concept.
- **Emergence:** Outbreak size, timing, detection patterns, spatial
  spread, `R_agent`, and `R_farm` are emergent properties from
  individual agent and farm-level interactions and processes.
- **Adaptation:** No behavioral adaptation by agents or farms is
  modeled.
- **Objectives:** Agents do not have explicit objectives guiding their
  behavior.
- **Learning:** Agents and farms do not learn or modify rules during a
  simulation.
- **Prediction:** Agents do not make predictions to inform actions.
- **Sensing:**
  - **Agents:** Implicitly sense viral exposure.
  - **Farms:** “Sense” disease presence through detection of clinical
    signs, triggering quarantine.
- **Interaction:**
  - **Direct Contact/Proximity:** Facilitates transmission in R2R
    (shared room) and Herd modules (homogeneous mixing, `contact_rate`).
  - **Network-Mediated Interaction:** Animal movement between farms
    (`simulate_farm_movements`) creates pathways for disease spread,
    governed by network structure (`graph_type`, geometric properties)
    and quarantine status.
- **Stochasticity:**
  - **Initialization:** Agent parameters are drawn from distributions.
    Farm network generation (e.g., `generate_farm_network`) can be
    stochastic.
  - **Viral Dynamics:** Includes `stochastic_noise`.
  - **Contact & Transmission:** Contact numbers (Herd module) and
    transmission success are probabilistic.
  - **Movement:** Selection of animals for movement (`movement_prob`)
    and potentially destination choice.
  - **State Transitions (Between-Farm):** Durations in
    preclinical/clinical states are sampled from Weibull distributions
    via `simulate_within_herd_probabilistic_b`.
  - The `seed` parameter ensures simulation reproducibility.
- **Collectives:** Herds on farms are implicit collectives. The farm
  network is a higher-level collective structure.
- **Observation:** The model outputs detailed data for analysis:
  - Agent-level time series of infection status, viral loads
    (`herd_states`).
  - Final infection status and transmission lineage information
    (`herd_agents`).
  - Movement logs including source and destination farms
    (`movement_log`).
  - Detection and quarantine events.
  - Aggregated metrics across simulations: `total_infected`,
    `infected_farms`, `R_agent`, `R_farm`, `avg_detection_time`,
    `avg_intro_time`, `avg_infected_at_detection`,
    `n_infections_from_preclinical`, `n_infections_from_clinical`, state
    prevalences (`pct_preclinical`, etc.), and network infection
    timelines.

### 5. Initialization

- **Agent Initialization:**
  - Individual agent parameters (viral dynamics, thresholds, recovery
    times) are drawn from statistical distributions defined by mean and
    SD values.
  - In the between-farm module, within the
    `simulate_within_herd_probabilistic_b` function called by
    `initialize_farm_status`, each agent’s onset and duration of
    infection states (preclinical, clinical) are sampled from Weibull
    distributions.
  - Agents start as `Noninfectious` unless designated as donors.
    `is_donor` status is set.
- **Module-Specific Initialization:**
  - **R2R & Herd Modules:** Donors (`num_donors` or `initial_donors`)
    are initialized as infected at time 0. Susceptible agents populate
    rooms or the herd.
  - **Between-Farm Module:**
    - A farm network of `n_farms` is created by `generate_farm_network`,
      defining topology (`graph_type`) and edge characteristics
      (distances, weights using `lambda`, `shape_gamma`, `scale_gamma`).
    - Each farm node is populated with a herd (`herd_size_range` or
      `total_herd_size`). The function `initialize_farm_status` calls
      `simulate_within_herd_probabilistic_b` for each farm to set up its
      initial agent population with their individual (probabilistically
      determined) disease progression timelines.
    - Initial infections (`num_donors` or `initial_donors`) are seeded
      onto specified farms.
- **Configuration:** Parameters are read from YAML files. Global
  parameters like `num_hours`, `delta_t`, `preclin_infect`, `seed` are
  set.

### 6. Input Data

Input consists of model parameters defined in YAML configuration files.
These files detail: \* Simulation control (`num_hours`, `delta_t`). \*
Initial conditions (`num_donors`, `initial_donors`, `total_herd_size`,
`herd_size_range`). \* Disease progression (`preclin_infect`,
`recovery_time_mean`, `preclin_onset_median`, `preclin_duration_median`,
`clinical_duration_median`, and CIs for Weibull fitting). \*
Transmission dynamics (`contact_rate`, dose-response parameters). \*
Within-host viral dynamics parameters (numerous means and SDs for
thresholds, growth rates, etc., primarily for R2R and Herd modules). \*
Between-herd network (`n_farms`, `graph_type`, `shape_gamma`,
`scale_gamma`, `lambda`). \* Between-herd movement and control
(`movement_prob`, `movement_interval`, `animals_moved_frac`,
`transmission_prob` between farms, `detection_delay_hours`,
`detection_triggers_quarantine`). \* Stochasticity control (`seed`).

The model does not use external time-series data to drive dynamics
during a run but uses empirical data offline for parameterization and
validation.

### 7. Submodels

**1. Within-Host Viral Dynamics (R2R and Herd Modules)** \* **Purpose:**
Simulates viral load change within an agent. \* **Description:**
Logistic growth with clearance and noise:
$\frac{dV}{dt} = rV \left(1 - \frac{V}{K}\right) - cV + \epsilon$
(Parameters: $r, K, c, \epsilon$ are agent-specific, derived from
configured means/SDs). Viral load determines clinical and infectious
status via thresholds.

**2. Dose-Response Function (Transmission in R2R and Herd Modules)** \*
**Purpose:** Determines infection probability/initial dose. \*
**Description:** Hill-type sigmoid function:
$\text{DoseEff} = \frac{E_{\max}}{1 + \exp\left[-\lambda \left(\frac{V_{\text{inf}}}{K_{\text{inf}}} - \theta_{\text{adj}}\right)\right]}$
with
$\theta_{\text{adj}} = \theta + \frac{1}{\lambda} \log\left(\frac{E_{\max}}{E_{\text{threshold}}} - 1\right)$.
(Parameters: `dose_max_efficiency`, `dose_scaling_factor`,
`dose_threshold`, `dose_efficiency_at_threshold`).

**3. Probabilistic State Transition
(`simulate_within_herd_probabilistic_b` in Between-Farm Module)** \*
**Purpose:** Models agent progression through Noninfectious,
Preclinical, and Clinical states without continuous viral titer
simulation for computational tractability in large networks. \*
**Description:** \* Each agent’s time to onset of preclinical state,
time from preclinical to clinical state, and duration of infectious
periods are sampled from Weibull distributions. These distributions are
parameterized using median values and confidence intervals
(`preclin_onset_median/CI`, `preclin_duration_median/CI`,
`clinical_duration_median/CI`). \* Sigmoid functions may map
time-since-infection or time-in-state to cumulative probabilities of
being in a subsequent state, providing a discrete-time approximation of
disease progression. \* This submodel handles intra-farm transmission
based on contact rates and dose-response parameters within its
probabilistic framework.

**4. Farm Network Generation (`generate_farm_network` in Between-Farm
Module)** \* **Purpose:** Creates the contact structure between farms.
\* **Description:** Generates a graph of `n_farms` nodes. \* Topology
can be Erdős–Rényi (`erdos`), Watts–Strogatz (`smallworld`), or
`geometric`. \* For `geometric` graphs, edges are assigned distances,
and movement weights are calculated using an exponential decay function:
$weight = \exp(-\lambda \times distance)$. Parameters `shape_gamma` and
`scale_gamma` are used for edge distance decay distributions if
applicable to the chosen geometric model variant.

**5. Farm Movement (`simulate_farm_movements` in Between-Farm Module)**
\* **Purpose:** Simulates livestock movement between farms. \*
**Description:** \* Triggered at `movement_interval` based on
`movement_prob`. \* A fraction of herd (`animals_moved_frac`) is
selected from non-quarantined, non-empty, non-donor-only farms. \*
Destination farm selected based on network connections and weights
(distance-dependent for geometric graphs). \* If infectious animals
move, they can seed infection in the recipient farm (governed by
`transmission_prob`). The recipient farm’s state is updated by
re-running `simulate_within_herd_probabilistic_b` with the new seeds.

**6. Quarantine Logic (Between-Farm Module, part of
`simulate_between_herds`)** \* **Purpose:** Simulates disease control by
farm isolation. \* **Description:** \* Triggered by clinical detection
on a farm. \* Enforced after `detection_delay_hours` if
`detection_triggers_quarantine` is TRUE. \* Prevents animal movement to
and from the quarantined farm.

These submodels interact to produce the overall simulated epidemiology
of FMDV.
