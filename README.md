# challengeABM: Agent-Based Model of FMDV Transmission
An Agent Based Model (ABM) simulating within-host virus dynamics, within-herd host transmission, and between-farm spread    

This work is in draft form, has not been verified, and is subject to change.  
   
### Room-to-Room Overview:  
https://github.com/geoepi/challengeABM/blob/main/articles/room_to_room_transmission.md  
  
### Within Herd Overview:  
https://github.com/geoepi/challengeABM/blob/main/articles/within_herd_transmission.md  
   
### Simplified Room-to-Rooom ShinyApp:   
https://geoepi.shinyapps.io/fmdv-preclinical/    

## Install: 
```
library(remotes)
remotes::install_github("geoepi/challengeABM")
```
---
---

## Table of Contents

* [Overview](#overview)
* [Model Purpose](#model-purpose)
* [Key Features](#key-features)
* [Modules](#modules)
    * [Room-to-Room (R2R) Module](#room-to-room-r2r-module)
    * [Herd Module](#herd-module)
    * [Between-Farm Module](#between-farm-module)
* [Technical Details](#technical-details)
    * [Core Mechanisms](#core-mechanisms)
    * [Parameters](#parameters)
* [System Requirements](#system-requirements)
* [Installation](#installation)
* [Getting Started](#getting-started)
    * [Running a Simulation](#running-a-simulation)
    * [Configuration Files](#configuration-files)
    * [Output Data](#output-data)
* [Model Documentation (ODD Protocol)](#model-documentation-odd-protocol)
* [Directory Structure](#directory-structure)
* [Contributing](#contributing)
* [License](#license)
* [Citation](#citation)
* [Contact](#contact)

---

## Overview

`challengeABM` is an agent-based modeling framework developed in R to simulate the transmission dynamics of Foot-and-Mouth Disease Virus (FMDV) in cattle. It is designed to explore the impact of various epidemiological factors, particularly preclinical transmission, on outbreak characteristics at different scales. The model allows for detailed simulation of within-host viral kinetics and its influence on transmission probability, as well as broader-scale dynamics such as inter-farm spread through animal movement.


## Model Purpose

The primary purpose of `challengeABM` is to:

1.  Investigate how **preclinical FMDV transmission** (infection spread before visible signs) influences outbreak dynamics within herds and between farms.
2.  Assess the impact of preclinical infectiousness on key epidemiological outcomes such as **outbreak size, speed, and spatial extent**.
3.  Simulate and **replicate findings from controlled FMDV transmission experiments**.
4.  Provide a flexible platform to explore the potential effects of **different intervention strategies** (e.g., quarantine, movement restrictions) on FMDV spread.
5.  Understand how **livestock movement patterns and network structures** contribute to regional FMDV epidemics.

## Key Features

* **Modular Design:** Separate, modules for different epidemiological scenarios:
    * Controlled experimental settings (Room-to-Room).
    * Single, well-mixed herd dynamics.
    * Inter-farm spread across a network.
* **Detailed Agent-Level Dynamics:**
    * Simulation of within-host viral loads (nasal and serum) using logistic growth models (in R2R and Herd modules).
    * Individualized, stochastically assigned parameters for viral replication, clearance, and disease progression thresholds.
    * Dose-response mechanism linking viral load of infector to probability of transmission.
* **Probabilistic State Transitions (Between-Farm Module):** Efficient agent-level progression through `Noninfectious`, `Preclinical`, and `Clinical` states using Weibull-distributed durations, enabling simulation of larger networks.
* **Network Modeling (Between-Farm Module):**
    * Generation of farm contact networks (Erdős–Rényi, Watts–Strogatz small-world, geometric).
    * Distance-weighted movement probabilities for realistic spread patterns.
* **Configurable Scenarios:** Key model parameters are managed through easily editable YAML configuration files, allowing for extensive scenario testing.
* **Stochasticity:** Incorporates randomness in parameter initialization, contact processes, transmission events, and animal movement to reflect natural variability.
* **Comprehensive Outputs:** Generates detailed logs of agent states, transmission events, farm statuses, and movement for in-depth analysis.

## Modules

### Room-to-Room (R2R) Module

* **Focus:** Simulates FMDV transmission in a controlled experimental setup where donor (infected) animals sequentially expose cohorts of susceptible animals in isolated rooms.
* **Mechanisms:** Employs detailed within-host viral dynamics for each agent and a dose-response function based on donor viral load to determine transmission to susceptibles in the same room.

### Herd Module

* **Focus:** Simulates FMDV transmission within a single, homogeneously mixed herd of cattle.
* **Mechanisms:** Agents have detailed within-host viral dynamics. Contacts between infectious and susceptible agents occur stochastically (Poisson process). Transmission success is governed by the infector's viral load and a dose-response model.

### Between-Farm Module

* **Focus:** Simulates FMDV spread across a network of interconnected farms, driven by livestock movement.
* **Mechanisms:**
    * Each farm runs an internal herd simulation using a computationally efficient probabilistic state-transition model (`simulate_within_herd_probabilistic_b`) where agents' progression through `Noninfectious`, `Preclinical`, and `Clinical` states is governed by Weibull-distributed durations.
    * Stochastic animal movements between farms (`simulate_farm_movements`) are based on network topology (Erdős–Rényi, small-world, or geometric with distance-weighting) and configurable movement probabilities/fractions.
    * Infected animals moving to susceptible farms can seed new outbreaks.
    * Optional quarantine logic (`detection_triggers_quarantine`, `detection_delay_hours`) can be applied to restrict movement from farms once clinical signs are detected.

## Technical Details

### Core Mechanisms

* **Within-Host Viral Dynamics (R2R, Herd):** Modeled using a logistic growth equation:
    ```
    dV/dt = rV(1 - V/K) - cV + ε
    ```
* **Dose-Response Function (R2R, Herd, implicitly in Between-Farm via `simulate_within_herd_probabilistic_b`):** A Hill-type sigmoid function determines infection probability based on exposure dose.
    ```
    DoseEff = E_max / (1 + exp[-λ(V_inf/K_inf - θ_adj)])
    ```
* **Probabilistic State Transitions (Between-Farm):** Agents transition through `Noninfectious`, `Preclinical`, and `Clinical` states with durations sampled from Weibull distributions.
* **Network Generation (Between-Farm):** Uses standard graph theory algorithms for `erdos`, `smallworld`, or `geometric` networks. Movement probability in geometric networks can decay exponentially with distance: `exp(-lambda * distance)`.


### Parameters

All model parameters are defined in YAML configuration files (see `config/` directory, e.g., [`base_config_fmdv.yaml`](https://github.com/geoepi/challengeABM/blob/main/config/base_config_fmdv.yaml)). Key parameter groups include:

* Simulation duration and time step
* Initial seeding conditions (number of donors, herd sizes)
* Preclinical infectiousness toggle and recovery parameters
* Transmission model parameters (contact rate, dose-response curve variables)
* Virus dynamics parameters (growth rates, clearance rates, thresholds, carrying capacities for nasal/serum titers)
* Probabilistic progression parameters for between-farm module (median onset/duration times for preclinical/clinical states and their CIs)
* Between-herd network parameters (number of farms, graph type, distance decay factors)
* Between-herd movement and control parameters (movement probability/interval/fraction, inter-farm transmission probability, detection delay, quarantine triggers)
* Random seed for reproducibility
