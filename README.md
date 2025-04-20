# NBC
Ischemic conditions cause an increase in the sodium concentration of astrocytes, driving the breakdown of ionic homeostasis and exacerbating cellular damage. Astrocytes express high levels of the electrogenic sodium-bicarbonate cotransporter1 (NBCe1), which couples intracellular Na+ homeostasis to regulation of pH and operates close to its reversal potential under physiological conditions. Here, we analyzed its mode of operation during transient energy deprivation via imaging astrocytic pH, Na+, and ATP in organotypic slice cultures of the mouse neocortex, complemented with patch-clamp and ion-selective microelectrode recordings and computational modeling. We found that a 2 min period of metabolic failure resulted in a transient acidosis accompanied by a Na+ increase in astrocytes. Inhibition of NBCe1 increased the acidosis while decreasing the Na+ load. Similar results were obtained when comparing ion changes in wild-type and Nbce1-deficient mice. Mathematical modeling replicated these findings and further predicted that NBCe1 activation contributes to the loss of cellular ATP under ischemic conditions, a result confirmed experimentally using FRET-based imaging of ATP. Altogether, our data demonstrate that transient energy failure stimulates the inward operation of NBCe1 in astrocytes. This causes a significant amelioration of ischemia-induced astrocytic acidification, albeit at the expense of increased Na+ influx and a decline in cellular ATP. Please use this link to read full paper https://www.mdpi.com/2073-4409/12/23/2675#










![image](https://github.com/user-attachments/assets/6123260e-bb66-4032-92f9-fdbf836ff4c5)





# NBC Simulation (Sodium-Bicarbonate Cotransporter)

This Fortran program simulates the activity of the Sodium-Bicarbonate Cotransporter (NBC), which plays a vital role in regulating intracellular pH and ion concentrations in glial and neuronal cells.

## File
- `NBC.f90` — Main simulation code written in Fortran 90.

---

## Prerequisites

You can use either of the following Fortran compilers:

### Option 1: Intel Fortran Compiler
- Intel oneAPI (includes `ifort`)
- Or Intel Parallel Studio

### Option 2: GNU Fortran Compiler
- `gfortran` (available via most package managers)

---

## Compilation & Execution

### Using Intel Fortran (`ifort`)

```bash
# Compile
ifort -O2 -o nbc_sim NBC.f90

# Run
./nbc_sim



### Using GNU Fortran (gfortran)

# Compile
gfortran -O2 -o nbc_sim NBC.f90

# Run
./nbc_sim

 ### Visualizing the Results (Python Example) matlab version is already provided in the above 
You can use Python to visualize .dat outputs:

import numpy as np
import matplotlib.pyplot as plt

# Load the .dat file (example: concentration vs. time)
data = np.loadtxt('concentration.dat')  # Update filename if needed

time = data[:, 0]
concentration = data[:, 1]

plt.plot(time, concentration, label='[NBC] activity')
plt.xlabel('Time (s)')
plt.ylabel('Concentration (mM)')
plt.title('NBC Simulation Result')
plt.grid(True)
plt.legend()
plt.tight_layout()
plt.show()


