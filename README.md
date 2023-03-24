# cleancaRbs

`cleancarRbs` is a package to clean ecosystem carbon fluxes according to the method presented in REF zhao18.
The aim is to automatically clean fluxes using a logarithmic model accounting for leaks, saturation and condensation on the chamber.
The package also includes functions to standardize fluxes using light response curves (LRC), to calculate fluxes and to match fluxes with the plots being mesured (when doing continuous measurements on the field).
It is developped for scientists measuring a lot of fluxes on the field who want to work with all the fluxes in a single file and want to get the cleaning done automatically to save time.