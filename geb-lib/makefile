# ------------------------------------------------------------------------------
# Makefile to build the centralized sources of the doctoral project of 
#
# Author:    Johannes Gebert - HLRS - NUM - gebert@hlrs.de
# Date:      27.12.2021
# Last edit: 29.12.2021
#
# For use of make visit: https://www.gnu.org/software/make/
# ------------------------------------------------------------------------------
long_name="Centralized Sources"
# -----------------------------------------------------------------------------
# Check for environment
# -----------------------------------------------------------------------------
check-env:
ifeq ($(SYS_ENV),)
	@echo "-----------------------------------------------"
	@echo "-- Please source environment.sh <system> first."
	@echo "-----------------------------------------------"
else
	@echo "-----------------------------------------------"
	@echo "-- Environment to build for: "$(SYS_ENV)
	@echo "-----------------------------------------------"
	$(MAKE) all
endif
#
# ------------------------------------------------------------------------------
# Build path
build_path = $(CURDIR)
export build_path
#
# ------------------------------------------------------------------------------
# Directories
st_mod_dir= $(build_path)/mod/
mod_dir   = $(build_path)/mod/
obj_dir   = $(build_path)/obj/
c-obj_dir = $(build_path)/obj/c_objects/
c-src_dir = $(build_path)/c-src/
f-src_dir = $(build_path)/f-src/
mpi-f-src_dir = $(build_path)/f-src/mpi/
#
# -----------------------------------------------------------------------------
# Directory for documentation
# -----------------------------------------------------------------------------
doc_dir  = $(build_path)/doc/
html_dir = $(build_path)/html/
tex_dir  = $(build_path)/latex/
#
# ------------------------------------------------------------------------------
# File extensions and suffixes
# -----------------------------------------------------------------------------
mpi_ext = .mpi
mod_ext = .mod
obj_ext = .o
sho_ext = .so
f90_ext = .f90
F90_ext = .F90
c_ext = .c
# ------------------------------------------------------------------------------
clean_cmd = rm -f
#
# ------------------------------------------------------------------------------
# Inlcude build options
include make.flags
#
# ------------------------------------------------------------------------------
# Generate objects
# -----------------------------------------------------------------------------
f-objects = $(obj_dir)mod_global_std$(obj_ext)\
			$(obj_dir)mod_strings$(obj_ext)\
			$(obj_dir)mod_math$(obj_ext)\
			$(obj_dir)mod_mechanical$(obj_ext)\
			$(obj_dir)mod_user_interaction$(obj_ext)\
			$(obj_dir)mod_user_interaction$(mpi_ext)$(obj_ext)\
			$(obj_dir)mod_system$(obj_ext)\
			$(obj_dir)mod_system$(mpi_ext)$(obj_ext)\
			$(obj_dir)mod_meta$(obj_ext)\
			$(obj_dir)mod_ser_binary$(obj_ext)\
			$(obj_dir)mod_par_binary$(mpi_ext)$(obj_ext)\
			$(obj_dir)mod_vtk$(obj_ext)\
			$(obj_dir)mod_formatted_plain$(obj_ext)\
			$(obj_dir)mod_image_manipulation$(obj_ext)

c-objects = $(c-obj_dir)mod_meta$(obj_ext)

# ------------------------------------------------------------------------------
# Begin Building
# -----------------------------------------------------------------------------
all: $(f-objects) $(c-objects)

# ------------------------------------------------------------------------------
# Fortran Standards Module
# -----------------------------------------------------------------------------
$(obj_dir)mod_global_std$(obj_ext):$(f-src_dir)mod_global_std$(f90_ext)
	@echo "----- Compiling " $(f-src_dir)mod_global_std$(f90_ext) "-----"
	$(f90_compiler) $(c_flags_f90) -c $(f-src_dir)mod_global_std$(f90_ext) -o $@
	@echo

# ------------------------------------------------------------------------------
# Fortran mechanical Module
# -----------------------------------------------------------------------------
$(obj_dir)mod_mechanical$(obj_ext):$(mod_dir)global_std$(mod_ext) \
									$(f-src_dir)mod_mechanical$(f90_ext)
	@echo "----- Compiling " $(f-src_dir)mod_mechanical$(f90_ext) "-----"
	$(f90_compiler) $(c_flags_f90) -c $(f-src_dir)mod_mechanical$(f90_ext) -o $@
	@echo

# ------------------------------------------------------------------------------
# Fortran External source to parse input
# -----------------------------------------------------------------------------
$(obj_dir)mod_strings$(obj_ext):$(mod_dir)global_std$(mod_ext)	$(f-src_dir)strings$(f90_ext)
	@echo "----- Compiling " $(f-src_dir)strings$(f90_ext) "-----"
	$(f90_compiler) $(c_flags_f90) -c $(f-src_dir)strings$(f90_ext) -o $@
	@echo

# ------------------------------------------------------------------------------
# Fortran math Module
$(obj_dir)mod_math$(obj_ext):$(mod_dir)global_std$(mod_ext)	\
							$(mod_dir)strings$(mod_ext)\
							$(f-src_dir)mod_math$(f90_ext)
	@echo "----- Compiling " $(f-src_dir)mod_math$(f90_ext) "-----"
	$(f90_compiler) $(c_flags_f90) $(libs) -c $(f-src_dir)mod_math$(f90_ext) -o $@
	@echo 
	
# -----------------------------------------------------------------------------
# Fortran module for User Interaction
# -----------------------------------------------------------------------------
$(obj_dir)mod_user_interaction$(obj_ext):$(mod_dir)global_std$(mod_ext) \
									$(mod_dir)strings$(mod_ext) \
									$(f-src_dir)mod_user_interaction$(f90_ext)
	@echo "----- Compiling " $(f-src_dir)mod_user_interaction$(f90_ext) "-----"
	$(f90_compiler) $(c_flags_f90) -c $(f-src_dir)mod_user_interaction$(f90_ext) -o $@
	@echo 

# -----------------------------------------------------------------------------
# Fortran meta Module 
# -----------------------------------------------------------------------------
$(obj_dir)mod_meta$(obj_ext):$(mod_dir)strings$(mod_ext) \
							$(mod_dir)user_interaction$(mod_ext) \
							$(f-src_dir)mod_meta$(f90_ext)
	@echo "----- Compiling " $(f-src_dir)mod_meta$(f90_ext) "-----"
	$(f90_compiler) $(c_flags_f90) -c $(f-src_dir)mod_meta$(f90_ext) -o $@
	@echo 

# -----------------------------------------------------------------------------
# Fortran Formatted Plain Module
# -----------------------------------------------------------------------------
$(obj_dir)mod_formatted_plain$(obj_ext):$(mod_dir)global_std$(mod_ext) \
										$(mod_dir)math$(mod_ext)\
										$(f-src_dir)mod_formatted_plain$(f90_ext)
	@echo "----- Compiling " $(f-src_dir)mod_formatted_plain$(f90_ext) "-----"
	$(f90_compiler) $(c_flags_f90) -c $(f-src_dir)mod_formatted_plain$(f90_ext) -o $@
	@echo 

# ------------------------------------------------------------------------------
# Fortran module for vtk operations
# -----------------------------------------------------------------------------
$(obj_dir)mod_vtk$(obj_ext):$(mod_dir)global_std$(mod_ext) \
								$(mod_dir)user_interaction$(mod_ext) \
								$(f-src_dir)mod_vtk$(f90_ext)
	@echo "----- Compiling " $(f-src_dir)mod_vtk$(f90_ext) "-----"
	$(f90_compiler) $(c_flags_f90) -c $(f-src_dir)mod_vtk$(f90_ext) -o $@
	@echo

# ------------------------------------------------------------------------------
# Fortran module for serial binary operations
# -----------------------------------------------------------------------------
$(obj_dir)mod_ser_binary$(obj_ext):$(mod_dir)global_std$(mod_ext) \
								$(mod_dir)user_interaction$(mod_ext) \
								$(f-src_dir)mod_ser_binary$(f90_ext)
	@echo "----- Compiling " $(f-src_dir)mod_ser_binary$(f90_ext) "-----"
	$(f90_compiler) $(c_flags_f90) -c $(f-src_dir)mod_ser_binary$(f90_ext) -o $@
	@echo

# ------------------------------------------------------------------------------
# Fortran module vtk structured points and binary data
# -----------------------------------------------------------------------------
$(obj_dir)mod_image_manipulation$(obj_ext):$(mod_dir)global_std$(mod_ext) \
								$(mod_dir)math$(mod_ext) \
								$(f-src_dir)mod_image_manipulation$(f90_ext)
	@echo "----- Compiling " $(f-src_dir)mod_image_manipulation$(f90_ext) " -----"
	$(f90_compiler) $(c_flags_f90) -c $(f-src_dir)mod_image_manipulation$(f90_ext) -o $@
	@echo

# ------------------------------------------------------------------------------
# Fortran module for system information
# -----------------------------------------------------------------------------
$(obj_dir)mod_system$(obj_ext):$(mod_dir)global_std$(mod_ext) $(f-src_dir)mod_system$(F90_ext)
	@echo "----- Compiling " $(f-src_dir)mod_system$(F90_ext) " -----"
	$(f90_compiler) $(c_flags_f90) -c $(f-src_dir)mod_system$(F90_ext) -o $@
	@echo


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# MPI # Fortran module for mpi parallel binary operations
# -----------------------------------------------------------------------------
$(obj_dir)mod_par_binary$(mpi_ext)$(obj_ext):$(mod_dir)global_std$(mod_ext) \
								$(mod_dir)ser_binary$(mod_ext) \
								$(mod_dir)user_interaction$(mod_ext) \
								$(mod_dir)mpi_user_interaction$(mod_ext) \
								$(mpi-f-src_dir)mod_par_binary$(mpi_ext)$(f90_ext)
	@echo "----- Compiling " $(mpi-f-src_dir)mod_par_binary$(mpi_ext)$(f90_ext) "-----"
	$(f90_compiler) $(c_flags_f90) -c $(mpi-f-src_dir)mod_par_binary$(mpi_ext)$(f90_ext) -o $@
	@echo
	
# -----------------------------------------------------------------------------
# MPI # Fortran module for User Interaction
# -----------------------------------------------------------------------------
$(obj_dir)mod_user_interaction$(mpi_ext)$(obj_ext):$(mod_dir)global_std$(mod_ext) \
									$(mod_dir)strings$(mod_ext) \
									$(mpi-f-src_dir)mod_user_interaction$(mpi_ext)$(f90_ext)
	@echo "----- Compiling " $(mpi-f-src_dir)mod_user_interaction$(mpi_ext)$(f90_ext) "-----"
	$(f90_compiler) $(c_flags_f90) -c $(mpi-f-src_dir)mod_user_interaction$(mpi_ext)$(f90_ext) -o $@
	@echo 
						
# -----------------------------------------------------------------------------
# MPI # Fortran module for system information
# -----------------------------------------------------------------------------
$(obj_dir)mod_system$(mpi_ext)$(obj_ext):$(mod_dir)global_std$(mod_ext) \
									$(mod_dir)system$(mod_ext) \
									$(mpi-f-src_dir)mod_system$(mpi_ext)$(f90_ext)
	@echo "----- Compiling " $(mpi-f-src_dir)mod_system$(mpi_ext)$(f90_ext) "-----"
	$(f90_compiler) $(c_flags_f90) -c $(mpi-f-src_dir)mod_system$(mpi_ext)$(f90_ext) -o $@
	@echo 



# -----------------------------------------------------------------------------
# C Meta Module 
# -----------------------------------------------------------------------------
$(c-obj_dir)mod_meta$(obj_ext):$(c-src_dir)mod_meta$(c_ext)
	@echo "----- Compiling " $(c-src_dir)mod_meta$(c_ext) "-----"
	$(c_compiler) $(c_flags_c) -c $(c-src_dir)mod_meta$(c_ext) -o $@
	@echo 

help:
	@echo "----------------------------------------------------------------------------------"
	@echo "Make targets"
	@echo "Regular:       »make (all)«   - Build all $(long_name)."
	@echo "Cleaning:      »make clean«   - Remove generated files, keep the config"
	@echo "Documentation: »make docs     - Build the html and the tex documentation"
	@echo "----------------------------------------------------------------------------------"

docs: 
	@echo "----------------------------------------------------------------------------------"
	@echo "-- Beginn buiding the documentation of the $(long_name)."
	@echo "----------------------------------------------------------------------------------"
	doxygen doc/doxy.conf
	$(MAKE) pdf -C $(tex_dir)  
	@echo "----------------------------------------------------------------------------------"
	@echo "-- Successfully build the documentation of the $(long_name)."
	@echo "----------------------------------------------------------------------------------"

cleandocs:
	@echo "----------------------------------------------------------------------------------"
	@echo "-- Cleaning html documentation"
	@echo "----------------------------------------------------------------------------------"
	$(clean_cmd) $(html_dir)/*
	@echo "----------------------------------------------------------------------------------"
	@echo "-- Cleaning tex documentation"
	@echo "----------------------------------------------------------------------------------"
	$(clean_cmd) $(tex_dir)/*
	@echo "----------------------------------------------------------------------------------"
	@echo "-- Documentation removed."
	@echo "----------------------------------------------------------------------------------"
	
clean:
	@echo "----------------------------------------------------------------------------------"
	@echo "-- Cleaning geb-lib Fortran module directory"
	@echo "----------------------------------------------------------------------------------"
	$(clean_cmd) $(mod_dir)*$(mod_ext)
	@echo "----------------------------------------------------------------------------------"
	@echo "-- Cleaning geb-lib Fortran object directory"
	@echo "----------------------------------------------------------------------------------"
	$(clean_cmd) $(obj_dir)*$(obj_ext)
	@echo "----------------------------------------------------------------------------------"
	@echo "-- Cleaning geb-lib C object directory"
	@echo "----------------------------------------------------------------------------------"
	$(clean_cmd) $(c-obj_dir)*$(obj_ext)
