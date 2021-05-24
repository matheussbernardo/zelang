# The name of your project (as given in the dune-project file)
# TODO
project_name = zelang

# The opam package configuration file
opam_file = $(project_name).opam

.PHONY: deps run run-debug

# Alis to update the opam file and install the needed deps
deps: $(opam_file)

# Build and run the app
run:
	dune exec $(project_name)

# Update the package dependencies when new deps are added to dune-project
$(opam_file): dune-project
	-dune build @install		# Update the $(project_name).opam file
	-git add $(opam_file)		# opam uses the state of master for it updates
	-git commit $(opam_file) -m "Updating package dependencies"
	opam install . --deps-only  # Install the new dependencies
