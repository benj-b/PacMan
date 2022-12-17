# PacMan
L2 Functional Programming Project

# Compile
`ocamlc -thread graphics.cma unix.cma threads.cma Projet.ml`

# Launch
`./a.out`

# Possible errors

If you launch it on wsl Windows, you can have an error like `Cannot open display`. To fix it, you need to install an external server (xlaunch for example), and add to the display path variable : `export DISPLAY=:0`

To save this variable, you can print it in the .bashrc file : `echo "export DISPLAY=:0" | cat >>.bashrc`, and then relaunch bash `source .bashrc`. Problem should be solved.
