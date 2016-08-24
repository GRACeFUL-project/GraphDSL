# GraphDSL
A DSL for CLDs

CLD = Causal Loop Diagram


----------------------------------------------------------------

## Install on Linux (Ubuntu 16.04)

```shell
cd src/GRACeFUL
### install the LINUX branch of haskelzinc
git clone https://github.com/GRACeFUL-project/haskelzinc.git
cd haskelzinc
git checkout LINUX
### install the master branch of GraphDSL
cd ..
git clone https://github.com/GRACeFUL-project/GraphDSL.git
cabal sandbox init
cabal install haskelzinc/ GraphDSL/
### install minizinc från http://www.minizinc.org/2.0/install-linux.html
cd ~/Downloads/
wget https://github.com/MiniZinc/libminizinc/releases/download/2.0.14/minizinc-2.0.14-linux64.tar.gz
tar -zxf minizinc-2.0.14-linux64.tar.gz
### Check the paths in GraphDSL/examples/HZconf/conf.txt
cd ~/src/GRACeFUL
cabal repl
:l GraphDSL/examples/Examples.hs
test1
test2
test3
```


For "Check the paths" I did the following:
```shell
cd /usr/local/bin
sudo ln -s ~/Downloads/minizinc-2.0.14/bin/mzn2fzn .
sudo ln -s ~/Downloads/minizinc-2.0.14/bin/flatzinc .
```
