#!/bin/sh -e

echo "Saisissez votre login ENSEEIHT :"
read login
mkdir $login
cp -p chaines.ml $login
cp -p chaines.mli $login
cp -p encodage.ml $login
cp -p intuitive.ml $login
cp -p naif.ml $login
cp -p dico_fr.txt $login
cp -p dune $login
cp -p dune-workspace $login
cp -p dune-project $login
tar -cvf  $login.tar $login
rm -rf $login

