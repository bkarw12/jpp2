# JPP - zadanie 2 #
### Bartłomiej Karwowski ###

##### Generowanie plików gramatyki i kompilacja parsera/leksera: #####
1. `cd grammar`
1. `bnfc -m ../grammar/gram.cf`
1. `make`
    1. (Jeśli wystąpi błąd kompilacji) Dodać `import Data.Char (ord)` do `LexGram.hs`

##### Kompilacja i uruchomienie interpretera #####
1. `cd src`
1. `make`
1. `make clean`
1. `./interpreter(.exe) <FILENAME>`
