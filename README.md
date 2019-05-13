# JPP - zadanie 2 #
### Bartłomiej Karwowski ###

##### Generowanie plików gramatyki i kompilacja parsera/leksera: #####
1. `cd grammar`
1. `bnfc -m ../grammar/gram.cf`
1. `make`
    1. (Jeśli wystąpi błąd kompilacji) Dodać `import Data.Char (ord)` do `LexGram.hs`

##### Kompilacja interpretera #####
1. patrz - _Generowanie plików gramatyki i kompilacja parsera/leksera_
1. `ghc -O --make Main`
    1. (Można zamiast tego) `sh make.sh`
1. `./Main(.exe) <FILENAME>`
