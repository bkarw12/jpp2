# Simple programming language coded in Haskell

### Compiling and running the interpreter
Note that you need to have [GHC](https://www.haskell.org/ghc/) and [Make](https://www.gnu.org/software/make/) installed.\
**Attention!** This project is probably not compatible with newer GHC versions. It was originally built and tested using [GHC 8.6.5](https://www.haskell.org/ghc/download_ghc_8_6_5.html).
1. Clone the repository.
1. `cd` to the cloned project directory.
1. `cd src`.
1. Run `make`, to build the program.
1. Run `make clean`, to clean files created during compilation.
1. Run the interpreter: `./interpreter <FILENAME>`, where <FILENAME> is the path to the file containing code (e.g. from examples in the `prog/` directory).

\
The parser files were generated from the file `gram/gram.cf` using [BNFC](https://hackage.haskell.org/package/BNFC).

<!-- ##### Generowanie plików gramatyki i kompilacja parsera/leksera: #####
1. `cd grammar`
1. `bnfc -m ../grammar/gram.cf`
1. `make`
    1. (Jeśli wystąpi błąd kompilacji) Dodać `import Data.Char (ord)` do `LexGram.hs`

##### Kompilacja i uruchomienie interpretera #####
1. `cd src`
1. `make`
1. `make clean`
1. `./interpreter(.exe) <FILENAME>`
 -->
