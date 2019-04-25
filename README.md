JPP - zadanie 2
Bartłomiej Karwowski

A) Generowanie plików gramatyki:
1. cd grammar
2. bnfc -m gram.cf

B) Kompilacja plików testujących parser/lekser:
1. punkt A)
2. make
3. (Jeśli wystąpi błąd kompilacji) dodać "import Data.Char (ord) do LexGram.hs
