ZIP_DIR="bk385713"

if [ -d "$ZIP_DIR" ]; then
    rm -r -f bk385713
fi

mkdir $ZIP_DIR
mkdir $ZIP_DIR/src
cp -r prog $ZIP_DIR
cp src/*.hs $ZIP_DIR/src
cp src/Makefile $ZIP_DIR/src 
cp other/decl.pdf $ZIP_DIR
cp grammar/gram.cf $ZIP_DIR
