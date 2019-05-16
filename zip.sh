ZIP_DIR="bartlomiej_karwowski"

if [ -d "$ZIP_DIR" ]; then
    rm -r -f $ZIP_DIR
fi

mkdir $ZIP_DIR
mkdir $ZIP_DIR/lang
cp -r prog $ZIP_DIR
cp src/*.hs $ZIP_DIR
cp src/Makefile $ZIP_DIR 
cp other/decl.pdf $ZIP_DIR/lang
cp grammar/gram.cf $ZIP_DIR/lang

cp other/README $ZIP_DIR
