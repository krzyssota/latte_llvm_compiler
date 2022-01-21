./src/latc_llvm $1.lat > $1.ll
echo ""
cat $1.ll
