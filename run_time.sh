./src/latc lattests/mine/$1.lat > lattests/mine/$1.ll
llvm-as lattests/mine/$1.ll -o lattests/mine/$1.bc
llvm-link -o lattests/mine/$1_linked.bc lattests/mine/$1.bc lib/runtime.bc
lli lattests/mine/$1_linked.bc

