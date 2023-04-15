$ErrorActionPreference = "Stop"

npm run res:build
node src/Demo.bs.js

clang.exe -c .\machine_code.s -o .\machine_code.o
llvm-ar.exe rc .\machine_code.lib .\machine_code.o

cargo.exe run -p shellcode
