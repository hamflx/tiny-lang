npm run res:build && node src/Demo.bs.js && clang -c machine_code.s -o machine_code.o && ar rc libmachine_code.a machine_code.o && cargo run -p shellcode
