@REM cabal.exe build Baleia
@REM call ./scripts/deploy.bat
cd D:\Baleia-1.0\ctest
g++.exe -std=c++17 -I"D:\ghcup\ghc\9.12.1\lib\x86_64-windows-ghc-9.12.1-7089\rts-1.0.2\include" -o tester tester.cpp "..\\Baleia.lib"
tester.exe 1000