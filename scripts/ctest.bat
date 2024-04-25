cabal.exe build Baleia
call ./scripts/deploy.bat
cd D:\VST_SDK\my_plugins\baleia\Baleia-1.0\test
g++.exe -std=c++17 -o tester tester.cpp MyModule.dll.a
tester.exe