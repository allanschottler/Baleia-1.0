cd D:\VST_SDK\my_plugins\baleia\Baleia-1.0\dist-newstyle\build\x86_64-windows\ghc-9.8.2\Baleia-1.0\f\Baleia\build\Baleia\

xcopy /Y Baleia.dll D:\VST_SDK\my_plugins\baleia\Baleia-1.0\bin\MyModule.dll*
xcopy /Y Baleia.dll.a D:\VST_SDK\my_plugins\baleia\Baleia-1.0\bin\MyModule.dll.a*
xcopy /Y Baleia-tmp\Start_stub.h D:\VST_SDK\my_plugins\baleia\Baleia-1.0\bin\Start_stub.h*

xcopy /Y Baleia.dll D:\VSTHost\MyModule.dll*
xcopy /Y Baleia.dll D:\VST_SDK\my_plugins\baleia\Baleia-1.0\test\MyModule.dll*
xcopy /Y Baleia.dll.a D:\VST_SDK\my_plugins\baleia\Baleia-1.0\test\MyModule.dll.a*
xcopy /Y Baleia-tmp\Start_stub.h D:\VST_SDK\my_plugins\baleia\Baleia-1.0\test\Start_stub.h*