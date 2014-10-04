rem Compilacion de fuentes RC
    RC /i C:\wint\include\ .\rc\appencoder.rc
    RES2OBJ .\rc\appencoder.RES .\rc\appencoder.obj

rem Compilacion de fuentes para windows
    lf95 -c .\rc\resource.f90          -o0 -chk

rem Compilacion de librería RS-232 fortran
    lf95 SCCINF.F95 -C -ML LF95 -FIX -IN -O0 -SAV

rem Compilacion de Programa
       
    lf95 -c main.f90					   	    -o0 -chk -mod .;C:\wint\lib.l95

rem Enlazar todo
    lf95 *.obj .\rc\*.obj -out AppEncoder2 -o0 -win -lib "C:\wint\lib.l95\winter" -lib SCCIMP

   del *.obj
   del *.mod
   del *.map
   del *.bak
