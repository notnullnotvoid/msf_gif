@echo off
rem del giff.exe
rem 2010 MSVC++ config (without profiling)
cl /c /Ox /W2 msf_gif.cpp /Fo:msf_gif.o
REM cl msf_gif.o trace.o test.o /Fe:giff.exe
REM giff.exe