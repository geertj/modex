# Makefile for a _VERY_ lame demo.

xdemo.exe : xdemo.c modex.h
         wcl386 /4r /mf /l=dos4g /k8192 xdemo.c x_wc32f.obj
