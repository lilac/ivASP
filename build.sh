#!/bin/sh
case `uname` in
  "Linux" ) scons --build-dir=linux $@;;
  "Darwin" ) scons $@ 
esac
