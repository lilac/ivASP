#!/bin/sh
case `uname` in
  "Linux" ) scons --build-dir=linux $@;;
  "darwin*" ) scons $@ 
esac
