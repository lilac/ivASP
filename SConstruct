#!/usr/bin/python
# {{{ GPL License

# This file is part of gringo - a grounder for logic programs.
# Copyright (C) 2013  Roland Kaminski

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# }}}

from os.path import join

AddOption('--build-dir', default='debug', metavar='DIR', nargs=1, type='string', dest='build_dir')
AddOption('--test-case', default=None, metavar='NAME', nargs=1, type='string', dest='test_case')

# Note: workaround for scons limitation (try to get a hand on the internal option parser)

opts_file = join("build", GetOption('build_dir') + ".py")

opts = Variables(opts_file, ARGUMENTS)
opts.AddVariables(
    ('CXX'      , 'compiler'),
    ('CXXFLAGS' , 'compiler flags'),
    ('LINKFLAGS', 'linker flags'),
    ('AR'       , 'path to ar'),
    ('ARFLAGS'  , 'ar flags'),
    ('RANLIB'   , 'path to ranlib'),
    ('BISON'    , 'path to bison executable'),
    ('RE2C'     , 'path to re2c executable'),
    )

env = Environment()
env['BISON']    = 'bison'
env['RE2C']     = 're2c'
env['CXX']      = 'clang++'
env['CXXFLAGS'] = ['-std=c++11', '-stdlib=libc++', '-g', '-Wall']
env['LINKFLAGS'] = ['-lc++']

if GetOption("build_dir") == "static":
    env['CXXFLAGS']  = ['-std=c++11', '-O3', '-Wall', '-pedantic', '-Werror']
    env['LINKFLAGS'] = ['-O3', '-static']
elif GetOption("build_dir") == "release":
    env['CXXFLAGS']  = ['-std=c++11', '-O3', '-march=native', '-Wall']
    env['LINKFLAGS'] = ['-O3']

opts.Update(env)
opts.Save(opts_file, env)
opts.FormatVariableHelpText = lambda env, opt, help, default, actual, other: "%10s: %s (%s)\n" % (opt, help, actual)

Help(
"""
usage: scons [OPTION] [TARGET] ...

Options:
  --build-dir=DIR             Sets the build directory to build/DIR. If DIR is
                              release or static then options are set,
                              respectively. Otherwise, debug options are set.
                              Default: debug
  --test-case=NAME            Selects which test case to run. If empty all
                              tests will be executed.
                              Default: ''

Targets:
  <no target>                 Build everything. (default)
  test                        Build and run unit tests.
  tags                        Generate ctags file.

Variables:
""" + opts.GenerateHelpText(env))

# Notes to use gold linker:
#   scons --build=gold \
#     CXXFLAGS="-std=c++11 -O4 -Wall" \
#     LINKFLAGS="-O4 -B build/gold/ld-gold/" \
#     RANLIB=true \
#     ARFLAGS="rc --plugin /usr/lib/llvm/LLVMgold.so"

# Notes to set rpath:
#   scons --build=release \
#     LINKFLAGS="-Wl,-rpath -Wl,/home/wv/bin/linux/64/gcc-4.7/lib"

if not env.GetOption('help'):
    SConscript('SConscript', variant_dir=join('build', GetOption('build_dir')), duplicate=0, exports=['env', 'opts'])

