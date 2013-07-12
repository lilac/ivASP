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

import os
from os.path import join

def find_files(env, path):
    oldcwd = os.getcwd()
    try:
        os.chdir(Dir('#').abspath)
        sources = []
        for root, dirnames, filenames in os.walk(path):
            for filename in filenames:
                if filename.endswith(".cc") or filename.endswith(".cpp"):
                    sources.append(os.path.join(root, filename))
                if filename.endswith(".yy"):
                    target = os.path.join(root, filename[:-3],  "grammar.cc")
                    source = os.path.join(root, filename)
                    sources.append(target)
                    env.Bison(target, source)
                if filename.endswith(".xh"):
                    target = os.path.join(root, filename[:-3] + ".hh")
                    source = os.path.join(root, filename)
                    env.Re2c(target, source)
        return sources
    finally:
        os.chdir(oldcwd)

def bison_emit(target, source, env):
    path = os.path.split(str(target[0]))[0];
    target += [os.path.join(path, "grammar.hh"), os.path.join(path, "position.hh"), os.path.join(path, "location.hh"), os.path.join(path, "grammar.out")]
    return target, source

def CheckBison(context):
    context.Message('Checking for bison 2.5... ')
    (result, output) = context.TryAction("${BISON} ${SOURCE} -o /dev/null", '%require "2.5"\n%%\nstart:', ".y")
    context.Result(result)
    return result

def CheckRe2c(context):
    context.Message('Checking for re2c... ')
    (result, output) = context.TryAction("${RE2C} ${SOURCE}", '', ".x")
    context.Result(result)
    return result


Import('env')
env['ENV']['PATH'] = os.environ['PATH']

bison_action = Action("${BISON} -r all --report-file=${str(TARGET)[:-3]}.out -o ${TARGET} ${SOURCE} ${test}")

bison_builder = Builder(
    action = bison_action,
    emitter = bison_emit,
    suffix = '.cc',
    src_suffix = '.yy'
    )

re2c_action = Action("${RE2C} -o ${TARGET} ${SOURCE}")

re2c_builder = Builder(
    action = re2c_action,
    suffix = '.hh',
    src_suffix = '.xh'
    )

env['BUILDERS']['Bison'] = bison_builder
env['BUILDERS']['Re2c']  = re2c_builder

conf = Configure(env, custom_tests = {'CheckBison' : CheckBison, 'CheckRe2c' : CheckRe2c}, log_file = join("build", GetOption('build_dir') + ".log"))

if not conf.CheckBison():
    print 'error: no usable bison version found'
    Exit(1)

if not conf.CheckRe2c():
    print 'error: no usable re2c version found'
    Exit(1)

cppunit = conf.CheckLibWithHeader('libcppunit', 'cppunit/TestFixture.h', 'C++')
if not cppunit:
    print "note: to run unit tests install cppunit"

env = conf.Finish()

# ProgramOpt: Library
programOptEnv = env.Clone()
programOptEnv['CPPPATH'] = [Dir('#libprogram_opts'), 'libprogram_opts/src']

LIBPROGRAMOPTS_SRC = find_files(programOptEnv, 'libprogram_opts/src')
programOptEnv.StaticLibrary('libprogram_opts', LIBPROGRAMOPTS_SRC)

# Clasp: Library + Program

claspEnv = env.Clone()
claspEnv['CPPPATH']  = [Dir('#libclasp'), 'libclasp/src']

CLASP_SOURCES    = find_files(claspEnv, 'app/clasp')
LIBCLASP_SOURCES = find_files(claspEnv, 'libclasp/src')

claspEnv.Append(CPPDEFINES={'WITH_THREADS' : '0'})
claspEnv.StaticLibrary('libclasp', LIBCLASP_SOURCES)
claspEnv.Append(CPPPATH = [Dir('#libprogram_opts')])
claspProgram = claspEnv.Program('clasp', CLASP_SOURCES, LIBS=['libclasp', 'libprogram_opts'], LIBSPATH=Dir('.'))

# Gringo: Library + Program

gringoEnv = env.Clone()
gringoEnv['CPPPATH']  = [Dir('#libgringo'), 'libgringo/src']

GRINGO_SOURCES    = find_files(gringoEnv, 'app/gringo')
LIBGRINGO_SOURCES = find_files(gringoEnv, 'libgringo/src')

gringoEnv.StaticLibrary('libgringo', LIBGRINGO_SOURCES)
gringoProgram = gringoEnv.Program('gringo', GRINGO_SOURCES, LIBS=[ 'libgringo' ], LIBPATH=Dir('.'))

if not env.GetOption('clean'):
    Default(gringoProgram)
    Default(claspProgram)

# Gringo: UnitTests

if cppunit:
    testGringoEnv             = gringoEnv.Clone()
    testGringoEnv['CPPPATH'] += [Dir('#libclasp'), 'libclasp/src']
    TEST_LIBGRINGO_SOURCES    = find_files(testGringoEnv, 'libgringo/tests')
    testGringoProgram         = testGringoEnv.Program('test_libgringo', TEST_LIBGRINGO_SOURCES, LIBS=['libclasp', 'libgringo', 'libcppunit'], LIBPATH=Dir('.'))
    testGringoAlias           = testGringoEnv.Alias('test', [testGringoProgram], testGringoProgram[0].path + (" " + GetOption("test_case") if GetOption("test_case") else ""))
    AlwaysBuild(testGringoAlias)

# Ctags

ctagsCommand = env.Command('ctags', [], 'ctags --c++-kinds=+p --fields=+imaS --extra=+q -R libgringo app')
ctagsAlias   = env.Alias('tags', [ctagsCommand])
env.AlwaysBuild(ctagsCommand)

