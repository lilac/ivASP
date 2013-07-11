// {{{ GPL License 

// This file is part of gringo - a grounder for logic programs.
// Copyright (C) 2013  Roland Kaminski

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

// }}}

#include "gringo/ground/program.hh"
#include "gringo/input/nongroundparser.hh"
#include "gringo/input/program.hh"
#include "gringo/output/output.hh"

#include "tests/tests.hh"

namespace Gringo { namespace Ground { namespace Test {

// {{{ declaration of TestProgram

class TestProgram : public CppUnit::TestFixture {
    CPPUNIT_TEST_SUITE(TestProgram);
        CPPUNIT_TEST(test_toGround);
        CPPUNIT_TEST(test_analyze);
    CPPUNIT_TEST_SUITE_END();

public:
    virtual void setUp();
    virtual void tearDown();

    void test_toGround();
    void test_analyze();

    virtual ~TestProgram();
};

// }}}

using namespace Gringo::IO;

// {{{ definition of auxiliary functions

namespace {

typedef std::string S;

Program parse(std::string const &str) {
    std::ostringstream oss;
    Output::OutputBase out(oss);
    Input::Program prg;
    Defines defs;
    Input::NongroundProgramBuilder pb{ prg, out, defs };
    Input::NonGroundParser ngp{ pb };
    ngp.pushStream("-", make_unique<std::stringstream>(str));
    ngp.parse();
    prg.rewrite(defs);
    return prg.toGround(out.domains);
}

} // namespace

// }}}
// {{{ definition of TestProgram

void TestProgram::setUp() {
}

void TestProgram::tearDown() {
}

void TestProgram::test_toGround() {
    Gringo::Test::Messages msg;
    CPPUNIT_ASSERT_EQUAL(S(
        "% positive component\n"
        "p(X):-q(X)."),
        to_string(parse("p(X):-q(X).")));
    CPPUNIT_ASSERT_EQUAL(S(
        "% positive component\n"
        "p(#X0):-#X0=1..2."),
        to_string(parse("p(1..2).")));
    CPPUNIT_ASSERT_EQUAL(S(
        "% positive component\n"
        "p(X):-X=1."),
        to_string(parse("p(X):-X=1.")));
    CPPUNIT_ASSERT_EQUAL(S(
        "% component\n"
        ":-0==0."),
        to_string(parse(":-.")));
    CPPUNIT_ASSERT_EQUAL(S(
        "% component\n"
        ":-#not #not p."),
        to_string(parse("not p.")));
    CPPUNIT_ASSERT_EQUAL(S(
        "% positive component\n"
        "p(X,Y):-X=Y,p(X),p(Y),Y=X."),
        to_string(parse("p(X,Y):-X=Y,p(X),p(Y).")));
    CPPUNIT_ASSERT_EQUAL(S(
        "% positive component\n"
        "#accumulate(#d0(Z,X,Y),tuple(#special)):-:-p(X,Y,Z),0>Z.\n"
        "% positive component\n"
        "#accumulate(#d0(Z,X,Y),tuple(A)):-q(A),r(A,X):-p(X,Y,Z).\n"
        "% positive component\n"
        "#accumulate(#d0(Z,X,Y),tuple(B,Y)):-a(B,Y):-p(X,Y,Z).\n"
        "% positive component\n"
        "x:-p(X,Y,Z),#d0(Z,X,Y)."),
        to_string(parse("x:-p(X,Y,Z),Z<#count{A:q(A),r(A,X);B,Y:a(B,Y)}.")));
    CPPUNIT_ASSERT_EQUAL(S(
        "% positive component\n"
        "#accumulate(#d0(Z,X,Y),tuple(#special)):-:-p(X,Y,Z),0==Z.\n"
        "% positive component\n"
        "#accumulate(#d0(Z,X,Y),tuple(A)):-q(A),r(A,X):-p(X,Y,Z).\n"
        "% positive component\n"
        "#accumulate(#d0(Z,X,Y),tuple(B,Y)):-a(B,Y):-p(X,Y,Z).\n"
        "% positive component\n"
        "x:-p(X,Y,Z),#d0(Z,X,Y)."),
        to_string(parse("x:-p(X,Y,Z),Z=#count{A:q(A),r(A,X);B,Y:a(B,Y)}.")));
    CPPUNIT_ASSERT_EQUAL(S(
        "% component\n"
        "Z<#count(#d1(Z,X,Y)):-p(X,Y,Z).\n"
        "% component\n"
        "#accumulate(#d1(Z,X,Y),tuple(B,Y),head(x)):-a(B,Y),#d1(Z,X,Y).\n"
        "#accumulate(#d1(Z,X,Y),tuple(A),head(q(A))):-r(A,X),#d1(Z,X,Y)."),
        to_string(parse("Z<#count{A:q(A):r(A,X);B,Y:x:a(B,Y)}:-p(X,Y,Z).")));
    CPPUNIT_ASSERT_EQUAL(S(
        "% positive component\n"
        "#accumulate(#d0(Z,X,Y),tuple(#special)):-:-p(X,Y,Z),0>Z.\n"
        "% positive component\n"
        "#accumulate(#d0(Z,X,Y),tuple(1,1,q(A))):-r(A,X),#not q(A):-p(X,Y,Z).\n"
        "% positive component\n"
        "#accumulate(#d0(Z,X,Y),tuple(1,0,a(B,Y))):-a(B,Y):-p(X,Y,Z).\n"
        "% positive component\n"
        "#accumulate(#d0(Z,X,Y),tuple(1,3,X,1)):-q(X),X>1:-p(X,Y,Z).\n"
        "% positive component\n"
        "x:-p(X,Y,Z),#d0(Z,X,Y)."),
        to_string(parse("x:-p(X,Y,Z),Z<{not q(A):r(A,X);a(B,Y);X>1:q(X)}.")));
    CPPUNIT_ASSERT_EQUAL(S(
        "% component\n"
        "Z<#count(#d1(Z,X,Y)):-p(X,Y,Z).\n"
        "% component\n"
        "#accumulate(#d1(Z,X,Y),tuple(1,3,X,1),head(#true)):-q(X),X>1,#d1(Z,X,Y).\n"
        "#accumulate(#d1(Z,X,Y),tuple(1,0,a(B,Y)),head(a(B,Y))):-#d1(Z,X,Y).\n"
        "#accumulate(#d1(Z,X,Y),tuple(1,1,q(A)),head(#true)):-r(A,X),#not q(A),#d1(Z,X,Y)."),
        to_string(parse("Z<{not q(A):r(A,X);a(B,Y);X>1:q(X)}:-p(X,Y,Z).")));
    CPPUNIT_ASSERT_EQUAL(S(
        "% component\n"
        "#d0(Y,X):-p(X,Y,Z).\n"
        "% component\n"
        "a(B,Y):-#d0(Y,X).\n"
        "% component\n"
        "q(A):-r(A,X),#d0(Y,X)."),
        to_string(parse("q(A):r(A,X);a(B,Y):-p(X,Y,Z).")));
    CPPUNIT_ASSERT_EQUAL(S(
        "% component\n"
        "#d0(Y,X):-p(X,Y,Z).\n"
        "% component\n"
        "a(B,Y):-#d0(Y,X).\n"
        "% component\n"
        "#true:-q(X),X>1,#d0(Y,X).\n"
        "% component\n"
        "#true:-r(A,X),#not q(A),#d0(Y,X)."),
        to_string(parse("not q(A):r(A,X);a(B,Y);X>1:q(X):-p(X,Y,Z).")));
    CPPUNIT_ASSERT_EQUAL(S(
        "% positive component\n"
        "#accumulate(#d0(Z,X,Y),tuple(#special)):-:-p(X,Y,Z),0>Z.\n"
        "% positive component\n"
        "#accumulate(#d0(Z,X,Y),tuple(A)):-q(A),r(A,X):-p(X,Y,Z).\n"
        "% positive component\n"
        "#accumulate(#d0(Z,X,Y),tuple(B,Y)):-a(B,Y):-p(X,Y,Z).\n"
        "% positive component\n"
        "x:-p(X,Y,Z),#d0(Z,X,Y)."),
        to_string(parse("x:-p(X,Y,Z),Z<#count{A:q(A),r(A,X);B,Y:a(B,Y)}.")));
}

void TestProgram::test_analyze() {
    Gringo::Test::Messages msg;
    CPPUNIT_ASSERT_EQUAL(S(
        "% positive component\n"
        "x:-x?.\n"
        "% positive component\n"
        "b:-a?.\n"
        "a:-b?,x,#not y.\n"
        "% positive component\n"
        "c:-a,b."),
        to_string(parse("x:-x.a:-b,x,not y.b:-a.c:-a,b.")));
    CPPUNIT_ASSERT_EQUAL(S(
        "% positive component\n"
        "x:-x?.\n"
        "% component\n"
        "a:-#not b?.\n"
        "% component\n"
        "b:-#not a!,a!,x."),
        to_string(parse("x:-x.a:-not b.b:-not a,a,x.")));
    // TODO: all the other statements are missing
}

TestProgram::~TestProgram() { }

// }}}

CPPUNIT_TEST_SUITE_REGISTRATION(TestProgram);

} } } // namespace Test Ground Gringo
