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

#include "gringo/ground/dependency.hh"

#include "tests/tests.hh"
#include "tests/term_helper.hh"
#include "tests/output/solver_helper.hh"

#include "gringo/input/nongroundparser.hh"

#include <clasp/reader.h>
#include <clasp/program_builder.h>
#include <clasp/solver.h>
#include <clasp/minimize_constraint.h>

#include <clasp/model_enumerators.h>
#include <clasp/solve_algorithms.h>

#include <functional>

namespace Gringo { namespace Output { namespace Test {

// {{{ declaration of TestLparse

class TestLparse : public CppUnit::TestFixture {
    CPPUNIT_TEST_SUITE(TestLparse);
        CPPUNIT_TEST(test_empty);
        CPPUNIT_TEST(test_projectionBug);
        CPPUNIT_TEST(test_head);
        CPPUNIT_TEST(test_assign);
        CPPUNIT_TEST(test_queens);
        CPPUNIT_TEST(test_conjunction);
        CPPUNIT_TEST(test_disjunction);
        CPPUNIT_TEST(test_show);
        CPPUNIT_TEST(test_aggregates);
    CPPUNIT_TEST_SUITE_END();
    using S = std::string;

public:
    virtual void setUp();
    virtual void tearDown();

    void test_empty();
    void test_projectionBug();
    void test_head();
    void test_assign();
    void test_queens();
    void test_conjunction();
    void test_disjunction();
    void test_show();
    void test_aggregates();
    virtual ~TestLparse();
};

// }}}

// {{{ definition of TestLparse

void TestLparse::setUp() {
}

void TestLparse::tearDown() {
}

void TestLparse::test_empty() {
    CPPUNIT_ASSERT_EQUAL(S("[[]]"), IO::to_string(solve("")));
}

void TestLparse::test_projectionBug() {
    CPPUNIT_ASSERT_EQUAL(S("[[p(1),p(2)]]"), IO::to_string(solve(
        "q((1,x),2).\n"
        "p(A) :- q((A,_),_).\n"
        "p(B) :- q((A,_),B).\n"
        , {"p("})));
}
void TestLparse::test_head() {
    CPPUNIT_ASSERT_EQUAL(S("[[],[a],[a,b],[b]]"), IO::to_string(solve("{a;b}.")));
    CPPUNIT_ASSERT_EQUAL(S("[[a],[b]]"), IO::to_string(solve("1{a;b}1.")));
    CPPUNIT_ASSERT_EQUAL(S("[[p(1)],[p(1),p(2)],[p(1),p(3)],[p(1),p(4)],[p(2)],[p(2),p(3)],[p(2),p(4)],[p(3)],[p(3),p(4)],[p(4)]]"), IO::to_string(solve("1#count{X:p(X):X=1..4}2.")));
    CPPUNIT_ASSERT_EQUAL(S("[[p(1)],[p(2)]]"), IO::to_string(solve("1#sum+{X:p(X):X=1..4}2.")));
    CPPUNIT_ASSERT_EQUAL(S("[[p(1)],[p(2)]]"), IO::to_string(solve("1#sum {X:p(X):X=1..4}2.")));
    CPPUNIT_ASSERT_EQUAL(S(
        "[[p(1)],[p(1),p(2)],[p(1),p(2),p(3)],[p(1),p(2),p(3),p(4)],[p(1),p(2),p(4)],[p(1),p(3)],[p(1),p(3),p(4)],[p(1),p(4)],"
        "[p(2)],[p(2),p(3)],[p(2),p(3),p(4)],[p(2),p(4)]]"),
        IO::to_string(solve("1#min{X:p(X):X=1..4}2.")));
    CPPUNIT_ASSERT_EQUAL(S("[[p(1)],[p(1),p(2)],[p(2)]]"), IO::to_string(solve("1#max{X:p(X):X=1..4}2.")));
}

void TestLparse::test_assign() {
    CPPUNIT_ASSERT_EQUAL(S("[[p,q(1)],[q(0)]]"),IO::to_string(solve("{p}. q(M):-M=#count{1:p}.")));
    CPPUNIT_ASSERT_EQUAL(S("[[p,q(1)],[q(0)]]"),IO::to_string(solve("{p}. q(M):-M=#sum+{1:p}.")));
    CPPUNIT_ASSERT_EQUAL(S("[[p,q(1)],[q(0)]]"),IO::to_string(solve("{p}. q(M):-M=#sum{1:p}.")));
    CPPUNIT_ASSERT_EQUAL(S("[[p,q(p)],[q(#sup)]]"),IO::to_string(solve("{p}. q(M):-M=#min{p:p}.")));
    CPPUNIT_ASSERT_EQUAL(S("[[p,q(p)],[q(#inf)]]"),IO::to_string(solve("{p}. q(M):-M=#max{p:p}.")));
    CPPUNIT_ASSERT_EQUAL(S(
        "[[p(1),p(2),q(1)],"
        "[p(1),p(3),q(1)],"
        "[p(1),p(4),q(1)],"
        "[p(2),p(3),q(2)],"
        "[p(2),p(4),q(2)],"
        "[p(3),p(4),q(3)]]"), 
        IO::to_string(solve("2{p(1..4)}2. q(M):-M=#min{X:p(X)}.")));
    CPPUNIT_ASSERT_EQUAL(S(
        "[[p(1),p(2),q(2)],"
        "[p(1),p(3),q(3)],"
        "[p(1),p(4),q(4)],"
        "[p(2),p(3),q(3)],"
        "[p(2),p(4),q(4)],"
        "[p(3),p(4),q(4)]]"), 
        IO::to_string(solve("2{p(1..4)}2. q(M):-M=#max{X:p(X)}.")));
}

void TestLparse::test_queens() {
    CPPUNIT_ASSERT_EQUAL(
        S("[[q(1,2),q(2,4),q(3,6),q(4,1),q(5,3),q(6,5)],"
          "[q(1,3),q(2,6),q(3,2),q(4,5),q(5,1),q(6,4)],"
          "[q(1,4),q(2,1),q(3,5),q(4,2),q(5,6),q(6,3)],"
          "[q(1,5),q(2,3),q(3,1),q(4,6),q(5,4),q(6,2)]]"),
        IO::to_string(solve(
            "#const n = 6.\n"
            "n(1..n).\n"
            "\n"
            "q(X,Y) :- n(X;Y), not not q(X,Y).\n"
            "\n"
            "        c(r,X; c,Y) :- q(X,Y).\n"
            "not not c(r,N; c,N) :- n(N).\n"
            "\n"
            "n(r,X,Y-1,X,Y; c,X-1,Y,X,Y; d1,X-1,Y-1,X,Y;     d2,X-1,Y+1,X,Y      ) :- n(X;Y).\n"
            "c(r,N,0;       c,0,N;       d1,N-1,0; d1,0,N-1; d2,N-1,n+1; d2,0,N+1) :- n(N).\n"
            "\n"
            "c(C,XX,YY) :-     c(C,X,Y), n(C,X,Y,XX,YY), not q(XX,YY).\n"
            "           :- not c(C,X,Y), n(C,X,Y,XX,YY),     q(XX,YY).\n", {"q("})));
}

void TestLparse::test_conjunction() {
    Gringo::Test::Messages msg;
    CPPUNIT_ASSERT_EQUAL(
        S("[]"),
        IO::to_string(solve(
            "a:-b:c.\n"
            "c:-a.\n")));
    CPPUNIT_ASSERT_EQUAL(S("[-:1:4-5: warning: atom is undefined:\n  b\n]"), IO::to_string(msg));
    CPPUNIT_ASSERT_EQUAL(
        S("[[a,b,c]]"),
        IO::to_string(solve(
            "a:-b:c.\n"
            "c:-a.\n"
            "b:-c.\n", {"a", "b", "c"})));
    CPPUNIT_ASSERT_EQUAL(
        S("[[a,b,c,d]]"),
        IO::to_string(solve(
            "a:-b:c,d.\n"
            "c:-a.\n"
            "d:-a.\n"
            "b:-c.\n"
            "b:-d.\n", {"a","b","c","d"})));
    CPPUNIT_ASSERT_EQUAL(
        S("[[a(1),a(2),a(3),c,q],[a(3)]]"),
        IO::to_string(solve(
            "{c}.\n"
            "a(1):-c.\n"
            "a(2):-c.\n"
            "a(3).\n"
            "q:-a(X):X=1..3.\n")));
}

void TestLparse::test_disjunction() {
    CPPUNIT_ASSERT_EQUAL(
        S("[[],[a,b,c,d],[c,x,y]]"),
        IO::to_string(solve(
            "{ y; d } 1.\n"
            "c :- y.\n"
            "c :- d.\n"
            "b :- d.\n"
            "x:y | a:b :- c.\n"
            "b :- a.\n"
            )));
}

void TestLparse::test_show() {
    CPPUNIT_ASSERT_EQUAL(
        S(
            "["
            "[(1,2,3),-q(1),42],"
            "[(1,2,3),-q(1),42],"
            "[(1,2,3),-q(1),42,p(1)],"
            "[(1,2,3),42],"
            "[(1,2,3),42],"
            "[(1,2,3),42,boo(1)],"
            "[(1,2,3),42,boo(1)],"
            "[(1,2,3),42,boo(1),p(1)],"
            "[(1,2,3),42,p(1)]"
            "]"),
        IO::to_string(solve(
            "#show p/1.\n"
            "#show -q/1.\n"
            "#show boo(X):q(X).\n"
            "#show -p/-1.\n"
            "#show (1,2,3).\n"
            "\n"
            "{p(1); q(1); -p(1); -q(1)}.\n"
            "\n"
            "#const p=42.\n")));
}

void TestLparse::test_aggregates() {
    CPPUNIT_ASSERT_EQUAL(
        S(
            "["
            "[]"
            "]"),
        IO::to_string(solve(
            "#sum { 1:b; 2:c } < 1.\n"
            )));
    CPPUNIT_ASSERT_EQUAL(S("[[p(1),p(2)],[p(1),p(3)],[p(2),p(3)]]"), IO::to_string(solve("{p(1..3)}.\n:-{p(X)}!=2.")));
    CPPUNIT_ASSERT_EQUAL(S("[[],[a,b],[b]]"), IO::to_string(solve("#sum { -1:a; 1:b } >= 0.")));
    CPPUNIT_ASSERT_EQUAL(S("[[],[a,b],[b]]"), IO::to_string(solve("#sum { 1:a; 2:b } != 1.")));
}

TestLparse::~TestLparse() { }

// }}}

CPPUNIT_TEST_SUITE_REGISTRATION(TestLparse);

} } } // namespace Test Output Gringo

