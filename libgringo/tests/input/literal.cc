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

#include "tests/tests.hh"
#include "tests/term_helper.hh"
#include "tests/input/lit_helper.hh"

#include <climits>
#include <sstream>

namespace Gringo { namespace Input { namespace Test {

// {{{ declaration of TestLiteral

class TestLiteral : public CppUnit::TestFixture {
    CPPUNIT_TEST_SUITE(TestLiteral);
        CPPUNIT_TEST(test_print);
        CPPUNIT_TEST(test_clone);
        CPPUNIT_TEST(test_hash);
        CPPUNIT_TEST(test_equal);
        CPPUNIT_TEST(test_unpool);
        CPPUNIT_TEST(test_rewrite);
    CPPUNIT_TEST_SUITE_END();

public:
    virtual void setUp();
    virtual void tearDown();

    void test_print();
    void test_clone();
    void test_hash();
    void test_equal();
    void test_unpool();
    void test_rewrite();

    virtual ~TestLiteral();
};

// }}}

using namespace Gringo::IO;

// {{{ auxiliary functions and classes

namespace {


ULit rewrite(ULit &&x) {
    unsigned auxNum = 0;
    Literal::ProjectionMap project;
    Literal::AssignVec assign;
    Term::DotsMap dots;
    Term::ArithmeticsMap arith;
    x->simplify(project, dots, auxNum);
    x->rewriteArithmetics(arith, assign, auxNum);
    return std::move(x);
}

} // namespace

// }}}
// {{{ definition of TestLiteral

void TestLiteral::setUp() { }

void TestLiteral::tearDown() { }

void TestLiteral::test_print() {
    CPPUNIT_ASSERT_EQUAL(std::string("x"), to_string(pred(NAF::POS, val("x"))));
    CPPUNIT_ASSERT_EQUAL(std::string("#not x"), to_string(pred(NAF::NOT, val("x"))));
    CPPUNIT_ASSERT_EQUAL(std::string("#not #not x"), to_string(pred(NAF::NOTNOT, val("x"))));
    CPPUNIT_ASSERT_EQUAL(std::string("x==y"), to_string(rel(Relation::EQ, val("x"), val("y"))));
    CPPUNIT_ASSERT_EQUAL(std::string("#range(x,y,z)"), to_string(range(val("x"), val("y"), val("z"))));
}

void TestLiteral::test_clone() {
    CPPUNIT_ASSERT_EQUAL(std::string("x"), to_string(get_clone(pred(NAF::POS, val("x")))));
    CPPUNIT_ASSERT_EQUAL(std::string("#not x"), to_string(get_clone(pred(NAF::NOT, val("x")))));
    CPPUNIT_ASSERT_EQUAL(std::string("#not #not x"), to_string(get_clone(pred(NAF::NOTNOT, val("x")))));
    CPPUNIT_ASSERT_EQUAL(std::string("x==x"), to_string(get_clone(rel(Relation::EQ, val("x"), val("x")))));
    CPPUNIT_ASSERT_EQUAL(std::string("#range(x,y,z)"), to_string(get_clone(range(val("x"), val("y"), val("z")))));
}

void TestLiteral::test_hash() {
    char const *msg = "warning: hashes are very unlikely to collide";
    CPPUNIT_ASSERT_MESSAGE(msg, pred(NAF::POS, val("x"))->hash() == pred(NAF::POS, val("x"))->hash());
    CPPUNIT_ASSERT_MESSAGE(msg, !(pred(NAF::POS, val("x"))->hash() == pred(NAF::NOT, val("x"))->hash()));
    CPPUNIT_ASSERT_MESSAGE(msg, !(pred(NAF::POS, val("x"))->hash() == pred(NAF::POS, val("y"))->hash()));
    CPPUNIT_ASSERT_MESSAGE(msg, rel(Relation::EQ, val("x"), val("x"))->hash() == rel(Relation::EQ, val("x"), val("x"))->hash());
    CPPUNIT_ASSERT_MESSAGE(msg, !(rel(Relation::EQ, val("x"), val("x"))->hash() == rel(Relation::LT, val("x"), val("x"))->hash()));
    CPPUNIT_ASSERT_MESSAGE(msg, !(rel(Relation::EQ, val("x"), val("x"))->hash() == rel(Relation::EQ, val("y"), val("x"))->hash()));
    CPPUNIT_ASSERT_MESSAGE(msg, !(rel(Relation::EQ, val("x"), val("x"))->hash() == rel(Relation::EQ, val("x"), val("y"))->hash()));
    CPPUNIT_ASSERT_MESSAGE(msg, range(val("x"), val("y"), val("z"))->hash() == range(val("x"), val("y"), val("z"))->hash());
    CPPUNIT_ASSERT_MESSAGE(msg, !(range(val("x"), val("x"), val("z"))->hash() == range(val("x"), val("y"), val("z"))->hash()));
    CPPUNIT_ASSERT_MESSAGE(msg, !(range(val("x"), val("y"), val("x"))->hash() == range(val("x"), val("y"), val("z"))->hash()));
    CPPUNIT_ASSERT_MESSAGE(msg, !(range(val("y"), val("y"), val("z"))->hash() == range(val("x"), val("y"), val("z"))->hash()));
}

void TestLiteral::test_equal() {
    CPPUNIT_ASSERT(is_value_equal_to(pred(NAF::POS, val("x")), pred(NAF::POS, val("x"))));
    CPPUNIT_ASSERT(!is_value_equal_to(pred(NAF::POS, val("x")), pred(NAF::NOT, val("x"))));
    CPPUNIT_ASSERT(!is_value_equal_to(pred(NAF::POS, val("x")), pred(NAF::POS, val("y"))));
    CPPUNIT_ASSERT(is_value_equal_to(rel(Relation::EQ, val("x"), val("x")), rel(Relation::EQ, val("x"), val("x"))));
    CPPUNIT_ASSERT(!is_value_equal_to(rel(Relation::EQ, val("x"), val("x")), rel(Relation::LT, val("x"), val("x"))));
    CPPUNIT_ASSERT(!is_value_equal_to(rel(Relation::EQ, val("x"), val("x")), rel(Relation::EQ, val("y"), val("x"))));
    CPPUNIT_ASSERT(!is_value_equal_to(rel(Relation::EQ, val("x"), val("x")), rel(Relation::EQ, val("x"), val("y"))));
    CPPUNIT_ASSERT(is_value_equal_to(range(val("x"), val("y"), val("z")), range(val("x"), val("y"), val("z"))));
    CPPUNIT_ASSERT(!is_value_equal_to(range(val("x"), val("x"), val("z")), range(val("x"), val("y"), val("z"))));
    CPPUNIT_ASSERT(!is_value_equal_to(range(val("x"), val("y"), val("x")), range(val("x"), val("y"), val("z"))));
    CPPUNIT_ASSERT(!is_value_equal_to(range(val("y"), val("y"), val("z")), range(val("x"), val("y"), val("z"))));
}

void TestLiteral::test_unpool() {
    CPPUNIT_ASSERT_EQUAL(std::string("[x,y,z]"), to_string(pred(NAF::POS, pool(val("x"), val("y"), val("z")))->unpool()));
    CPPUNIT_ASSERT_EQUAL(std::string("[#not x,#not y,#not z]"), to_string(pred(NAF::NOT, pool(val("x"), val("y"), val("z")))->unpool()));
    CPPUNIT_ASSERT_EQUAL(std::string("[a!=x,a!=y,b!=x,b!=y]"), to_string(rel(Relation::NEQ, pool(val("a"), val("b")), pool(val("x"), val("y")))->unpool()));
}

void TestLiteral::test_rewrite() {
    CPPUNIT_ASSERT_EQUAL(std::string("3"), to_string(rewrite(pred(NAF::POS, binop(BinOp::ADD, val(1), val(2))))));
    CPPUNIT_ASSERT_EQUAL(std::string("3>7"), to_string(rewrite(rel(Relation::GT, binop(BinOp::ADD, val(1), val(2)), binop(BinOp::ADD, val(3), val(4))))));
}

TestLiteral::~TestLiteral() { }
// }}}

CPPUNIT_TEST_SUITE_REGISTRATION(TestLiteral);

} } } // namespace Test Input Gringo

