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

#include "gringo/input/nongroundparser.hh"
#include "gringo/input/programbuilder.hh"
#include "input/nongroundgrammar/grammar.hh"
#include "gringo/value.hh"

#include <cppunit/TestFixture.h>
#include <cppunit/TestAssert.h>
#include <cppunit/extensions/HelperMacros.h>
#include <climits>
#include <sstream>

namespace Gringo { namespace Input { namespace Test {

// {{{ declaration of TestNongroundProgramBuilder

class TestNongroundProgramBuilder : public INongroundProgramBuilder {
public:

    // {{{ terms
    virtual TermUid term(Location const &loc, Value val);
    virtual TermUid term(Location const &loc, FWString name);
    virtual TermUid term(Location const &loc, bool anonymous);
    virtual TermUid term(Location const &loc, UnOp op, TermUid a);
    virtual TermUid term(Location const &loc, UnOp op, TermVecUid a);
    virtual TermUid term(Location const &loc, BinOp op, TermUid a, TermUid b);
    virtual TermUid term(Location const &loc, TermUid a, TermUid b);
    virtual TermUid term(Location const &loc, FWString name, TermVecVecUid b, bool lua);
    // }}}
    // {{{ term vectors
    virtual TermVecUid termvec();
    virtual TermVecUid termvec(TermVecUid uid, TermUid term);
    // }}}
    // {{{ term vector vectors
    virtual TermVecVecUid termvecvec();
    virtual TermVecVecUid termvecvec(TermVecVecUid uid, TermVecUid termvecUid);
    // }}}
    // {{{ literals
    virtual LitUid boollit(Location const &loc, bool type);
    virtual LitUid predlit(Location const &loc, NAF naf, bool neg, FWString name, TermVecVecUid argvecvecUid);
    virtual LitUid rellit(Location const &loc, Relation rel, TermUid termUidLeft, TermUid termUidRight);
    // }}}
    // {{{ literal vectors
    virtual LitVecUid litvec();
    virtual LitVecUid litvec(LitVecUid uid, LitUid literalUid);
    // }}}
    // {{{ count aggregate elements (body/head)
    virtual CondLitVecUid condlitvec();
    virtual CondLitVecUid condlitvec(CondLitVecUid uid, LitUid lit, LitVecUid litvec);
    // }}}
    // {{{ body aggregate elements
    virtual BdAggrElemVecUid bodyaggrelemvec();
    virtual BdAggrElemVecUid bodyaggrelemvec(BdAggrElemVecUid uid, TermVecUid termvec, LitVecUid litvec);
    // }}}
    // {{{ head aggregate elements
    virtual HdAggrElemVecUid headaggrelemvec();
    virtual HdAggrElemVecUid headaggrelemvec(HdAggrElemVecUid uid, TermVecUid termvec, LitUid lit, LitVecUid litvec);
    // }}}
    // {{{ bounds
    virtual BoundVecUid boundvec();
    virtual BoundVecUid boundvec(BoundVecUid uid, Relation rel, TermUid term);
    // }}}
    // {{{ heads
    virtual HdLitUid headlit(LitUid lit);
    virtual HdLitUid headaggr(Location const &loc, AggregateFunction fun, BoundVecUid bounds, HdAggrElemVecUid headaggrelemvec);
    virtual HdLitUid headaggr(Location const &loc, AggregateFunction fun, BoundVecUid bounds, CondLitVecUid headaggrelemvec);
    virtual HdLitUid disjunction(Location const &loc, CondLitVecUid condlitvec);
    // }}}
    // {{{ bodies
    virtual BdLitVecUid body();
    virtual BdLitVecUid bodylit(BdLitVecUid body, LitUid bodylit);
    virtual BdLitVecUid bodyaggr(BdLitVecUid body, Location const &loc, NAF naf, AggregateFunction fun, BoundVecUid bounds, BdAggrElemVecUid bodyaggrelemvec);
    virtual BdLitVecUid bodyaggr(BdLitVecUid body, Location const &loc, NAF naf, AggregateFunction fun, BoundVecUid bounds, CondLitVecUid bodyaggrelemvec);
    virtual BdLitVecUid conjunction(BdLitVecUid body, Location const &loc, LitUid head, LitVecUid litvec);
    // }}}
    // {{{ statements
    virtual void rule(Location const &loc, HdLitUid head);
    virtual void rule(Location const &loc, HdLitUid head, BdLitVecUid body);
    virtual void define(Location const &loc, FWString name, TermUid value);
    virtual void optimize(Location const &loc, TermUid weight, TermUid priority, TermVecUid cond, BdLitVecUid body);
    virtual void showsig(Location const &loc, FWSignature x);
    virtual void show(Location const &loc, TermUid t, BdLitVecUid body);
    // }}}

    std::string toString();

    virtual ~TestNongroundProgramBuilder();

private:
    // {{{ typedefs
    typedef std::vector<std::string> StringVec;
    typedef std::vector<StringVec> StringVecVec;
    typedef std::pair<std::string, std::string> StringPair;
    typedef std::vector<StringPair> StringPairVec;
    typedef std::vector<TermUid> TermUidVec;
    typedef std::vector<TermVecUid> TermVecUidVec;
    typedef std::vector<LitUid> LitUidVec;

    typedef Indexed<std::string, TermUid> Terms;
    typedef Indexed<StringVec, TermVecUid> TermVecs;
    typedef Indexed<StringVecVec, TermVecVecUid> TermVecVecs;
    typedef Indexed<std::string, LitUid> Lits;
    typedef Indexed<StringVec, LitVecUid> LitVecs;
    typedef Indexed<StringVec, BdAggrElemVecUid> BodyAggrElemVecs;
    typedef Indexed<StringVec, CondLitVecUid> CondLitVecs;
    typedef Indexed<StringVec, HdAggrElemVecUid> HeadAggrElemVecs;
    typedef Indexed<StringVec, BdLitVecUid> Bodies;
    typedef Indexed<std::string, HdLitUid> Heads;
    typedef Indexed<StringPairVec, BoundVecUid> Bounds;
    typedef std::vector<std::string> Statements;
    // }}}
    // {{{ auxiliary functions
    std::string str();
    void print(StringVec const &vec, char const *sep);
    void print(StringVecVec const &vec);
    void print(AggregateFunction fun, BoundVecUid boundvecuid, StringVec const &elems);
    void print(NAF naf);
    // }}}
    // {{{ member variables
    Terms terms_;
    TermVecs termvecs_;
    TermVecVecs termvecvecs_;
    Lits lits_;
    LitVecs litvecs_;
    BodyAggrElemVecs bodyaggrelemvecs_;
    CondLitVecs condlitvecs_;
    HeadAggrElemVecs headaggrelemvecs_;
    Bodies bodies_;
    Heads heads_;
    Bounds bounds_;
    Statements statements_;
    std::stringstream current_;
    // }}}
};

// }}}
// {{{ declaration of TestNongroundGrammar
class TestNongroundGrammar : public CppUnit::TestFixture {
    CPPUNIT_TEST_SUITE(TestNongroundGrammar);
        CPPUNIT_TEST(test_term);
        CPPUNIT_TEST(test_atomargs);
        CPPUNIT_TEST(test_literal);
        CPPUNIT_TEST(test_bdaggr);
        CPPUNIT_TEST(test_hdaggr);
        CPPUNIT_TEST(test_conjunction);
        CPPUNIT_TEST(test_disjunction);
        CPPUNIT_TEST(test_rule);
        CPPUNIT_TEST(test_define);
        CPPUNIT_TEST(test_optimize);
        CPPUNIT_TEST(test_show);
    CPPUNIT_TEST_SUITE_END();

public:
    virtual void setUp();
    virtual void tearDown();

    void test_term();
    void test_atomargs();
    void test_literal();
    void test_bdaggr();
    void test_hdaggr();
    void test_conjunction();
    void test_disjunction();
    void test_rule();
    void test_define();
    void test_optimize();
    void test_show();

    virtual ~TestNongroundGrammar();

    std::string parse(std::string const &str);
};

// }}}

// {{{ definition of TestNongroundProgramBuilder

// {{{ terms

TermUid TestNongroundProgramBuilder::term(Location const &, Value val) {
    current_ << val;
    return terms_.emplace(str());
}

TermUid TestNongroundProgramBuilder::term(Location const &, FWString name) {
    current_ << *name;
    return terms_.emplace(str());
}

TermUid TestNongroundProgramBuilder::term(Location const &, bool anonymous) {
    current_ << (anonymous ? "_" : "*");
    return terms_.emplace(str());
}

TermUid TestNongroundProgramBuilder::term(Location const &, UnOp op, TermUid a) { 
    if(op == UnOp::ABS) { current_ << "|"; }
    else { current_ << "(" << op; }
    current_ << terms_.erase(a) << (op == UnOp::ABS ? "|" : ")");
    return terms_.emplace(str());
}

TermUid TestNongroundProgramBuilder::term(Location const &, UnOp op, TermVecUid a) { 
    if(op == UnOp::ABS) { current_ << "|"; }
    else { current_ << op << "("; }
    print(termvecs_.erase(a), ";");
    current_ << (op == UnOp::ABS ? "|" : ")");
    return terms_.emplace(str());
}

TermUid TestNongroundProgramBuilder::term(Location const &, BinOp op, TermUid a, TermUid b) {
    current_ << "(" << terms_.erase(a) << op << terms_.erase(b) << ")";
    return terms_.emplace(str());
}

TermUid TestNongroundProgramBuilder::term(Location const &, TermUid a, TermUid b) {
    current_ << "(" << terms_.erase(a) << ".." << terms_.erase(b) << ")";
    return terms_.emplace(str());
}

TermUid TestNongroundProgramBuilder::term(Location const &, FWString name, TermVecVecUid a, bool lua) {
    assert(!termvecvecs_[a].empty());
    bool nempty = lua || termvecvecs_[a].size() > 1 || !termvecvecs_[a].front().empty() || *name == "";
    if (lua) { current_ << "@"; }
    current_ << *name;
    if (nempty) { current_ << "("; }
    print(termvecvecs_.erase(a));
    if (nempty) { current_ << ")"; }
    return terms_.emplace(str());
}

// }}}
// {{{ term vectors

TermVecUid TestNongroundProgramBuilder::termvec() { 
    return termvecs_.emplace();
}

TermVecUid TestNongroundProgramBuilder::termvec(TermVecUid uid, TermUid term) {
    termvecs_[uid].emplace_back(terms_.erase(term));
    return uid;
}

// }}}
// {{{ term vector vectors

TermVecVecUid TestNongroundProgramBuilder::termvecvec() {
    return termvecvecs_.emplace();
}

TermVecVecUid TestNongroundProgramBuilder::termvecvec(TermVecVecUid uid, TermVecUid termvecUid) { 
    termvecvecs_[uid].push_back(termvecs_.erase(termvecUid));
    return uid;
}

// }}}
// {{{ literals

LitUid TestNongroundProgramBuilder::boollit(Location const &, bool type) { 
    return lits_.emplace(type ? "#true" : "#false");
}

LitUid TestNongroundProgramBuilder::predlit(Location const &, NAF naf, bool neg, FWString name, TermVecVecUid tvvUid) {
    print(naf);
    if (neg) { current_ << "-"; }
    current_ << *name;
    bool nempty = termvecvecs_[tvvUid].size() > 1 || !termvecvecs_[tvvUid].front().empty();
    if (nempty) { current_ << "("; }
    print(termvecvecs_.erase(tvvUid));
    if (nempty) { current_ << ")"; }
    return lits_.emplace(str());
}

LitUid TestNongroundProgramBuilder::rellit(Location const &, Relation rel, TermUid termUidLeft, TermUid termUidRight) { 
    current_ << terms_.erase(termUidLeft) << rel << terms_.erase(termUidRight);
    return lits_.emplace(str());
}

// }}}
// {{{ literal vectors

LitVecUid TestNongroundProgramBuilder::litvec() { 
    return litvecs_.emplace();
}

LitVecUid TestNongroundProgramBuilder::litvec(LitVecUid uid, LitUid literalUid) { 
    litvecs_[uid].emplace_back(lits_.erase(literalUid));
    return uid;
}

// }}}
// {{{ count aggregate elements (body/head)

CondLitVecUid TestNongroundProgramBuilder::condlitvec() { 
    return condlitvecs_.emplace();
}

CondLitVecUid TestNongroundProgramBuilder::condlitvec(CondLitVecUid uid, LitUid lit, LitVecUid litvec) {
    current_ << lits_.erase(lit);
    current_ << ":";
    print(litvecs_.erase(litvec), ",");
    condlitvecs_[uid].emplace_back(str());
    return uid; 
}

// }}}
// {{{ body aggregate elements

BdAggrElemVecUid TestNongroundProgramBuilder::bodyaggrelemvec() {
    return bodyaggrelemvecs_.emplace();
}

BdAggrElemVecUid TestNongroundProgramBuilder::bodyaggrelemvec(BdAggrElemVecUid uid, TermVecUid termvec, LitVecUid litvec) {
    print(termvecs_.erase(termvec), ",");
    current_ << ":";
    print(litvecs_.erase(litvec), ",");
    bodyaggrelemvecs_[uid].emplace_back(str());
    return uid;
}

// }}}
// {{{ head aggregate elements

HdAggrElemVecUid TestNongroundProgramBuilder::headaggrelemvec() {
    return headaggrelemvecs_.emplace();
}

HdAggrElemVecUid TestNongroundProgramBuilder::headaggrelemvec(HdAggrElemVecUid uid, TermVecUid termvec, LitUid lit, LitVecUid litvec) { 
    print(termvecs_.erase(termvec), ",");
    current_ << ":" << lits_.erase(lit) << ":";
    print(litvecs_.erase(litvec), ",");
    headaggrelemvecs_[uid].emplace_back(str());
    return uid;
}

// }}}
// {{{ bounds

BoundVecUid TestNongroundProgramBuilder::boundvec() { 
    return bounds_.emplace();
}

BoundVecUid TestNongroundProgramBuilder::boundvec(BoundVecUid uid, Relation rel, TermUid term) {
    std::stringstream ss;
    current_ << terms_.erase(term);
    ss << rel << current_.str();
    current_ << inv(rel);
    bounds_[uid].emplace_back(str(), ss.str());
    return uid;
}

// }}}
// {{{ heads

HdLitUid TestNongroundProgramBuilder::headlit(LitUid lit) {
    return heads_.emplace(lits_.erase(lit));
}

HdLitUid TestNongroundProgramBuilder::headaggr(Location const &, AggregateFunction fun, BoundVecUid boundvecuid, HdAggrElemVecUid headaggrelemvec) {
    print(fun, boundvecuid, headaggrelemvecs_.erase(headaggrelemvec));
    return heads_.emplace(str());
}

HdLitUid TestNongroundProgramBuilder::headaggr(Location const &, AggregateFunction fun, BoundVecUid boundvecuid, CondLitVecUid headaggrelemvec) {
    print(fun, boundvecuid, condlitvecs_.erase(headaggrelemvec));
    return heads_.emplace(str());
}

HdLitUid TestNongroundProgramBuilder::disjunction(Location const &, CondLitVecUid condlitvec) {
    print(condlitvecs_.erase(condlitvec), ";");
    return heads_.emplace(str());
}

// }}}
// {{{ bodies

BdLitVecUid TestNongroundProgramBuilder::body() {
    return bodies_.emplace();
}

BdLitVecUid TestNongroundProgramBuilder::bodylit(BdLitVecUid uid, LitUid lit) {
    bodies_[uid].emplace_back(lits_.erase(lit));
    return uid;
}

BdLitVecUid TestNongroundProgramBuilder::bodyaggr(BdLitVecUid uid, Location const &, NAF naf, AggregateFunction fun, BoundVecUid bounds, BdAggrElemVecUid bodyaggrelemvec) {
    print(naf);
    print(fun, bounds, bodyaggrelemvecs_.erase(bodyaggrelemvec));
    bodies_[uid].emplace_back(str());
    return uid;
}

BdLitVecUid TestNongroundProgramBuilder::bodyaggr(BdLitVecUid uid, Location const &, NAF naf, AggregateFunction fun, BoundVecUid bounds, CondLitVecUid bodyaggrelemvec) {
    print(naf);
    print(fun, bounds, condlitvecs_.erase(bodyaggrelemvec));
    bodies_[uid].emplace_back(str());
    return uid;
}

BdLitVecUid TestNongroundProgramBuilder::conjunction(BdLitVecUid uid, Location const &, LitUid head, LitVecUid litvec) {
    current_ << lits_.erase(head) << ":";
    print(litvecs_.erase(litvec), ",");
    bodies_[uid].emplace_back(str());
    return uid;
}

// }}}
// {{{ statements

void TestNongroundProgramBuilder::rule(Location const &, HdLitUid head) {
    current_ << heads_.erase(head) << ".";
    statements_.emplace_back(str());
}

void TestNongroundProgramBuilder::rule(Location const &, HdLitUid head, BdLitVecUid bodyuid) {
    current_ << heads_.erase(head);
    StringVec body(bodies_.erase(bodyuid));
    if (!body.empty()) {
        current_ << ":-";
        print(body, ";");
    }
    current_ << ".";
    statements_.emplace_back(str());
}

void TestNongroundProgramBuilder::define(Location const &, FWString name, TermUid value) {
    current_ << "#const " << *name << "=" << terms_.erase(value) << ".";
    statements_.emplace_back(str());
}

void TestNongroundProgramBuilder::optimize(Location const &, TermUid weight, TermUid priority, TermVecUid cond, BdLitVecUid body) {
    current_ << ":~";
    StringVec bd(bodies_.erase(body));
    print(bd, ";");
    current_ << ".[" << terms_.erase(weight) << "@" << terms_.erase(priority);
    StringVec cd(termvecs_.erase(cond));
    if (!cd.empty()) { 
        current_ << ",";
        print(cd, ",");
    }
    current_ << "]";
    statements_.emplace_back(str());
}

void TestNongroundProgramBuilder::showsig(Location const &, FWSignature x) {
    current_ << "#showsig " << x << ".";
    statements_.emplace_back(str());
}

void TestNongroundProgramBuilder::show(Location const &, TermUid t, BdLitVecUid bodyuid) {
    current_ << "#show " << terms_.erase(t);
    StringVec body(bodies_.erase(bodyuid));
    if (!body.empty()) {
        current_ << ":";
        print(body, ";");
    }
    current_ << ".";
}

// }}}
// {{{ auxiliary functions

void TestNongroundProgramBuilder::print(AggregateFunction fun, BoundVecUid boundvecuid, StringVec const &elems) {
    StringPairVec bounds(bounds_.erase(boundvecuid));
    auto it = bounds.begin(), end = bounds.end();
    if (it != end) { current_ << it->first; ++it; }
    current_ << fun << "{";
    print(elems, ";");
    current_ << "}";
    for (; it != end; ++it) { current_ << it->second; }
}

void TestNongroundProgramBuilder::print(NAF naf) {
    switch (naf) {
        case NAF::NOT:    { current_ << "#not "; break; }
        case NAF::NOTNOT: { current_ << "#not #not "; break; }
        case NAF::POS:    { break; }
    }
}

std::string TestNongroundProgramBuilder::str() {
    std::string str(current_.str());
    current_.str("");
    return str;
}

void TestNongroundProgramBuilder::print(StringVec const &vec, char const *sep) {
    auto it = vec.begin(), end = vec.end();
    if (it != end) {
        current_ << *it++;
        for (; it != end; ++it) { current_ << sep << *it; }
    }
}

void TestNongroundProgramBuilder::print(StringVecVec const &vec) {
    auto it = vec.begin(), end = vec.end();
    if (it != end) {
        print(*it++, ",");
        for (; it != end; ++it) {
            current_ << ";";
            print(*it, ",");
        }
    }
}

// }}}

std::string TestNongroundProgramBuilder::toString() {
    auto it = statements_.begin(), end = statements_.end();
    if (it != end) {
        current_ << *it;
        for (++it; it != end; ++it) { current_ << '\n' << *it; }
    }
    statements_.clear();
    return str();
}

TestNongroundProgramBuilder::~TestNongroundProgramBuilder() { }

// }}}
// {{{ definition of TestNongroundGrammar

void TestNongroundGrammar::setUp() { }

void TestNongroundGrammar::tearDown() { }

std::string TestNongroundGrammar::parse(std::string const &str) {
    TestNongroundProgramBuilder pb;
    NonGroundParser ngp(pb);
    ngp.pushStream("-", std::unique_ptr<std::istream>(new std::stringstream(str)));
    ngp.parse();
    return pb.toString();
}

void TestNongroundGrammar::test_term() {
    // testing constants
    CPPUNIT_ASSERT_EQUAL(std::string("p(x)."), parse("p(x)."));
    CPPUNIT_ASSERT_EQUAL(std::string("p(1)."), parse("p(1)."));
    CPPUNIT_ASSERT_EQUAL(std::string("p(\"1\")."), parse("p(\"1\")."));
    CPPUNIT_ASSERT_EQUAL(std::string("p(#inf)."), parse("p(#inf)."));
    CPPUNIT_ASSERT_EQUAL(std::string("p(#sup)."), parse("p(#sup)."));
    CPPUNIT_ASSERT_EQUAL(std::string("p(X)."), parse("p(X)."));
    CPPUNIT_ASSERT_EQUAL(std::string("p(_)."), parse("p(_)."));
    // absolute
    CPPUNIT_ASSERT_EQUAL(std::string("p(|1|)."), parse("p(|1|)."));
    CPPUNIT_ASSERT_EQUAL(std::string("p(|1;2;3|)."), parse("p(|1;2;3|)."));
    // lua function calls
    CPPUNIT_ASSERT_EQUAL(std::string("p(@f())."), parse("p(@f())."));
    CPPUNIT_ASSERT_EQUAL(std::string("p(@f(1))."), parse("p(@f(1))."));
    CPPUNIT_ASSERT_EQUAL(std::string("p(@f(1,2))."), parse("p(@f(1,2))."));
    CPPUNIT_ASSERT_EQUAL(std::string("p(@f(1,2,3))."), parse("p(@f(1,2,3))."));
    CPPUNIT_ASSERT_EQUAL(std::string("p(@f(;;;1,2;3))."), parse("p(@f(;;;1,2;3))."));
    // function symbols
    CPPUNIT_ASSERT_EQUAL(std::string("p(f)."), parse("p(f())."));
    CPPUNIT_ASSERT_EQUAL(std::string("p(f(1))."), parse("p(f(1))."));
    CPPUNIT_ASSERT_EQUAL(std::string("p(f(1,2))."), parse("p(f(1,2))."));
    CPPUNIT_ASSERT_EQUAL(std::string("p(f(1,2,3))."), parse("p(f(1,2,3))."));
    CPPUNIT_ASSERT_EQUAL(std::string("p(f(;;;1,2;3))."), parse("p(f(;;;1,2;3))."));
    // tuples / parenthesis
    CPPUNIT_ASSERT_EQUAL(std::string("p(())."), parse("p(())."));
    CPPUNIT_ASSERT_EQUAL(std::string("p((1))."), parse("p((1))."));
    CPPUNIT_ASSERT_EQUAL(std::string("p((1,2))."), parse("p((1,2))."));
    CPPUNIT_ASSERT_EQUAL(std::string("p((1,2,3))."), parse("p((1,2,3))."));
    CPPUNIT_ASSERT_EQUAL(std::string("p((;;;1,2;3))."), parse("p((;;;1,2;3))."));
    // unary operations
    CPPUNIT_ASSERT_EQUAL(std::string("p((-1))."), parse("p(-1)."));
    CPPUNIT_ASSERT_EQUAL(std::string("p((~1))."), parse("p(~1)."));
    // binary operations
    CPPUNIT_ASSERT_EQUAL(std::string("p((1**2))."), parse("p(1**2)."));
    CPPUNIT_ASSERT_EQUAL(std::string("p((1\\2))."), parse("p(1\\2)."));
    CPPUNIT_ASSERT_EQUAL(std::string("p((1/2))."), parse("p(1/2)."));
    CPPUNIT_ASSERT_EQUAL(std::string("p((1*2))."), parse("p(1*2)."));
    CPPUNIT_ASSERT_EQUAL(std::string("p((1-2))."), parse("p(1-2)."));
    CPPUNIT_ASSERT_EQUAL(std::string("p((1+2))."), parse("p(1+2)."));
    CPPUNIT_ASSERT_EQUAL(std::string("p((1&2))."), parse("p(1&2)."));
    CPPUNIT_ASSERT_EQUAL(std::string("p((1?2))."), parse("p(1?2)."));
    CPPUNIT_ASSERT_EQUAL(std::string("p((1^2))."), parse("p(1^2)."));
    CPPUNIT_ASSERT_EQUAL(std::string("p((1..2))."), parse("p(1..2)."));
    // precedence
    CPPUNIT_ASSERT_EQUAL(std::string("p(((1+2)+((3*4)*(5**(6**7)))))."), parse("p(1+2+3*4*5**6**7)."));
    // nesting
    CPPUNIT_ASSERT_EQUAL(std::string("p((f(1,(;;;1,(x..Y);3),3)+p(f(1,#sup,3))))."), parse("p(f(1,(;;;1,x..Y;3),3)+p(f(1,#sup,3)))."));
}

void TestNongroundGrammar::test_atomargs() {
    // identifier
    CPPUNIT_ASSERT_EQUAL(std::string("p."), parse("p."));
    // identifier LPAREN argvec RPAREN
    CPPUNIT_ASSERT_EQUAL(std::string("p."), parse("p()."));
    CPPUNIT_ASSERT_EQUAL(std::string("p(1)."), parse("p(1)."));
    CPPUNIT_ASSERT_EQUAL(std::string("p(;;;1,2;3,4)."), parse("p(;;;1,2;3,4)."));
    // identifier LPAREN MUL RPAREN
    CPPUNIT_ASSERT_EQUAL(std::string("p(_)."), parse("p(_)."));
    // identifier LPAREN MUL cpredargvec RPAREN
    CPPUNIT_ASSERT_EQUAL(std::string("p(_,1)."), parse("p(_,1)."));
    CPPUNIT_ASSERT_EQUAL(std::string("p(_,1,2)."), parse("p(_,1,2)."));
    CPPUNIT_ASSERT_EQUAL(std::string("p(_,1,2;2,3)."), parse("p(_,1,2;2,3)."));
    // identifier LPAREN MUL spredargvec RPAREN
    CPPUNIT_ASSERT_EQUAL(std::string("p(_;1)."), parse("p(_;1)."));
    CPPUNIT_ASSERT_EQUAL(std::string("p(_;1,2)."), parse("p(_;1,2)."));
    CPPUNIT_ASSERT_EQUAL(std::string("p(_;1,2;2,3)."), parse("p(_;1,2;2,3)."));
    // identifier LPAREN argvec COMMAMUL MUL RPAREN
    CPPUNIT_ASSERT_EQUAL(std::string("p(1,_)."), parse("p(1,_)."));
    CPPUNIT_ASSERT_EQUAL(std::string("p(1,2,_)."), parse("p(1,2,_)."));
    // identifier LPAREN argvec COMMAMUL MUL cpredargvec RPAREN
    CPPUNIT_ASSERT_EQUAL(std::string("p(1,2,_,3,_;_)."), parse("p(1,2,_,3,_;_)."));
    CPPUNIT_ASSERT_EQUAL(std::string("p(1,2,_,3,4,_;_)."), parse("p(1,2,_,3,4,_;_)."));
    // identifier LPAREN argvec COMMAMUL MUL spredargvec RPAREN
    CPPUNIT_ASSERT_EQUAL(std::string("p(1,2,_;3,_;_)."), parse("p(1,2,_;3,_;_)."));
    CPPUNIT_ASSERT_EQUAL(std::string("p(1,2,_;3,4,_;_)."), parse("p(1,2,_;3,4,_;_)."));
    // identifier LPAREN argvec SEMMUL MUL RPAREN
    CPPUNIT_ASSERT_EQUAL(std::string("p(1;_)."), parse("p(1;_)."));
    CPPUNIT_ASSERT_EQUAL(std::string("p(1,2;_)."), parse("p(1,2;_)."));
    // identifier LPAREN argvec SEMMUL MUL cpredargvec RPAREN
    CPPUNIT_ASSERT_EQUAL(std::string("p(1,2;_,3,_;_)."), parse("p(1,2;_,3,_;_)."));
    CPPUNIT_ASSERT_EQUAL(std::string("p(1,2;_,3,4,_;_)."), parse("p(1,2;_,3,4,_;_)."));
    // identifier LPAREN argvec SEMMUL MUL spredargvec RPAREN
    CPPUNIT_ASSERT_EQUAL(std::string("p(1,2;_;3,_;_)."), parse("p(1,2;_;3,_;_)."));
    CPPUNIT_ASSERT_EQUAL(std::string("p(1,2;_;3,4,_;_)."), parse("p(1,2;_;3,4,_;_)."));
}

void TestNongroundGrammar::test_literal() {
    // TRUE
    CPPUNIT_ASSERT_EQUAL(std::string("#true."), parse("#true."));
    // FALSE
    CPPUNIT_ASSERT_EQUAL(std::string("#false."), parse("#false."));
    // atom
    CPPUNIT_ASSERT_EQUAL(std::string("p."), parse("p."));
    CPPUNIT_ASSERT_EQUAL(std::string("-p."), parse("-p."));
    // NOT atom
    CPPUNIT_ASSERT_EQUAL(std::string("#not p."), parse("not p."));
    // NOT NOT atom
    CPPUNIT_ASSERT_EQUAL(std::string("#not #not p."), parse("not not p."));
    // term cmp term
    CPPUNIT_ASSERT_EQUAL(std::string("(1+2)!=3."), parse("1+2!=3."));
}

void TestNongroundGrammar::test_bdaggr() {
    // aggregatefunction
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-#count{}."), parse(":-{}."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-#sum{}."), parse(":-#sum{}."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-#sum+{}."), parse(":-#sum+{}."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-#min{}."), parse(":-#min{}."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-#max{}."), parse(":-#max{}."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-#count{}."), parse(":-#count{}."));
    // LBRACE altbodyaggrelemvec RBRACE
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-#count{p:;p:;p:p;p:p,q}."), parse(":-{p;p:;p:p;p:p,q}."));
    // aggregatefunction LBRACE bodyaggrelemvec RBRACE
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-#count{:;:p;:p,q}."), parse(":-#count{:;:p;:p,q}."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-#count{p:;p,q:}."), parse(":-#count{p;p,q}."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-#count{p:;p,q:}."), parse(":-#count{p:;p,q:}."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-#count{p:q,r,s}."), parse(":-#count{p:q,r,s}."));
    // test lower
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-1<=#count{}."), parse(":-1{}."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-1<#count{}."), parse(":-1<{}."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-1<=#count{}."), parse(":-1<={}."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-1>#count{}."), parse(":-1>{}."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-1>=#count{}."), parse(":-1>={}."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-1==#count{}."), parse(":-1=={}."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-1=#count{}."), parse(":-1={}."));
    // test upper
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-1>=#count{}."), parse(":-{}1."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-1<#count{}."), parse(":-{}>1."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-1<=#count{}."), parse(":-{}>=1."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-1>#count{}."), parse(":-{}<1."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-1>=#count{}."), parse(":-{}<=1."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-1==#count{}."), parse(":-{}==1."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-1=#count{}."), parse(":-{}=1."));
    // test both
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-1<=#count{}<=2."), parse(":-1{}2."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-1<#count{}<2."), parse(":-1<{}<2."));
}

void TestNongroundGrammar::test_hdaggr() {
    // aggregatefunction
    CPPUNIT_ASSERT_EQUAL(std::string("#count{}."), parse("{}."));
    CPPUNIT_ASSERT_EQUAL(std::string("#sum{}."), parse("#sum{}."));
    CPPUNIT_ASSERT_EQUAL(std::string("#sum+{}."), parse("#sum+{}."));
    CPPUNIT_ASSERT_EQUAL(std::string("#min{}."), parse("#min{}."));
    CPPUNIT_ASSERT_EQUAL(std::string("#max{}."), parse("#max{}."));
    CPPUNIT_ASSERT_EQUAL(std::string("#count{}."), parse("#count{}."));
    // LBRACE altheadaggrelemvec RBRACE
    CPPUNIT_ASSERT_EQUAL(std::string("#count{p:;p:;p:p;p:p,q}."), parse("{p;p:;p:p;p:p,q}."));
    // aggregatefunction LBRACE headaggrelemvec RBRACE
    CPPUNIT_ASSERT_EQUAL(std::string("#count{:p:;:q:r;:s:t,u}."), parse("#count{:p;:q:r;:s:t,u}."));
    CPPUNIT_ASSERT_EQUAL(std::string("#count{p:q:;r,s:t:}."), parse("#count{p:q;r,s:t}."));
    CPPUNIT_ASSERT_EQUAL(std::string("#count{p:q:;r,s:t:}."), parse("#count{p:q:;r,s:t:}."));
    CPPUNIT_ASSERT_EQUAL(std::string("#count{p:q:x,y,z;r,s:t:q,w,e}."), parse("#count{p:q:x,y,z;r,s:t:q,w,e}."));
    // test lower
    CPPUNIT_ASSERT_EQUAL(std::string("1<=#count{}."), parse("1{}."));
    CPPUNIT_ASSERT_EQUAL(std::string("1<#count{}."), parse("1<{}."));
    CPPUNIT_ASSERT_EQUAL(std::string("1<=#count{}."), parse("1<={}."));
    CPPUNIT_ASSERT_EQUAL(std::string("1>#count{}."), parse("1>{}."));
    CPPUNIT_ASSERT_EQUAL(std::string("1>=#count{}."), parse("1>={}."));
    CPPUNIT_ASSERT_EQUAL(std::string("1==#count{}."), parse("1=={}."));
    CPPUNIT_ASSERT_EQUAL(std::string("1=#count{}."), parse("1={}."));
    // test upper
    CPPUNIT_ASSERT_EQUAL(std::string("1>=#count{}."), parse("{}1."));
    CPPUNIT_ASSERT_EQUAL(std::string("1<#count{}."), parse("{}>1."));
    CPPUNIT_ASSERT_EQUAL(std::string("1<=#count{}."), parse("{}>=1."));
    CPPUNIT_ASSERT_EQUAL(std::string("1>#count{}."), parse("{}<1."));
    CPPUNIT_ASSERT_EQUAL(std::string("1>=#count{}."), parse("{}<=1."));
    CPPUNIT_ASSERT_EQUAL(std::string("1==#count{}."), parse("{}==1."));
    CPPUNIT_ASSERT_EQUAL(std::string("1=#count{}."), parse("{}=1."));
    // test both
    CPPUNIT_ASSERT_EQUAL(std::string("1<=#count{}<=2."), parse("1{}2."));
    CPPUNIT_ASSERT_EQUAL(std::string("1<#count{}<2."), parse("1<{}<2."));
}

void TestNongroundGrammar::test_conjunction() {
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-a:."), parse(":-a:."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-a:b."), parse(":-a:b."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-a:b,c."), parse(":-a:b,c."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-a:b,c;x."), parse(":-a:b,c;x."));
}

void TestNongroundGrammar::test_disjunction() {
    // Note: first disjunction element moves to the end (parsing related)
    // literal COLON litvec
    CPPUNIT_ASSERT_EQUAL(std::string("a."), parse("a."));
    CPPUNIT_ASSERT_EQUAL(std::string("a:b."), parse("a:b."));
    CPPUNIT_ASSERT_EQUAL(std::string("a:b,c."), parse("a:b,c."));
    // literal COMMA disjunctionsep literal optcondition
    CPPUNIT_ASSERT_EQUAL(std::string("b:;c:;a:."), parse("a,b,c."));
    CPPUNIT_ASSERT_EQUAL(std::string("b:;c:;d:;a:."), parse("a,b;c;d."));
    CPPUNIT_ASSERT_EQUAL(std::string("b:;c:d,e;a:."), parse("a,b,c:d,e."));
    CPPUNIT_ASSERT_EQUAL(std::string("b:d,e;c:;a:."), parse("a,b:d,e;c."));
    // literal SEM disjunctionsep literal optcondition
    CPPUNIT_ASSERT_EQUAL(std::string("b:;c:;a:."), parse("a;b,c."));
    CPPUNIT_ASSERT_EQUAL(std::string("b:;c:;d:;a:."), parse("a;b;c;d."));
    CPPUNIT_ASSERT_EQUAL(std::string("b:;c:d,e;a:."), parse("a;b,c:d,e."));
    CPPUNIT_ASSERT_EQUAL(std::string("b:d,e;c:;a:."), parse("a;b:d,e;c."));
    // literal COLON litvec SEM disjunctionsep literal optcondition
    CPPUNIT_ASSERT_EQUAL(std::string("c:;a:."), parse("a;c."));
    CPPUNIT_ASSERT_EQUAL(std::string("c:;a:x."), parse("a:x;c."));
    CPPUNIT_ASSERT_EQUAL(std::string("c:;a:x,y."), parse("a:x,y;c."));
    CPPUNIT_ASSERT_EQUAL(std::string("b:;c:;a:x,y."), parse("a:x,y;b,c."));
    CPPUNIT_ASSERT_EQUAL(std::string("b:;c:;d:;a:x,y."), parse("a:x,y;b;c;d."));
    CPPUNIT_ASSERT_EQUAL(std::string("b:;c:d,e;a:x,y."), parse("a:x,y;b,c:d,e."));
    CPPUNIT_ASSERT_EQUAL(std::string("b:d,e;c:;a:x,y."), parse("a:x,y;b:d,e;c."));
}

void TestNongroundGrammar::test_rule() {
    // body literal 
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-a;x."), parse(":-a,x."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-a;x."), parse(":-a;x."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-a;x."), parse(":-a,x."));
    // body aggregate
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-#count{};#count{}."), parse(":-{},{}."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-#count{};#count{}."), parse(":-{};{}."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-#not #count{};#not #count{}."), parse(":-not{},not{}."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-#not #count{};#not #count{}."), parse(":-not{};not{}."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-#not #not #count{};#not #not #count{}."), parse(":-not not{},not not{}."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-#not #not #count{};#not #not #count{}."), parse(":-not not{};not not{}."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-#not #not nott<=#count{};#not #not nott<=#count{}."), parse(":-not not nott{},not not nott{}."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-#not #not nott<=#count{};#not #not nott<=#count{}."), parse(":-not not nott{};not not nott{}."));
    // conjunction
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-a:;b:."), parse(":-a:;b:."));
    // head literal
    CPPUNIT_ASSERT_EQUAL(std::string("a."), parse("a."));
    // head aggregate
    CPPUNIT_ASSERT_EQUAL(std::string("#count{}."), parse("{}."));
    CPPUNIT_ASSERT_EQUAL(std::string("nott<=#count{}."), parse("nott{}."));
    // disjunction
    CPPUNIT_ASSERT_EQUAL(std::string("b:;a:."), parse("a,b."));
    // rules
    CPPUNIT_ASSERT_EQUAL(std::string("a."), parse("a."));
    CPPUNIT_ASSERT_EQUAL(std::string("a:-b."), parse("a:-b."));
    CPPUNIT_ASSERT_EQUAL(std::string("a:-b;c."), parse("a:-b,c."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false."), parse(":-."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-b."), parse(":-b."));
    CPPUNIT_ASSERT_EQUAL(std::string("#false:-b;c."), parse(":-b,c."));
}

void TestNongroundGrammar::test_define() {
    CPPUNIT_ASSERT_EQUAL(std::string("#const a=10."), parse("#const a=10."));
}

void TestNongroundGrammar::test_optimize() {
    CPPUNIT_ASSERT_EQUAL(std::string(":~p(X,Y).[X@Y,a]"), parse(":~ p(X,Y). [X@Y,a]"));
    CPPUNIT_ASSERT_EQUAL(std::string(":~p(X,Y);r;s.[X@0]"), parse(":~ p(X,Y),r,s. [X]"));
    CPPUNIT_ASSERT_EQUAL(std::string(":~p(X,Y);s.[X@Y,a]\n:~q(Y).[Y@f]\n:~.[1@0]"), parse("#minimize { X@Y,a : p(X,Y),s; Y@f : q(Y); 1 }."));
    CPPUNIT_ASSERT_EQUAL(std::string(""), parse("#minimize { }."));
    CPPUNIT_ASSERT_EQUAL(std::string(":~p(X,Y);r.[(-X)@Y,a]\n:~q(Y).[(-Y)@f]\n:~.[(-2)@0]"), parse("#maximize { X@Y,a : p(X,Y), r; Y@f : q(Y); 2 }."));
    CPPUNIT_ASSERT_EQUAL(std::string(""), parse("#maximize { }."));
}

void TestNongroundGrammar::test_show() {
    CPPUNIT_ASSERT_EQUAL(std::string("#showsig p/1."), parse("#show p/1."));
    CPPUNIT_ASSERT_EQUAL(std::string("#showsig p/1."), parse("#show p  /  1."));
    CPPUNIT_ASSERT_EQUAL(std::string("#showsig -p/1."), parse("#show -p/1."));
    CPPUNIT_ASSERT_EQUAL(std::string("#show ((-p)/(-1))."), parse("#show -p/-1."));
    CPPUNIT_ASSERT_EQUAL(std::string("#show X:p(X);1<=#count{q(X):p(X)}."), parse("#show X:p(X), 1 { q(X):p(X) }."));
}

TestNongroundGrammar::~TestNongroundGrammar() { }

// }}}

CPPUNIT_TEST_SUITE_REGISTRATION(TestNongroundGrammar);

} } } // namespace Test Input Gringo

