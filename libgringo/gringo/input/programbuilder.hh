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

#ifndef _GRINGO_INPUT_PROGRAMBUILDER_HH
#define _GRINGO_INPUT_PROGRAMBUILDER_HH

#include <gringo/flyweight.hh>
#include <gringo/locatable.hh>
#include <gringo/value.hh>

#include <gringo/base.hh>

#include <vector>
#include <memory>

namespace Gringo { namespace Output { struct OutputBase; } }

namespace Gringo { namespace Input {

// {{{ declaration of unique ids of program elements

enum TermUid : unsigned { };
enum TermVecUid : unsigned { };
enum TermVecVecUid : unsigned { };
enum LitUid : unsigned { };
enum LitVecUid : unsigned { };
enum CondLitVecUid : unsigned { };
enum BdAggrElemVecUid : unsigned { };
enum HdAggrElemVecUid : unsigned { };
enum HdLitUid : unsigned { };
enum BdLitVecUid : unsigned { };
enum BoundVecUid : unsigned { };

// }}}
// {{{ declaration of INongroundProgramBuilder

class INongroundProgramBuilder {
public:
    // {{{ terms
    virtual TermUid term(Location const &loc, Value val) = 0;                                // constant
    virtual TermUid term(Location const &loc, FWString name) = 0;                            // variable
    virtual TermUid term(Location const &loc, UnOp op, TermUid a) = 0;                       // unary operation
    virtual TermUid term(Location const &loc, UnOp op, TermVecUid a) = 0;                    // unary operation
    virtual TermUid term(Location const &loc, BinOp op, TermUid a, TermUid b) = 0;           // binary operation
    virtual TermUid term(Location const &loc, TermUid a, TermUid b) = 0;                     // dots
    virtual TermUid term(Location const &loc, FWString name, TermVecVecUid b, bool lua) = 0; // function or lua function
    // }}}
    // {{{ term vectors
    virtual TermVecUid termvec() = 0;
    virtual TermVecUid termvec(TermVecUid uid, TermUid term) = 0;
    // }}}
    // {{{ term vector vectors
    virtual TermVecVecUid termvecvec() = 0;
    virtual TermVecVecUid termvecvec(TermVecVecUid uid, TermVecUid termvecUid) = 0;
    // }}}
    // {{{ literals
    virtual LitUid boollit(Location const &loc, bool type) = 0;
    virtual LitUid predlit(Location const &loc, NAF naf, bool neg, FWString name, TermVecVecUid argvecvecUid) = 0;
    virtual LitUid rellit(Location const &loc, Relation rel, TermUid termUidLeft, TermUid termUidRight) = 0;
    // }}}
    // {{{ literal vectors
    virtual LitVecUid litvec() = 0;
    virtual LitVecUid litvec(LitVecUid uid, LitUid literalUid) = 0;
    // }}}
    // {{{ conditional literals
    virtual CondLitVecUid condlitvec() = 0;
    virtual CondLitVecUid condlitvec(CondLitVecUid uid, LitUid lit, LitVecUid litvec) = 0;
    // }}}
    // {{{ body aggregate elements
    virtual BdAggrElemVecUid bodyaggrelemvec() = 0;
    virtual BdAggrElemVecUid bodyaggrelemvec(BdAggrElemVecUid uid, TermVecUid termvec, LitVecUid litvec) = 0;
    // }}}
    // {{{ head aggregate elements
    virtual HdAggrElemVecUid headaggrelemvec() = 0;
    virtual HdAggrElemVecUid headaggrelemvec(HdAggrElemVecUid uid, TermVecUid termvec, LitUid lit, LitVecUid litvec) = 0;
    // }}}
    // {{{ bounds
    virtual BoundVecUid boundvec() = 0;
    virtual BoundVecUid boundvec(BoundVecUid uid, Relation rel, TermUid term) = 0;
    // }}}
    // {{{ heads
    virtual HdLitUid headlit(LitUid lit) = 0;
    virtual HdLitUid headaggr(Location const &loc, AggregateFunction fun, BoundVecUid bounds, HdAggrElemVecUid headaggrelemvec) = 0;
    virtual HdLitUid headaggr(Location const &loc, AggregateFunction fun, BoundVecUid bounds, CondLitVecUid headaggrelemvec) = 0;
    virtual HdLitUid disjunction(Location const &loc, CondLitVecUid condlitvec) = 0;
    // }}}
    // {{{ bodies
    virtual BdLitVecUid body() = 0;
    virtual BdLitVecUid bodylit(BdLitVecUid body, LitUid bodylit) = 0;
    virtual BdLitVecUid bodyaggr(BdLitVecUid body, Location const &loc, NAF naf, AggregateFunction fun, BoundVecUid bounds, BdAggrElemVecUid bodyaggrelemvec) = 0;
    virtual BdLitVecUid bodyaggr(BdLitVecUid body, Location const &loc, NAF naf, AggregateFunction fun, BoundVecUid bounds, CondLitVecUid bodyaggrelemvec) = 0;
    virtual BdLitVecUid conjunction(BdLitVecUid body, Location const &loc, LitUid head, LitVecUid litvec) = 0;
    // }}}
    // {{{ statements
    virtual void rule(Location const &loc, HdLitUid head) = 0;
    virtual void rule(Location const &loc, HdLitUid head, BdLitVecUid body) = 0;
    virtual void define(Location const &loc, FWString name, TermUid value) = 0;
    virtual void optimize(Location const &loc, TermUid weight, TermUid priority, TermVecUid cond, BdLitVecUid body) = 0;
    virtual void showsig(Location const &loc, FWSignature x) = 0;
    virtual void show(Location const &loc, TermUid t, BdLitVecUid body) = 0;
    virtual void incr(Location const &loc, FWString name) = 0;
    // }}}
    virtual ~INongroundProgramBuilder() { }
};

// }}}
// {{{ declaration of NongroundProgramBuilder

class Program;
struct Statement;
struct BodyAggregate;
struct HeadAggregate;
struct Literal;
using ULit = std::unique_ptr<Literal>;
using ULitVec = std::vector<ULit>;
using UHeadAggr = std::unique_ptr<HeadAggregate>;
using UBodyAggr = std::unique_ptr<BodyAggregate>;
using UStm = std::unique_ptr<Statement>;
using BoundVec = std::vector<Bound>;
using BodyAggrElem = std::pair<UTermVec, ULitVec>;
using BodyAggrElemVec = std::vector<BodyAggrElem>;
using CondLit = std::pair<ULit, ULitVec>;
using CondLitVec = std::vector<CondLit>;
using HeadAggrElem = std::tuple<UTermVec, ULit, ULitVec>;
using HeadAggrElemVec = std::vector<HeadAggrElem>;
using UBodyAggrVec = std::vector<UBodyAggr>;

class NongroundProgramBuilder : public INongroundProgramBuilder {
public:
    NongroundProgramBuilder(Program &prg, Output::OutputBase &out, Defines &defs);
    // {{{ terms
    virtual TermUid term(Location const &loc, Value val);                                // constant
    virtual TermUid term(Location const &loc, FWString name);                            // variable
    virtual TermUid term(Location const &loc, UnOp op, TermUid a);                       // unary operation
    virtual TermUid term(Location const &loc, UnOp op, TermVecUid a);                    // unary operation
    virtual TermUid term(Location const &loc, BinOp op, TermUid a, TermUid b);           // binary operation
    virtual TermUid term(Location const &loc, TermUid a, TermUid b);                     // assignment
    virtual TermUid term(Location const &loc, FWString name, TermVecVecUid b, bool lua); // function or lua function
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
    // {{{ conditional literal vectors
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
    virtual void incr(Location const &loc, FWString name);
    // }}}
    virtual ~NongroundProgramBuilder();
    
    //! Definitions added with defaultDef=false override defintions added with defaultDef=true.
    bool defaultDef = true;
private:

    typedef Indexed<UTerm, TermUid> Terms;
    typedef Indexed<UTermVec, TermVecUid> TermVecs;
    typedef Indexed<UTermVecVec, TermVecVecUid> TermVecVecs;
    typedef Indexed<ULit, LitUid> Lits;
    typedef Indexed<ULitVec, LitVecUid> LitVecs;
    typedef Indexed<BodyAggrElemVec, BdAggrElemVecUid> BodyAggrElemVecs;
    typedef Indexed<CondLitVec, CondLitVecUid> CondLitVecs;
    typedef Indexed<HeadAggrElemVec, HdAggrElemVecUid> HeadAggrElemVecs;
    typedef Indexed<UBodyAggrVec, BdLitVecUid> Bodies;
    typedef Indexed<UHeadAggr, HdLitUid> Heads;
    typedef std::vector<UStm> Statements;
    typedef Indexed<BoundVec, BoundVecUid> Bounds;
    typedef std::unordered_map<FWString, Term::SVal> VarVals;

    Terms                   terms_;
    TermVecs                termvecs_;
    TermVecVecs             termvecvecs_;
    Lits                    lits_;
    LitVecs                 litvecs_;
    BodyAggrElemVecs        bodyaggrelemvecs_;
    HeadAggrElemVecs        headaggrelemvecs_;
    CondLitVecs             condlitvecs_;
    Bounds                  bounds_;
    Bodies                  bodies_;
    Heads                   heads_;
    VarVals                 vals_;
    Program                &prg_;
    Output::OutputBase &out;
    Defines                &defs_;
};

// }}}

} } // namespace Input Gringo

#endif // _GRINGO_PROGRAMBUILDER_HH
