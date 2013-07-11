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

#ifndef _GRINGO_INPUT_AGGREGATE_HH
#define _GRINGO_INPUT_AGGREGATE_HH

#include <gringo/input/literal.hh>
#include <gringo/safetycheck.hh>
#include <gringo/ground/statement.hh>
#include <list>

namespace Gringo { namespace Input {

// {{{ declaration of auxiliary types

typedef std::pair<ULit, ULitVec> CondLit;
typedef std::vector<CondLit> CondLitVec;
typedef std::pair<UTermVec, ULitVec> BodyAggrElem;
typedef std::vector<BodyAggrElem> BodyAggrElemVec;

typedef std::tuple<UTermVec, ULit, ULitVec> HeadAggrElem;
typedef std::vector<HeadAggrElem> HeadAggrElemVec;

struct BodyAggregate;
typedef std::unique_ptr<BodyAggregate> UBodyAggr;
typedef std::vector<UBodyAggr> UBodyAggrVec;

struct HeadAggregate;
typedef std::unique_ptr<HeadAggregate> UHeadAggr;
typedef std::vector<UHeadAggr> UHeadAggrVec;

// }}}

// {{{ declaration of AssignLevel

struct AssignLevel {
    typedef std::unordered_map<FWString, unsigned> BoundSet;

    void add(VarTermBoundVec &vars);
    AssignLevel &subLevel();
    void assignLevels();
    void assignLevels(unsigned level, BoundSet const &bound);
    virtual ~AssignLevel();

    std::list<AssignLevel> childs;
    std::unordered_map<FWString, std::vector<VarTerm*>> occurr;
};

// }}}
// {{{ declaration of CheckLevel

struct CheckLevel {
    struct Ent {
        bool operator<(Ent const &) const;
    };
    using SC     = SafetyChecker<VarTerm*, Ent>;
    using VarMap = std::unordered_map<FWString, SC::VarNode *>;

    CheckLevel(Location const &loc, Printable const &p);
    CheckLevel(CheckLevel &&);
    SC::VarNode &var(VarTerm &var);
    bool check();
    ~CheckLevel();

    Location         loc;
    Printable const &p;
    SC               dep;
    SC::EntNode     *current = 0;
    VarMap           vars;
};
using ChkLvlVec = std::vector<CheckLevel>;

// }}}
// {{{ declaration of ToGroundArg

using CreateLit     = std::function<void (Ground::ULitVec &, bool)>;
using CreateStm     = std::function<Ground::UStm (Ground::ULitVec &&)>;
using CreateStmVec  = std::vector<CreateStm>;
using CreateBody    = std::pair<CreateLit, CreateStmVec>;
using CreateBodyVec = std::vector<CreateBody>;
using CreateHead    = std::pair<CreateStm, CreateBody>;

struct ToGroundArg {
    ToGroundArg(PredDomMap &domains);
    FWString newId(bool increment = true);
    UTermVec getGlobal(VarTermBoundVec const &vars);
    UTerm newId(UTermVec &&global, Location const &loc, bool increment = true);
    template <class T>
    UTerm newId(T const &x) {
        VarTermBoundVec vars;
        x.collect(vars);
        return newId(getGlobal(vars), x.loc());
    }
    ~ToGroundArg();

    unsigned    auxNames  = 0;
    PredDomMap &domains;
};

// }}}

// {{{ declaration of BodyAggregate

struct BodyAggregate : Printable, Hashable, Locatable, Clonable<BodyAggregate>, Comparable<BodyAggregate> {
    //! Unpool the aggregate and aggregate elements.
    virtual void unpool(UBodyAggrVec &x) = 0;
    //! Simplify the aggregate and aggregate elements.
    //! \pre Must be called after unpool.
    virtual void simplify(Literal::ProjectionMap &project, Term::DotsMap &dots, unsigned &auxNum) = 0;
    //! Assign levels to variables using the VarCollector.
    //! \pre Must be called after simplify.
    virtual void assignLevels(AssignLevel &lvl) = 0;
    virtual bool check(ChkLvlVec &lvl) const = 0;
    //! Rewrite arithmetics.
    //! \pre Requires variables assigned to levels.
    virtual void rewriteArithmetics(Term::ArithmeticsMap &arith, Literal::AssignVec &assign, unsigned &auxNum) = 0;
    //! Rewrite aggregates.
    //! Separates assignment aggregates from ordinary bounds, and
    //! transforms literal aggregates into tuple aggregates.
    //! Returns false iff the aggregates must be removed.
    //! Argument aggr contains the rewritten aggregates.
    virtual bool rewriteAggregates(UBodyAggrVec &aggr) = 0;
    //! Returns true if the aggregate is an assignment aggregate.
    //! \note Does not consider relation literals.
    virtual void removeAssignment() = 0;
    virtual bool isAssignment() const = 0;
    //! Collects all variables occuring in the aggregate.
    //! Occurrences bound by the aggregate are marked as such
    //! (occurrences bound in nested scopes are not marked).
    virtual void collect(VarTermBoundVec &vars) const = 0;
    virtual bool hasPool() const = 0;
    virtual void replace(Defines &dx) = 0;
    virtual CreateBody toGround(ToGroundArg &x, Ground::UStmVec &stms) const = 0;
    virtual ~BodyAggregate() { }
};

// }}}
// {{{ declaration of TupleBodyAggregate

struct TupleBodyAggregate : BodyAggregate {
    TupleBodyAggregate(NAF naf, AggregateFunction fun, BoundVec &&bounds, BodyAggrElemVec &&elems);
    virtual bool rewriteAggregates(UBodyAggrVec &aggr);
    virtual bool isAssignment() const;
    virtual void removeAssignment();
    virtual void collect(VarTermBoundVec &vars) const;
    virtual bool operator==(BodyAggregate const &other) const;
    virtual void print(std::ostream &out) const;
    virtual size_t hash() const;
    virtual TupleBodyAggregate *clone() const;
    virtual void unpool(UBodyAggrVec &x);
    virtual void simplify(Literal::ProjectionMap &project, Term::DotsMap &dots, unsigned &auxNum);
    virtual void assignLevels(AssignLevel &lvl);
    virtual void rewriteArithmetics(Term::ArithmeticsMap &arith, Literal::AssignVec &assign, unsigned &auxNum);
    virtual bool hasPool() const;
    virtual bool check(ChkLvlVec &lvl) const;
    virtual void replace(Defines &dx);
    virtual CreateBody toGround(ToGroundArg &x, Ground::UStmVec &stms) const;
    virtual ~TupleBodyAggregate();

    NAF naf;
    AggregateFunction fun;
    BoundVec bounds;
    BodyAggrElemVec elems;
};

// }}}
// {{{ declaration of LitBodyAggregate

struct LitBodyAggregate : BodyAggregate {
    LitBodyAggregate(NAF naf, AggregateFunction fun, BoundVec &&bounds, CondLitVec &&elems);
    virtual bool rewriteAggregates(UBodyAggrVec &aggr);
    virtual bool isAssignment() const;
    virtual void removeAssignment();
    virtual void collect(VarTermBoundVec &vars) const;
    virtual bool operator==(BodyAggregate const &other) const;
    virtual void print(std::ostream &out) const;
    virtual size_t hash() const;
    virtual LitBodyAggregate *clone() const;
    virtual void unpool(UBodyAggrVec &x);
    virtual void simplify(Literal::ProjectionMap &project, Term::DotsMap &dots, unsigned &auxNum);
    virtual void assignLevels(AssignLevel &lvl);
    virtual void rewriteArithmetics(Term::ArithmeticsMap &arith, Literal::AssignVec &assign, unsigned &auxNum);
    virtual bool hasPool() const;
    virtual bool check(ChkLvlVec &lvl) const;
    virtual void replace(Defines &dx);
    virtual CreateBody toGround(ToGroundArg &x, Ground::UStmVec &stms) const;
    virtual ~LitBodyAggregate();

    NAF naf;
    AggregateFunction fun;
    BoundVec bounds;
    CondLitVec elems;
};

// }}}
// {{{ declaration of Conjunction

struct Conjunction : BodyAggregate {
    Conjunction(ULit &&head, ULitVec &&cond);
    virtual bool rewriteAggregates(UBodyAggrVec &aggr);
    virtual bool isAssignment() const;
    virtual void removeAssignment();
    virtual void collect(VarTermBoundVec &vars) const;
    virtual bool operator==(BodyAggregate const &other) const;
    virtual void print(std::ostream &out) const;
    virtual size_t hash() const;
    virtual Conjunction *clone() const;
    virtual void unpool(UBodyAggrVec &x);
    virtual void simplify(Literal::ProjectionMap &project, Term::DotsMap &dots, unsigned &auxNum);
    virtual void assignLevels(AssignLevel &lvl);
    virtual void rewriteArithmetics(Term::ArithmeticsMap &arith, Literal::AssignVec &assign, unsigned &auxNum);
    virtual bool hasPool() const;
    virtual bool check(ChkLvlVec &lvl) const;
    virtual void replace(Defines &dx);
    virtual CreateBody toGround(ToGroundArg &x, Ground::UStmVec &stms) const;
    virtual ~Conjunction();

    ULit head;
    ULitVec cond;
};

// }}}
// {{{ declaration of SimpleBodyLiteral

struct SimpleBodyLiteral : BodyAggregate {
    SimpleBodyLiteral(ULit &&lit);
    virtual Location const &loc() const;
    virtual bool rewriteAggregates(UBodyAggrVec &aggr);
    virtual void removeAssignment();
    virtual bool isAssignment() const;
    virtual void collect(VarTermBoundVec &vars) const;
    virtual bool operator==(BodyAggregate const &other) const;
    virtual void print(std::ostream &out) const;
    virtual size_t hash() const;
    virtual SimpleBodyLiteral *clone() const;
    virtual void unpool(UBodyAggrVec &x);
    virtual void simplify(Literal::ProjectionMap &project, Term::DotsMap &dots, unsigned &auxNum);
    virtual void assignLevels(AssignLevel &lvl);
    virtual void rewriteArithmetics(Term::ArithmeticsMap &arith, Literal::AssignVec &assign, unsigned &auxNum);
    virtual bool hasPool() const;
    virtual bool check(ChkLvlVec &lvl) const;
    virtual void replace(Defines &dx);
    virtual CreateBody toGround(ToGroundArg &x, Ground::UStmVec &stms) const;
    virtual ~SimpleBodyLiteral();

    ULit lit;
};

// }}}

// {{{ declaration of HeadAggregate

struct HeadAggregate : Printable, Hashable, Locatable, Clonable<HeadAggregate>, Comparable<HeadAggregate> {

    //! Unpool the aggregate and aggregate elements.
    virtual void unpool(UHeadAggrVec &x) = 0;
    //! Simplify the aggregate and aggregate elements.
    //! \pre Must be called after unpool.
    virtual void simplify(Literal::ProjectionMap &project, Term::DotsMap &dots, unsigned &auxNum) = 0;
    //! Assign levels to variables using the VarCollector.
    //! \pre Must be called after simplify.
    virtual void assignLevels(AssignLevel &lvl) = 0;
    virtual bool check(ChkLvlVec &lvl) const = 0;
    //! Rewrite arithmetics.
    //! \pre Requires variables assigned to levels.
    virtual void rewriteArithmetics(Term::ArithmeticsMap &arith, unsigned &auxNum) = 0;
    virtual UHeadAggr rewriteAggregates(UBodyAggrVec &aggr) = 0;
    //! Collects all variables occuring in the aggregate.
    //! Occurrences bound by the aggregate are marked as such
    //! (occurrences bound in nested scopes are not marked).
    virtual void collect(VarTermBoundVec &vars) const = 0;
    virtual bool hasPool() const = 0;
    virtual void replace(Defines &dx) = 0;
    virtual CreateHead toGround(ToGroundArg &x) const = 0;
    virtual Value isEDB() const;
    virtual ~HeadAggregate() { }
};

// }}}
// {{{ declaration of TupleHeadAggregate

struct TupleHeadAggregate : HeadAggregate {
    TupleHeadAggregate(AggregateFunction fun, BoundVec &&bounds, HeadAggrElemVec &&elems);
    virtual UHeadAggr rewriteAggregates(UBodyAggrVec &aggr);
    virtual void collect(VarTermBoundVec &vars) const;
    virtual bool operator==(HeadAggregate const &other) const;
    virtual void print(std::ostream &out) const;
    virtual size_t hash() const;
    virtual TupleHeadAggregate *clone() const;
    virtual void unpool(UHeadAggrVec &x);
    virtual void simplify(Literal::ProjectionMap &project, Term::DotsMap &dots, unsigned &auxNum);
    virtual void assignLevels(AssignLevel &lvl);
    virtual void rewriteArithmetics(Term::ArithmeticsMap &arith, unsigned &auxNum);
    virtual bool hasPool() const;
    virtual bool check(ChkLvlVec &lvl) const;
    virtual void replace(Defines &dx);
    virtual CreateHead toGround(ToGroundArg &x) const;
    virtual ~TupleHeadAggregate();

    AggregateFunction fun;
    BoundVec bounds;
    HeadAggrElemVec elems;
};

// }}}
// {{{ declaration of LitHeadAggregate

struct LitHeadAggregate : HeadAggregate {
    LitHeadAggregate(AggregateFunction fun, BoundVec &&bounds, CondLitVec &&elems);
    virtual UHeadAggr rewriteAggregates(UBodyAggrVec &aggr);
    virtual void collect(VarTermBoundVec &vars) const;
    virtual bool operator==(HeadAggregate const &other) const;
    virtual void print(std::ostream &out) const;
    virtual size_t hash() const;
    virtual LitHeadAggregate *clone() const;
    virtual void unpool(UHeadAggrVec &x);
    virtual void simplify(Literal::ProjectionMap &project, Term::DotsMap &dots, unsigned &auxNum);
    virtual void assignLevels(AssignLevel &lvl);
    virtual void rewriteArithmetics(Term::ArithmeticsMap &arith, unsigned &auxNum);
    virtual bool hasPool() const;
    virtual bool check(ChkLvlVec &lvl) const;
    virtual void replace(Defines &dx);
    virtual CreateHead toGround(ToGroundArg &x) const;
    virtual ~LitHeadAggregate();

    AggregateFunction fun;
    BoundVec bounds;
    CondLitVec elems;
};

// }}}
// {{{ declaration of Disjunction

struct Disjunction : HeadAggregate {
    Disjunction(CondLitVec &&elems);
    virtual UHeadAggr rewriteAggregates(UBodyAggrVec &aggr);
    virtual void collect(VarTermBoundVec &vars) const;
    virtual bool operator==(HeadAggregate const &other) const;
    virtual void print(std::ostream &out) const;
    virtual size_t hash() const;
    virtual Disjunction *clone() const;
    virtual void unpool(UHeadAggrVec &x);
    virtual void simplify(Literal::ProjectionMap &project, Term::DotsMap &dots, unsigned &auxNum);
    virtual void assignLevels(AssignLevel &lvl);
    virtual void rewriteArithmetics(Term::ArithmeticsMap &arith, unsigned &auxNum);
    virtual bool hasPool() const;
    virtual bool check(ChkLvlVec &lvl) const;
    virtual void replace(Defines &dx);
    virtual CreateHead toGround(ToGroundArg &x) const;
    virtual ~Disjunction();

    CondLitVec elems;
};

// }}}
// {{{ declaration of SimpleHeadLiteral

struct SimpleHeadLiteral : HeadAggregate {
    SimpleHeadLiteral(ULit &&lit);
    virtual Location const &loc() const;
    virtual UHeadAggr rewriteAggregates(UBodyAggrVec &aggr);
    virtual void collect(VarTermBoundVec &vars) const;
    virtual bool operator==(HeadAggregate const &other) const;
    virtual void print(std::ostream &out) const;
    virtual size_t hash() const;
    virtual SimpleHeadLiteral *clone() const;
    virtual void unpool(UHeadAggrVec &x);
    virtual void simplify(Literal::ProjectionMap &project, Term::DotsMap &dots, unsigned &auxNum);
    virtual void assignLevels(AssignLevel &lvl);
    virtual void rewriteArithmetics(Term::ArithmeticsMap &arith, unsigned &auxNum);
    virtual bool hasPool() const;
    virtual bool check(ChkLvlVec &lvl) const;
    virtual void replace(Defines &dx);
    virtual CreateHead toGround(ToGroundArg &x) const;
    virtual Value isEDB() const;
    virtual ~SimpleHeadLiteral();

    ULit lit;
};

// }}}

} } // namespace Input Gringo

GRINGO_HASH(Input::BodyAggregate)
GRINGO_HASH(Input::HeadAggregate)

#endif // _GRINGO_INPUT_AGGREGATE_HH
